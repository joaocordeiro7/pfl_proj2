:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).

% Entry point for the game. Displays the menu and instructions.
play :-
    write('Welcome to STAQS!'), nl,
    write('Instructions:'), nl,
    write('1. During the PLACEMENT phase, type "X Y" to place your piece.'), nl,
    write('   Example: "1 1" places your piece at the top-left corner of the board.'), nl,
    write('2. During the MOVEMENT phase, type "FromX FromY ToX ToY" to move a stack.'), nl,
    write('   Example: "1 1 2 2" moves the stack from (1, 1) to (2, 2).'), nl,
    write('3. Follow the prompts and enjoy the game!'), nl, nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs AI'), nl,
    write('3. AI vs AI'), nl,
    write('Choose an option (1-3): '), nl,
    get_clean_char(Option),
    handle_option(Option).


% Reads a clean character, ignoring newlines and clearing the buffer.
get_clean_char(Char) :-
    get_char(Char),
    skip_line.

% Handles the players menu choice.
handle_option('1') :-
    write('Starting Human vs Human...'), nl,
    initial_state(game(Board, blue, placement)),
    game_loop(game(Board, blue, placement)).
handle_option('2') :-
    write('Starting Human vs AI...'), nl,
    initial_state(game(Board, blue, placement)),
    game_loop(game(Board, blue, placement)).
handle_option('3') :-
    write('Starting AI vs AI...'), nl,
    initial_state(game(Board, blue, placement)),
    game_loop(game(Board, blue, placement)).
handle_option(_) :-
    write('Invalid option. Please try again.'), nl,
    play.

% Sets up the initial game board with neutral pieces.
initial_state(game(Board, blue, placement)) :-
    create_board(5, Board).

% Creates a 5x5 board with neutral pieces.
create_board(Size, Board) :-
    length(Board, Size),
    maplist(create_row(Size), Board).

% Creates a row of neutral pieces.
create_row(Size, Row) :-
    length(Row, Size),
    maplist(=([neutral]), Row).

% Displays the game board with labeled rows and columns.
display_game(game(Board, CurrentPlayer, Phase)) :-
    nl,
    write('Current Player: '), write(CurrentPlayer), nl,
    write('Phase: '), write(Phase), nl,
    write('   1   2   3   4   5'), nl,
    write('  +---+---+---+---+---+'), nl,
    print_board(Board, 1),
    nl.

% Prints the board row by row with row numbers.
print_board([], _).
print_board([Row|Rest], RowNum) :-
    format('~d |', [RowNum]),   % Print row number
    print_row(Row),
    nl,
    write('  +---+---+---+---+---+'), nl,
    NextRowNum is RowNum + 1,
    print_board(Rest, NextRowNum).

% Prints a single row, showing stacks in each cell.
print_row([]).
print_row([Stack|Rest]) :-
    print_stack(Stack),
    write('|'),
    print_row(Rest).

% Formats and prints a single stack.
print_stack([]) :- write('   ').
print_stack([neutral|Rest]) :-
    length([neutral|Rest], Height),
    format(' n~d', [Height]).
print_stack([blue|Rest]) :-
    length([blue|Rest], Height),
    format(' B~d', [Height]).
print_stack([white|Rest]) :-
    length([white|Rest], Height),
    format(' W~d', [Height]).

% Main game loop handling both placement and movement phases.
game_loop(game(Board, Player, Phase)) :-
    display_game(game(Board, Player, Phase)),
    (game_over(game(Board, Player, Phase), Winner) ->
        write('Game over! Winner: '), write(Winner), nl
    ;
        (Phase = placement ->
            handle_placement(game(Board, Player, Phase), NewGameState)
        ;
            handle_movement(game(Board, Player, Phase), NewGameState)
        ),
        game_loop(NewGameState)
    ).

% Handles the placement phase with user instructions.
handle_placement(game(Board, Player, placement), game(NewBoard, NextPlayer, NewPhase)) :-
    write('PLACE YOUR PIECE'), nl,
    write('Instructions: Choose a position (X Y) to place your piece on top of a neutral stack.'), nl,
    read_placement(X, Y),
    (valid_placement(Board, X, Y) ->
        add_player_piece(Board, X, Y, Player, NewBoard),
        (all_pieces_placed(NewBoard) ->
            NewPhase = movement,
            write('All pieces placed. Transitioning to movement phase!'), nl
        ;
            NewPhase = placement
        ),
        next_player(Player, NextPlayer)
    ;
        write('Invalid placement! Ensure it is on a neutral stack. Try again.'), nl,
        handle_placement(game(Board, Player, placement), game(NewBoard, NextPlayer, NewPhase))
    ).

% Handles the movement phase with user instructions.
handle_movement(game(Board, Player, movement), game(NewBoard, NextPlayer, movement)) :-
    write('MOVE YOUR STACK'), nl,
    write('Instructions: Choose a move (FromX FromY ToX ToY) to move a stack to an adjacent cell.'), nl,
    read_move(FromX, FromY, ToX, ToY),
    (valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) ->
        execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
        next_player(Player, NextPlayer)
    ;
        write('Invalid move! Ensure it follows the rules. Try again.'), nl,
        handle_movement(game(Board, Player, movement), game(NewBoard, NextPlayer, movement))
    ).

% Convert character to number
char_to_number(Char, Number) :-
    atom_codes(Char, [Code]),
    Number is Code - 48.  % ASCII conversion

% Read coordinates for placement
read_placement(X, Y) :-
    write('Enter X coordinate (1-5): '), 
    get_clean_char(XChar),
    char_to_number(XChar, X),
    write('Enter Y coordinate (1-5): '), 
    get_clean_char(YChar),
    char_to_number(YChar, Y).

% Read coordinates for movement
read_move(FromX, FromY, ToX, ToY) :-
    write('From X (1-5): '), 
    get_clean_char(FXChar),
    char_to_number(FXChar, FromX),
    write('From Y (1-5): '), 
    get_clean_char(FYChar),
    char_to_number(FYChar, FromY),
    write('To X (1-5): '), 
    get_clean_char(TXChar),
    char_to_number(TXChar, ToX),
    write('To Y (1-5): '), 
    get_clean_char(TYChar),
    char_to_number(TYChar, ToY).

add_player_piece(Board, X, Y, Player, NewBoard) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, Stack, RestCells),
    write('Debug: Current stack at (X, Y) = '), write(Stack), nl,
    Stack = [neutral|RestStack], % Ensure the stack starts with neutral.
    nth1(X, NewRow, [Player, neutral|RestStack], RestCells),
    nth1(Y, NewBoard, NewRow, RestRows),
    write('Debug: New stack at (X, Y) = '), write([Player, neutral|RestStack]), nl.

% Checks if a placement is valid.
valid_placement(Board, X, Y) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, [neutral|_]).

% Checks if all pieces are placed.
all_pieces_placed(Board) :-
    count_pieces(Board, blue, BlueCount),
    count_pieces(Board, white, WhiteCount),
    write('Debug: Blue pieces placed = '), write(BlueCount), nl,
    write('Debug: White pieces placed = '), write(WhiteCount), nl,
    BlueCount =:= 4,
    WhiteCount =:= 4.

% Counts pieces of a specific color.
count_pieces(Board, Color, Count) :-
    findall(1, (
        member(Row, Board), 
        member(Stack, Row), 
        member(Color, Stack) % Count all occurrences of the color in the stack
    ), Pieces),
    length(Pieces, Count).

% Checks if the game is over (no valid moves for both players).
game_over(game(Board, _, movement), Winner) :-
    valid_moves(game(Board, blue, movement), BlueMoves),
    valid_moves(game(Board, white, movement), WhiteMoves),
    write('Debug: White moves = '), write(WhiteMoves), nl,
    BlueMoves = [], % No moves for blue
    WhiteMoves = [], % No moves for white
    determine_winner(Board, Winner).
game_over(_, _) :- fail. % Continue the game if there are valid moves.

% Determines the winner based on the tallest stack.
determine_winner(Board, Winner) :-
    findall(Height-Player, tallest_stack(Board, Player, Height), Heights),
    keysort(Heights, SortedHeights),
    reverse(SortedHeights, [Tallest-Player|Rest]),
    (Rest = [Tallest-_|_] -> Winner = tie ; Winner = Player).

% Finds the tallest stack for a player.
tallest_stack(Board, Player, Height) :-
    member(Row, Board),
    member(Stack, Row),
    last(Stack, Player),
    length(Stack, Height).

% Checks if a move is valid.
valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) :-
    piece_at(Board, FromX, FromY, Player),
    write('Debug: Found piece for '), write(Player), write(' at ('), write(FromX), write(', '), write(FromY), write(').'), nl,
    adjacent_cell(FromX, FromY, ToX, ToY),
    is_valid_destination(Board, ToX, ToY).


piece_at(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    (last(Stack, Player) -> 
        write('Debug: Stack at ('), write(X), write(', '), write(Y), write(') is controlled by '), write(Player), nl
    ;
        fail).

% Get all valid adjacent cells as a list
get_adjacent_cells(X, Y, AdjacentCells) :-
    % Generate all possible adjacent coordinates
    findall((ToX, ToY), (
        % Right
        (ToX is X + 1, ToY = Y);
        % Left
        (ToX is X - 1, ToY = Y);
        % Down
        (ToX = X, ToY is Y + 1);
        % Up
        (ToX = X, ToY is Y - 1)
    ), PossibleCells),
    % Filter only valid cells within bounds
    findall((ValidX, ValidY), 
        (member((ValidX, ValidY), PossibleCells), within_bounds(ValidX, ValidY)), 
        AdjacentCells),
    write('Debug: Adjacent cells = '), write(AdjacentCells), nl.

% Validate destination.
is_valid_destination(Board, X, Y) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    length(Stack, Height),
    Height < 5, % Max height is 5
    write('Debug: Destination valid at ('), write(X), write(', '), write(Y), write(').'), nl.

% Replace old adjacent_cell with new version using list
valid_moves(game(Board, Player, _), Moves) :-
    findall(move(FromX, FromY, ToX, ToY), (
        between(1, 5, FromX),
        between(1, 5, FromY),
        piece_at(Board, FromX, FromY, Player),
        get_adjacent_cells(FromX, FromY, AdjacentCells),
        member((ToX, ToY), AdjacentCells),
        is_valid_destination(Board, ToX, ToY)
    ), Moves).

% Executes a valid move.
execute_move(Board, FromX, FromY, ToX, ToY, NewBoard) :-
    get_stack(Board, FromX, FromY, Stack),
    remove_stack(Board, FromX, FromY, TempBoard),
    add_to_stack(TempBoard, ToX, ToY, Stack, NewBoard).

% Get stack from position
get_stack(Board, X, Y, Stack) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack).

% Remove stack from position
remove_stack(Board, X, Y, NewBoard) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, _, RestCells),
    nth1(X, NewRow, [], RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).

% Add stack to position
add_to_stack(Board, X, Y, Stack, NewBoard) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, ExistingStack, RestCells),
    append(Stack, ExistingStack, NewStack),
    nth1(X, NewRow, NewStack, RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).


% Switches to the next player.
next_player(blue, white).
next_player(white, blue).

% Checks if within board bounds.
within_bounds(X, Y) :-
    X > 0, X =< 5,
    Y > 0, Y =< 5.
