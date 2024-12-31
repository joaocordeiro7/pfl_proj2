% filepath: /home/joaocordeiro77/pfl/PFL_TP2_T10_STAQS_7/src/game.pl

% ===================== MODULE IMPORTS =====================
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).

% ===================== GAME SETUP AND ENTRY POINT =====================

% Entry point for the game. Displays the menu and instructions.
play :-
    write('Welcome to STAQS!'), nl,
    write('Instructions:'), nl,
    write('1. During the PLACEMENT phase, type "X Y" to place your piece.'), nl,
    write('   Example: "1 1" places your piece at the top-left corner of the board.'), nl,
    write('2. During the MOVEMENT phase, type "FromX FromY ToX ToY" to move a stack.'), nl,
    write('   Example: "1 1 2 2" moves the stack from (1, 1) to (2, 2).'), nl,
    write('3. Follow the prompts and enjoy the game!'), nl, nl,
    write('Choose cell padding size:'), nl,
    write('1. Small'), nl,
    write('2. Medium'), nl,
    write('3. Large'), nl,
    write('Choose an option (1-3): '), nl,
    get_clean_char(SizeOptionChar),
    char_to_number(SizeOptionChar, SizeOption),
    (map_cell_size(SizeOption, Config) ->
        write('1. Human vs Human'), nl,
        write('2. Human vs AI'), nl,
        write('3. AI vs AI'), nl,
        write('Choose an option (1-3): '), nl,
        get_clean_char(GameModeChar),
        handle_option(GameModeChar, Config)
    ;
        write('Invalid padding option. Please try again.'), nl,
        play
    ).

% Handles the player's menu choice.
handle_option('1', Config) :-
    write('Starting Human vs Human...'), nl,
    initial_state(Config, GameState),
    game_loop(GameState).
handle_option('2', Config) :-
    write('Starting Human vs AI...'), nl,
    initial_state(Config, GameState),
    game_loop(GameState).
handle_option('3', Config) :-
    write('Starting AI vs AI...'), nl,
    initial_state(Config, GameState),
    game_loop(GameState).
handle_option(_, _) :-
    write('Invalid game mode. Please try again.'), nl,
    play.

% ===================== GAME INITIALIZATION =====================

% Sets up the initial game board with neutral pieces.
initial_state(Config, game(Board, blue, placement, Config)) :-
    create_board(5, Board).

% Creates a 5x5 board with neutral pieces.
create_board(Size, Board) :-
    length(Board, Size),
    maplist(create_row(Size), Board).

% Creates a row of neutral pieces.
create_row(Size, Row) :-
    length(Row, Size),
    maplist(=([neutral]), Row).

% ===================== GAME DISPLAY UTILITIES =====================

% Displays the game board with labeled rows and columns.
display_game(game(Board, CurrentPlayer, Phase, Config)) :-
    nl,
    write('Current Player: '), write(CurrentPlayer), nl,
    write('Phase: '), write(Phase), nl,
    get_cell_padding(Config, HPad, VPad, Separator), % Retrieve padding and separator from config
    reverse(Board, InvertedBoard), % Reverse rows for lower-left corner
    write('  '), write(Separator), nl,
    print_board(InvertedBoard, 5, HPad, Separator, VPad),
    write('     1     2     3     4     5'), nl.

% Prints the board row by row with row numbers.
print_board([], _, _, _, _).
print_board([Row|Rest], RowNum, HPad, Separator, VPad) :-
    print_vpad(VPad, HPad), % Print vertical padding above
    format('~d |', [RowNum]), % Print row number
    print_row(Row, HPad),
    nl,
    print_vpad(VPad, HPad), % Print vertical padding below
    write('  '), write(Separator), nl,
    NextRowNum is RowNum - 1,
    print_board(Rest, NextRowNum, HPad, Separator, VPad).

% Prints a single row, adjusting for cell size.
print_row([], _).
print_row([Stack|Rest], HPad) :-
    write(HPad), % Add horizontal padding
    print_stack_content(Stack),
    write(HPad), % Add horizontal padding
    write('|'),
    print_row(Rest, HPad).

% Prints vertical padding lines (1 line above and 1 line below for medium/large sizes).
print_vpad(0, _).
print_vpad(N, HPad) :-
    N > 0,
    write('  |'), % Add the left edge of the board
    print_vpad_row(5, HPad), % Number of columns
    nl,
    N1 is N - 1,
    print_vpad(N1, HPad).

% Prints vertical padding for one row.
print_vpad_row(0, _).
print_vpad_row(N, HPad) :-
    write(HPad),
    write('     |'),
    N1 is N - 1,
    print_vpad_row(N1, HPad).

% Formats and prints a single stack with padding.
print_stack_content([]) :-
    write('   ').
print_stack_content([neutral|Rest]) :-
    length([neutral|Rest], Height),
    format(' n~d ', [Height]).
print_stack_content([blue|Rest]) :-
    length([blue|Rest], Height),
    format(' B~d ', [Height]).
print_stack_content([white|Rest]) :-
    length([white|Rest], Height),
    format(' W~d ', [Height]).

% ===================== GAME LOOP =====================

% Main game loop handling both placement and movement phases.
game_loop(game(Board, Player, Phase, Config)) :-
    display_game(game(Board, Player, Phase, Config)),
    (Phase = placement ->
        handle_placement(game(Board, Player, Phase, Config), NewGameState),
        game_loop(NewGameState)
    ;
        handle_movement(game(Board, Player, Phase, Config), NewGameState),
        (game_over(NewGameState, Winner) ->
            write('Game over! Winner: '), write(Winner), nl
        ;
            game_loop(NewGameState)
        )
    ).

% ===================== GAME PHASES =====================

% Handles the placement phase with user instructions.
handle_placement(game(Board, Player, placement, Config), game(NewBoard, NextPlayer, NewPhase, Config)) :-
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
        handle_placement(game(Board, Player, placement, Config), game(NewBoard, NextPlayer, NewPhase, Config))
    ).

% Handles the movement phase with user instructions.
handle_movement(game(Board, Player, movement, Config), game(NewBoard, NextPlayer, movement, Config)) :-
    write('MOVE YOUR STACK'), nl,
    write('Instructions: Choose a move (FromX FromY ToX ToY) to move a stack to an adjacent cell.'), nl,
    read_move(FromX, FromY, ToX, ToY),
    (valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) ->
        execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
        next_player(Player, NextPlayer)
    ;
        write('Invalid move! Ensure it follows the rules. Try again.'), nl,
        handle_movement(game(Board, Player, movement, Config), game(NewBoard, NextPlayer, movement, Config))
    ).

% ===================== MOVE VALIDATION =====================

% Validates moves for a given player.
valid_moves(game(Board, Player, _), Moves) :-
    findall(move(FromX, FromY, ToX, ToY), (
        between(1, 5, FromX),
        between(1, 5, FromY),
        piece_at(Board, FromX, FromY, Player),
        get_adjacent_cells(FromX, FromY, AdjacentCells),
        member((ToX, ToY), AdjacentCells),
        is_valid_destination(Board, ToX, ToY)
    ), Moves).

% Checks if a move is valid.
valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) :-
    piece_at(Board, FromX, FromY, Player),
    get_adjacent_cells(FromX, FromY, AdjacentCells),
    member((ToX, ToY), AdjacentCells),
    is_valid_destination(Board, ToX, ToY).


% Validate destination.
is_valid_destination(Board, X, Y) :-
    within_bounds(X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    length(Stack, Height),
    Height < 5.

% Get all valid adjacent cells as a list
get_adjacent_cells(X, Y, AdjacentCells) :-
    findall((ToX, ToY), (
        (ToX is X + 1, ToY = Y);
        (ToX is X - 1, ToY = Y);
        (ToX = X, ToY is Y + 1);
        (ToX = X, ToY is Y - 1)
    ), PossibleCells),
    findall((ValidX, ValidY), 
        (member((ValidX, ValidY), PossibleCells), within_bounds(ValidX, ValidY)), 
        AdjacentCells).

% ===================== STACK OPERATIONS =====================

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

% Update move execution with coordinate transformation
execute_move(Board, FromX, FromY, ToX, ToY, NewBoard) :-
    transform_coordinates(FromX, FromY, BoardFromX, BoardFromY),
    transform_coordinates(ToX, ToY, BoardToX, BoardToY),
    get_stack(Board, BoardFromX, BoardFromY, Stack),
    remove_stack(Board, BoardFromX, BoardFromY, TempBoard),
    add_to_stack(TempBoard, BoardToX, BoardToY, Stack, NewBoard),
    write(' from ('), write(FromX), write(','), write(FromY),
    write(') to ('), write(ToX), write(','), write(ToY), write(')'), nl.

% ===================== PLAYER MANAGEMENT =====================

% Switches to the next player.
next_player(blue, white).
next_player(white, blue).

% ===================== GAME STATE CHECKS =====================

% Checks if the game is over (no valid moves for both players).
game_over(game(Board, _, movement), Winner) :-
    valid_moves(game(Board, blue, movement), BlueMoves),
    valid_moves(game(Board, white, movement), WhiteMoves),
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

% ===================== UTILITY PREDICATES =====================

% Update coordinate transformation for moves
transform_coordinates(X, Y, BoardX, BoardY) :-
    BoardX = X,
    BoardY is 6 - Y.

% Map size option to cell padding and separators.
map_cell_size(1, Config) :-
    Config = [cell_padding(''), vertical_padding(0), separator('+----+----+----+----+----+')]. % Small: no changes
map_cell_size(2, Config) :-
    Config = [cell_padding(' '), vertical_padding(1), separator('+------+------+------+------+------+')]. % Medium
map_cell_size(3, Config) :-
    Config = [cell_padding('   '), vertical_padding(2), separator('+-------+-------+-------+-------+-------+')]. % Large

% Retrieve cell padding and separator from configuration.
get_cell_padding([cell_padding(Pad), vertical_padding(VPad), separator(Sep)], Pad, VPad, Sep).

within_bounds(X, Y) :-
    X > 0, X =< 5,
    Y > 0, Y =< 5.

get_clean_char(Char) :-
    get_char(Char),
    skip_line.

char_to_number(Char, Number) :-
    atom_codes(Char, [Code]),
    Number is Code - 48.

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
    transform_coordinates(X, Y, BoardX, BoardY),
    within_bounds(BoardX, BoardY),
    nth1(BoardY, Board, Row, RestRows),
    nth1(BoardX, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(BoardX, NewRow, [Player, neutral|RestStack], RestCells),
    nth1(BoardY, NewBoard, NewRow, RestRows).

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

% Checks if a piece is at a specific position
piece_at(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, [First|_]),
    First = Player.