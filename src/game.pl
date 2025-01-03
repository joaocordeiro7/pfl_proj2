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
    write('   Example: "1 1" places your piece at the lower-left corner of the board.'), nl,
    write('2. During the MOVEMENT phase, type "FromX FromY ToX ToY" to move a stack.'), nl,
    write('   Example: "1 1 2 2" moves the stack from (1, 1) to (2, 2).'), nl,
    write('3. Follow the prompts and enjoy the game!'), nl, nl,
    write('Choose board size (N for NxN grid): '), nl,
    get_clean_char(BoardSizeChar),
    char_to_number(BoardSizeChar, BoardSize),
    validate_board_size(BoardSize),
    write('1. Human vs Human'), nl,
    write('2. Human vs PC'), nl,
    write('3. PC vs PC'), nl,
    write('Choose an option (1-3): '), nl,
    get_clean_char(GameModeChar),
    handle_option(GameModeChar, BoardSize). 

% Validates the input for board size
validate_board_size(BoardSize) :-
    BoardSize >= 3, !.
validate_board_size(_) :-
    write('Invalid size. The board must be at least 3x3.'), nl, nl, nl,
    play.

% Handles Human vs Human
handle_option('1', BoardSize) :-
    write('Enter name for Player 1 (Blue): '), 
    read_line(user_input, Player1NameCodes), atom_codes(Player1Name, Player1NameCodes),
    write('Enter name for Player 2 (White): '), 
    read_line(user_input, Player2NameCodes), atom_codes(Player2Name, Player2NameCodes),
    GameConfig = game_config(human(Player1Name, blue), human(Player2Name, white), BoardSize),
    initial_state(GameConfig, GameState),
    game_loop(GameState, 0).

% Handles Human vs PC
handle_option('2', BoardSize) :-
    write('Enter name for Player 1 (Blue): '), 
    read_line(user_input, Player1NameCodes), atom_codes(Player1Name, Player1NameCodes),
    write('Choose difficulty level for PC (1-2): '), nl,
    get_clean_char(LevelChar),
    char_to_number(LevelChar, Level),
    GameConfig = game_config(human(Player1Name, blue), pc(Level, white), BoardSize),
    initial_state(GameConfig, GameState),
    game_loop(GameState, 0).

% Handles PC vs PC
handle_option('3', BoardSize) :-
    write('Choose difficulty level for PC 1 (1-2): '), nl,
    get_clean_char(Level1Char),
    char_to_number(Level1Char, Level1),
    write('Choose difficulty level for PC 2 (1-2): '), nl,
    get_clean_char(Level2Char),
    char_to_number(Level2Char, Level2),
    GameConfig = game_config(pc(Level1, blue), pc(Level2, white), BoardSize),
    initial_state(GameConfig, GameState),
    game_loop(GameState, 0).

% ===================== GAME INITIALIZATION =====================

% Sets up the initial game state based on the game configuration
initial_state(game_config(Player1, Player2, BoardSize), game(Board, Player1, placement, BoardSize, game_config(Player1, Player2, BoardSize))) :-
    create_board(BoardSize, Board).

% Creates a board with neutral pieces.
create_board(Size, Board) :-
    length(Board, Size),
    maplist(create_row(Size), Board).

% Creates a row of neutral pieces.
create_row(Size, Row) :-
    length(Row, Size),
    maplist(=([neutral]), Row).

% ===================== GAME DISPLAY UTILITIES =====================

% Displays the game board with labeled rows and columns.
display_game(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig)) :-
    nl,
    display_current_player(CurrentPlayer),
    write('Phase: '), write(Phase), nl, nl,
    reverse(Board, InvertedBoard), % Reverse rows for lower-left corner
    print_separator(BoardSize),
    print_board(InvertedBoard, BoardSize, BoardSize),
    print_column_headers(BoardSize).

% Displays the current player information.
display_current_player(human(Name, _)) :-
    write('Current Player: '), write(Name), nl.
display_current_player(pc(_, _)) :-
    write('Current Player: PC'), nl.

% Prints the board row by row with row numbers.
print_board([], _, _).
print_board([Row|Rest], RowNum, BoardSize) :-
    format('~d |', [RowNum]), % Print row number
    print_row(Row),
    nl,
    print_separator(BoardSize),
    NextRowNum is RowNum - 1,
    print_board(Rest, NextRowNum, BoardSize).

% Prints a single row.
print_row([]).
print_row([Stack|Rest]) :-
    print_stack_content(Stack),
    write('|'),
    print_row(Rest).

% Prints column headers.
print_column_headers(Size) :-
    write('    '),
    numlist(1, Size, Columns),
    maplist(print_column_header, Columns),
    nl.

% Prints a single column header.
print_column_header(Col) :-
    format(' ~d   ', [Col]).

% Prints a separator line.
print_separator(Size) :-
    write('  +'),
    numlist(1, Size, Columns),
    maplist(print_separator_segment, Columns),
    nl.

% Prints a single separator segment.
print_separator_segment(_) :-
    write('----+').

% Formats and prints a single stack.
print_stack_content([]) :-
    write('    ').
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

game_loop(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig), PassCount) :-
    % Extract players from the game config
    GameConfig = game_config(Player1, Player2, _),
    % Display the game state
    display_game(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig)),
    process_phase(Phase, game(Board, CurrentPlayer, Phase, BoardSize, GameConfig), PassCount).

% Handles the current game phase
process_phase(placement, GameState, _) :-
    handle_placement(GameState, NewGameState),
    game_loop(NewGameState, 0).
process_phase(movement, GameState, PassCount) :-
    handle_movement(GameState, PassCount).
    
% ===================== GAME PHASES =====================

handle_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), 
                 game(NewBoard, NextPlayer, NewPhase, BoardSize, GameConfig)) :-
    write('PLACE YOUR PIECE'), nl,
    write('Instructions: Choose a position (X Y) to place your piece on top of a neutral stack.'), nl,
    read_placement(X, Y),
    valid_placement(Board, X, Y),
    add_player_piece(Board, X, Y, CurrentPlayer, NewBoard),
    pieces_per_player(BoardSize, PiecesPerPlayer),
    determine_phase(NewBoard, PiecesPerPlayer, NewPhase),
    next_player(CurrentPlayer, GameConfig, NextPlayer).
handle_placement(CurrentGameState, ResultGameState) :-
    write('Invalid placement! Ensure it is on a neutral stack. Try again.'), nl,
    handle_placement(CurrentGameState, ResultGameState).

% Determines if the placement phase has ended.
determine_phase(Board, PiecesPerPlayer, movement) :-
    all_pieces_placed(Board, PiecesPerPlayer),
    write('All pieces placed. Transitioning to movement phase!'), nl.
determine_phase(_, _, placement).

% Handles the movement phase with user instructions.
handle_movement(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount) :-
    valid_moves(game(Board, CurrentPlayer, movement), Moves),
    handle_movement_moves(Moves, game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount).

handle_movement_moves([], game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount) :-
    CurrentPlayer = human(Name, _),
    write(Name), write(' has no valid moves. Passing turn...'), nl,
    next_player(CurrentPlayer, GameConfig, NextPlayer),
    NewPassCount is PassCount + 1,
    handle_pass_end(Board, NextPlayer, movement, BoardSize, GameConfig, NewPassCount).
handle_movement_moves(Moves, GameState, _) :-
    GameState = game(_, CurrentPlayer, _, _, _),
    handle_player_move(Moves, CurrentPlayer, GameState).

handle_pass_end(Board, NextPlayer, Phase, BoardSize, GameConfig, 2) :-
    determine_winner(Board, GameConfig, Winner),
    write('Game over! Winner: '), write(Winner), nl.
handle_pass_end(Board, NextPlayer, Phase, BoardSize, GameConfig, PassCount) :-
    game_loop(game(Board, NextPlayer, Phase, BoardSize, GameConfig), PassCount).

handle_player_move(Moves, pc(Level), game(Board, CurrentPlayer, movement, BoardSize, GameConfig)) :-
    choose_move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Level, Move),
    write('PC chooses move: '), write(Move), nl,
    move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Move, NewGameState, GameConfig),
    game_loop(NewGameState, 0).
handle_player_move(_, human(_, _), game(Board, CurrentPlayer, movement, BoardSize, GameConfig)) :-
    write('MOVE YOUR STACK'), nl,
    write('Instructions: Choose a move (FromX FromY ToX ToY) to move a stack to an adjacent cell.'), nl,
    read_move(FromX, FromY, ToX, ToY),
    write('Debug: Checking move ('), write(FromX), write(', '), write(FromY),
    write(') to ('), write(ToX), write(', '), write(ToY), write(')'), nl,
    move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), move(FromX, FromY, ToX, ToY), NewGameState, GameConfig),
    game_loop(NewGameState, 0).
handle_player_move(_, _, GameState) :-
    write('Invalid move! Ensure it follows the rules. Try again.'), nl,
    handle_movement(GameState, 0).


% ===================== MOVE VALIDATION =====================

% Validates moves for a given player.
valid_moves(game(Board, Player, _), Moves) :-
    player_color(Player, Color),
    findall(move(FromX, FromY, ToX, ToY), (
        between(1, 5, FromX),
        between(1, 5, FromY),
        piece_at(Board, FromX, FromY, Color),
        get_adjacent_cells(Board, FromX, FromY, AdjacentCells),
        member((ToX, ToY), AdjacentCells),
        is_valid_destination(Board, ToX, ToY)
    ), Moves),
    % Debug output
    Player = human(Name, _),
    write('Debug: Valid moves for '), write(Name), write(': '), write(Moves), nl.

% Checks if a move is valid.
valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) :-
    player_color(Player, Color),
    within_bounds(Board, FromX, FromY),
    piece_at(Board, FromX, FromY, Color),
    get_adjacent_cells(Board, FromX, FromY, AdjacentCells),
    member((ToX, ToY), AdjacentCells),
    is_valid_destination(Board, ToX, ToY).

% Determine the player's color
player_color(human(_, Color), Color).
player_color(pc(_, Color), Color).

is_valid_destination(Board, X, Y) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    length(Stack, Height),
    Height = 1. % Ensure the target is a neutral stack.

% Get all valid adjacent cells as a list
get_adjacent_cells(Board, X, Y, AdjacentCells) :-
    findall((ToX, ToY), adjacent_cell(X, Y, ToX, ToY), PossibleCells),
    include(valid_cell(Board), PossibleCells, AdjacentCells).

% Define adjacent cells in all possible directions
adjacent_cell(X, Y, ToX, ToY) :- ToX is X + 1, ToY is Y.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X - 1, ToY is Y.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X, ToY is Y + 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X, ToY is Y - 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X - 1, ToY is Y - 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X - 1, ToY is Y + 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X + 1, ToY is Y - 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X + 1, ToY is Y + 1.

% Check if a cell is within the board bounds
valid_cell(Board, (X, Y)) :-
    within_bounds(Board, X, Y).

% ===================== STACK OPERATIONS =====================

% Validates and executes a move, returning the new game state
move(game(Board, Player, Phase, BoardSize, GameConfig), move(FromX, FromY, ToX, ToY), game(NewBoard, NextPlayer, Phase, BoardSize, GameConfig), GameConfig) :-
    valid_move(Board, Player, move(FromX, FromY, ToX, ToY)),
    execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
    next_player(Player, GameConfig, NextPlayer).


% Get stack from position
get_stack(Board, X, Y, Stack) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack).

% Remove stack from position
remove_stack(Board, X, Y, NewBoard) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, _, RestCells),
    nth1(X, NewRow, [], RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).

% Add stack to position
add_to_stack(Board, X, Y, Stack, NewBoard) :-
    within_bounds(Board, X, Y),
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

% ===================== PC MOVE GENERATION =====================

% Chooses a move for the computer player based on the difficulty level
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(GameState, 2, Move) :-
    valid_moves(GameState, Moves),
    findall(Value-M, (member(M, Moves), move(GameState, M, NewGameState, GameConfig), value(NewGameState, Player, Value)), ValuedMoves),
    keysort(ValuedMoves, SortedMoves),
    last(SortedMoves, _-Move).

% ===================== PLAYER MANAGEMENT =====================

% Switches to the next player based on the game configuration.
next_player(human(Name1, Color1), game_config(human(Name1, Color1), human(Name2, Color2), _), human(Name2, Color2)).
next_player(human(Name2, Color2), game_config(human(Name1, Color1), human(Name2, Color2), _), human(Name1, Color1)).

next_player(pc(Level1, Color1), game_config(pc(Level1, Color1), pc(Level2, Color2), _), pc(Level2, Color2)).
next_player(pc(Level2, Color2), game_config(pc(Level1, Color1), pc(Level2, Color2), _), pc(Level1, Color1)).

next_player(human(Name, Color1), game_config(human(Name, Color1), pc(Level, Color2), _), pc(Level, Color2)).
next_player(pc(Level, Color2), game_config(human(Name, Color1), pc(Level, Color2), _), human(Name, Color1)).

% ===================== GAME STATE CHECKS =====================

% Checks if the game is over (no valid moves for both players).
game_over(game(Board, _, movement, _, GameConfig), Winner) :-
    % Check if both players have no valid moves
    GameConfig = game_config(Player1, Player2, _),
    valid_moves(game(Board, Player1, movement), Moves1),
    valid_moves(game(Board, Player2, movement), Moves2),
    Moves1 = [],
    Moves2 = [],
    determine_winner(Board, GameConfig, Winner).
game_over(_, _) :-
    fail. % Continue the game if there are valid moves.

% Determines the winner based on the tallest stack, highest total stacks, and most pieces.
determine_winner(Board, game_config(Player1, Player2, _), Winner) :-
    % Collect all stack heights and their respective colors
    findall(Height-Color, (
        member(Row, Board),
        member(Stack, Row),
        Stack = [Color|_], % Get the color of the top piece
        length(Stack, Height)
    ), Heights),
    
    % Sort and reverse to find the tallest stack(s)
    keysort(Heights, SortedHeights),
    reverse(SortedHeights, [MaxHeight-Color|Rest]),

    % Collect all players with the tallest stack
    findall(Player, (
        member(MaxHeight-PlayerColor, [MaxHeight-Color|Rest]),
        PlayerColor = blue,
        Player = Player1
    ;   PlayerColor = white,
        Player = Player2
    ), TallestPlayers),

    % Tie-break based on total stacks of the tallest height
    (TallestPlayers = [Winner] ->
        true % Single winner
    ;   
        findall(TotalHeight-Player, (
            member(Player, TallestPlayers),
            count_tallest_stacks(Board, MaxHeight, Player, TotalHeight)
        ), TotalStacks),
        keysort(TotalStacks, SortedStacks),
        reverse(SortedStacks, [_-Winner|RestTotalStacks]),

        % Further tie-break based on total pieces
        (RestTotalStacks = [_-_|_] ->
            findall(TotalPieces-Player, (
                member(Player, TallestPlayers),
                (Player = human(_, blue) -> Color = blue ; Color = white),
                count_pieces(Board, Color, TotalPieces)
            ), TotalPiecesCount),
            keysort(TotalPiecesCount, SortedPieces),
            reverse(SortedPieces, [_-Winner|_]) % Final winner
        ;
            true % Winner determined by highest total stacks
        )
    ).

% Finds the tallest stack for a player.
tallest_stack(Board, Player, Height) :-
    member(Row, Board),
    member(Stack, Row),
    Stack = [Player|_], % Get the first element of the stack
    length(Stack, Height).

% ===================== UTILITY PREDICATES =====================

% Update coordinate transformation for moves
transform_coordinates(X, Y, BoardX, BoardY) :-
    BoardX is X,
    BoardY is Y.

% Checks if coordinates are within bounds.
within_bounds(Board, X, Y) :-
    length(Board, Size),
    X > 0, X =< Size,
    Y > 0, Y =< Size.

get_clean_char(Char) :-
    get_char(Char),
    skip_line.

char_to_number(Char, Number) :-
    atom_codes(Char, [Code]),
    Number is Code - 48.

% Read coordinates for placement
read_placement(X, Y) :-
    write('Enter X coordinate: '), 
    get_clean_char(XChar),
    char_to_number(XChar, X),
    write('Enter Y coordinate: '), 
    get_clean_char(YChar),
    char_to_number(YChar, Y).

% Read coordinates for movement
read_move(FromX, FromY, ToX, ToY) :-
    write('From X: '), 
    get_clean_char(FXChar),
    char_to_number(FXChar, FromX),
    write('From Y: '), 
    get_clean_char(FYChar),
    char_to_number(FYChar, FromY),
    write('To X: '), 
    get_clean_char(TXChar),
    char_to_number(TXChar, ToX),
    write('To Y: '), 
    get_clean_char(TYChar),
    char_to_number(TYChar, ToY).

% Adds a player's piece to the board at the specified position.
add_player_piece(Board, X, Y, human(_, Color), NewBoard) :-
    transform_coordinates(X, Y, BoardX, BoardY),
    within_bounds(Board, BoardX, BoardY),
    nth1(BoardY, Board, Row, RestRows),
    nth1(BoardX, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(BoardX, NewRow, [Color, neutral|RestStack], RestCells),
    nth1(BoardY, NewBoard, NewRow, RestRows).

add_player_piece(Board, X, Y, pc(_, Color), NewBoard) :-
    transform_coordinates(X, Y, BoardX, BoardY),
    within_bounds(Board, BoardX, BoardY),
    nth1(BoardY, Board, Row, RestRows),
    nth1(BoardX, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(BoardX, NewRow, [Color, neutral|RestStack], RestCells),
    nth1(BoardY, NewBoard, NewRow, RestRows).

% Checks if a placement is valid.
valid_placement(Board, X, Y) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, [neutral|_]).

% Checks if all pieces are placed.
all_pieces_placed(Board, PiecesPerPlayer) :-
    count_pieces(Board, blue, BlueCount),
    count_pieces(Board, white, WhiteCount),
    write('Debug: Blue pieces placed = '), write(BlueCount), nl,
    write('Debug: White pieces placed = '), write(WhiteCount), nl,
    BlueCount =:= PiecesPerPlayer,
    WhiteCount =:= PiecesPerPlayer.

% Counts pieces of a specific color.
count_pieces(Board, Color, Count) :-
    findall(1, (
        member(Row, Board), 
        member(Stack, Row), 
        member(Color, Stack) % Count all occurrences of the color in the stack
    ), Pieces),
    length(Pieces, Count).

% Counts the total stacks of a given height for a specific player.
count_tallest_stacks(Board, MaxHeight, Player, TotalHeight) :-
    player_color(Player, Color),
    findall(1, (
        member(Row, Board),
        member(Stack, Row),
        Stack = [Color|_], % Stack belongs to the player
        length(Stack, MaxHeight) % Stack is of the tallest height
    ), Stacks),
    length(Stacks, TotalHeight).

% Checks if a piece is at a specific position
piece_at(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, [First|_]),
    First = Player.

pieces_per_player(BoardSize, PiecesPerPlayer) :-
    PiecesPerPlayer is BoardSize - 1.