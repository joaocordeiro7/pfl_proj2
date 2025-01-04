% ===================== MODULE IMPORTS =====================
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system)).

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
    get_number(3, 9, 'Choose board size (N for NxN grid)', BoardSize), nl,
        write('1. Human vs Human'), nl,
        write('2. Human vs PC'), nl,
        write('3. PC vs Human'), nl,
        write('4. PC vs PC'), nl,
        get_number(1, 4, 'Choose an option', GameMode), nl,
        handle_option(GameMode, BoardSize).

% Handles Human vs Human
handle_option(1, BoardSize) :-
    write('Enter name for Player 1 (Blue): '), 
    read_line(user_input, Player1NameCodes), atom_codes(Player1Name, Player1NameCodes),
    write('Enter name for Player 2 (White): '), 
    read_line(user_input, Player2NameCodes), atom_codes(Player2Name, Player2NameCodes),
    GameConfig = game_config(human(Player1Name, blue), human(Player2Name, white), BoardSize),
    initial_state(GameConfig, GameState),
    game_loop(GameState, 0).

% Handles Human vs PC
handle_option(2, BoardSize) :-
    write('Enter name for Player 1 (Blue): '), 
    read_line(user_input, Player1NameCodes), atom_codes(Player1Name, Player1NameCodes),
    get_number(1, 2, 'Choose difficulty level for PC', Level),
    GameConfig = game_config(human(Player1Name, blue), pc(Level, white, 'PCWhite'), BoardSize),
    initial_state(GameConfig, GameState),
    game_loop(GameState, 0).

% Handles PC vs Human
handle_option(3, BoardSize) :-
    get_number(1, 2, 'Choose difficulty level for PC', Level), nl,
    write('Enter name for Player 2 (White): '), 
    read_line(user_input, Player2NameCodes), atom_codes(Player2Name, Player2NameCodes),
    GameConfig = game_config(pc(Level, blue, 'PCBlue'), human(Player2Name, white), BoardSize),
    initial_state(GameConfig, GameState),
    game_loop(GameState, 0).

% Handles PC vs PC
handle_option(4, BoardSize) :-
    get_number(1, 2, 'Choose difficulty level for PC Blue', Level1), nl,
    get_number(1, 2, 'Choose difficulty level for PC White', Level2), nl,
    GameConfig = game_config(pc(Level1, blue, 'PCBlue'), pc(Level2, white, 'PCWhite'), BoardSize),
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
display_current_player(pc(_, _, PCName)) :-
    write('Current Player: '), write(PCName), nl.

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
    CurrentPlayer = pc(Level, _, Name),
    write(Name), write(' is thinking...'), nl,
    sleep(2),
    choose_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), Level, X, Y),
    write('PC places piece at: ('), write(X), write(', '), write(Y), write(')'), nl,
    sleep(1),
    process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, NewPhase).

handle_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), 
                 game(NewBoard, NextPlayer, NewPhase, BoardSize, GameConfig)) :-
    CurrentPlayer = human(_, _),
    write('PLACE YOUR PIECE'), nl,
    write('Instructions: Choose a position (X Y) to place your piece on top of a neutral stack.'), nl,
    read_placement(X, Y, BoardSize),
    process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, NewPhase).

process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, movement) :-
    valid_placement(Board, X, Y),
    add_player_piece(Board, X, Y, CurrentPlayer, NewBoard),
    pieces_per_player(BoardSize, PiecesPerPlayer),
    all_pieces_placed(NewBoard, PiecesPerPlayer),
    write('All pieces placed. Transitioning to movement phase!'), nl,
    next_player(CurrentPlayer, GameConfig, NextPlayer).

process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, placement) :-
    valid_placement(Board, X, Y),
    add_player_piece(Board, X, Y, CurrentPlayer, NewBoard),
    pieces_per_player(BoardSize, PiecesPerPlayer),
    \+ all_pieces_placed(NewBoard, PiecesPerPlayer),
    next_player(CurrentPlayer, GameConfig, NextPlayer).

process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), _, _, Board, CurrentPlayer, placement) :-
    \+ valid_placement(Board, X, Y),
    write('Invalid placement! Ensure it is on a neutral stack. Try again.'), nl.

% Handles the movement phase with user instructions.
handle_movement(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount) :-
    valid_moves(game(Board, CurrentPlayer, movement), Moves),
    handle_movement_moves(Moves, game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount).

% Handles cases where no valid moves are available.
handle_movement_moves([], game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount) :-
    write(CurrentPlayer), write(' has no valid moves. Passing turn...'), nl,
    next_player(CurrentPlayer, GameConfig, NextPlayer),
    NewPassCount is PassCount + 1,
    handle_pass_end(Board, NextPlayer, movement, BoardSize, GameConfig, NewPassCount).

% Handles cases where valid moves exist.
handle_movement_moves(Moves, GameState, _) :-
    GameState = game(_, CurrentPlayer, _, _, _),
    handle_player_move(Moves, CurrentPlayer, GameState).

% Ends the turn when passes reach 2 or transitions to the next player.
handle_pass_end(Board, NextPlayer, Phase, BoardSize, GameConfig, 2) :-
    game_over(game(Board, NextPlayer, Phase, BoardSize, GameConfig), Winner).
handle_pass_end(Board, NextPlayer, Phase, BoardSize, GameConfig, PassCount) :-
    game_loop(game(Board, NextPlayer, Phase, BoardSize, GameConfig), PassCount).

handle_player_move(Moves, pc(Level, Color, _), game(Board, CurrentPlayer, movement, BoardSize, GameConfig)) :-
    CurrentPlayer = pc(Level, _, Name),
    write(Name), write(' is thinking...'), nl,
    sleep(2),
    choose_move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Level, Move),
    write('PC chooses move: '), write(Move), nl,
    sleep(1),
    move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Move, NewGameState, GameConfig),
    game_loop(NewGameState, 0).
handle_player_move(_, human(_, _), game(Board, CurrentPlayer, movement, BoardSize, GameConfig)) :-
    write('MOVE YOUR STACK'), nl,
    write('Instructions: Choose a move (FromX FromY ToX ToY).'), nl,
    read_move(FromX, FromY, ToX, ToY, BoardSize),
    move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), move(FromX, FromY, ToX, ToY), NewGameState, GameConfig),
    game_loop(NewGameState, 0).

% Handles invalid moves by retrying the turn.
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
    ), Moves).

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
player_color(pc(_, Color, _), Color).

is_valid_destination(Board, X, Y) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    length(Stack, Height),
    Height = 1. 

% Get all valid adjacent cells as a list, including diagonal moves
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
    add_to_stack(TempBoard, BoardToX, BoardToY, Stack, NewBoard).

% ===================== PC MOVE GENERATION =====================

% Level 1: Random Placement
choose_placement(game(Board, _, _, _, _), 1, X, Y) :-
    findall((Row, Col), (
        between(1, 5, Row),
        between(1, 5, Col),
        valid_placement(Board, Row, Col)
    ), ValidPlacements),
    random_member((X, Y), ValidPlacements).

% Level 2: Strategic Placement
choose_placement(game(Board, CurrentPlayer, _, _, _), 2, X, Y) :-
    player_color(Player, Color),    
    findall((Row, Col), (
        between(1, 5, Row),
        between(1, 5, Col),
        valid_placement(Board, Row, Col)
    ), ValidPlacements),
    findall(Value-(Row, Col), (
        member((Row, Col), ValidPlacements),
        simulate_placement(Board, Row, Col, Color, Value)
    ), ValuedPlacements),
    keysort(ValuedPlacements, SortedPlacements),
    last(SortedPlacements, _-(X, Y)).

simulate_placement(Board, X, Y, Color, Value) :-
    add_player_piece(Board, X, Y, pc(_, Color, _), NewBoard),
    findall(Height, (
        member(Row, NewBoard),
        member(Stack, Row),
        Stack = [Color|_],
        length(Stack, Height)
    ), Heights),
    max_list(Heights, MaxHeight),
    board_center(Board, CenterX, CenterY),
    centrality_bonus(X, Y, CenterX, CenterY, Bonus),
    crowding_penalty(NewBoard, X, Y, Color, Penalty),
    Value is MaxHeight + Bonus - Penalty.

% Calculates the crowding penalty
crowding_penalty(Board, X, Y, Color, Penalty) :-
    findall(NeighborColor, (
        neighbor_positions(X, Y, NX, NY),
        within_bounds(Board, NX, NY),
        nth1(NY, Board, Row),
        nth1(NX, Row, Stack),
        Stack = [NeighborColor|_]
    ), Neighbors),
    count_occurrences(Color, Neighbors, Count),
    Penalty is Count * 2. % Penalty proportional to nearby same-color stacks

% Helper predicate to count occurrences of an element
count_occurrences(_, [], 0).
count_occurrences(Element, [Element|Rest], Count) :-
    count_occurrences(Element, Rest, RestCount),
    Count is RestCount + 1.
count_occurrences(Element, [_|Rest], Count) :-
    count_occurrences(Element, Rest, Count).

% Calculates neighbor positions
neighbor_positions(X, Y, NX, NY) :-
    (NX is X + 1, NY is Y).
neighbor_positions(X, Y, NX, NY) :-
    (NX is X - 1, NY is Y).
neighbor_positions(X, Y, NX, NY) :-
    (NX is X, NY is Y + 1).
neighbor_positions(X, Y, NX, NY) :-
    (NX is X, NY is Y - 1).

% Calculates the centrality bonus
centrality_bonus(X, Y, CenterX, CenterY, Bonus) :-
    Distance is abs(X - CenterX) + abs(Y - CenterY),
    Bonus is 10 - Distance. % Higher bonus for closer to the center

% Finds the center of the board
board_center(Board, CenterX, CenterY) :-
    length(Board, Size),
    CenterX is (Size + 1) // 2,
    CenterY is (Size + 1) // 2.

% Level 1: Random valid move
choose_move(game(Board, CurrentPlayer, movement, _, _), 1, Move) :-
    valid_moves(game(Board, CurrentPlayer, movement), Moves),
    random_member(Move, Moves).

% Level 2: Strategic move
choose_move(game(Board, CurrentPlayer, movement, _, _), 2, Move) :-
    CurrentPlayer = pc(_, Color, _),
    valid_moves(game(Board, CurrentPlayer, movement), Moves),
    findall(Value-M, (
        member(M, Moves),
        simulate_move(Board, M, Color, Value)
    ), ValuedMoves),
    keysort(ValuedMoves, SortedMoves),
    last(SortedMoves, _-Move).

% Simulates the value of a move
simulate_move(Board, move(FromX, FromY, ToX, ToY), Color, Value) :-
    execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
    value(NewBoard, Color, Value).

% Evaluates the board for a given color.
value(Board, Color, Value) :-
    findall(StackHeight, (
        member(Row, Board),
        member(Stack, Row),
        Stack = [Color|_],
        length(Stack, StackHeight)
    ), Heights),
    max_list(Heights, MaxHeight),
    findall(Move, valid_moves(game(Board, pc(_, Color, _), movement), Move), MobilityMoves),
    length(MobilityMoves, MobilityScore),
    Value is MaxHeight * 10 + MobilityScore. % Stack height is prioritized


% ===================== PLAYER MANAGEMENT =====================
% Switches to the next player based on the game configuration.
next_player(human(Name1, Color1), game_config(human(Name1, Color1), human(Name2, Color2), _), human(Name2, Color2)).
next_player(human(Name2, Color2), game_config(human(Name1, Color1), human(Name2, Color2), _), human(Name1, Color1)).
% Switch between two PC players
next_player(pc(Level1, Color1, Name1), game_config(pc(Level1, Color1, Name1), pc(Level2, Color2, Name2), _), pc(Level2, Color2, Name2)).
next_player(pc(Level2, Color2, Name2), game_config(pc(Level1, Color1, Name1), pc(Level2, Color2, Name2), _), pc(Level1, Color1, Name1)).
% Switch between a human and a PC player
next_player(human(Name, Color1), game_config(human(Name, Color1), pc(Level, Color2, PCName), _), pc(Level, Color2, PCName)).
next_player(pc(Level, Color2, PCName), game_config(human(Name, Color1), pc(Level, Color2, PCName), _), human(Name, Color1)).
next_player(pc(Level, Color1, PCName), game_config(pc(Level, Color1, PCName), human(Name, Color2), _), human(Name, Color2)).
next_player(human(Name, Color2), game_config(pc(Level, Color1, PCName), human(Name, Color2), _), pc(Level, Color1, PCName)).

% ===================== GAME STATE CHECKS =====================

% Checks if the game is over (no valid moves for both players).
game_over(game(Board, _, movement, _, GameConfig), Winner) :-
    % Check if both players have no valid moves
    GameConfig = game_config(Player1, Player2, _),
    valid_moves(game(Board, Player1, movement), Moves1),
    valid_moves(game(Board, Player2, movement), Moves2),
    Moves1 = [],
    Moves2 = [],
    determine_winner(Board, GameConfig, Winner), !.
game_over(_, _) :-
    fail. % Continue the game if there are valid moves.

% Determines the winner based on the tallest stack or highest total stacks.
determine_winner(Board, game_config(Player1, Player2, _), Winner) :-
    extract_stack_heights(Board, blue, BlueStacks),
    extract_stack_heights(Board, white, WhiteStacks),
    reverse_sort(BlueStacks, SortedBlueStacks),
    reverse_sort(WhiteStacks, SortedWhiteStacks),
    compare_stacks(SortedBlueStacks, SortedWhiteStacks, Winner),
    write('-----------------------'), nl,
    write('Game Over!'), nl,
    get_player_name(WinnerColor, Player1, Player2),
    write('-----------------------'), nl.

% Gets the winner name based on their color.
get_player_name(blue, human(Name, _), _) :-
    write('The winner is: '), write(Name), write('.'), nl.
get_player_name(blue, pc(_, _, _), _) :-
    write('The winner is: PCBlue.'), nl.
get_player_name(white, _, human(Name, _)) :-
    write('The winner is: '), write(Name), write('.'), nl.
get_player_name(white, _, pc(_, _, _)) :-
    write('The winner is: PCWhite.'), nl.
get_player_name(draw, _, _) :-
    write('The game ended in a draw.'), nl.

% Reverses the list of stacks heights
reverse_sort(List, Sorted) :-
    sort(List, AscSorted),
    reverse(AscSorted, Sorted).

% Gets the list of stacks heights
extract_stack_heights(Board, Color, Heights) :-
    findall(Height, (
        member(Row, Board),
        member(Stack, Row),
        Stack = [Color|_],
        length(Stack, Height)
    ), Heights).

% Compares the list of heights
compare_stacks([], [], draw). 
compare_stacks([H1|T1], [H2|T2], Winner) :-
    H1 > H2,
    Winner = blue.
compare_stacks([H1|T1], [H2|T2], Winner) :-
    H1 < H2,
    Winner = white.
compare_stacks([H1|T1], [H2|T2], Winner) :-
    H1 =:= H2,
    compare_stacks(T1, T2, Winner).

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

read_number(X):-
    read_number_aux(X, 0).
read_number_aux(X, Acc) :- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10 * Acc + (C - 48),
    read_number_aux(X, Acc1).
read_number_aux(X, X).

get_number(Min, Max, Context, Value):-
    format('~a (~d - ~d): ', [Context, Min, Max]),
    repeat,                          
    read_number(Value),
    between(Min, Max, Value), 
    !.  

% Read coordinates for placement
read_placement(X, Y, BoardSize) :-
    get_number(1, BoardSize, 'Enter X coordinate', X),
    get_number(1, BoardSize, 'Enter Y coordinate', Y).

% Read coordinates for movement
read_move(FromX, FromY, ToX, ToY, BoardSize) :-
    get_number(1, BoardSize, 'From X', FromX),
    get_number(1, BoardSize, 'From Y', FromY),
    get_number(1, BoardSize, 'To X', ToX),
    get_number(1, BoardSize, 'To Y', ToY).

% Adds a player's piece to the board at the specified position.
add_player_piece(Board, X, Y, human(_, Color), NewBoard) :-
    transform_coordinates(X, Y, BoardX, BoardY),
    within_bounds(Board, BoardX, BoardY),
    nth1(BoardY, Board, Row, RestRows),
    nth1(BoardX, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(BoardX, NewRow, [Color, neutral|RestStack], RestCells),
    nth1(BoardY, NewBoard, NewRow, RestRows).

add_player_piece(Board, X, Y, pc(_, Color, _), NewBoard) :-
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
    nth1(X, Row, Stack),
    Stack = [neutral|_].

% Checks if all pieces are placed.
all_pieces_placed(Board, PiecesPerPlayer) :-
    count_pieces(Board, blue, BlueCount),
    count_pieces(Board, white, WhiteCount),
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

% Checks if a piece is at a specific position
piece_at(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, [First|_]),
    First = Player.

pieces_per_player(BoardSize, PiecesPerPlayer) :-
    PiecesPerPlayer is BoardSize - 1.

max_list([H|T], Max) :-
    max_list(T, H, Max).

max_list([], Max, Max).
max_list([H|T], CurrentMax, Max) :-
    H > CurrentMax,
    max_list(T, H, Max).
max_list([H|T], CurrentMax, Max) :-
    H =< CurrentMax,
    max_list(T, CurrentMax, Max).