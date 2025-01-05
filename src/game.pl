% ===================== MODULE IMPORTS =====================

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system)).


% ===================== GAME SETUP AND ENTRY POINT =====================

% play/0
% Entry point for the game. Displays the instructions and the menu that allows choosing the board size 
% and the game type (H/H, H/PC, PC/H, or PC/PC). 
play :-
    write('Welcome to STAQS!'), nl, nl,
    write('Instructions:'), nl,
    write('1. During the PLACEMENT phase, type "X Y" to place your piece on top of a neutral stack.'), nl,
    write('   Example: "1 1" places your piece at the lower-left corner of the board.'), nl,
    write('2. During the MOVEMENT phase, type "FromX FromY ToX ToY" to move a stack to an adjacent cell (vertical, horizontal, diagonal).'), nl,
    write('   Example: "1 1 2 2" moves the stack from (1, 1) to (2, 2).'), nl,
    write('3. Stacks can only move onto cells containing exactly one neutral piece.'), nl,
    write('4. The game ends if there is no valid moves for both players.'), nl,
    write('5. Wins the player with the tallest stack or with the most stacks of the tallest height.'), nl,
    write('6. Follow the prompts and enjoy the game!'), nl, nl,
    get_number(3, 9, 'Choose board size (N for NxN grid)', BoardSize), nl,
    write('Game Modes:'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs PC'), nl,
    write('3. PC vs Human'), nl,
    write('4. PC vs PC'), nl,
    get_number(1, 4, 'Choose an option', GameMode), nl,
    handle_option(GameMode, BoardSize).


% handle_option(+GameMode, +BoardSize)
% Configures the game based on the game type and board size chosen by the user.
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

% initial_state(+GameConfig, -GameState)
% Sets up the initial game state based on the game configuration
initial_state(game_config(Player1, Player2, BoardSize), game(Board, Player1, placement, BoardSize, game_config(Player1, Player2, BoardSize))) :-
    create_board(BoardSize, Board).


% create_board(+Size, -Board)
% Create a board of the given size with neutral pieces in each position.
create_board(Size, Board) :-
    length(Board, Size),
    maplist(create_row(Size), Board).


% create_row(+Size, -Row)
% Creates a row of neutral pieces.
create_row(Size, Row) :-
    length(Row, Size),
    maplist(=([neutral]), Row).



% ===================== GAME DISPLAY UTILITIES =====================

% display_game(+GameState)
% Displays the game board with labeled rows and columns.
display_game(game(Board, CurrentPlayer, Phase, BoardSize, _)) :-
    nl,
    write('Current Player: '),
    display_current_player(CurrentPlayer), nl,
    write('Phase: '), write(Phase), nl, nl,
    reverse(Board, InvertedBoard), % Reverse rows for lower-left corner
    print_separator(BoardSize),
    print_board(InvertedBoard, BoardSize, BoardSize),
    print_column_headers(BoardSize).


% display_current_player(+Player)
% Displays the current player information.
display_current_player(human(Name, _)) :-
    write(Name).
display_current_player(pc(_, _, PCName)) :-
    write(PCName).


% print_board(+Board, +RowNum, +BoardSize)
% Prints the board row by row with row numbers.
print_board([], _, _).
print_board([Row|Rest], RowNum, BoardSize) :-
    format('~d |', [RowNum]),
    print_row(Row),
    nl,
    print_separator(BoardSize),
    NextRowNum is RowNum - 1,
    print_board(Rest, NextRowNum, BoardSize).


% print_row(+Row)
% Prints a single row of the board.
print_row([]).
print_row([Stack|Rest]) :-
    print_stack_content(Stack),
    write('|'),
    print_row(Rest).


% print_column_headers(+Size)
% Prints the column headers (1, 2, ..., Size) above the board.
print_column_headers(Size) :-
    write('    '),
    numlist(1, Size, Columns),
    maplist(print_column_header, Columns),
    nl.


% print_column_header(+Col)
% Prints a single column header.
print_column_header(Col) :-
    format(' ~d   ', [Col]).


% print_separator(+Size)
% Prints a horizontal separator line for the board.
print_separator(Size) :-
    write('  +'),
    numlist(1, Size, Columns),
    maplist(print_separator_segment, Columns),
    nl.


% print_separator_segment(+_)
% Prints a single segment of the horizontal separator.
print_separator_segment(_) :-
    write('----+').


% print_stack_content(+Stack)
% Formats and prints the content of a single stack.
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

% game_loop(+GameState, +PassCount)
% Main game cycle, alternates between phases and players.
game_loop(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig), PassCount) :-
    display_game(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig)),
    process_phase(Phase, game(Board, CurrentPlayer, Phase, BoardSize, GameConfig), PassCount).


% process_phase(+Phase, +GameState, +PassCount)
% Handles the current game phase (placement or movement).
process_phase(placement, GameState, _) :-
    handle_placement(GameState, NewGameState),
    game_loop(NewGameState, 0).
process_phase(movement, GameState, PassCount) :-
    handle_movement(GameState, PassCount).
    


% ===================== GAME PHASES =====================

% handle_placement(+GameState, -NewGameState)
% Handles the placement phase where players place their pieces.
% For PC player
handle_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), 
                 game(NewBoard, NextPlayer, NewPhase, BoardSize, GameConfig)) :-
    CurrentPlayer = pc(Level, Color, Name),
    remaining_pieces(Board, BoardSize, Color, Remaining),
    write('Pieces remaining: '), write(Remaining), nl,
    write(Name), write(' is thinking...'), nl,
    sleep(2),
    choose_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), Level, X, Y),
    write(Name), write(' places piece at: ('), write(X), write(', '), write(Y), write(')'), nl,
    sleep(1),
    process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, NewPhase).

% For Human player
handle_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), 
                 game(NewBoard, NextPlayer, NewPhase, BoardSize, GameConfig)) :-
    CurrentPlayer = human(_, Color),
    remaining_pieces(Board, BoardSize, Color, Remaining),
    write('Pieces remaining: '), write(Remaining), nl,
    write('PLACE YOUR PIECE'), nl,
    write('Instructions: Choose a position (X Y) to place your piece on top of a neutral stack.'), nl,
    read_placement(X, Y, BoardSize),
    process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, NewPhase).


% process_placement(+GameState, +X, +Y, -NewBoard, -NextPlayer, -NewPhase)
% Processes a placement action, updating the board and transitioning phases if needed.
% Case in which all pieces are placed.
process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, movement) :-
    valid_placement(Board, X, Y),
    add_player_piece(Board, X, Y, CurrentPlayer, NewBoard),
    pieces_per_player(BoardSize, PiecesPerPlayer),
    all_pieces_placed(NewBoard, PiecesPerPlayer),
    write('All pieces placed. Transitioning to movement phase!'), nl,
    next_player(CurrentPlayer, GameConfig, NextPlayer).

% Case in which there are still pieces to be placed.
process_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), X, Y, NewBoard, NextPlayer, placement) :-
    valid_placement(Board, X, Y),
    add_player_piece(Board, X, Y, CurrentPlayer, NewBoard),
    pieces_per_player(BoardSize, PiecesPerPlayer),
    \+ all_pieces_placed(NewBoard, PiecesPerPlayer),
    next_player(CurrentPlayer, GameConfig, NextPlayer), !.

% Case in which a piece is placed in an invalid position.
process_placement(game(Board, CurrentPlayer, placement, _, _), _, _, Board, CurrentPlayer, placement) :-
    write('Invalid placement! Ensure it is on a neutral stack. Try again.'), nl.


% handle_movement(+GameState, +PassCount)
% Handles the movement phase with user instructions, checking valid moves or handling pass scenarios.
handle_movement(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount) :-
    valid_moves(game(Board, CurrentPlayer, movement), Moves),
    handle_movement_moves(Moves, game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount).


% handle_movement_moves(+Moves, +GameState, +PassCount)
% Handles scenarios based on available moves during the movement phase.
% Cases where no valid moves are available.
handle_movement_moves([], game(Board, CurrentPlayer, movement, BoardSize, GameConfig), PassCount) :-
    display_current_player(CurrentPlayer), write(' has no valid moves. Passing turn...'), nl,
    next_player(CurrentPlayer, GameConfig, NextPlayer),
    NewPassCount is PassCount + 1,
    handle_pass_end(Board, NextPlayer, movement, BoardSize, GameConfig, NewPassCount).

% Cases where valid moves exist.
handle_movement_moves(_, GameState, _) :-
    GameState = game(_, CurrentPlayer, _, _, _),
    handle_player_move(CurrentPlayer, GameState).


% handle_pass_end(+Board, +NextPlayer, +Phase, +BoardSize, +GameConfig, +PassCount)
% Ends the game if both players pass consecutively (when passes reach 2) or continues to the next player.
handle_pass_end(Board, NextPlayer, Phase, BoardSize, GameConfig, 2) :-
    game_over(game(Board, NextPlayer, Phase, BoardSize, GameConfig), _), !.
handle_pass_end(Board, NextPlayer, Phase, BoardSize, GameConfig, PassCount) :-
    game_loop(game(Board, NextPlayer, Phase, BoardSize, GameConfig), PassCount).


% handle_player_move(+CurrentPlayer, +GameState)
% Handles the current player's move during the movement phase.
% For PC player
handle_player_move(pc(Level, _, _), game(Board, CurrentPlayer, movement, BoardSize, GameConfig)) :-
    CurrentPlayer = pc(Level, _, Name),
    write(Name), write(' is thinking...'), nl,
    sleep(2),
    choose_move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Level, Move),
    write(Name), write(' chooses move: '), write(Move), nl,
    sleep(1),
    move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Move, NewGameState),
    game_loop(NewGameState, 0).

% For Human player
handle_player_move(human(_, _), game(Board, CurrentPlayer, movement, BoardSize, GameConfig)) :-
    write('MOVE YOUR STACK'), nl,
    write('Instructions: Choose a move (FromX FromY ToX ToY).'), nl,
    read_move(FromX, FromY, ToX, ToY, BoardSize),
    move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), move(FromX, FromY, ToX, ToY), NewGameState),
    game_loop(NewGameState, 0).

% For invalid moves - retrying the turn.
handle_player_move(_, GameState) :-
    write('Invalid move! Ensure it follows the rules. Try again.'), nl,
    handle_movement(GameState, 0).



% ===================== MOVE VALIDATION =====================

% valid_moves(+GameState, -ListOfMoves)
% Generates a list of all valid moves for the current player in the given game state.
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


% valid_move(+Board, +Player, +Move)
% Checks if a move is valid.
valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) :-
    player_color(Player, Color),
    within_bounds(Board, FromX, FromY),
    piece_at(Board, FromX, FromY, Color),
    get_adjacent_cells(Board, FromX, FromY, AdjacentCells),
    member((ToX, ToY), AdjacentCells),
    is_valid_destination(Board, ToX, ToY).


% player_color(+Player, -Color)
% Retrieves the color associated with the player.
player_color(human(_, Color), Color).
player_color(pc(_, Color, _), Color).


% is_valid_destination(+Board, +X, +Y)
% Checks if the destination cell is valid for a move.
is_valid_destination(Board, X, Y) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    length(Stack, Height),
    Height = 1. 


% get_adjacent_cells(+Board, +X, +Y, -AdjacentCells)
% Retrieves all valid adjacent cells as a list for the given coordinates.
get_adjacent_cells(Board, X, Y, AdjacentCells) :-
    findall((ToX, ToY), adjacent_cell(X, Y, ToX, ToY), PossibleCells),
    include(valid_cell(Board), PossibleCells, AdjacentCells).


% adjacent_cell(+X, +Y, -ToX, -ToY)
% Defines adjacent cells in all possible directions.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X + 1, ToY is Y.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X - 1, ToY is Y.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X, ToY is Y + 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X, ToY is Y - 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X - 1, ToY is Y - 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X - 1, ToY is Y + 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X + 1, ToY is Y - 1.
adjacent_cell(X, Y, ToX, ToY) :- ToX is X + 1, ToY is Y + 1.


% valid_cell(+Board, +Coordinates)
% Checks if a cell is within the board bounds and valid for movement.
valid_cell(Board, (X, Y)) :-
    within_bounds(Board, X, Y).



% ===================== STACK OPERATIONS =====================

% move(+GameState, +Move, -NewGameState)
% Validates and executes a move, returning the updated game state.
move(game(Board, Player, Phase, BoardSize, GameConfig), move(FromX, FromY, ToX, ToY), game(NewBoard, NextPlayer, Phase, BoardSize, GameConfig)) :-
    valid_move(Board, Player, move(FromX, FromY, ToX, ToY)),
    execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
    next_player(Player, GameConfig, NextPlayer).


% get_stack(+Board, +X, +Y, -Stack)
% Gets the stack located at the specified coordinates on the board.
get_stack(Board, X, Y, Stack) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack).


% remove_stack(+Board, +X, +Y, -NewBoard)
% Removes the stack from the specified position, leaving an empty cell.
remove_stack(Board, X, Y, NewBoard) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, _, RestCells),
    nth1(X, NewRow, [], RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).


% add_to_stack(+Board, +X, +Y, +Stack, -NewBoard)
% Adds a stack to the specified position, stacking it on any existing pieces.
add_to_stack(Board, X, Y, Stack, NewBoard) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, ExistingStack, RestCells),
    append(Stack, ExistingStack, NewStack),
    nth1(X, NewRow, NewStack, RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).


% execute_move(+Board, +FromX, +FromY, +ToX, +ToY, -NewBoard)
% Moves a stack from one position to another, updating the board.
execute_move(Board, FromX, FromY, ToX, ToY, NewBoard) :-
    get_stack(Board, FromX, FromY, Stack),
    remove_stack(Board, FromX, FromY, TempBoard),
    add_to_stack(TempBoard, ToX, ToY, Stack, NewBoard).



% ===================== PC MOVE GENERATION =====================

% choose_placement(+GameState, +Level, -X, -Y)
% Chooses a placement position for the PC player based on the difficulty level.
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
    player_color(CurrentPlayer, Color),    
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


% simulate_placement(+Board, +X, +Y, +Color, -Value)
% Simulates a placement and calculates its value based on height, centrality, and crowding.
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


% crowding_penalty(+Board, +X, +Y, +Color, -Penalty)
% Calculates a penalty based on nearby stacks of the same color.
crowding_penalty(Board, X, Y, Color, Penalty) :-
    findall(NeighborColor, (
        adjacent_cell(X, Y, NX, NY),
        within_bounds(Board, NX, NY),
        nth1(NY, Board, Row),
        nth1(NX, Row, Stack),
        Stack = [NeighborColor|_]
    ), Neighbors),
    count_occurrences(Color, Neighbors, Count),
    Penalty is Count * 2. % Penalty proportional to nearby same-color stacks


% count_occurrences(+Element, +List, -Count)
% Counts the number of occurrences of an element in a list.
count_occurrences(_, [], 0).
count_occurrences(Element, [Element|Rest], Count) :-
    count_occurrences(Element, Rest, RestCount),
    Count is RestCount + 1.
count_occurrences(Element, [_|Rest], Count) :-
    count_occurrences(Element, Rest, Count).


% centrality_bonus(+X, +Y, +CenterX, +CenterY, -Bonus)
% Calculates a bonus for being close to the center of the board.
centrality_bonus(X, Y, CenterX, CenterY, Bonus) :-
    Distance is abs(X - CenterX) + abs(Y - CenterY),
    Bonus is 10 - Distance. % Higher bonus for closer to the center


% board_center(+Board, -CenterX, -CenterY)
% Finds the center coordinates of the board.
board_center(Board, CenterX, CenterY) :-
    length(Board, Size),
    CenterX is (Size + 1) // 2,
    CenterY is (Size + 1) // 2.


% choose_move(+GameState, +Level, -Move)
% Chooses a move for the PC player based on the difficulty level.
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


% simulate_move(+Board, +Move, +Color, -Value)
% Simulates a move and its value and evaluates its resulting board state.
simulate_move(Board, move(FromX, FromY, ToX, ToY), Color, Value) :-
    execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
    value(NewBoard, Color, Value).


% value(+Board, +Color, -Value)
% Evaluates the board for a given color, considering stack height and mobility.
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

% next_player(+CurrentPlayer, +GameConfig, -NextPlayer)
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

% game_over(+GameState, -Winner)
% Checks if the game is over (no valid moves for both players) and identifies the winner.
game_over(game(Board, _, movement, _, GameConfig), Winner) :-
    % Check if both players have no valid moves
    GameConfig = game_config(Player1, Player2, _),
    valid_moves(game(Board, Player1, movement), Moves1),
    valid_moves(game(Board, Player2, movement), Moves2),
    Moves1 = [],
    Moves2 = [],
    !,
    determine_winner(Board, GameConfig, Winner).
game_over(_, _) :-
    fail. % Continue the game if there are valid moves.


% determine_winner(+Board, +GameConfig, -Winner)
% Determines the winner based on the tallest stack or highest total stacks.
determine_winner(Board, game_config(Player1, Player2, _), Winner) :-
    extract_stack_heights(Board, blue, BlueStacks),
    extract_stack_heights(Board, white, WhiteStacks),
    reverse_sort(BlueStacks, SortedBlueStacks),
    reverse_sort(WhiteStacks, SortedWhiteStacks),
    compare_stacks(SortedBlueStacks, SortedWhiteStacks, Winner),
    write('-----------------------'), nl,
    write('Game Over!'), nl,
    get_player_name(Winner, Player1, Player2),
    write('-----------------------'), nl, !,
    end_menu.


% get_player_name(+WinnerColor, +Player1, +Player2)
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


% reverse_sort(+List, -Sorted)
% Sorts a list in descending order.
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


% compare_stacks(+BlueStacks, +WhiteStacks, -Winner)
% Compares the list of stacks heights of two players to determine the winner.
compare_stacks([], [], draw). 
compare_stacks([H1|_], [H2|_], Winner) :-
    H1 > H2,
    Winner = blue.
compare_stacks([H1|_], [H2|_], Winner) :-
    H1 < H2,
    Winner = white.
compare_stacks([H1|T1], [H2|T2], Winner) :-
    H1 =:= H2,
    compare_stacks(T1, T2, Winner).


% end_menu/0
% Displays a menu to replay or exit the game.
end_menu :-
    nl,
    write('What would you like to do?'), nl,
    write('1. Play Again'), nl,
    write('2. Exit'), nl,
    get_number(1, 2, 'Choose an option', Choice),
    handle_end_menu_choice(Choice).


% handle_end_menu_choice(+Choice)
% Handles the player's choice to replay or exit.
handle_end_menu_choice(1) :-
    play. % Replay the game.
handle_end_menu_choice(2) :-
    write('Thank you for playing! Goodbye!'), nl.



% ===================== UTILITY PREDICATES =====================

% within_bounds(+Board, +X, +Y)
% Checks if coordinates are within the board's limits.
within_bounds(Board, X, Y) :-
    length(Board, Size),
    X > 0, X =< Size,
    Y > 0, Y =< Size.


% read_number(-X)
% Reads a numeric input from the user.
read_number(X):-
    read_number_aux(X, 0).


% read_number_aux(-X, +Acc)
% Reads numeric input recursively, accumulating digits.
read_number_aux(X, Acc) :- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10 * Acc + (C - 48),
    read_number_aux(X, Acc1).
read_number_aux(X, X).


% get_number(+Min, +Max, +Prompt, -Value)
% Prompts the user for a number between Min and Max.
get_number(Min, Max, Context, Value):-
    format('~a (~d - ~d): ', [Context, Min, Max]),
    repeat,                          
    read_number(Value),
    between(Min, Max, Value), !.  


% read_placement(-X, -Y, +BoardSize)
% Reads coordinates for a piece placement.
read_placement(X, Y, BoardSize) :-
    get_number(1, BoardSize, 'Enter X coordinate', X),
    get_number(1, BoardSize, 'Enter Y coordinate', Y).


% read_move(-FromX, -FromY, -ToX, -ToY, +BoardSize)
% Reads coordinates for a piece movement.
read_move(FromX, FromY, ToX, ToY, BoardSize) :-
    get_number(1, BoardSize, 'From X', FromX),
    get_number(1, BoardSize, 'From Y', FromY),
    get_number(1, BoardSize, 'To X', ToX),
    get_number(1, BoardSize, 'To Y', ToY).


% add_player_piece(+Board, +X, +Y, +Player, -NewBoard)
% Adds a player's piece to the board at the specified position.
% For Human player
add_player_piece(Board, X, Y, human(_, Color), NewBoard) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(X, NewRow, [Color, neutral|RestStack], RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).

% For PC player
add_player_piece(Board, X, Y, pc(_, Color, _), NewBoard) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(X, NewRow, [Color, neutral|RestStack], RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).


% valid_placement(+Board, +X, +Y)
% Checks if a placement is valid at the given coordinates.
valid_placement(Board, X, Y) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    Stack = [neutral|_].


% all_pieces_placed(+Board, +PiecesPerPlayer)
% Checks if all pieces have been placed by both players.
all_pieces_placed(Board, PiecesPerPlayer) :-
    count_pieces(Board, blue, BlueCount),
    count_pieces(Board, white, WhiteCount),
    BlueCount =:= PiecesPerPlayer,
    WhiteCount =:= PiecesPerPlayer.


% count_pieces(+Board, +Color, -Count)
% Counts the number of pieces of a specific color on the board.
count_pieces(Board, Color, Count) :-
    findall(1, (
        member(Row, Board), 
        member(Stack, Row), 
        member(Color, Stack) % Count all occurrences of the color in the stack
    ), Pieces),
    length(Pieces, Count).


% piece_at(+Board, +X, +Y, -Color)
% Checks if a piece of a specific color is at the given position.
piece_at(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, [First|_]),
    First = Player.


% pieces_per_player(+BoardSize, -PiecesPerPlayer)
% Determines the number of pieces per player based on board size.
pieces_per_player(BoardSize, PiecesPerPlayer) :-
    PiecesPerPlayer is BoardSize - 1.


% remaining_pieces(+Board, +Player, -Remaining)
% Calculates the number of pieces a player has left to place.
remaining_pieces(Board, BoardSize, Color, Remaining) :-
    pieces_per_player(BoardSize, PiecesPerPlayer),
    count_pieces(Board, Color, Placed),
    Remaining is PiecesPerPlayer - Placed.


% max_list(+List, -Max)
% Finds the maximum value in a list.
max_list([H|T], Max) :-
    max_list(T, H, Max).

max_list([], Max, Max).
max_list([H|T], CurrentMax, Max) :-
    H > CurrentMax,
    max_list(T, H, Max).
max_list([H|T], CurrentMax, Max) :-
    H =< CurrentMax,
    max_list(T, CurrentMax, Max).



% ===================== PREDICATES FOR DEMONSTRATION =====================

% Intermediate state: After a few pieces have been placed.
% intermediate_state_placement(GameState), game_loop(GameState, 0).
intermediate_state_placement(game([
    [[neutral], [neutral], [neutral], [neutral], [neutral]],
    [[neutral], [blue, neutral], [neutral], [neutral], [neutral]],
    [[neutral], [neutral], [neutral], [neutral], [neutral]],
    [[neutral], [neutral], [neutral], [white, neutral], [neutral]],
    [[neutral], [neutral], [neutral], [neutral], [neutral]]
], human('Player1', blue), placement, 5, game_config(human('Player1', blue), human('Player2', white), 5))).


% Intermediate state: During the movement phase with some stacks formed.
% intermediate_state_movement(GameState), game_loop(GameState, 0).
intermediate_state_movement(game([
    [[white, neutral], [], [neutral], [neutral], [blue, neutral]],
    [[neutral], [blue, neutral, neutral], [neutral], [neutral], [neutral]],
    [[neutral], [white, neutral], [neutral], [neutral], [neutral]],
    [[blue, neutral, neutral], [neutral], [], [white, neutral], []],
    [[], [neutral], [blue, neutral, neutral], [], [white, neutral, neutral, neutral]]
], human('Player2', white), movement, 5, game_config(human('Player1', blue), human('Player2', white), 5))).


% Near-final state: Move (1, 4, 1 , 3) - draw.
% near_final_state_draw(GameState), game_loop(GameState, 0).
near_final_state_draw(game([
    [[blue, neutral, neutral, neutral], [], [], [blue, neutral, neutral, neutral]],
    [[], [], [white, neutral, neutral, neutral], []],
    [[neutral], [], [], []],
    [[white, neutral, neutral], [blue, neutral, neutral], [], [white, neutral, neutral]]
], human('Player2', white), movement, 4, game_config(human('Player1', blue), human('Player2', white), 4))).


% Near-final state: Move (1, 1, 1, 2) - Player1 win.
% near_final_state_blue(GameState), game_loop(GameState, 0).
near_final_state_blue(game([
    [[blue, neutral, neutral, neutral], [], [], [blue, neutral, neutral, neutral]],
    [[neutral], [], [white, neutral, neutral, neutral], []],
    [[], [], [], []],
    [[white, neutral, neutral], [blue, neutral, neutral], [], [white, neutral, neutral]]
], human('Player2', white), movement, 4, game_config(human('Player1', blue), human('Player2', white), 4))).


% Near-final state: Move (4, 4, 3, 4) - Player2 win.
% near_final_state_white(GameState), game_loop(GameState, 0).
near_final_state_white(game([
    [[blue, neutral, neutral, neutral], [], [], [blue, neutral, neutral, neutral]],
    [[], [], [white, neutral, neutral, neutral], []],
    [[], [], [], []],
    [[white, neutral, neutral, neutral], [blue, neutral], [neutral], [white, neutral]]
], human('Player2', white), movement, 4, game_config(human('Player1', blue), human('Player2', white), 4))).