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
    (BoardSize < 3 ->
        write('Invalid size. The board must be at least 3x3.'), nl,
        play
    ;
        write('1. Human vs Human'), nl,
        write('2. Human vs PC'), nl,
        write('3. PC vs PC'), nl,
        write('Choose an option (1-3): '), nl,
        get_clean_char(GameModeChar),
        handle_option(GameModeChar, BoardSize)
    ).

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
    (CurrentPlayer = human(Name, _) ->
        write('Current Player: '), write(Name), nl
    ;
        write('Current Player: PC'), nl
    ),
    write('Phase: '), write(Phase), nl, nl,
    reverse(Board, InvertedBoard), % Reverse rows for lower-left corner
    print_separator(BoardSize),
    print_board(InvertedBoard, BoardSize, BoardSize),
    print_column_headers(BoardSize).

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

    (Phase = placement ->
        % Handle placement phase
        handle_placement(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig), NewGameState),
        game_loop(NewGameState, 0) % Reset pass count after a successful placement
    ;
        % Check valid moves in the movement phase
        valid_moves(game(Board, CurrentPlayer, movement), Moves),
        (Moves = [] ->
            % No valid moves, pass the turn
            write(CurrentPlayer), write(' has no valid moves. Passing turn...'), nl,
            next_player(CurrentPlayer, GameConfig, NextPlayer),
            NewPassCount is PassCount + 1,
            (NewPassCount =:= 2 ->
                % Game ends after two consecutive passes
                determine_winner(Board, GameConfig, Winner),
                write('Game over! Winner: '), write(Winner), nl
            ;
                % Continue to the next turn
                game_loop(game(Board, NextPlayer, Phase, BoardSize, GameConfig), NewPassCount)
            )
        ;
            % Handle movement phase
            handle_movement(game(Board, CurrentPlayer, Phase, BoardSize, GameConfig), NewGameState),
            game_loop(NewGameState, 0) % Reset pass count after a valid move
        )
    ).

% ===================== GAME PHASES =====================

handle_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), 
                 game(NewBoard, NextPlayer, NewPhase, BoardSize, GameConfig)) :-
    write('PLACE YOUR PIECE'), nl,
    write('Instructions: Choose a position (X Y) to place your piece on top of a neutral stack.'), nl,
    read_placement(X, Y),
    (valid_placement(Board, X, Y) ->
        % Add the piece for the current player
        add_player_piece(Board, X, Y, CurrentPlayer, NewBoard),
        pieces_per_player(BoardSize, PiecesPerPlayer),
        (all_pieces_placed(NewBoard, PiecesPerPlayer) ->
            NewPhase = movement,
            write('All pieces placed. Transitioning to movement phase!'), nl
        ;
            NewPhase = placement
        ),
        % Switch to the next player
        next_player(CurrentPlayer, GameConfig, NextPlayer)
    ;
        % Invalid placement, retry
        write('Invalid placement! Ensure it is on a neutral stack. Try again.'), nl,
        handle_placement(game(Board, CurrentPlayer, placement, BoardSize, GameConfig), 
                         game(NewBoard, NextPlayer, NewPhase, BoardSize, GameConfig))
    ).


% Handles the movement phase with user instructions.
handle_movement(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), 
                game(NewBoard, NextPlayer, movement, BoardSize, GameConfig)) :-
    write('MOVE YOUR STACK'), nl,
    write('Instructions: Choose a move (FromX FromY ToX ToY) to move a stack to an adjacent cell.'), nl,
    valid_moves(game(Board, CurrentPlayer, movement), Moves),
    (Moves = [] ->
        write(CurrentPlayer), write(' has no valid moves. Passing turn...'), nl,
        next_player(CurrentPlayer, GameConfig, NextPlayer),
        game_loop(game(Board, NextPlayer, movement, BoardSize, GameConfig), 1) % Pass turn and increment pass count
    ;
        (CurrentPlayer = pc(Level) ->
            choose_move(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), Level, Move),
            write('PC chooses move: '), write(Move), nl,
            move(game(Board, CurrentPlayer, movement, BoardSize), Move, game(NewBoard, NextPlayer, movement, BoardSize))
        ;
            read_move(FromX, FromY, ToX, ToY),
            (move(game(Board, CurrentPlayer, movement, BoardSize), move(FromX, FromY, ToX, ToY), game(NewBoard, NextPlayer, movement, BoardSize)) ->
                true
            ;
                write('Invalid move! Ensure it follows the rules. Try again.'), nl,
                handle_movement(game(Board, CurrentPlayer, movement, BoardSize, GameConfig), 
                                game(NewBoard, NextPlayer, movement, BoardSize, GameConfig))
            )
        ),
        next_player(CurrentPlayer, GameConfig, NextPlayer)
    ).


% ===================== MOVE VALIDATION =====================

% Validates moves for a given player.
valid_moves(game(Board, Player, _), Moves) :-
    % Extract color from the player's structure
    (Player = human(_, Color) ; Player = pc(_, Color)),

    % Find all valid moves for the player's color
    findall(move(FromX, FromY, ToX, ToY), (
        between(1, 5, FromX),
        between(1, 5, FromY),
        piece_at(Board, FromX, FromY, Color), % Check if the piece belongs to the player
        get_adjacent_cells(Board, FromX, FromY, AdjacentCells),
        member((ToX, ToY), AdjacentCells),
        is_valid_destination(Board, ToX, ToY)
    ), Moves),
    % Debug output
    write('Debug: Valid moves for '), write(Player), write(': '), write(Moves), nl.

% Checks if a move is valid.
valid_move(Board, Player, move(FromX, FromY, ToX, ToY)) :-
    (Player = human(_, Color) ; Player = pc(_, Color)),
    within_bounds(Board, FromX, FromY),
    piece_at(Board, FromX, FromY, Color),
    get_adjacent_cells(Board, FromX, FromY, AdjacentCells),
    member((ToX, ToY), AdjacentCells),
    is_valid_destination(Board, ToX, ToY).

is_valid_destination(Board, X, Y) :-
    within_bounds(Board, X, Y),
    nth1(Y, Board, Row),
    nth1(X, Row, Stack),
    length(Stack, Height),
    Height = 1. % Ensure the target is a neutral stack.

% Get all valid adjacent cells as a list
get_adjacent_cells(Board, X, Y, AdjacentCells) :-
    findall((ToX, ToY), (
        (ToX is X + 1, ToY = Y);
        (ToX is X - 1, ToY = Y);
        (ToX = X, ToY is Y + 1);
        (ToX = X, ToY is Y - 1)
    ), PossibleCells),
    findall((ValidX, ValidY), 
        (member((ValidX, ValidY), PossibleCells), within_bounds(Board, ValidX, ValidY)), 
        AdjacentCells).

% ===================== STACK OPERATIONS =====================

% Validates and executes a move, returning the new game state
move(game(Board, Player, Phase, BoardSize), move(FromX, FromY, ToX, ToY), game(NewBoard, NextPlayer, Phase, BoardSize)) :-
    valid_move(Board, Player, move(FromX, FromY, ToX, ToY)),
    execute_move(Board, FromX, FromY, ToX, ToY, NewBoard),
    next_player(Player, NextPlayer).


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
    findall(Value-M, (member(M, Moves), move(GameState, M, NewGameState), value(NewGameState, Player, Value)), ValuedMoves),
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

determine_winner(Board, game_config(Player1, Player2, _), Winner) :-
    % Collect all stack heights and their respective colors
    findall(Height-Color, tallest_stack(Board, Color, Height), Heights),
    keysort(Heights, SortedHeights),
    reverse(SortedHeights, [MaxHeight-Color|Rest]),

    % Check for tie conditions
    (Rest = [MaxHeight-_|_] ->
        Winner = 'Draw' % Multiple tallest stacks of the same height
    ;
        (Color = blue -> Player1 = human(Name, blue), Winner = Name
        ; Color = white -> Player2 = human(Name, white), Winner = Name
        ; Winner = 'PC') % Handle PC case
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
    nth1(BoardX, NewRow, [Color|RestStack], RestCells),
    nth1(BoardY, NewBoard, NewRow, RestRows).

add_player_piece(Board, X, Y, pc(_, Color), NewBoard) :-
    transform_coordinates(X, Y, BoardX, BoardY),
    within_bounds(Board, BoardX, BoardY),
    nth1(BoardY, Board, Row, RestRows),
    nth1(BoardX, Row, Stack, RestCells),
    Stack = [neutral|RestStack],
    nth1(BoardX, NewRow, [Color|RestStack], RestCells),
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

% Checks if a piece is at a specific position
piece_at(Board, X, Y, Player) :-
    nth1(Y, Board, Row),
    nth1(X, Row, [First|_]),
    First = Player.

pieces_per_player(BoardSize, PiecesPerPlayer) :-
    PiecesPerPlayer is BoardSize - 1.