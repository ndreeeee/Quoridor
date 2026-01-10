:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).

:- use_module(quoridor).

% --- Server Setup ---

% Salva la directory del progetto durante il caricamento
:- dynamic web_directory/1.
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/web', WebDir),
   assertz(web_directory(WebDir)).

% Handler per i file statici
:- http_handler(root(.), serve_static_files, [prefix]).
:- http_handler(root(game/start), start_game_handler, []).
:- http_handler(root(game/move), move_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% --- Handlers ---

% Handler per servire i file statici dalla cartella 'web'
serve_static_files(Request) :-
    web_directory(WebDir),
    http_reply_from_files(WebDir, [], Request).

start_game_handler(_Request) :-
    initial_state(State),
    state_to_json(State, JSON),
    reply_json(JSON).

move_handler(Request) :-
    http_read_json_dict(Request, Dict),
    % Dict contains: difficulty, state (optional, or we track it?), move
    % For simplicity, let's assume the client sends the current state and the move.
    % Ideally, server should track state, but stateless is easier for dev.
    % Let's expect: { "difficulty": "easy", "move": { "type": "move", "x": 5, "y": 8 }, "state": ... }
    % Actually, simpler: Client sends move, Server applies, then Server runs AI, returns NEW state.
    
    atom_string(Difficulty, Dict.difficulty),
    CurrentState = Dict.state,
    PlayerMove = Dict.move,
    
    json_to_state(CurrentState, State),
    json_to_move(PlayerMove, Move),
    
    format(user_error, '~n=== DEBUG ===~n', []),
    format(user_error, 'Received Move: ~w~n', [Move]),
    format(user_error, 'Reconstructed State: ~w~n', [State]),
    format(user_error, 'Testing is_valid_move...~n', []),
    
    (is_valid_move(State, Move) ->
        format(user_error, 'Move is valid, applying...~n', []),
        make_move(State, Move, StateAfterPlayer),
        format(user_error, 'StateAfterPlayer: ~w~n', [StateAfterPlayer]),
        (game_over(StateAfterPlayer, _) ->
            format(user_error, 'Game over after player move~n', []),
            FinalState = StateAfterPlayer,
            Winner = "player"
        ;
            format(user_error, 'Game continues, AI turn...~n', []),
            % AI Turn
            (choose_move(Difficulty, StateAfterPlayer, AIMove) ->
                format(user_error, 'AI chose: ~w~n', [AIMove]),
                make_move(StateAfterPlayer, AIMove, FinalState),
                format(user_error, 'FinalState: ~w~n', [FinalState]),
                (game_over(FinalState, _) -> Winner = "ai" ; Winner = null)
            ;
                % AI cannot move? Should not happen if game not over
                format(user_error, 'AI cannot move!~n', []),
                FinalState = StateAfterPlayer, Winner = null
            )
        ),
        format(user_error, 'Converting FinalState to JSON...~n', []),
        state_to_json(FinalState, ResponseState),
        format(user_error, 'ResponseState: ~w~n', [ResponseState]),
        format(user_error, 'Move ACCEPTED~n=============~n~n', []),
        reply_json(_{valid: true, state: ResponseState, winner: Winner})
    ;
        format(user_error, 'Move REJECTED~n=============~n~n', []),
        reply_json(_{valid: false})
    ).

% --- JSON Conversion Helpers ---

state_to_json(game(player(X1, Y1, W1), player(X2, Y2, W2), Walls, Turn), _{
    p1: _{x: X1, y: Y1, walls: W1},
    p2: _{x: X2, y: Y2, walls: W2},
    walls: WallList,
    turn: Turn,
    valid_moves: []
}) :-
    findall(_{x: WX, y: WY, o: O}, member(wall(WX, WY, O), Walls), WallList).

json_to_state(Dict, game(player(X1, Y1, W1), player(X2, Y2, W2), Walls, Turn)) :-
    ensure_number(Dict.p1.x, X1), ensure_number(Dict.p1.y, Y1), ensure_number(Dict.p1.walls, W1),
    ensure_number(Dict.p2.x, X2), ensure_number(Dict.p2.y, Y2), ensure_number(Dict.p2.walls, W2),
    atom_string(Turn, Dict.turn), % Ensure Turn is an atom
    % Convert walls list
    maplist(json_wall_to_term, Dict.walls, Walls).

json_wall_to_term(_{x: X, y: Y, o: O}, wall(X, Y, AtomO)) :-
    atom_string(AtomO, O).

json_to_move(_{type: "move", x: X, y: Y}, move(NX, NY)) :-
    ensure_number(X, NX), ensure_number(Y, NY).
json_to_move(_{type: "wall", x: X, y: Y, o: O}, place_wall(NX, NY, AtomO)) :-
    ensure_number(X, NX), ensure_number(Y, NY),
    atom_string(AtomO, O).

ensure_number(N, N) :- number(N), !.
ensure_number(S, N) :- string(S), number_string(N, S), !.
ensure_number(A, N) :- atom(A), atom_number(A, N), !.
