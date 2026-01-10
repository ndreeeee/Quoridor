% ========================================
% MODULO QUORIDOR - Logica del Gioco
% ========================================
% Questo modulo implementa tutte le regole del gioco Quoridor,
% inclusi i movimenti dei pedoni, il posizionamento dei muri,
% e l'intelligenza artificiale con diversi livelli di difficoltà.

:- module(quoridor, [
    initial_state/1,        % Stato iniziale del gioco
    is_valid_move/2,        % Verifica se una mossa è valida
    make_move/3,            % Applica una mossa e genera il nuovo stato
    game_over/2,            % Controlla se il gioco è terminato
    choose_move/3,          % AI: sceglie la mossa in base alla difficoltà
    get_valid_pawn_moves/2  % Restituisce le mosse valide del pedone
]).

:- discontiguous choose_move/3.

% ========================================
% CONFIGURAZIONE DELLA GRIGLIA
% ========================================
% Qui puoi modificare le dimensioni della griglia:
% - Per griglia 9x9 (standard): board_size(9), initial_walls(10)
% - Per griglia 5x5 (veloce):   board_size(5), initial_walls(5)
board_size(9).       % Dimensione della griglia (9x9)
initial_walls(10).   % Numero di muri disponibili per ogni giocatore

% ========================================
% RAPPRESENTAZIONE DELLO STATO DI GIOCO
% ========================================
% Lo stato del gioco è rappresentato come: game(P1, P2, Walls, Turn)
%
% Dove:
%   - P1, P2 = player(X, Y, WallsLeft)
%       X, Y: posizione del pedone sulla griglia
%       WallsLeft: numero di muri rimanenti
%   - Walls = [wall(X, Y, Orientation), ...]
%       Lista di tutti i muri posizionati
%       Orientation può essere 'h' (orizzontale) o 'v' (verticale)
%   - Turn = p1 o p2 (indica di chi è il turno)
%
% STATO INIZIALE:
% - Giocatore 1 (P1) parte dalla riga 9 (in basso)
% - Giocatore 2 (P2) parte dalla riga 1 (in alto)
% - Entrambi partono dalla colonna 5 (centro)
% - Entrambi hanno 10 muri disponibili
% - Il primo turno è del giocatore 1
initial_state(game(player(5, 9, 10), player(5, 1, 10), [], p1)).

% ========================================
% VALIDAZIONE DELLE COORDINATE
% ========================================
% Verifica che una posizione (X, Y) sia valida sulla griglia.
% Una posizione è valida se entrambe le coordinate sono comprese
% tra 1 e la dimensione della griglia.
valid_pos(X, Y) :- 
    board_size(Size),
    between(1, Size, X),  % X deve essere tra 1 e Size
    between(1, Size, Y).  % Y deve essere tra 1 e Size

% ========================================
% VALIDAZIONE DELLE MOSSE
% ========================================

% Punto di ingresso principale: verifica se una mossa è valida
% dato lo stato corrente del gioco.
is_valid_move(game(P1, P2, Walls, Turn), Move) :-
    % Determina chi è il giocatore corrente e chi è l'avversario
    (Turn == p1 -> CurrentPlayer = P1, OtherPlayer = P2 ; CurrentPlayer = P2, OtherPlayer = P1),
    % Delega la validazione alla funzione is_valid_action
    is_valid_action(CurrentPlayer, OtherPlayer, Walls, Move).

% --- SALTO SOPRA L'AVVERSARIO ---
% Questa regola permette di saltare l'avversario quando è adiacente.
% Il salto porta il pedone nella casella immediatamente dietro l'avversario.
is_valid_action(player(X, Y, _), player(OX, OY, _), Walls, move(JumpX, JumpY)) :-
    % 1. L'avversario deve essere adiacente al giocatore corrente
    adjacent(X, Y, OX, OY),
    % 2. Non ci devono essere muri tra giocatore e avversario
    \+ wall_blocks(X, Y, OX, OY, Walls),
    % 3. Calcola la destinazione del salto (2 caselle nella stessa direzione)
    %    Esempio: se sei in (3,3) e l'avversario in (3,4), salti in (3,5)
    JumpX is OX + (OX - X),
    JumpY is OY + (OY - Y),
    % 4. La destinazione del salto deve essere una posizione valida
    valid_pos(JumpX, JumpY),
    % 5. Non ci devono essere muri tra avversario e destinazione
    \+ wall_blocks(OX, OY, JumpX, JumpY, Walls).
% --- MOVIMENTO NORMALE DEL PEDONE ---
% Permette di muovere il pedone in una casella adiacente vuota.
is_valid_action(player(X, Y, _), player(OX, OY, _), Walls, move(NX, NY)) :-
    valid_pos(NX, NY),                      % La destinazione deve essere valida
    adjacent(X, Y, NX, NY),                 % La destinazione deve essere adiacente
    \+ wall_blocks(X, Y, NX, NY, Walls),    % Non ci devono essere muri nel percorso
    (NX \= OX ; NY \= OY).                  % Non puoi occupare la casella dell'avversario

% --- POSIZIONAMENTO DI UN MURO ---
% Permette di piazzare un muro se:
% 1. Il giocatore ha ancora muri disponibili
% 2. La posizione è valida
% 3. Non c'è già un muro in quella posizione
% 4. Il muro non si sovrappone ad altri muri
% 5. Il muro non blocca completamente il percorso verso l'obiettivo
is_valid_action(player(PX, PY, WallsLeft), player(OX, OY, _), Walls, place_wall(X, Y, O)) :-
    WallsLeft > 0,                          % Devi avere muri disponibili
    valid_wall_pos(X, Y),                   % La posizione del muro deve essere valida
    \+ member(wall(X, Y, _), Walls),        % Non ci deve essere già un muro qui
    \+ wall_overlaps(X, Y, O, Walls),       % Il muro non deve sovrapporsi ad altri
    % Verifica che entrambi i giocatori possano ancora raggiungere il loro obiettivo
    \+ blocks_path(player(PX, PY, WallsLeft), player(OX, OY, _), [wall(X, Y, O)|Walls]).

% ========================================
% PATHFINDING - Ricerca del Percorso (BFS)
% ========================================
% Queste funzioni verificano se un giocatore può ancora raggiungere
% il suo obiettivo. Usa l'algoritmo BFS (Breadth-First Search).

% Verifica se il posizionamento di un muro blocca il percorso di un giocatore
blocks_path(P1, _, Walls) :-
    \+ has_path_to_goal(P1, Walls, p1),  % Se P1 non ha più un percorso, blocca
    !.
blocks_path(_, P2, Walls) :-
    \+ has_path_to_goal(P2, Walls, p2).  % Se P2 non ha più un percorso, blocca

% Verifica se un giocatore ha un percorso valido verso il suo obiettivo
has_path_to_goal(player(X, Y, _), Walls, Player) :-
    goal_row(Player, GoalY),             % Ottiene la riga obiettivo del giocatore
    bfs([[X, Y]], GoalY, Walls, []).     % Esegue BFS dalla posizione corrente

% Definisce la riga obiettivo per ciascun giocatore:
% - P1 parte dalla riga 9 (in basso) e deve raggiungere la riga 1 (in alto)
% - P2 parte dalla riga 1 (in alto) e deve raggiungere la riga Size (in basso)
goal_row(p1, 1).
goal_row(p2, Size) :- board_size(Size).

% BFS: Algoritmo di ricerca in ampiezza
% Caso base: se la posizione corrente è sulla riga obiettivo, successo!
bfs([[_, Y]|_], GoalY, _, _) :- Y == GoalY, !.
% Caso ricorsivo: esplora le posizioni adiacenti
bfs([[X, Y]|Rest], GoalY, Walls, Visited) :-
    % Trova tutte le mosse valide dalla posizione corrente
    findall([NX, NY], (
        adjacent(X, Y, NX, NY),              % Posizione adiacente
        valid_pos(NX, NY),                   % Posizione valida sulla griglia
        \+ wall_blocks(X, Y, NX, NY, Walls), % Nessun muro blocca il movimento
        \+ member([NX, NY], Visited),        % Non già visitata
        \+ member([NX, NY], [[X, Y]|Rest])   % Non già in coda
    ), NextMoves),
    append(Rest, NextMoves, NewQueue),       % Aggiungi le nuove mosse alla coda
    bfs(NewQueue, GoalY, Walls, [[X, Y]|Visited]).  % Continua la ricerca

% ========================================
% FUNZIONI HELPER
% ========================================

% Definisce le 4 posizioni adiacenti a (X, Y): su, giù, sinistra, destra
adjacent(X, Y, X, NY) :- NY is Y + 1.   % Movimento in alto
adjacent(X, Y, X, NY) :- NY is Y - 1.   % Movimento in basso
adjacent(X, Y, NX, Y) :- NX is X + 1.   % Movimento a destra
adjacent(X, Y, NX, Y) :- NX is X - 1.   % Movimento a sinistra
% Verifica che una posizione per un muro sia valida
% I muri possono essere posizionati solo tra le caselle, quindi
% le coordinate valide vanno da 1 a (Size-1)
valid_wall_pos(X, Y) :- 
    board_size(Size),
    MaxWall is Size - 1,
    between(1, MaxWall, X), 
    between(1, MaxWall, Y).

% --- LOGICA DI BLOCCO DEI MURI ---

% Verifica se un muro blocca il movimento verticale (su/giù)
wall_blocks(X, Y, X, NY, Walls) :-
    MinY is min(Y, NY),
    % Un muro orizzontale in (X, MinY) o (X-1, MinY) blocca il movimento
    (member(wall(X, MinY, h), Walls) ; member(wall(X1, MinY, h), Walls), X1 is X - 1).

% Verifica se un muro blocca il movimento orizzontale (sinistra/destra)
wall_blocks(X, Y, NX, Y, Walls) :-
    MinX is min(X, NX),
    % Un muro verticale in (MinX, Y) o (MinX, Y-1) blocca il movimento
    (member(wall(MinX, Y, v), Walls) ; member(wall(MinX, Y1, v), Walls), Y1 is Y - 1).

% --- LOGICA DI SOVRAPPOSIZIONE DEI MURI ---

% Un muro orizzontale si sovrappone se c'è un altro muro orizzontale adiacente
wall_overlaps(X, Y, h, Walls) :-
    member(wall(X1, Y, h), Walls), (X1 is X - 1 ; X1 is X + 1).
% Un muro verticale si sovrappone se c'è un altro muro verticale adiacente
wall_overlaps(X, Y, v, Walls) :-
    member(wall(X, Y1, v), Walls), (Y1 is Y - 1 ; Y1 is Y + 1).
% I muri si sovrappongono anche se si incrociano nella stessa posizione
wall_overlaps(X, Y, h, Walls) :- member(wall(X, Y, v), Walls).
wall_overlaps(X, Y, v, Walls) :- member(wall(X, Y, h), Walls).

% ========================================
% APPLICAZIONE DELLE MOSSE
% ========================================
% Queste funzioni applicano una mossa allo stato corrente
% e generano il nuovo stato di gioco.

% Applica una mossa per il giocatore 1 (P1)
make_move(game(P1, P2, Walls, p1), Move, game(NewP1, P2, NewWalls, p2)) :-
    apply_move(P1, Walls, Move, NewP1, NewWalls).

% Applica una mossa per il giocatore 2 (P2)
make_move(game(P1, P2, Walls, p2), Move, game(P1, NewP2, NewWalls, p1)) :-
    apply_move(P2, Walls, Move, NewP2, NewWalls).

% Applica un movimento del pedone: aggiorna solo la posizione
apply_move(player(_, _, W), Walls, move(NX, NY), player(NX, NY, W), Walls).
% Applica il posizionamento di un muro: aggiunge il muro e decrementa il contatore
apply_move(player(X, Y, W), Walls, place_wall(WX, WY, O), player(X, Y, NW), [wall(WX, WY, O)|Walls]) :-
    NW is W - 1.

% ========================================
% CONDIZIONI DI VITTORIA
% ========================================
% Il gioco termina quando un giocatore raggiunge la riga obiettivo.

% P1 vince se raggiunge la riga 1 (in alto)
game_over(game(player(_, 1, _), _, _, _), p1).
% P2 vince se raggiunge la riga Size (in basso)
game_over(game(_, player(_, Y, _), _, _), p2) :- 
    board_size(Y).

% ========================================
% INTELLIGENZA ARTIFICIALE (AI)
% ========================================
% Implementa 4 livelli di difficoltà: easy, medium, hard, extreme

% --- DIFFICOLTÀ FACILE (EASY) ---
% Sceglie una mossa casuale tra tutte le mosse valide.
% choose_move(+Difficulty, +State, -Move)
choose_move(easy, State, Move) :-
    findall_valid_moves(State, Moves),  % Trova tutte le mosse valide
    Moves \= [],                         % Verifica che ci siano mosse disponibili
    random_member(Move, Moves).          % Sceglie una mossa a caso

% --- DIFFICOLTÀ MEDIA (MEDIUM) ---
% Usa una strategia greedy: sceglie la mossa che minimizza la distanza
% dall'obiettivo. Preferisce muovere il pedone rispetto ai muri.
choose_move(medium, State, Move) :-
    findall_pawn_moves(State, Moves),    % Trova tutte le mosse del pedone
    (Moves \= [] ->
        % Se ci sono mosse del pedone, scegli la migliore
        best_greedy_move(State, Moves, Move)
    ;
        % Nessuna mossa del pedone, piazza un muro casuale
        findall_wall_moves(State, WallMoves),
        WallMoves \= [],
        random_member(Move, WallMoves)
    ).
% --- DIFFICOLTÀ DIFFICILE (HARD) ---
% Strategia ibrida: usa muri difensivi quando l'avversario è vicino,
% altrimenti usa minimax con profondità 2 per scegliere la mossa ottimale.
choose_move(hard, State, Move) :-
    State = game(P1, P2, _, Turn),
    (should_place_defensive_wall(P1, P2, Turn) ->
        % Cerca di trovare un buon muro difensivo
        findall_wall_moves(State, WallMoves),
        findall(Score-WallMove, (
            member(WallMove, WallMoves),
            make_move(State, WallMove, NextState),
            evaluate(NextState, Score)
        ), ScoredWalls),
        (ScoredWalls \= [] ->
            % Ordina i muri per punteggio e sceglie il migliore
            sort(ScoredWalls, Sorted),
            reverse(Sorted, [_-Move|_])
        ;
            % Nessun muro valido, usa minimax
            minimax(State, 2, -1000, 1000, Move, _)
        )
    ;
        % Usa minimax per il movimento del pedone
        minimax(State, 2, -1000, 1000, Move, _)
    ).

% Decide se l'AI dovrebbe piazzare un muro difensivo
% Condizioni: avere muri disponibili E avversario vicino (< 4 righe)
should_place_defensive_wall(player(_, Y1, W1), player(_, Y2, _), p2) :-
    W1 > 0,                              % Deve avere muri disponibili
    Distance is abs(Y1 - Y2),
    Distance < 4.                        % L'avversario deve essere vicino

% --- DIFFICOLTÀ ESTREMA (EXTREME) ---
% Strategia avanzata: valuta le migliori 5 mosse del pedone e i migliori 3 muri,
% poi usa minimax con profondità 5 su questo set ridotto per massimizzare
% la qualità della decisione riducendo il tempo di calcolo.
choose_move(extreme, State, Move) :-
    findall_pawn_moves(State, AllPawnMoves),
    (AllPawnMoves \= [] ->
        % Valuta tutte le mosse del pedone e le ordina per punteggio
        findall(Score-M, (
            member(M, AllPawnMoves),
            make_move(State, M, NextState),
            evaluate(NextState, Score)
        ), ScoredPawns),
        sort(ScoredPawns, SortedPawns),
        reverse(SortedPawns, TopPawns),
        take_n(5, TopPawns, BestPawns),          % Prendi le migliori 5 mosse del pedone
        findall(PM, member(_-PM, BestPawns), PawnMoves),
        
        % Genera le migliori mosse con i muri (solo se strategicamente utile)
        State = game(player(_, _, W1), _, _, _),
        (W1 > 3 ->
            % Se abbiamo più di 3 muri, considera anche i muri
            findall_wall_moves(State, AllWallMoves),
            findall(Score-WM, (
                member(WM, AllWallMoves),
                make_move(State, WM, NextState),
                evaluate(NextState, Score)
            ), ScoredWalls),
            (ScoredWalls \= [] ->
                sort(ScoredWalls, SortedWalls),
                reverse(SortedWalls, TopWalls),
                take_n(3, TopWalls, BestWalls),  % Prendi i migliori 3 muri
                findall(WM, member(_-WM, BestWalls), WallMoves),
                append(PawnMoves, WallMoves, CombinedMoves)
            ;
                CombinedMoves = PawnMoves
            )
        ;
            CombinedMoves = PawnMoves
        ),
        
        % Usa minimax su questo set ridotto di mosse migliori
        (CombinedMoves \= [] ->
            (minimax_limited(State, CombinedMoves, 5, -1000, 1000, TempMove, _) ->
                Move = TempMove
            ;
                % Minimax fallito, usa la prima mossa migliore
                CombinedMoves = [Move|_]
            )
        ;
            % Nessuna mossa valida trovata, usa qualsiasi mossa del pedone
            AllPawnMoves = [Move|_]
        )
    ;
        % Nessuna mossa del pedone disponibile, prova qualsiasi mossa valida
        findall_valid_moves(State, AllMoves),
        (AllMoves \= [] -> AllMoves = [Move|_] ; fail)
    ).

% ========================================
% FUNZIONI HELPER PER L'AI
% ========================================

% Prende i primi N elementi da una lista
take_n(0, _, []) :- !.
take_n(_, [], []) :- !.
take_n(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take_n(N1, T, R).
% ========================================
% ALGORITMO MINIMAX CON MOSSE LIMITATE
% ========================================
% Versione ottimizzata di minimax che lavora su un set pre-selezionato
% di mosse invece di generarle tutte. Usato dalla difficoltà "extreme".

% Caso base: profondità 0, valuta lo stato
minimax_limited(State, _Moves, 0, _, _, nil, Score) :-
    evaluate(State, Score), !.

% Caso base: gioco terminato
minimax_limited(State, _, _, _, _, nil, BestScore) :-
    game_over(State, Winner),
    (Winner == p2 -> BestScore = 1000 ; BestScore = -1000), !.

% Caso ricorsivo: valuta le mosse limitate
minimax_limited(State, Moves, Depth, Alpha, Beta, BestMove, BestScore) :-
    Depth > 0,
    Moves \= [],
    State = game(_, _, _, Turn),
    (Turn == p2 ->
        % Turno dell'AI: massimizza il punteggio
        maximize_limited(Moves, State, Depth, Alpha, Beta, nil, -10000, BestMove, BestScore)
    ;
        % Turno del giocatore: minimizza il punteggio
        minimize_limited(Moves, State, Depth, Alpha, Beta, nil, 10000, BestMove, BestScore)
    ).

% Massimizzazione (AI vuole massimizzare)
maximize_limited([], _, _, _, _, BestMove, BestScore, BestMove, BestScore) :- !.
maximize_limited([Move|Rest], State, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, FinalMove, FinalScore) :-
    (make_move(State, Move, NextState) ->
        NewDepth is Depth - 1,
        findall_pawn_moves(NextState, NextMoves),
        (NextMoves \= [] ->
            (minimax_limited(NextState, NextMoves, NewDepth, Alpha, Beta, _, Score) ->
                true
            ;
                Score = CurrentBestScore
            )
        ;
            evaluate(NextState, Score)
        ),
        (Score > CurrentBestScore ->
            NewBestMove = Move, 
            NewBestScore = Score
        ;
            NewBestMove = CurrentBestMove,
            NewBestScore = CurrentBestScore
        ),
        % Potatura Alpha-Beta
        (NewBestScore >= Beta ->
            FinalMove = NewBestMove, 
            FinalScore = NewBestScore
        ;
            NewAlpha is max(Alpha, NewBestScore),
            maximize_limited(Rest, State, Depth, NewAlpha, Beta, NewBestMove, NewBestScore, FinalMove, FinalScore)
        )
    ;
        % Mossa fallita, salta
        maximize_limited(Rest, State, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, FinalMove, FinalScore)
    ).

% Minimizzazione (giocatore vuole minimizzare)
minimize_limited([], _, _, _, _, BestMove, BestScore, BestMove, BestScore) :- !.
minimize_limited([Move|Rest], State, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, FinalMove, FinalScore) :-
    (make_move(State, Move, NextState) ->
        NewDepth is Depth - 1,
        findall_pawn_moves(NextState, NextMoves),
        (NextMoves \= [] ->
            (minimax_limited(NextState, NextMoves, NewDepth, Alpha, Beta, _, Score) ->
                true
            ;
                Score = CurrentBestScore
            )
        ;
            evaluate(NextState, Score)
        ),
        (Score < CurrentBestScore ->
            NewBestMove = Move, 
            NewBestScore = Score
        ;
            NewBestMove = CurrentBestMove,
            NewBestScore = CurrentBestScore
        ),
        % Potatura Alpha-Beta
        (NewBestScore =< Alpha ->
            FinalMove = NewBestMove, 
            FinalScore = NewBestScore
        ;
            NewBeta is min(Beta, NewBestScore),
            minimize_limited(Rest, State, Depth, Alpha, NewBeta, NewBestMove, NewBestScore, FinalMove, FinalScore)
        )
    ;
        % Mossa fallita, salta
        minimize_limited(Rest, State, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, FinalMove, FinalScore)
    ).
% ========================================
% STRATEGIA GREEDY (per difficoltà media)
% ========================================
% Sceglie la mossa che porta al punteggio più alto secondo la funzione di valutazione.
best_greedy_move(State, Moves, BestMove) :-
    findall(Score-M, (
        member(M, Moves),
        make_move(State, M, NextState),
        evaluate(NextState, Score)
    ), ScoredMoves),
    sort(ScoredMoves, Sorted),
    reverse(Sorted, [_-BestMove|_]).

% ========================================
% FUNZIONI PER GENERARE LE MOSSE
% ========================================

% Trova tutte le mosse valide (pedone + muri)
findall_valid_moves(State, Moves) :-
    findall_pawn_moves(State, PawnMoves),
    findall_wall_moves(State, WallMoves),
    append(PawnMoves, WallMoves, Moves).

% Trova tutte le mosse valide del pedone
findall_pawn_moves(State, Moves) :-
    board_size(Size),
    findall(move(X, Y), (
        between(1, Size, X),
        between(1, Size, Y),
        is_valid_move(State, move(X, Y))
    ), Moves).

% Trova tutte le mosse valide per piazzare muri
findall_wall_moves(State, Moves) :-
    board_size(Size),
    MaxWall is Size - 1,
    findall(place_wall(X, Y, O), (
        between(1, MaxWall, X),
        between(1, MaxWall, Y),
        member(O, [h, v]),
        is_valid_move(State, place_wall(X, Y, O))
    ), Moves).

% ========================================
% ALGORITMO MINIMAX STANDARD
% ========================================
% Implementazione classica di minimax con potatura Alpha-Beta.
% Usato dalle difficoltà "hard" e "medium".

% Caso base: profondità 0, valuta lo stato corrente
minimax(State, 0, _, _, nil, Score) :-
    evaluate(State, Score), !.

% Caso base: il gioco è terminato
minimax(State, _, _, _, nil, BestScore) :-
    game_over(State, Winner),
    (Winner == p2 -> BestScore = 1000 ; BestScore = -1000), !.

% Caso ricorsivo: genera e valuta tutte le mosse del pedone
minimax(State, Depth, Alpha, Beta, BestMove, BestScore) :-
    findall_pawn_moves(State, Moves),
    (Moves \= [] ->
        State = game(_, _, _, Turn),
        (Turn == p2 ->
            % Turno dell'AI: massimizza
            maximize(Moves, State, Depth, Alpha, Beta, nil, -10000, BestMove, BestScore)
        ;
            % Turno del giocatore: minimizza
            minimize(Moves, State, Depth, Alpha, Beta, nil, 10000, BestMove, BestScore)
        )
    ;
        % Nessuna mossa disponibile, valuta lo stato
        evaluate(State, BestScore),
        BestMove = nil
    ).

% Massimizzazione: cerca la mossa con il punteggio più alto
maximize([], _, _, _, _, BestMove, BestScore, BestMove, BestScore).
maximize([Move|Rest], State, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, FinalBestMove, FinalBestScore) :-
    make_move(State, Move, NextState),
    NewDepth is Depth - 1,
    minimax(NextState, NewDepth, Alpha, Beta, _, Score),
    (Score > CurrentBestScore ->
        NewBestScore = Score, NewBestMove = Move
    ;   
        NewBestScore = CurrentBestScore, NewBestMove = CurrentBestMove
    ),
    % Potatura Beta: se il punteggio supera Beta, interrompi
    (NewBestScore >= Beta ->
        FinalBestMove = NewBestMove, FinalBestScore = NewBestScore
    ;   
        NewAlpha is max(Alpha, NewBestScore),
        maximize(Rest, State, Depth, NewAlpha, Beta, NewBestMove, NewBestScore, FinalBestMove, FinalBestScore)
    ).
% Minimizzazione: cerca la mossa con il punteggio più basso
minimize([], _, _, _, _, BestMove, BestScore, BestMove, BestScore).
minimize([Move|Rest], State, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, FinalBestMove, FinalBestScore) :-
    make_move(State, Move, NextState),
    NewDepth is Depth - 1,
    minimax(NextState, NewDepth, Alpha, Beta, _, Score),
    (Score < CurrentBestScore ->
        NewBestScore = Score, NewBestMove = Move
    ;   
        NewBestScore = CurrentBestScore, NewBestMove = CurrentBestMove
    ),
    % Potatura Alpha: se il punteggio scende sotto Alpha, interrompi
    (NewBestScore =< Alpha ->
        FinalBestMove = NewBestMove, FinalBestScore = NewBestScore
    ;   
        NewBeta is min(Beta, NewBestScore),
        minimize(Rest, State, Depth, Alpha, NewBeta, NewBestMove, NewBestScore, FinalBestMove, FinalBestScore)
    ).

% ========================================
% FUNZIONE DI VALUTAZIONE
% ========================================
% Calcola quanto è favorevole uno stato per l'AI (P2).
%
% Euristica: Score = (Distanza di P1 dall'obiettivo) - (Distanza di P2 dall'obiettivo)
%
% - Se P2 è vicino all'obiettivo (distanza piccola), lo Score aumenta (positivo è buono per P2)
% - Se P1 è lontano dall'obiettivo (distanza grande), lo Score aumenta
% - L'AI cerca di massimizzare questo punteggio

evaluate(game(P1, P2, Walls, _), Score) :-
    shortest_path_len(P1, Walls, p1, D1),  % Distanza di P1 dall'obiettivo
    shortest_path_len(P2, Walls, p2, D2),  % Distanza di P2 dall'obiettivo
    Score is D1 - D2.                       % Punteggio finale

% Calcola la lunghezza del percorso più breve verso l'obiettivo
shortest_path_len(player(X, Y, _), Walls, Player, Dist) :-
    goal_row(Player, GoalY),
    bfs_dist([[X, Y, 0]], GoalY, Walls, [], Dist).

% BFS con calcolo della distanza
% Caso base: raggiunta la riga obiettivo
bfs_dist([[_, Y, D]|_], GoalY, _, _, D) :- Y == GoalY, !.
% Caso ricorsivo: continua la ricerca
bfs_dist([[X, Y, D]|Rest], GoalY, Walls, Visited, Dist) :-
    D1 is D + 1,  % Incrementa la distanza
    findall([NX, NY, D1], (
        adjacent(X, Y, NX, NY),
        valid_pos(NX, NY),
        \+ wall_blocks(X, Y, NX, NY, Walls),
        \+ member([NX, NY], Visited),
        \+ member([NX, NY, _], [[X, Y, D]|Rest])
    ), NextMoves),
    append(Rest, NextMoves, NewQueue),
    bfs_dist(NewQueue, GoalY, Walls, [[X, Y]|Visited], Dist).

% ========================================
% INTERFACCIA PER L'UI
% ========================================
% Restituisce le mosse valide del pedone per l'interfaccia utente.
get_valid_pawn_moves(State, Moves) :-
    findall([X, Y], is_valid_move(State, move(X, Y)), Moves).