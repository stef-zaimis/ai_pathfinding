% True if link L appears on A's wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).

% Attempt to solve by visiting each oracle in ID order
eliminate(As,A,K) :- 
    As=[A], !
    ;
    solve_task(find(o(K)),_), !,
    my_agent(N),
    agent_ask_oracle(N,o(K),link,L), 
    include(actor_has_link(L),As,ViableAs), 
    K1 is K+1, 
    eliminate(ViableAs,A,K1).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    initialise_grid(Oracles, Stations),
    format("All stations: ~w ~n", [Stations]),
    findall(Actor, actor(Actor), AllActors),
    format("All actors: ~w ~n", [AllActors]),
    find_identity_helper(Oracles, AllActors, Stations, A).
    %findall(A,actor(A),As), eliminate(As,A,1).

initialise_grid(Oracles, Stations) :-
    ailp_grid_size(N),
    findall(o(ID), (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), o(ID))), Oracles),
    findall(c(ID)-p(X, Y), (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), c(ID))), Stations).

find_identity_helper(_, [ActorName], _, ActorName).
find_identity_helper([], CurrentActors, _, ActorName) :-
    (
        length(CurrentActors, 1) -> CurrentActors = [ActorName]
    ;
        ActorName = unknown
    ).
find_identity_helper([Oracle|Rest], CurrentActors, Stations, ActorName) :-
    (
        format("Actor list: ~w ~n", [CurrentActors]),
        ailp_grid_size(N), Threshold is ceil(((N*N)/4)/4),
        visit_and_query_oracle(Oracle, CurrentActors, Stations, Threshold, NewActors) -> find_identity_helper(Rest, NewActors, Stations, ActorName)
    ;
        find_identity_helper(Rest, CurrentActors, Stations, ActorName)
    ).

visit_and_query_oracle(Oracle, Actors, Stations, Threshold, NewActors) :-
    format("Going to new oracle ~n"),
    my_agent(A), get_agent_position(A, Pos), get_agent_energy(A, Energy),
    ailp_grid_size(N), QueryCost is ceil(((N*N)/4)/10),
    heuristic(find(Oracle), Pos, F), solve_task_astar(find(Oracle), [[F, Pos, []]], [], Path), length(Path, PathCost),

    TotalCost is PathCost + QueryCost + Threshold,
    (
        Energy >= TotalCost -> 
            (
                format("We can go there in one go ~n"),
                agent_do_moves(A, Path),
                agent_ask_oracle(A, Oracle, link, L),
                format("Queried oracle ~n"),
                include(actor_has_link(L), Actors, NewActors),
                format("Included actors: ~w ~n", [NewActors])
            )
        ;
            (
                format("We need to topup, current energy: ~w ~n", Energy),
                get_best_station(find(Oracle), Stations, Pos, Energy, Station, ChargePath, TargetPath, _),
                format("Going to charging station ~n"),
                agent_do_moves(A, ChargePath),
                agent_topup_energy(A, Station),
                format("Going to  oracle ~n"),
                agent_do_moves(A, TargetPath),
                agent_ask_oracle(A, Oracle, link, L),
                format("Queried oracle ~n"),
                include(actor_has_link(L), Actors, NewActors),
                format("Included actors: ~w ~n", [NewActors])
            )
    ).
