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
    findall(o(ID)-p(X, Y), (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), o(ID))), Oracles),
    findall(c(ID)-p(X, Y), (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), c(ID))), Stations).

get_sorted_oracles(Oracles, SortedOracles) :-
    my_agent(Agent), get_agent_position(Agent, Pos),
    clustering_scores(Oracles, ClusteringScores),
    total_scores(Pos, ClusteringScores, Scores),
    sort_oracles(Scores, SortedOracles).

%find_identity_helper(_, [ActorName], _, ActorName).
find_identity_helper([], CurrentActors, _, ActorName) :-
    (
        length(CurrentActors, 1) -> CurrentActors = [ActorName]
    ;
        ActorName = unknown
    ).

find_identity_helper([Oracle], CurrentActors, Stations, ActorName) :-
    (
        visit_and_query_oracle(Oracle, CurrentActors, Stations, 0, NewActors) -> find_identity_helper([], NewActors, Stations, ActorName)
    ;
        find_identity_helper([], CurrentActors, Stations, ActorName)
    ).
     
find_identity_helper(Oracles, CurrentActors, Stations, ActorName) :-
    get_sorted_oracles(Oracles, [Oracle|Rest]), List=[Oracle|Rest],
    format("Sorted oracles: ~w~n", [List]),
    (
        format("Actor list: ~w ~n", [CurrentActors]),
        ailp_grid_size(N), Threshold is ceiling(((N*N)/4)/6),
        visit_and_query_oracle(Oracle, CurrentActors, Stations, Threshold, NewActors) -> find_identity_helper(Rest, NewActors, Stations, ActorName)
    ;
        find_identity_helper(Rest, CurrentActors, Stations, ActorName)
    ).

visit_and_query_oracle(Oracle-OraclePos, Actors, Stations, Threshold, NewActors) :-
    format("Going to new oracle ~n"),
    my_agent(A), get_agent_position(A, Pos), get_agent_energy(A, Energy),
    ailp_grid_size(N), QueryCost is ceiling(((N*N)/4)/10),
    format("Running A* ~n"),
    heuristic(find(Oracle), Pos, F), solve_task_astar(find(Oracle), [[F, Pos, []]], [], Path), length(Path, PathCost),
    format("A* done ~n"),

    TotalCost is PathCost + QueryCost + Threshold,
    (
        Energy >= TotalCost -> 
            (
                format("We can go there in one go ~n"),
                agent_do_moves(A, Path),
                agent_ask_oracle(A, Oracle, link, L),
                format("Queried oracle, Including actors ~n"),
                include(actor_has_link(L), Actors, NewActors),
                format("Included actors: ~w ~n", [NewActors])
            )
        ;
            (
                format("We need to topup, current energy: ~w ~n", Energy),
                get_best_station_distance(find(Oracle), Stations, Pos, OraclePos, Energy, Station, ChargePath, TargetPath, _),
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

calculate_total_cost(0, QueryCost, Threshold, TotalCost) :- TotalCost is QueryCost+ceiling(Threshold*0.1).
calculate_total_cost(1, QueryCost, Threshold, TotalCost) :- TotalCost is 1+QueryCost+ceiling(Threshold*0.3).
calculate_total_cost(PathCost, QueryCost, Threshold, TotalCost) :- between(2, 5, PathCost), TotalCost is PathCost+QueryCost+ceiling(Threshold*0.7).
calculate_total_cost(PathCost, QueryCost, Threshold, TotalCost) :- TotalCost is PathCost+QueryCost+Threshold.

sort_oracles(Scores, SortedOracles) :-
    sort(Scores, SortedPairs), 
    pairs_values(SortedPairs, SortedOracles).

pairs_values([], []).
pairs_values([_-Value|Rest], [Value|Values]) :- pairs_values(Rest, Values).

total_scores(AgentPos, ClusteringScores, Scores) :-
    W1 is 1, W2 is 1,
    findall(Score-(Oracle-Pos), (member(ClusterScore-(Oracle-Pos), ClusteringScores), map_distance(AgentPos, Pos, DAgent), Score is W1*DAgent-W2*ClusterScore), Scores).

clustering_scores(Oracles, Scores) :-
findall(Score-(Oracle-Pos), (member(Oracle-Pos, Oracles), findall(Distance, (member(OtherOracle-OtherPos, Oracles), OtherOracle \= Oracle, map_distance(Pos, OtherPos, Distance), Distance>0), Distances),
        sum_inverse_distances(Distances, 0, Score)
    ), Scores).

sum_inverse_distances([], Acc, Acc).
sum_inverse_distances([D|Ds], Acc, Total) :-
    InvD is 1/D,
    NewAcc is Acc+InvD,
    sum_inverse_distances(Ds, NewAcc, Total).

get_best_station_distance(Task, Stations, StartPos, TargetPos, EnergyAvailable, BestStation, BestChargePath, BestTargetPath, BestCost) :-
    findall(Cost-(Station-StationPos), (member(Station-StationPos, Stations), map_distance(StartPos, StationPos, H1), map_distance(StationPos, TargetPos, H2), Cost is H1+H2), Costs),
    sort(Costs, SortedPairs), pairs_values(SortedPairs, SortedStations),
    format("Sorted stations: ~w ~n", [SortedPairs]),
    try_stations(Task, SortedStations, StartPos, EnergyAvailable, BestStation, BestChargePath, BestTargetPath, BestCost). 

try_stations(_, [], _, _, _, _, _, _) :- fail.
try_stations(Task, [Station-_|_], Pos, Energy, Station, ChargePath, TargetPath, Cost) :-
    heuristic(find(Station), Pos, F), solve_task_astar(find(Station), [[F, Pos, []]], [], ChargePath), length(ChargePath, StationCost), Energy>=StationCost,
    get_final_position(ChargePath, StationPos), heuristic(Task, StationPos, F1), solve_task_astar(Task, [[F1, StationPos, []]], [], TargetPath), length(TargetPath, TargetCost),
    Cost is StationCost+TargetCost.
try_stations(Task, [_-_|Rest], Pos, Energy, Station, ChargePath, TargetPath, Cost) :- try_stations(Task, Rest, Pos, Energy, Station, ChargePath, TargetPath, Cost).
