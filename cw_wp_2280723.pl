:- dynamic actor_wikipedia_text/2.

% True if link L appears on A's wikipedia page
actor_has_link(L,A) :-
    actor_wikipedia_text(A, WT), !, wt_link(WT, L).
actor_has_link(L, A) :-
    actor(A), wp(A,WT), assert(actor_wikipedia_text(A, WT)), wt_link(WT,L).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    initialise_grid(Oracles, Stations),
    findall(Actor, actor(Actor), AllActors),
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
    format("Final list: ~w ~n", [CurrentActors]),
    (
        length(CurrentActors, 1) -> CurrentActors = [ActorName]
    ;
        ActorName = unknown
    ).

find_identity_helper([Oracle], CurrentActors, Stations, ActorName) :-
    visit_and_query_oracle(Oracle, CurrentActors, Stations, 0, NewActors, Status),
    (
        Status=success -> find_identity_helper([], NewActors, Stations, ActorName), !
    ;
        Status=recharge -> find_identity_helper([Oracle], CurrentActors, Stations, ActorName), !
    ;
        Status=unreachable -> find_identity_helper([], CurrentActors, Stations, ActorName), !
    ).

find_identity_helper(Oracles, CurrentActors, Stations, ActorName) :-
    get_sorted_oracles(Oracles, [Oracle|Rest]),
    ailp_grid_size(N), Threshold is ceiling(((N*N)/4)/6),
    visit_and_query_oracle(Oracle, CurrentActors, Stations, Threshold, NewActors, Status),
    (
        Status=success -> find_identity_helper(Rest, NewActors, Stations, ActorName)
    ;
        Status=recharge -> find_identity_helper(Oracles, CurrentActors, Stations, ActorName)
    ;
        Status=unreachable -> find_identity_helper(Rest, CurrentActors, Stations, ActorName)
    ;
        Status=no_energy -> exhaust_oracles(Oracles, CurrentActors, ActorName)
    ).

visit_and_query_oracle(Oracle-_, Actors, Stations, Threshold, NewActors, Status) :-
    my_agent(A), get_agent_position(A, Pos), get_agent_energy(A, Energy),
    ailp_grid_size(N), QueryCost is ceiling(((N*N)/4)/10),
    (
        heuristic(find(Oracle), Pos, F), solve_task_astar(find(Oracle), [[F, Pos, []]], [], Path), length(Path, PathCost),

        TotalCost is PathCost + QueryCost + Threshold,
        (
            Energy >= TotalCost -> 
                (
                    agent_do_moves(A, Path),
                    agent_ask_oracle(A, Oracle, link, L),
                    include(actor_has_link(L), Actors, NewActors),
                    Status=success, !
                )
            ;
                (
                    (
                        format("Not enough energy, topping up ~n"),
                        find_closest_station_distance(Stations, Pos, Energy, Station, ChargePath, _),
                        agent_do_moves(A, ChargePath),
                        agent_topup_energy(A, Station),
                        NewActors=Actors,
                        Status=recharge, !
                    ;
                        format("Can't reach any station, looking for nearby oracles ~n"),
                        NewActors=Actors,
                        Status=no_energy, !
                    )
                )
        )
        ;
            NewActors=Actors,
            Status=unreachable
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

find_closest_station_distance(Stations, StartPos, EnergyAvailable, BestStation, BestPath, BestCost) :-
    findall(Cost-(Station-StationPos), (member(Station-StationPos, Stations), map_distance(StartPos, StationPos, Cost)), Costs),
    sort(Costs, SortedPairs), pairs_values(SortedPairs, SortedStations),
    try_stations_no_return(SortedStations, StartPos, EnergyAvailable, BestStation, BestPath, BestCost). 

try_stations_no_return([], _, _, _, _, _) :- fail.
try_stations_no_return([Station-_|_], Pos, Energy, Station, Path, Cost) :-
    heuristic(find(Station), Pos, F), solve_task_astar(find(Station), [[F, Pos, []]], [], Path), length(Path, Cost), Energy>=Cost.
try_stations_no_return([_-_|Rest], Pos, Energy, Station, Path, Cost) :- try_stations_no_return(Rest, Pos, Energy, Station, Path, Cost).

exhaust_oracles([], CurrentActors, ActorName) :- format("No more oracles to attempt. Final list of actors: ~w~n", [CurrentActors]), (length(CurrentActors, 1), CurrentActors=[ActorName] ; ActorName=unknown).
exhaust_oracles(Oracles, CurrentActors, ActorName) :-
    my_agent(A), get_agent_position(A, Pos), get_agent_energy(A, Energy),
    sort_oracles_by_distance(Oracles, Pos, [Oracle-_|Rest]),
    (
        heuristic(find(Oracle), Pos, F), solve_task_astar(find(Oracle), [[F, Pos, []]], [], Path), length(Path, PathCost), QueryCost is ceiling(((N*N)/4)/10), Cost is PathCost+QueryCost, Energy>=Cost,
        agent_do_moves(A, Path), agent_ask_oracle(A, Oracle, link, L), include(actor_has_link(L), CurrentActors, NewActors), exhaust_oracles(Rest, NewActors, ActorName)
    ;
        exhaust_oracles(Rest, CurrentActors, ActorName)
    ).

sort_oracles_by_distance(Oracles, Pos, SortedOracles) :-
    findall(Distance-(Oracle-OraclePos), (member(Oracle-OraclePos, Oracles), map_distance(Pos, OraclePos, Distance)), Distances),
    sort(Distances, SortedDistances), pairs_values(SortedDistances, SortedOracles).
