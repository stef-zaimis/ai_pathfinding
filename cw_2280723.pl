% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), get_agent_position(A,P),
    get_agent_energy(A, Energy),
    (
        can_reach_target(Task, P, Path, PathCost),
        (
            Energy >= PathCost,
            agent_do_moves(A, Path), length(Path, Cost)
        ;
            format("Not enough energy, getting path to charging station ~n"),
            find_best_charging_station(Task, P, Energy, Station, ChargePath, TargetPath, Cost),
            agent_do_moves(A, ChargePath),
            agent_topup_energy(A, Station),
            agent_do_moves(A, TargetPath)
        )
        ;
        fail
    ).

find_all_stations(Stations) :-
    ailp_grid_size(N),
    findall(c(ID)-p(X, Y), (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), c(ID))), Stations).

can_reach_target(Task, StartPos, Path, Cost) :-
    heuristic(Task, StartPos, F),
    solve_task_astar(Task, [[F, StartPos, []]], [], Path),
    length(Path, Cost).

find_best_charging_station(Task, StartPos, EnergyAvailable, Station, ChargePath, TargetPath, Cost) :-
    find_all_stations(Stations),
    Stations \= [],
    get_target_position(Task, StartPos, TargetPos),
    get_best_station(Task, Stations, StartPos, TargetPos, EnergyAvailable, Station, ChargePath, TargetPath, Cost).

get_target_position(go(Pos), _, Pos).
get_target_position(Task, StartPos, Pos) :-
    can_reach_target(Task, StartPos, Path, _), get_final_position(Path, Pos).

get_best_station(Task, Stations, StartPos, TargetPos, EnergyAvailable, BestStation, BestChargePath, BestTargetPath, BestCost) :-
    findall(Cost-(Station-StationPos), (member(Station-StationPos, Stations), map_distance(StartPos, StationPos, H1), map_distance(StationPos, TargetPos, H2), Cost is H1+H2), Costs),
    sort(Costs, SortedPairs), pairs_values(SortedPairs, SortedStations),
    try_stations(Task, SortedStations, StartPos, EnergyAvailable, BestStation, BestChargePath, BestTargetPath, BestCost). 

try_stations(_, [], _, _, _, _, _, _) :- fail.
try_stations(Task, [Station-_|_], Pos, Energy, Station, ChargePath, TargetPath, Cost) :-
    heuristic(find(Station), Pos, F), solve_task_astar(find(Station), [[F, Pos, []]], [], ChargePath), length(ChargePath, StationCost), Energy>=StationCost,
    get_final_position(ChargePath, StationPos), heuristic(Task, StationPos, F1), solve_task_astar(Task, [[F1, StationPos, []]], [], TargetPath), length(TargetPath, TargetCost),
    Cost is StationCost+TargetCost.
try_stations(Task, [_-_|Rest], Pos, Energy, Station, ChargePath, TargetPath, Cost) :- try_stations(Task, Rest, Pos, Energy, Station, ChargePath, TargetPath, Cost).

get_final_position([Pos], Pos).
get_final_position([_|Rest], Pos) :- get_final_position(Rest, Pos).

solve_task_astar(Task, [[_, Pos|Path]|_],_, RPath) :-
	achieved(Task, Pos),
	reverse([Pos|Path], [_|[_|RPath]]).

solve_task_astar(Task, [[_, Pos|Path]|Rest], Visited, Solution) :-
	findall([F1, NewPos, Pos|Path], (
		map_adjacent(Pos, NewPos, empty), \+ member(NewPos, Visited), \+ member([_, NewPos|_], Rest),
		length([NewPos, Pos|Path], G), heuristic(Task, NewPos, H), F1 is G+H
	), Children),
	append(Rest, Children, N),
	sort(N, S),
	solve_task_astar(Task, S, [Pos|Visited], Solution).

heuristic(go(TargetPos), Pos, H) :-
    map_distance(Pos, TargetPos, H).

heuristic(find(_), _, 0).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).
