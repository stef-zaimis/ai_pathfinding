:- dynamic dead/2.

% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    my_agents(Agents),
    get_agent_positions(Agents, Pos), update_agent_positions(Agents, Pos, [], AgentStates),
    exploration_phase(Agents, AgentStates, _, _, 0),
    my_agents(NewAgents),
    pathfinding_phase(NewAgents), !.

exploration_phase(Agents, AgentStates, Agents, AgentStates, 1) :- !.
exploration_phase(Agents, AgentStates, FinalAgents, FinalAgentStates, 0) :-
    find_moves(Agents, AgentStates, Moves),
    agents_do_moves(Agents, Moves),
    update_agent_positions(Agents, Moves, AgentStates, NewAgentStates),
    check_end(Agents, NewAgentStates, NextAgents, NextAgentStates, NewEnd),
    exploration_phase(NextAgents, NextAgentStates, FinalAgents, FinalAgentStates, NewEnd).

pathfinding_phase(Agents) :-
    ailp_grid_size(N),
    get_paths_astar(Agents, p(N, N), Paths),
    exit_agents(Agents, Paths).

%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([], _, []).
find_moves([A|As], AgentStates, [M|Moves]) :-
    member((A, PrevPos), AgentStates),
    findall(P,agent_adjacent(A,P,_),PosMoves),
    categorise_positions(A, PrevPos, PosMoves, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls),
    (
	GlobalUnexplored \= [],  MovesList = GlobalUnexplored ;
	LocalUnexplored \= [], MovesList = LocalUnexplored ;
	Empty \= [], MovesList = Empty ;
	GlobalDead \= [], MovesList = GlobalDead ;
	LocalDead \= [], MovesList = LocalDead ;
	MovesList = Walls
    ),
    random_member(M,MovesList),
    find_moves(As,AgentStates, Moves).

categorise_positions(_, _, [], [], [], [], [], [], []).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, [Pos|LocalDead], Walls) :- dead(Pos, A), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, Empty, [Pos|GlobalDead], LocalDead, Walls) :- dead(Pos, _), \+ dead(Pos, A), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, [Pos|Walls]) :- known_maze(Pos, wall), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, [Pos|Empty], GlobalDead, LocalDead, Walls) :- known_maze(Pos, empty), member(Pos, PrevPos), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, [Pos|LocalUnexplored], [Pos|Empty], GlobalDead, LocalDead, Walls) :- known_maze(Pos, empty), \+ member(Pos, PrevPos), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], [Pos|GlobalUnexplored], LocalUnexplored, Empty, GlobalDead, LocalDead, Walls) :- \+ known_maze(Pos, _), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, GlobalDead, LocalDead, Walls).

get_agent_positions([], []).
get_agent_positions([A|As], [Pos|Rest]) :-
    get_agent_position(A, Pos),
    get_agent_positions(As, Rest).

update_agent_positions([], _, _, []).
update_agent_positions([A|As], [M|Moves], [], [(A, [M])|Rest]) :-
    assertz(visited(M)),
    assertz(known_maze(M, empty)),
    update_adjacent(A),
    check_and_mark_dead(A),
    update_agent_positions(As, Moves, [], Rest).
update_agent_positions([A|As], [M|Moves], [(A, PrevPos)|RestPrev], [(A, [M|PrevPos])|Rest]) :-
    (\+ visited(M) -> assertz(visited(M)) ; true),
    (\+ known_maze(M, _) -> assertz(known_maze(M, empty)) ; true),
    update_adjacent(A),
    check_and_mark_dead(A),
    update_agent_positions(As, Moves, RestPrev, Rest).

update_adjacent(A) :-
    get_agent_position(A, Pos),
    findall(Adj-O, map_adjacent(Pos, Adj, O), AdjPos),
    mark_walls(AdjPos).

mark_walls([]).
mark_walls([Pos-O|Rest]) :-
    (
	O = t(_) -> (\+ known_maze(Pos, wall) -> assertz(known_maze(Pos, wall)) ; true) ; true
    ),
    mark_walls(Rest).

check_and_mark_dead(A) :-
    get_agent_position(A, Pos),
    findall(Adj, map_adjacent(Pos, Adj, _), AdjPos),
    length(AdjPos, TotalAdj),
    count_deads(A, AdjPos, 0, Count),
    (
	TotalAdj=4, Count >= 3 -> mark_dead(Pos, A) ;
	TotalAdj=3, Count >= 2 -> mark_dead(Pos, A) ;
	TotalAdj=2, Count >= 1 -> mark_dead(Pos, A) ;
	true
    ).

count_deads(_, [], Acc, Acc).
count_deads(A, [Pos|Rest], Acc, Count) :-
    (known_maze(Pos, wall) ; dead(Pos, A)), !,
    NewAcc is Acc+1,
    count_deads(A, Rest, NewAcc, Count).
count_deads(A, [_|Rest], Acc, Count) :-
    count_deads(A, Rest, Acc, Count).

mark_dead(Pos, A) :-
    (dead(Pos, A) -> retract(dead(Pos, A)) ; true),
    assertz(dead(Pos, A)).

check_end(Agents, AgentStates, NewAgents, NewAgentStates, End) :-
    ailp_grid_size(N),
    findall(A, (member(A, Agents), get_agent_position(A, Pos), Pos = p(N, N)), AgentsAtEnd),
    (   
        AgentsAtEnd \= [] ->
        (
            maplist(leave_maze, AgentsAtEnd),
            subtract(Agents, AgentsAtEnd, RemainingAgents),
            findall((A, PosList), (member((A, PosList), AgentStates), \+ member(A, AgentsAtEnd)), RemainingAgentStates),
	    End=1,
            NewAgents = RemainingAgents,
            NewAgentStates = RemainingAgentStates
        )
    ;   
        End = 0,
        NewAgents = Agents,
        NewAgentStates = AgentStates
    ).
    
% Implementention of A* tailored for agents to find exit quickly once the exit path has been found
get_paths_astar([], _, []).
get_paths_astar([A|As], Goal, [Path|Rest]) :-
    get_agent_position(A,Pos),
    astar_heuristic(go(Goal), Pos, F), (astar(go(Goal), [[F, 0, Pos, []]], [], Path) ; format("A* failed for agent ~w, trying bfs ~n", [A]), bfs(go(Goal), [[Pos]], [], Path)),
    get_paths_astar(As, Goal, Rest).

astar(Task, [[_, _, Pos|Path]|_],_, RPath) :-
	astar_achieved(Task, Pos),
	reverse([Pos|Path], [_|[_|RPath]]).

astar(Task, [[_, G, Pos|Path]|Rest], Visited, Solution) :-
	findall([F1, G1, NewPos, Pos|Path], (
		map_adjacent(Pos, NewPos, _), get_cost(NewPos, Cost), \+ member(NewPos, Visited), \+ member([_, NewPos|_], Rest),
		G1 is G+Cost, astar_heuristic(Task, NewPos, H), F1 is G1+H
	), Children),
	append(Rest, Children, N),
	sort(N, S),
	astar(Task, S, [Pos|Visited], Solution).

astar_heuristic(go(TargetPos), Pos, H) :-
    map_distance(Pos, TargetPos, H).
astar_heuristic(find(_), _, 0).

astar_achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).

get_cost(Pos, Cost) :- known_maze(Pos, empty), \+ dead(Pos, _), Cost is 1.
get_cost(Pos, Cost) :- dead(Pos, _), Cost is 5.
get_cost(_, Cost) :- Cost is 100.

% In case A* fails (very peculiar, idk why but sometimes it does?), try bfs
bfs(Task, [[Pos|Path]|_], _, RPath) :-
	astar_achieved(Task, Pos),
	reverse([Pos|Path], [_|RPath]).

bfs(Task, [[Pos|Path]|Rest], Visited, Solution) :-
	findall([NewPos, Pos|Path], (map_adjacent(Pos, NewPos, O), (O=empty ; O=a(_)), \+ member(NewPos, Visited), \+ member([NewPos|_], Rest)), Children),
	append(Rest, Children, N),
	bfs(Task,N, [Pos|Visited], Solution).

exit_agents([], _).
exit_agents(Agents, Paths) :-
    Paths \= [],
    extract_moves(Agents, Paths, Moves, NewPaths),
    agents_do_moves(Agents, Moves),
    attempt_agent_exit(Agents),
    my_agents(NewAgents),
    exit_agents(NewAgents, NewPaths).
exit_agents(_, _) :- my_agents(NewAgents), NewAgents \= [], pathfinding_phase(NewAgents).
    
extract_moves(_, [], [], []).
extract_moves([_|As], [[]|Rest], Moves, NewPaths) :- extract_moves(As, Rest, Moves, NewPaths).
extract_moves([_|As], [[Move|Path]|Rest], [Move|Moves], [Path|NewPaths]) :- extract_moves(As, Rest, Moves, NewPaths).

attempt_agent_exit([]).
attempt_agent_exit([A|As]) :-
    get_agent_position(A, Pos),
    ailp_grid_size(N),
    (
	Pos=p(N,N) -> leave_maze(A) ;
	true
    ),
    attempt_agent_exit(As).
