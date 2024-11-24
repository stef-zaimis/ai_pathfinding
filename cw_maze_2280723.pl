:- dynamic dead/2.

% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    my_agents(Agents),
    get_agent_positions(Agents, Pos), update_agent_positions(Agents, Pos, [], AgentStates),
    exploration_phase(Agents, AgentStates, NewAgents, NewAgentStates, 0),
    pathfinding_phase(NewAgents, NewAgentStates).

exploration_phase(Agents, AgentStates, FinalAgents, FinalAgentStates, 1). 
exploration_phase(Agents, AgentStates, _, _, 0) :-
    find_moves(Agents, AgentStates, Moves),
    agents_do_moves(Agents, Moves),
    update_agent_positions(Agents, Moves, AgentStates, NewAgentStates),
    check_end(Agents, NewAgentStates, NextAgents, NextAgentStates, NewEnd),
    exploration_phase(NextAgents, NextAgentStates, FinalAgents, FinalAgentStates, NewEnd).

pathfinding_phase(Agents, _) :-
    format("Pathfinding Phase started.~n"),
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
	GlobalUnexplored \= [] -> MovesList = GlobalUnexplored ;
	LocalUnexplored \= [] -> MovesList = LocalUnexplored ;
	Empty \= [] -> MovesList = Empty ;
	LocalDead \= [] -> MovesList = LocalDead ;
	GlobalDead \= [] -> MovesList = GlobalDead ;
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
get_paths_astar([],_).
get_paths_astar([A|As], Goal, [Path|Rest]) :-
    get_agent_position(A, Pos),
    astar(Pos, Goal, [_,Path]),
    get_paths_astar(As, Goal, Rest).

exit_agents(Agents, []).
exit_agents(Agents, [M|Moves]) :-
    agents_do_moves(Agents, M),
    (maplist(leave_maze, Agents) ; true),
    exit_agents(Agents, Moves).

astar(Start, Goal, Path) :-
    astar_heuristic(Goal, Start, H),
    astar_search([node(Start, [], 0, H)], [], Goal, RevPath),
    reverse(RevPath, Path).

astar_search([node(Pos, Path, G, _)|_], _, Goal, [Pos|Path]) :-
    Pos=Goal.
astar_search([node(Pos, Path, G, _)|OpenRest], ClosedSet, Goal, FinalPath) :-
    findall(
        node(AdjPos, [Pos|Path], G1, F1),
	(
	    known_adjacent(Pos, AdjPos),
	    \+ member(AdjPos, ClosedSet),
	    \+ member(node(AdjPos, _, _, _), OpenRest),
	    G1 is G+1,
	    astar_heuristic(Goal, AdjPos, H1),
	    F1 is G1+H1
        ),
	Children
    ),
    append(OpenRest, Children, OpenSet1),
    sort_open_set(OpenSet1, OpenSetSorted),
    astar_search(OpenSetSorted, [Pos|ClosedSet], Goal, FinalPath).

sort_open_set(OpenSet, SortedOpenSet) :- predsort(compare_nodes, OpenSet, SortedOpenSet).

compare_nodes(Delta, node(_, _, _, F1), node(_, _, _, F2)) :- compare(Delta, F1, F2).

astar_heuristic(p(X1, Y1), p(X2, Y2), H) :- H is abs(X1-X2) + abs(Y1-Y2).

known_adjacent(Pos, AdjPos) :-
    map_adjacent(Pos, AdjPos, empty),
    known_maze(AdjPos, empty),
    \+ dead(AdjPos, _).
