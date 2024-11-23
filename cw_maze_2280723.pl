:- dynamic dead/2.

% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    my_agents(Agents),
    get_agent_positions(Agents, Pos), update_agent_positions(Agents, Pos, [], AgentStates),
    exploration_phase(Agents, AgentStates, 0).

exploration_phase(Agents, AgentStates, 1) :- pathfinding_phase(Agents, AgentStates). 
exploration_phase(Agents, AgentStates, _) :-
    find_moves(Agents, AgentStates, Moves),
    agents_do_moves(Agents, Moves),
    update_agent_positions(Agents, Moves, AgentStates, NewAgentStates),
    check_end(Agents, NewAgentStates, NextAgents, NextAgentStates, NewEnd),
    exploration_phase(NextAgents, NextAgentStates, NewEnd).

pathfinding_phase(Agents, AgentStates) :-
    format("Pathfinding Phase started.~n"),
    exit_path(ExitPath),
    find_path(Agents, ExitPath, Paths),
    agents_do_moves(Agents, Paths),
    exit_maze(Agents).

%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([], _, []).
find_moves([A|As], AgentStates, [M|Moves]) :-
    member((A, PrevPos), AgentStates),
    findall(P,agent_adjacent(A,P,_),PosMoves),
    categorise_positions(A, PrevPos, PosMoves, GlobalUnexplored, LocalUnexplored, Empty, Dead, Walls),
    (
	GlobalUnexplored \= [] -> MovesList = GlobalUnexplored ;
	LocalUnexplored \= [] -> MovesList = LocalUnexplored ;
	Empty \= [] -> MovesList = Empty ;
	Dead \= [] -> MovesList = Dead ;
	MovesList = Walls
    ),
    random_member(M,MovesList),
    find_moves(As,AgentStates, Moves).

categorise_positions(_, _, [], [], [], [], [], []).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, Empty, [Pos|Dead], Walls) :- dead(Pos, A), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, Dead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, Empty, Dead, [Pos|Walls]) :- known_maze(Pos, wall), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, Dead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, LocalUnexplored, [Pos|Empty], Dead, Walls) :- known_maze(Pos, empty), member(Pos, PrevPos), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, Dead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], GlobalUnexplored, [Pos|LocalUnexplored], [Pos|Empty], Dead, Walls) :- known_maze(Pos, empty), \+ member(Pos, PrevPos), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, Dead, Walls).
categorise_positions(A, PrevPos, [Pos|Rest], [Pos|GlobalUnexplored], LocalUnexplored, Empty, Dead, Walls) :- \+ known_maze(Pos, _), categorise_positions(A, PrevPos, Rest, GlobalUnexplored, LocalUnexplored, Empty, Dead, Walls).

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
            format("Agents at the end: ~w~n", [AgentsAtEnd]),
            maplist(leave_maze, AgentsAtEnd),
	    format("Agent left ~n"),
            subtract(Agents, AgentsAtEnd, RemainingAgents),
	    format("Subtracted agents ~n"),
            findall((A, PosList), (member((A, PosList), AgentStates), \+ member(A, AgentsAtEnd)), RemainingAgentStates),
	    format("Foundall ~n"),
	    End=1,
	    format("Set end to 1 ~n"),
            NewAgents = RemainingAgents,
	    format("Set new agents ~n"),
            NewAgentStates = RemainingAgentStates,
	    format("Set new agent states ~n")
        )
    ;   
        End = 0,
        NewAgents = Agents,
        NewAgentStates = AgentStates
    ).
    
