% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze_demo :-
    my_agents(Agents),
    find_moves(Agents,Moves),
    agents_do_moves(Agents,Moves),
    solve_maze_demo.
    
solve_maze :-
    my_agents(Agents),
    get_agent_positions(Agents, Pos), update_agent_positions(Agents, Pos, [], AgentStates),
    exploration_phase(Agents, AgentStates, 0),
    pathfinding_phase(Agents, AgentStates).

exploration_phase(_, _, 1).
exploration_phase(Agents, AgentStates, End) :-
    find_moves(Agents, Moves),
    agents_do_moves(Agents, Moves),
    update_agent_positions(Agents, Moves, AgentStates, NewAgentStates)
    exploration_phase(Agents, End).

pathfinding_phase(Agents) :-
    exit_path(ExitPath),
    find_path(Agents, ExitPath, Paths),
    agents_do_moves(Agents, Paths),
    exit_maze(Agents).

%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([],[]).
find_moves([A|As],[M|Moves]) :-
    findall(P,agent_adjacent(A,P,_),PosMoves),
    random_member(M,PosMoves),
    find_moves(As,Moves).

get_agent_positions([], []).
get_agent_positions([A|As], [Pos|Rest]) :-
    get_agent_position(A, Pos).
    get_agent_positions(As, Rest).

update_agent_positions([], _, _, []).
update_agent_positions([A|As], [M|Moves], [], [(A, [M])|Rest]) :-
    assertz(visited(M)),
    assertz(known_maze(M, empty)),
    update_agent_positions(As, Moves, [], Rest).
update_agent_positions([A|As], [M|Moves], [(A, [PrevPos])|RestPrev], [(A, [M|PrevPos])|Rest]) :-
    assertz(visited(M)),
    lookup_pos(M, O), assertz(known_maze(M, O)),
    update_agent_positions(As, Moves, RestPrev, Rest).

find_path(Agents, ExitPath, Paths)

exit_maze([]).
exit_maze([A|As]) :- 
    leave_maze(A),
    exit_maze(As).
