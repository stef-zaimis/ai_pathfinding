% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    my_agents(Agents),
    find_moves(Agents,Moves),
    agents_do_moves(Agents,Moves),
    solve_maze.
    
solve_maze :-
    my_agents(Agents),
    exploration_phase(Agents),
    pathfinding_phase(Agents).

exploration_phase(Agents) :-
    exit_found -> true;
    find_moves(Agents, Moves),
    agents_do_moves(Agents, Moves),
    check_for_exit(Moves),
    exploration_phase(Agents).

pathfinding_phase(Agents) :-
    exit_position(ExitPos),
    find_path(Agents, ExitPos, Paths),
    agents_do_moves(Agents, Paths),
    exit_maze(Agents).

%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([],[]).
find_moves([A|As],[M|Moves]) :-
    findall(P,agent_adjacent(A,P,_),PosMoves),
    random_member(M,PosMoves),
    find_moves(As,Moves).

exit_maze([A]) :- leave_maze(A).
exit_maze([A|Rest]) :- 
    leave_maze(A),
    exit_maze(Rest).
