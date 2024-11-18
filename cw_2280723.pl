% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), get_agent_position(A,P),
    %solve_task_dfs(Task,[P],[P|Path]), !,
    solve_task_bfs(Task, [[P]], [], Path),
    agent_do_moves(A,Path), length(Path,Cost).

% Calculate the path required to achieve a Task
solve_task_dfs(Task,[P|Ps],Path) :-
    achieved(Task,P), reverse([P|Ps],Path)
    ;
    map_adjacent(P,Q,empty), \+ member(Q,Ps),
    solve_task_dfs(Task,[Q,P|Ps],Path).

solve_task_bfs(Task, [[Pos|Path]|_], _, RPath) :-
	achieved(Task, Pos),
	reverse([Pos|Path], [_|RPath]).

solve_task_bfs(Task, [[Pos|Path]|Rest], Visited, Solution) :-
	findall([NewPos, Pos|Path], (map_adjacent(Pos, NewPos, empty), \+ member(NewPos, Visited), \+ member([NewPos|_], Rest)), Children),
	append(Rest, Children, N),
	solve_task_bfs(Task,N, [Pos|Visited], Solution).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).
