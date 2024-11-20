% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), get_agent_position(A,P),
    %solve_task_dfs(Task,[P],[P|Path]), !,
    solve_task_bfs(Task, [[P]], [], Pathbfs),
    solve_task_astar(Task, [[0, P, []]], [], Path),
    format("A* Path: ~w~n", [Path]),
    format("BFS Path: ~w~n", [Pathbfs]),
    agent_do_moves(A,Path), length(Path,Cost).

solve_task_astar(Task, [[_, Pos|Path]|_],_, RPath) :-
	achieved(Task, Pos),
	reverse([Pos|Path], [_|[_|RPath]]).

solve_task_astar(Task, [[F, Pos|Path]|Rest], Visited, Solution) :-
	findall([F1, NewPos, Pos|Path], (
		map_adjacent(Pos, NewPos, empty), \+ member(NewPos, Visited), \+ member([_, NewPos|_], Rest),
		length([NewPos, Pos|Path], G), F1 is G+0
	), Children),
	append(Rest, Children, N),
	sort(N, S),
	solve_task_astar(Task, S, [Pos|Visited], Solution).

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
