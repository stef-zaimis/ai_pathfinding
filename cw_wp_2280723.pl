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
    findall(A,actor(A),As), eliminate(As,A,1).