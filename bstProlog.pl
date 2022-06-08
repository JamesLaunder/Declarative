intset_member(N, tree(_,N,_)).
intset_member(N, tree(L,N0,_)) :-
	N < N0,
	intset_member(N, L).
intset_member(N, tree(_,N0,R)) :-
	N > N0,
	intset_member(N, R).

intset_insert(N, _, _).
% intset_insert(N, tree(L,N,R), tree(L,N,R)).
intset_insert(N, tree(L0,N0,R), tree(L,N0,R)) :-
	N < N0,
	intset_insert(N, L0, L).
intset_insert(N, tree(L,N0,R0), tree(L,N0,R)) :-
	N > N0,
	intset_insert(N, R0, R).


    difference_append(OpenList1-Hole1, OpenList2-Hole2, OpenList1-Hole2) :- Hole1=OpenList2.
    