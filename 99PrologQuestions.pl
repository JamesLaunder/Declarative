% sumlist(+List, -Sum)
% holds if Sum is the sum of List, a list of numbers.
%head and tail of list, and then add count the numbers in that list and then sum them
%basecase
sumlist([],0).

sumlist([X|Xs],Sum) :- sumlist(Xs,S1),Sum is X+S1.



last_element([],_).
last_element([Head|Tail],X) :- last(Tail,X).

last_but_one([X,_],X).
last_but_one([_|Tail],X) :- last_but_one(Tail,X).

 % you want to get the second last element of the list
%base case if 1 element in list
kElement(X,[X|_],1).
%this is saying if more than base case than you want to -1 because element start from 0 and then iterate thruogh to it and return it 
kElement(X,[_|Tail],K) :- K>1, K1 is K-1, kElement(X,Tail,K1).

numElement([],_).
numElement(List,X) :- length(List ,X).


reverseList([],[]).
reverseList(List,X) :- reverse(List,X).


%palindrome

isPalindrome([],_).
isPalindrome(L) :- reverse(L,L).



flattenList([],[]).
flattenList(X,S) :- flatten(X,S).


removeDups([],[]).
removeDups([X],[X]).

removeDups([X,X|T],[X|R]) :-
    removeDups([X|T], [X|R]).
removeDups([X,Y|T],[X|R]) :-   X \== Y, removeDups([Y|T],R).


pack([], []).
pack(L, Pack) :-
    pack(L, [], Pack).

pack([X], FrontPack, [[X|FrontPack]]).
pack([X,X|T], FrontPack, Pack) :-
    pack([X|T], [X|FrontPack], Pack).
pack([X,Y|T], FrontPack, [[X|FrontPack]|Pack]) :-
    X \= Y,
    pack([Y|T], [], Pack).




encode([],[]).
encode([X|T],[[X,C1]|R]) :- encode(T,[[X,C]|R]), !, C1 is C+1.
encode([X|T],[[X,1]|R]) :- encode(T,R).



encodemod(L1,L2) :- encode(L1,L) ,strip(L,L2).
strip([],[]).
strip([[X,1]|Ys],[X|Zs]) :- strip(Ys,Zs).
strip([[N,X]|Ys],[[N,X]|Zs]) :- X > 1, strip(Ys,Zs).


 
decode([],[]).
decode([X|Ys],[X|Zs]) :- \+ is_list(X), decode(Ys,Zs).
decode([[1,X]|Ys],[X|Zs]) :- decode(Ys,Zs).
decode([[N,X]|Ys],[X|Zs]) :- N > 1, N1 is N - 1, decode([[N1,X]|Ys],Zs).


duplicate([],[]).
duplicate([H|TL1],[H,H|TL2]) :- duplicate(TL1,TL2).

duplicateN(L1,N,L2) :- duplicateN(L1,N,L2,N).

duplicateN([],_,[],_).

duplicateN([_|TL1],N,Tl2,0):-duplicateN(TL1,N,Tl2,N).
duplicateN([H|Tl1],N,[H|Tl2],K) :- K>0 ,  K1 is K-1, duplicateN([H|Tl1],N,Tl2,K1).


drop(L1,N,L2) :- drop(L1,N,L2,N).
drop([],_,[],_).
drop([_|Xs],N,Ys,1) :- drop(Xs,N,Ys,N).
drop([X|Xs],N,[X|Ys],K) :- K>1 ,  K1 is K-1,drop(Xs,N,Ys,K1).


splitList(L,1,[],L).
splitList([X|Xs],N,[X|Ys],S2) :- N>0 , N1 is N-1,splitList(Xs,N1,Ys,S2).
    

shift(L1, N, L2) :-
    N < 0, !,
    N1 is -N,
    shift(L2, N1, L1).
shift(L1, N, L2) :- 
    append(Lx, Ly, L1), % L1 is Lx || Ly
    append(Ly, Lx, L2), % L2 is Ly || Lx
    length(Lx, N).      % The length of Lx is N

remove(X,[X|Xs],1,Xs).

remove(X,[Y|Xs],K,[Y|Ys]) :- K>0 , K1 is K- 1 , remove(X,Xs,K1,Ys).


:- use_module(library(clpfd)).

insert(X,[_|Xs],0,[X|Xs]).
insert(X,[X|Xs0],K,Xs) :-
    X #>0,
    X#= X0+1, insert(X,Xs0,X0,Xs).


%or 
insertAt(E,N,Xs,Ys) :-
   same_length([E|Xs],Ys),
   append(Before,Xs0,Xs),
   length(Before,N),
   append(Before,[E|Xs0],Ys).




range(R,N,L):- findall(X, between(R,N,X),L).



randomDraw(R,N,L):- findall(X, between(R,N,X),L).



rnd_select(_,0,[]).
rnd_select(Xs,N,[X|Zs]) :- N > 0,
    length(Xs,L),
    I is random(L) + 1,
    remove(X,Xs,I,Ys),
    N1 is N - 1,
    rnd_select(Ys,N1,Zs).

lottodraw(N,Hi,L) :- range(1,Hi,L1) ,rnd_select(L1,N,L).

randompermutation(L1,L2):-  length(L1,N), rnd_select(L1,N,L2).



comb(0,_,[]).

comb(N,[X|T],[X|Comb]) :-
    N>0,
    N1 is N-1,
    comb(N1,T,Comb).

comb(N,[_|T],Comb) :-
    N>0,
    comb(N,T,Comb).

use_module(library(lists)).

comb_perm(N,List,Result) :-
    comb(N,List,Comb),
    permutation(Comb,Result).





group3(G,G1,G2,G3) :- 
   selectN(2,G,G1),
   subtract(G,G1,R1),
   selectN(3,R1,G2),
   subtract(R1,G2,R2),
   selectN(4,R2,G3),
   subtract(R2,G3,[]).
 
% selectN(N,L,S) :- select N elements of the list L and put them in 
%    the set S. Via backtracking return all posssible selections, but
%    avoid permutations; i.e. after generating S = [a,b,c] do not return
%    S = [b,a,c], etc.
 
selectN(0,_,[]) :- !.
selectN(N,L,[X|S]) :- N > 0, 
   el(X,L,R), 
   N1 is N-1,
   selectN(N1,R,S).
 
el(X,[X|L],L).
el(X,[_|L],R) :- el(X,L,R).
 
% subtract/3 is predefined
 
% Problem b): Generalization
 
% group(G,Ns,Gs) :- distribute the elements of G into the groups Gs.
%    The group sizes are given in the list Ns.
 
group([],[],[]).
group(G,[N1|Ns],[G1|Gs]) :- 
   selectN(N1,G,G1),
   subtract(G,G1,R),
   group(R,Ns,Gs).

elsort(A,C):- count_blocks(A,B),sort(B, @=<, A, C).




count_blocks(L, R) :-
    maplist(length, L, R).

isPrime(X) :- X>1, primeNumber(X). 
    
primeNumber(A) :-
    A > 2,
    \+ 0 is A mod 2,
    L is floor(sqrt(A) / 2),
    \+ (between(1, L, X),
        0 is A mod (1 + 2*X)).


gcd(A,B,X):- A=0,X=B. % base case
gcd(A,B,X):- B=0,X=A. % base case
gcd(A,B,X):- A>B, gcd(B, A, X).
gcd(A,B,X):- A<B, T is B mod A, gcd(A, T, X).




and(A,B) :- A, B.
 
or(A,_) :- A.
or(_,B) :- B.
 
equ(A,B) :- or(and(A,B), and(not(A),not(B))).
 
xor(A,B) :- not(equ(A,B)).
 
nor(A,B) :- not(or(A,B)).
 
nand(A,B) :- not(and(A,B)).
 
impl(A,B) :- or(not(A),B).
bind(true).
bind(fail).
table(A,B,Expr) :- bind(A), bind(B), do(A,B,Expr), fail.
 
do(A,B,_) :- write(A), write('  '), write(B), write('  '), fail.
do(_,_,Expr) :- Expr, !, write(true), nl.
do(_,_,_) :- write(fail), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% trees different structure to how we do them though
tree_list(empty,[]).
tree_list(node(Left,Elt,Right),List) :- tree_list(Left,List1),
    tree_list(Right,List2), append(List1,[Elt|List2],List).

list_tree([],empty).
list_tree([E|List],node(Left,Elt,Right)) :-
    length(List,Len),
    Len2 is Len //2, %divisible
    length(Front, Len2),
    append(Front,[Elt|Back],[E|List]),
    list_tree(Front,Left),
    list_tree(Back,Right).
