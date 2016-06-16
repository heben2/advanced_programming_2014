upd :- consult('family.pl').

male(michael).
male(ken).
male(erik).
male(troels).
male(oleks).
male(simon).
male('Peter Naur').

female(alice).
female(julia).

parent(michael, erik).
parent(michael, simon).
parent(ken, troels).
parent(ken, oleks).
parent(alice, troels).
parent(alice, oleks).
parent(alice, simon).
parent(julia, erik).
parent('Peter Naur', michael).
parent('Peter Naur', ken).

% father/2: X is father to Y
father(X, Y) :- male(X), parent(X, Y).

% mother/2: X is mother to Y
mother(X, Y) :- female(X), parent(X, Y).

% grandfather/2: X is grandfather to Y
grandfather(X, Y) :- 
	father(X, Z), 
	parent(Z,Y).

% parents/3: X (father) and Y (mother) are prettents to Z
parents(X, Y, Z) :- father(X, Z), mother(Y, Z).

% haveChild/2: X (father) and Y (mother) have a child
haveChild(X, Y) :- parents(X, Y, _).

% ancestor/2 X is ancestor to Y
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% brother/2: X is brother to Y
halfbrother(X, Y) :- male(X), parent(Z, X), parent(Z, Y).

brother(X, Y) :- male(X), parents(Z, V, X), parents(Z, V, Y).























