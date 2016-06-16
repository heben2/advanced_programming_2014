%%%---------------------------------------------------------------------
%%% @authors: Michael Kirkedal Thomsen <shapper@diku.dk>
%%%           Erik Partridge 
%%% @copyright (C) 2013, Michael Kirkedal Thomsen, Erik Partridge
%%% Created : Aug 2013 for exercise in Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student: Henrik Bendt
%%% KU-Id:   gwk553
%%%---------------------------------------------------------------------
upd :- consult('nats.pl').

%%%---------------------------------------------------------------------
%%% Part 1:
%%%---------------------------------------------------------------------
% less (X,Y) => X < Y
less(z,s(_)).
less(s(X),s(Y)) :- less(X,Y).

% add (X,Y,Z) => Z = X + Y
add(z,z,z).
add(z,s(Y),s(Z)) :- add(z,Y,Z).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

% mult(X,Y,Z) => Z = X * Y
mult(z,z,z).
mult(z,s(_),z).
mult(s(_),z,z).
mult(s(X),Y,Z) :-
    less(Y,s(Z)),
    less(X,Z),
    mult(X,Y,K), 
    add(Y,K,Z).

% mod(X,Y,Z) => Z = X mod Y.
mod(X,X,z).
mod(X,Y,X) :- less(X,Y).
mod(X,Y,Z) :- less(Y,X),
              less(z,Y),
              add(K,Y,X),
              mod(K,Y,Z).


%%%---------------------------------------------------------------------
%%% Part 2:
%%%---------------------------------------------------------------------

% notdividable => X*n != Y
notdividable(X,Y) :- 
    mod(Y,X,V),
    less(z,V).

% Ugly, but works. Should not return multiple truths.
% notprime(X) => X is not a prime
notprime(z).
notprime(s(z)).
notprime(X) :- 
    mod(X,s(s(z)),z), %If even X, prime unless X=2=s(s(z))
    less(s(s(z)),X).
notprime(X) :- 
    mod(X,s(s(z)),s(z)), %If uneven X
    mult(_,K,X), % If K exists such that 1 < K < X, then prime.
    less(K,X),
    less(s(z),K),
    notdividable(I,K), % Remove dublicate truths
    less(I,K),
    less(s(z),I).


%Helper predicate for prime
runprime(s(s(z)),_).
runprime(s(X),Y) :- 
    less(s(z),X),
    notdividable(X,Y), 
    runprime(X,Y).

% prime(X) => X is a prime
prime(X) :- runprime(X,X).



%Helper predicates for listPrimes
%Makes a list of all natural numbers to X.
makeList([X],X,X).
makeList([C|T],C,X) :- less(C,X), makeList(T,s(C),X).

%extractPrimes(X,Y) - Extracts all prime numbers of list X to the list Y
extractPrimes([],[]).
extractPrimes([H|T],[H|L]) :- prime(H), extractPrimes(T,L).
extractPrimes([H|T],L) :- notprime(H), extractPrimes(T,L).


% listPrimes(L,X) => L is the list of primes smaller than or equal to X
listPrimes(L,X) :- makeList(N,z,X), extractPrimes(N,L).


%Helper predicate for superprime
%findSuperPrime(L,c,X) => list of primes L, counter c (starting from z) and prime to find X.
findSuperPrime([H|_],C,H) :- prime(C).
findSuperPrime([H|L],C,X) :- findSuperPrime(L,s(C),X).

% superprime(X) => X is a super-prime
superprime(X) :- listPrimes(L,X), findSuperPrime(L,s(z),X).


%%%---------------------------------------------------------------------
%%% Part 3:
%%%---------------------------------------------------------------------


%%%---------------------------------------------------------------------
%%% Tests: 
%%%---------------------------------------------------------------------


%%%---------------------------------------------------------------------
%%% Help of convert to and from unary numbers
%%%---------------------------------------------------------------------

% Binary to unary
i2n(X,s(Y)) :- X > 0, Z is (X-1), i2n(Z,Y).
i2n(0,z).

% Unary to binary
n2i(s(X),Y) :- n2i(X,Z), Y is (Z+1).
n2i(z,0).
