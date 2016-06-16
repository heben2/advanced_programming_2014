upd :- consult('lists.pl').

% sndElement/2 such that X is the second element of L.
%sndElement(X, L) :- L = [_, X | _].
% This is better:
sndElement(X,[_, X | _]).

oneAndRest(Rest,[1, _ | Rest]).

% concat_lists/3 such that A 'concat' B is L.s
concat_lists([], B, B).
concat_lists([H|T],B,[H|L]) :- concat_lists(T,B,L).

% Shows all elements in list and fails!.!
show(List) :-
	member(Element,List),
	write(Element),
	nl,
	fail.
	
% Does not calculate the lenght (as a binary number), 
% but the unary number representing the length.
len_u([], 0).
len_u([_ | Tail], N + 1) :-
	len_u(Tail, N).

% Correct calculation of length.
len([], 0).
len([_ | Tail], N1) :-
	len(Tail, N),
	N1 is N + 1.
	
% Remove duplicates with cut (!),
% or with negation commented out.
remove_duplicates([], []).
remove_duplicates([Head | Tail], Result) :-
    member(Head, Tail), !
    remove_duplicates(Tail, Result).
remove_duplicates([Head | Tail], [Head | Result]) :-
%	not(member(Head, Tail)),
	remove_duplicates(Tail, Result).
	
% ?- remove_duplicates([a, b, b, c, a], List).
