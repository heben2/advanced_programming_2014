%:- op(500, xfx, is_bigger).

upd :- consult('animals.pl').

bigger(elephant, horse).
bigger(horse, donkey).
bigger(donkey, dog).
bigger(donkey, monkey).

%?- bigger(donkey, dog).
%?- bigger(monkey, elephant).
%?- bigger(elephant, monkey).

is_bigger(X, Y) :- bigger(X, Y).
is_bigger(X, Y) :- bigger(X, Z), is_bigger(Z, Y).

