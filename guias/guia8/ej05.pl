% EJERCICIO 5
% ítem I
% last(?L, ?U)
last(L, U) :- append(_, [U], L).

% ítem II
% reverse(+L, ?R)
reverse([], []). 
reverse([X|L], R) :- reverse(L, Lrec), append(Lrec, [X], R).

% ítem III
% prefijo(?P, +L)
prefijo(P, L) :- append(P, _, L).

% ítem IV
% sufijo(?S, +L)
sufijo(S, L) :- append(_, S, L).

% sufijoFacha(?S, +L)
sufijoFacha(S, L) :- reverse(L, R), prefijo(S, R).

% ítem V
% sublista(?S, +L)
sublista([], L).
sublista(S, L) :- S = [_|_], prefijo(Pre, L), sufijo(Suf, L), append(Pre, S, PreSub), append(PreSub, Suf, L).

% sublistaFacha(?S, +L)
sublistaFacha([], L).
sublistaFacha(S, L) :- S = [_|_], prefijo(Pre, L), sufijo(S, Pre).

% ítem VI
% pertenece(?X, +L)
pertenece(X, L) :- append(_, [X|_], L).
