% ejercicio 7
% ítem I
% intersección(+L1, +L2, -L3)
interseccion([], _, []).
interseccion([X|L1], L2, L3) :- not(member(X, L2)), interseccion(L1, L2, L3).
interseccion([X|L1], L2, [X|L3]) :- member(X, L2), interseccion(L1, L2, L3).

% partir(?N, ?L, ?L1, ?L2)
partir(N, L, L1, L2) :- append(L1, L2, L), length(L1, N).

% ítem II
% borrarRec(+ListaOriginal, +X, -Xs)
borrarRec([], _, []).
borrarRec([X|L], X, Xs) :- borrarRec(L, X, Xs).
borrarRec([Y|L], X, [Y|Xs]) :- Y \= X, borrarRec(L, X, Xs).

borrar(L, X, L) :- not(member(X, L)).
borrar(L, X, Xs) :- not(not(member(X, L))), append(L1, [X|L2], L), not(member(X, L1)), borrar(L2, X, L2X), append(L1, L2X, Xs).

% ítem III
% sacarDuplicados(+L1, -L2)
sacarDuplicados([], []).
sacarDuplicados([X|L1], L2) :- member(X, L1), sacarDuplicados(L1, L2).
sacarDuplicados([X|L1], [X|L2]) :- not(member(X, L1)), sacarDuplicados(L1, L2).

% ítem IV
% permutación(+L1, ?L2) 
permutacion([], []).
permutacion(L1, L2) :- length(L1, N), length(L2, N), interseccion(L1, L2, L1).

% ítem V
% reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto(L, N, [X|LL]) :- append(X, Lrec, L), N > 0 , N2 is N-1, reparto(Lrec, N2, LL).

%ítem VI
% hayVacias
hayVacias(XS) :- member([], XS).

% repartoSinVacias(+L, +N, -LListas)
repartoSinVacias(L1, N, L2) :- reparto(L1, N, L2), not(hayVacias(L2)).