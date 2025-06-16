% ejercicio 6
aplanar([], []).
aplanar([X | L], [X | YS]) :- X \= [], X \= [_|_], aplanar(L, YS).
aplanar([X | L], L3) :- is_list(X), aplanar(X, Y), aplanar(L, L2), append(Y, L2, L3).
