% ejercicio 10
% intercalar(L1, L2, L3)
intercalar([], L, L) :- L \= [].
intercalar(L, [], L).
intercalar([X|L1], [Y|L2], [X|[Y|L3]]) :- intercalar(L1, L2, L3).
