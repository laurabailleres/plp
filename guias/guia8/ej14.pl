% ejercicio 14
% ítem I
% listaQueSuma(+S, +L, -XS)
% S = número que tiene que sumar
% L = largo de la lista
% XS = lista
listaQueSuma(0, 0, []).
listaQueSuma(S, L, [X|XS]) :- L > 0, between(0, S, X), S1 is S-X, L1 is L-1, listaQueSuma(S1, L1, XS).

% matrizQueSuma(+S, +F, +C, -XS)
% S = lo que tiene que sumar cada lista
% F = cantidad de listas (filas)
% C = largo de las listas (columnas)
% XS = matriz
matrizQueSuma(_, 0, _, []).
matrizQueSuma(S, F, C, [X|XS]) :- F > 0, listaQueSuma(S, C, X), F1 is F-1, matrizQueSuma(S, F1, C, XS).

% cuadradoSemiMágico(+N, -XS)
cuadradoSemiMagico(N, XS) :- between(0, inf, M), matrizQueSuma(M, N, N, XS).
% instancio la cantidad de filas y de columnas con el mismo número (el tamaño del
% cuadrado, o sea N) porque quiero justamente que sea un cuadrado. con between
% genero naturales y con eso genero cuadrados semimágicos.
