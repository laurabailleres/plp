% ejercicio 11
% vacio(+B)
vacio(nil).

% raiz(+B, ?R)
raiz(bin(_, X, _), X).

% max(+X, +Y, -M)
max(X, Y, X) :- X>Y, !.
max(X, Y, Y) :- Y>=X.

% altura(+B, ?A)
altura(nil, 0).
altura(bin(I, _, D), A) :- altura(I,A1), altura(D,A2), max(A1, A2, M), A is M+1.

% cantidadDeNodos(+B, ?N)
cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(I, _, D), N) :- cantidadDeNodos(I, CI), cantidadDeNodos(D, CD), N is 1+CI+CD.