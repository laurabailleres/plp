% ejercicio 11
% vacio(+B)
vacio(nil).

% raiz(+B, ?R)
raiz(bin(_, X, _), X).

% altura(+B, ?A)
altura(nil, 0).
altura(bin(I, _, D), A) :- altura(I,A1), altura(D,A2), A is max(A1, A2) + 1.

% cantidadDeNodos(+B, ?N)
cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(I, _, D), N) :- cantidadDeNodos(I, CI), cantidadDeNodos(D, CD), N is 1+CI+CD.