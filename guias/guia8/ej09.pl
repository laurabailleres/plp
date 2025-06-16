% ejercicio 9
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% ítem I
% para que no se produzca un error tengo que instanciar N antes
% de instanciar Y con el llamado recursivo. por q? no se

% ítem II
% desdeReversible(+X, ?Y)
desdeReversible(X, Y) :- nonvar(Y), Y >= X.
desdeReversible(X, Y) :- var(Y), desde(X, Y).
