% ejercicio 20
% esPrimo(+N)
esPrimo(2).
esPrimo(N) :- N>2, M is N-1, not((between(2, M, B), mod(N, B) =:= 0)).

% numeroPoderoso(+M)
numeroPoderoso(M) :- not((between(2, M, P), esPrimo(P), 
    mod(M,P) =:= 0, B2 is P*P, mod(M,B2) =\= 0)).

% minimoDesde(-X, +C)
minimoDesde(C, C) :- numeroPoderoso(C).
minimoDesde(X, C) :- not(numeroPoderoso(C)), C1 is C+1, minimoDesde(X, C1).

% próximoNumPoderoso(+X,-Y)
proximoNumPoderoso(X, Y) :- X1 is X+1, minimoDesde(Y, X1).