% ejercicio 13
% coprimos(-X,-Y)
coprimos(X, Y) :- between(1, inf, X), between(1, X, Y), 1 is gcd(X, Y).