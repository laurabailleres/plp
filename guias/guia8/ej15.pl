% ejercicio 15
tri(A,B,C).

% ítem I
% esTriangulo(+T)
esTriangulo(tri(A,B,C)) :- 0<A, 0<B, 0<C, A < B+C, B < A+C, C < B+A.

tupla((A,B)) :- between(0,inf,N), between(0,N,A), B is N-A.

% ítem II
% perimetro(?T, ?P)
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), P is A+B+C.
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), nonvar(P), 
    between(1,P,A), between(1,P,B), between(1,P,C), P is A+B+C, 
    esTriangulo(tri(A,B,C)).
perimetro(tri(A,B,C), P) :- not(ground((tri(A,B,C)))), var(P), between(1,inf,P), perimetro(tri(A,B,C), P).

% ítem III
% triangulo(-T)
triangulo(T) :- perimetro(T, _).