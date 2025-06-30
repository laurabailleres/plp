% ejercicio 12
% ítem I
% inOrder(+AB, -Lista)
inOrder(nil, []).
inOrder(bin(I, R, D), L) :- inOrder(I, LI), inOrder(D, LD), append(LI, [R], A1), append(A1, LD, L). 

% ítem II
% árbolConInorder(+Lista, -AB)
arbolConInorder([], nil).
arbolConInorder(XS, AB) :- append(LI, [X|LD], XS), arbolConInorder(LI, AI), arbolConInorder(LD, AD), AB = bin(AI, X, AD).

% ítem III
% aBB(+T)
aBB(nil).
aBB(bin(I, R, D)) :- R.

% ítem IV
% aBBInsertar(+X, +T1, -T2) ; ¿es reversible en alguno de sus parámetros?