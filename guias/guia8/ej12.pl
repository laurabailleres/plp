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
aBB(B) :- inOrder(B, L), msort(L, L).

% ítem IV
% insertar(+E, +A1, -A2).
insertar(E, nil, bin(nil, E, nil)).
insertar(E, bin(BI, R, BD), bin(A2, R, BD)) :- insertar(E, BI, A2).
insertar(E, bin(BI, R, BD), bin(BI, R, A2)) :- insertar(E, BD, A2).

% aBBInsertar(+X, +T1, -T2) ; ¿es reversible en alguno de sus parámetros?
aBBInsertar(X, T1, T2) :- insertar(X, T1, T2), aBB(T2).