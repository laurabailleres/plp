% ejercicio 12
% ítem I
% inOrder(+AB, -Lista)
inOrder(nil, []).
inOrder(bin(I, R, D), L) :- inOrder(I, LI), inOrder(D, LD), append(LI, [R], A1), append(A1, LD, L). 

% ítem II
% árbolConInorder(+Lista, -AB)
arbolConInorder([], nil).
%

% ítem III
% aBB(+T)

% ítem IV
% aBBInsertar(+X, +T1, -T2) ; ¿es reversible en alguno de sus parámetros?