% EJERCICIO 3
natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X,X) :- natural(X).

% ítem I
% ?- menorOIgual(0,X).
% ERROR: Stack limit (1.0Gb) exceeded

% acá lo que pasa es que entra al primer caso, intancia X con 0 y
% busca algo para instanciar a Y. pero se mete al llamado recursivo 
% y no termina en nada. creo que si cambio de orden las dos cosas 
% que tengo debería funcionar .....

% ítem III

menorOIgual2(X,X) :- natural(X).
menorOIgual2(X, suc(Y)) :- menorOIgual2(X, Y).

% ?- menorOIgual2(0,X).
% X = 0 ;
% X = suc(0) ;
% X = suc(suc(0)) ;
% X = suc(suc(suc(0))) ;
% X = suc(suc(suc(suc(0)))) ;
% X = suc(suc(suc(suc(suc(0))))) ;
% X = suc(suc(suc(suc(suc(suc(0)))))) .

% si