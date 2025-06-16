% EJERCICIO 4
% juntar(?Lista1,?Lista2,?Lista3)
juntar([], L2, L2).
juntar([X|L1], L2, [X|Lrec]) :- juntar(L1, L2, Lrec).

% si antes de decir quién va a ser L3 llamo a juntar(L1, L2, Lrec)
% se cuelga !! porque sigue unificando infinitamente
% si tengo antes que L3 = [X|Lrec] L3 no va a unificar con la lista 
% vacía
