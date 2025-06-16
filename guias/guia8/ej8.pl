% ejercicio 8
% parteQueSuma(+L,+S,-P)
parteQueSuma(_, 0, []).
parteQueSuma([X|L], S, [X|P]) :- S > 0, N is S-X, parteQueSuma(L, N, P).
parteQueSuma([_|L], S, P) :- S > 0, parteQueSuma(L, S, P).