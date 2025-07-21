% ejercicio 18
% esCorte(+L,-L1,-L2)
esCorte(L, [X|XS], [Y|YS]) :- append([X|XS], [Y|YS], L).

% esMejorCorte(+C1, C2)
esMejorCorte(C1, C2, D1, D2) :- 
    sumlist(C1, SC1), sumlist(C2, SC2), S1 is abs(SC1-SC2), 
    sumlist(D1, SD1), sumlist(D2, SD2), S2 is abs(SD1-SD2), 
    S1 < S2.

% corteMásParejo(+L,-L1,-L2)
corteMásParejo(L, L1, L2) :- esCorte(L, L1, L2), not((esCorte(L, M1, M2), 
    esMejorCorte(M1, M2, L1, L2))).