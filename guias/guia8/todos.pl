% EJERCICIO 1
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

% ítem I
% ?- abuelo(X,manuel).
% X = juan ;
% false.

% ítem II
%hijo(?X, ?Y)
hijo(X, Y) :- padre(Y, X).

%hermano(?X, ?Y)
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X\=Y.

%descendiente(?X, ?Y)
descendiente(X, Y) :- hijo(X, Y).
descendiente(X, Y) :- hijo(X, Z), descendiente(Z, Y).

%agrego casos para probar:
padre(daniel, matias).
padre(ramiro, ricardo).

% ítem IV
% ?- abuelo(juan,X).

% ítem V
% ?- hermano(pablo,X).
% ó
% ?- hermano(X,pablo).

% ítem VI
% ancestro(X, X).
% ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% ítem VII
% ?- ancestro(juan,X).
% X = juan ;
% X = carlos ;
% X = luis ;
% X = daniel ;
% X = diego ;
% X = pablo ;
% X = manuel ;
% X = ramiro ;
% X = matias ;
% X = ricardo ;
% ;
% ERROR: Stack limit (1.0Gb) exceeded

% acá lo que yo entiendo que pasa es que la consulta se termina
% colgando porque en el segundo caso tiene el llamado recursivo
% como primer término y hace recursión al infinito ?? no se

% ítem VIII
% pero si swapeo el orden en el segundo caso anda ok

ancestro(X, X).
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).

% ?- ancestro(juan,X).
% X = juan ;
% X = carlos ;
% X = daniel ;
% X = matias ;
% X = diego ;
% X = luis ;
% X = pablo ;
% X = manuel ;
% X = ramiro ;
% X = ricardo ;
% false.

% si


% EJERCICIO 2
vecino(X, Y, [X|[Y|Ls]]).
vecino(X, Y, [W|Ls]) :- vecino(X, Y, Ls).

% ítem I
% vecino(5, Y, [5,6,5,3]).
% Y = 6 ;
% Y = 3 ;
% false.

% arranca unificando 5 con X y [5,6,5,3] con [X|[Y|Ls]]. como no se
% rompe la unificación (o sea, no estoy unificando X con dos cosas
% distintas...... ver guía 5), instancia Y con el segundo elemento de
% [5,6,5,3] (o sea 6). OK
% después unifica [5,6,5,3] con [W|Ls] (5|[6,5,3]) y hace un llamado 
% recursivo de esta forma: vecino(5, Y, [6,5,3]). vuelve al primer caso
% y acá sí se rompe la unificación porque va a querer unificar X = 5 con 
% X = 6 y no. entonces acá no instancia nada. 
% después va al segundo caso e instancia [W|Ls] con [6,5,3] (6|[5,3]) y
% hace un llamado recursivo de la forma vecino(5,Y,[5,3]).
% va al primer caso y acá si unifica. entonces instancia Y con 3.
% va al segundo llamado e instancia [5,3] con [W|Ls] (5|[3]) y hace
% un llamado recursivo de la forma vecino(5,Y,[3]).
% acá se rompe la unificación porque no puedo instanciar [X|[Y|Ls]]
% así que false y chau.

% ítem II
% veo qué pasa
vecino2(X, Y, [W|Ls]) :- vecino2(X, Y, Ls).
vecino2(X, Y, [X|[Y|Ls]]).

% ?- vecino2(5, Y, [5,6,5,3]).
% Y = 3 ;
% Y = 6.

% aparentemente anda ok pero me da las cosas en otro orden (o sea
% me devuelve primero el último vecino en la lista) pero pruebo con otro
% caso

% ?- vecino2(6, Y, [5,6,5,3]).
% Y = 5 ;
% false.

% bueno parecería andar bien......

% caso borde
% ?- vecino2(3, Y, [5,6,5,3]).
% false.

% está bien porque no tiene vecino



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



% EJERCICIO 4
% juntar(?Lista1,?Lista2,?Lista3)
juntar([], L2, L2).
juntar([X|L1], L2, [X|Lrec]) :- juntar(L1, L2, Lrec).

% si antes de decir quién va a ser L3 llamo a juntar(L1, L2, Lrec)
% se cuelga !! porque sigue unificando infinitamente
% si tengo antes que L3 = [X|Lrec] L3 no va a unificar con la lista 
% vacía



% EJERCICIO 5
% ítem I
% last(?L, ?U)
last(L, U) :- append(_, [U], L).

% ítem II
% reverse(+L, ?R)
reverse([], []). 
reverse([X|L], R) :- reverse(L, Lrec), append(Lrec, [X], R).

% ítem III
% prefijo(?P, +L)
prefijo(P, L) :- append(P, _, L).

% ítem IV
% sufijo(?S, +L)
sufijo(S, L) :- append(_, S, L).

% sufijoFacha(?S, +L)
sufijoFacha(S, L) :- reverse(L, R), prefijo(S, R).

% ítem V
% sublista(?S, +L)
sublista([], L).
sublista(S, L) :- S = [_|_], prefijo(Pre, L), sufijo(Suf, L), append(Pre, S, PreSub), append(PreSub, Suf, L).

% sublistaFacha(?S, +L)
sublistaFacha([], L).
sublistaFacha(S, L) :- S = [_|_], prefijo(Pre, L), sufijo(S, Pre).

% ítem VI
% pertenece(?X, +L)
pertenece(X, L) :- append(_, [X|_], L).



% EJERCICIO 6 
% ejercicio 6
aplanar([], []).
aplanar([X | L], [X | YS]) :- X \= [], X \= [_|_], aplanar(L, YS).
aplanar([X | L], L3) :- is_list(X), aplanar(X, Y), aplanar(L, L2), append(Y, L2, L3).



% EJERCICIO 7
% ítem I
% intersección(+L1, +L2, -L3)
interseccion([], _, []).
interseccion([X|L1], L2, L3) :- not(member(X, L2)), interseccion(L1, L2, L3).
interseccion([X|L1], L2, [X|L3]) :- member(X, L2), interseccion(L1, L2, L3).

% partir(?N, ?L, ?L1, ?L2)
partir(N, L, L1, L2) :- append(L1, L2, L), length(L1, N).

% ítem II
% borrarRec(+ListaOriginal, +X, -Xs)
borrar(L, X, L) :- not(member(X, L)).
borrar(L, X, Xs) :- not(not(member(X, L))), append(L1, [X|L2], L), not(member(X, L1)), borrar(L2, X, L2X), append(L1, L2X, Xs).

% ítem III
% sacarDuplicados(+L1, -L2)
sacarDuplicados([], []).
sacarDuplicados([X|L1], L2) :- member(X, L1), sacarDuplicados(L1, L2).
sacarDuplicados([X|L1], [X|L2]) :- not(member(X, L1)), sacarDuplicados(L1, L2).

% ítem IV
% permutación(+L1, ?L2) 
permutacion([], []).
permutacion(L1, L2) :- length(L1, N), length(L2, N), interseccion(L1, L2, L1).

% ítem V
% reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto(L, N, [X|LL]) :- append(X, Lrec, L), N > 0 , N2 is N-1, reparto(Lrec, N2, LL).

%ítem VI
% hayVacias
hayVacias(XS) :- member([], XS).

% repartoSinVacias(+L, +N, -LListas)
repartoSinVacias(L1, N, L2) :- reparto(L1, N, L2), not(hayVacias(L2)).



% EJERCICIO 8
% parteQueSuma(+L,+S,-P)
parteQueSuma(_, 0, []).
parteQueSuma([X|L], S, [X|P]) :- S > 0, N is S-X, parteQueSuma(L, N, P).
parteQueSuma([_|L], S, P) :- S > 0, parteQueSuma(L, S, P).



% EJERCICIO 9
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% ítem I
% para que no se produzca un error tengo que instanciar N antes
% de instanciar Y con el llamado recursivo. por q? no se

% ítem II
% desdeReversible(+X, ?Y)
desdeReversible(X, Y) :- nonvar(Y), Y >= X.
desdeReversible(X, Y) :- var(Y), desde(X, Y).



% EJERCICIO 10
% intercalar(?L1, ?L2, ?L3)
intercalar([], L, L) :- L \= [].
intercalar(L, [], L).
intercalar([X|L1], [Y|L2], [X|[Y|L3]]) :- intercalar(L1, L2, L3).



% EJERCICIO 11
% vacio(+B)
vacio(nil).

% raiz(+B, ?R)
raiz(bin(_, X, _), X).

% altura(+B, ?A)
altura(nil, 0).
altura(bin(I, _, D), A) :- altura(I,A1), altura(D,A2), A is max(A1, A2) + 1.

% cantidadDeNodos(+B, ?N)
cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(I, _, D), N) :- cantidadDeNodos(I, CI), cantidadDeNodos(D, CD), N is 1+CI+CD.



% EJERCICIO 12
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



% EJERCICIO 13
% coprimos(-X,-Y)
coprimos(X, Y) :- between(1, inf, X), between(1, X, Y), 1 is gcd(X, Y).



% EJERCICIO 14
% ítem I
% listaQueSuma(+S, +L, -XS)
% S = número que tiene que sumar
% L = largo de la lista
% XS = lista
listaQueSuma(0, 0, []).
listaQueSuma(S, L, [X|XS]) :- L > 0, between(0, S, X), S1 is S-X, L1 is L-1, listaQueSuma(S1, L1, XS).

% matrizQueSuma(+S, +F, +C, -XS)
% S = lo que tiene que sumar cada lista
% F = cantidad de listas (filas)
% C = largo de las listas (columnas)
% XS = matriz
matrizQueSuma(_, 0, _, []).
matrizQueSuma(S, F, C, [X|XS]) :- F > 0, listaQueSuma(S, C, X), F1 is F-1, matrizQueSuma(S, F1, C, XS).

% cuadradoSemiMágico(+N, -XS)
cuadradoSemiMagico(N, XS) :- between(0, inf, M), matrizQueSuma(M, N, N, XS).
% instancio la cantidad de filas y de columnas con el mismo número (el tamaño del
% cuadrado, o sea N) porque quiero justamente que sea un cuadrado. con between
% genero naturales y con eso genero cuadrados semimágicos.



% EJERCICIO 15
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



% EJERCICIO 16
frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).

% leGusta(X) :- frutal(X), cremoso(X).
% cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% ítem I
% ?- cucurucho(X,Y).
% X = Y, Y = frutilla ;
% X = frutilla,
% Y = banana ;
% X = banana,
% Y = frutilla ;
% X = Y, Y = banana ;
% false.

% primero veo que los gustos que le gustan son los frutales y cremosos
% a la vez: banana y frutilla.
% primero se mete en la rama X=frutilla y ve que también puede instanciar a Y
% con frutilla. luego, "vuelve" al nodo X=frutilla y busca otra posible
% instanciación para Y. encuentra Y=banana. acá ve que ya no hay otra posible 
% instanciación de Y en el subárbol X=frutilla y busca otra forma de instanciar
% X. encuentra X=banana y luego, en este subárbol, encuentra Y=frutilla e
% Y=banana. por último busca otra instanciación para X y como no la encuentra
% -> false.



% EJERCICIO 18
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



% EJERCICIO 20
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