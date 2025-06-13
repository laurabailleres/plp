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