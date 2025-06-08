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