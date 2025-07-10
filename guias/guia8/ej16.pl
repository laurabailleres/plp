% ejercicio 16
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