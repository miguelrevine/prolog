:- module(_,_,[]).
author_data('Reviriego','Gine','Miguel','b190180').


% Predicados color y rule del enunciado 
color(o).
color(x).

rule(o,o,o, _, o). % Regla nula
rule(x,o,o, r(A,_,_,_,_,_,_), A) :- color(A).
rule(o,x,o, r(_,B,_,_,_,_,_), B) :- color(B).
rule(o,o,x, r(_,_,C,_,_,_,_), C) :- color(C).
rule(x,o,x, r(_,_,_,D,_,_,_), D) :- color(D).
rule(x,x,o, r(_,_,_,_,E,_,_), E) :- color(E).
rule(o,x,x, r(_,_,_,_,_,F,_), F) :- color(F).
rule(x,x,x, r(_,_,_,_,_,_,G), G) :- color(G).

% Predicado para concatenar una lista con otra (principalmente para concatenarlo con una 'o')
concatenar([], List, List).
concatenar([Head|Tail], List, [Head|Rest]) :-
    concatenar(Tail, List, Rest).


% Predicado para ver si el último elemento es una 'o'
ultimo(X,[X]).
ultimo(X,[_|Z]) :- ultimo(X,Z).

% Predicado para ver si el primer elemento es una 'o'
primero(X,[X|_]).


% Comprueba si un estado es válido (empieza y termina con células blancas)
empieza_y_termina_con(X, Lista) :-
    primero(X, Lista), % Comprueba si X es igual al primer elemento de la lista
    ultimo(X, Lista). % Comprueba si X es igual al último elemento de la lista

% Predicado para añadir un 'o' al principio y al final de la lista dada
aniadir_o(Lista, Resultado) :-
    concatenar([o], Lista, Temporal), % Agrega 'o' al principio de la lista
    concatenar(Temporal, [o], Resultado). % Agrega 'o' al final de la lista

% Condiciom de parada, si quedan solo dos celulas que seran blancas (no aplica nada, sale de recursion)
aplicar_reglas(_, [_Last1, _Last2], []).

aplicar_reglas(Reglas, [A,B,C|Resto], [CabezaNueva|RestoNuevo]) :-
    rule(A, B, C, Reglas, CabezaNueva),
    aplicar_reglas(Reglas, [B,C|Resto], RestoNuevo).

% Predicado cells/3
cells(EstadoInicial, Reglas, EstadoFinal) :-
    empieza_y_termina_con(o, EstadoInicial),
    aniadir_o(EstadoInicial, EstadoTemporal),
    aplicar_reglas(Reglas, EstadoTemporal, FinalTemporal),
    aniadir_o(FinalTemporal, EstadoFinal).
	
% Definición de números de Peano, si X es un numero, s(X) tambien lo es, sirve para "contar" s(s(s(0))) = 3
peano(0).
peano(s(X)) :- peano(X).


% Predicado evol/3
evol(N, RuleSet, Cells) :-
    peano(N),
    evol_rec(N, RuleSet, [o,x,o], Cells).

% Recursivo de evol para conseguir los pasos deseados de evolucion
evol_rec(0, _, Cells, Cells).
evol_rec(s(N), Reglas, EstadoInicial, Cells) :-
    cells(EstadoInicial, Reglas, EstadoFinal),
    evol_rec(N, Reglas, EstadoFinal, Cells).

% Predicado steps/2
steps(Cells, N) :-
    empieza_y_termina_con(o, Cells),
    peano(N),
    evol(N, _, Cells).

% Predicado ruleset/2
ruleset(_,Cells):-
empieza_y_termina_con(o,Cells).
