nodo(i2m, -2, 0).
nodo(i1a, -1, 1).
nodo(i1m, -1, 0).
nodo(i1b, -1, -1).
nodo(ma, 0, 1).
nodo(mm, 0, 0).
nodo(mb, 0, -1).
nodo(d1a, 1, 1).
nodo(d1m, 1, 0).
nodo(d1b, 1, -1).
nodo(d2m, 2, 0).

adyacentes(i2m, [i1a, i1m, i1b]).
adyacentes(i1a, [i2m, i1m, mm, ma]).
adyacentes(i1m, [i2m, i1a, i1b, mm]).
adyacentes(i1b, [i2m, i1m, mm, mb]).
adyacentes(ma, [i1a, mm, d1a]).
adyacentes(mm, [i1a, i1m, i1b, ma, mb, d1a, d1m, d1b]).
adyacentes(mb, [i1b, mm, d1b]).
adyacentes(d1a, [ma, mm, d1m, d2m]).
adyacentes(d1m, [mm, d1a, d1b, d2m]).
adyacentes(d1b, [mm, mb, d1m, d2m]).
adyacentes(d2m, [d1a, d1m, d1b]).


estado_inicial(estado(d2m, [i1a, i2m, i1b], liebre)).

mostrar_estado(estado(Liebre, Sabuesos, Turno)) :-
    format("Liebre en: ~w~n", [Liebre]),
    format("Sabuesos en: ~w~n", [Sabuesos]),
    format("Turno de: ~w~n~n", [Turno]),
    pintar_tablero(estado(Liebre, Sabuesos, Turno)).




pintar_tablero(Estado) :-
    simbolo(i1a, Estado, I1A), simbolo(ma, Estado, MA), simbolo(d1a, Estado, D1A),
    simbolo(i2m, Estado, I2M), simbolo(i1m, Estado, I1M), simbolo(mm, Estado, MM),
    simbolo(d1m, Estado, D1M), simbolo(d2m, Estado, D2M),
    simbolo(i1b, Estado, I1B), simbolo(mb, Estado, MB), simbolo(d1b, Estado, D1B),

    format("         ~w -- ~w -- ~w~n", [I1A, MA, D1A]),
    format("      /  |  \\ |  / | \\~n"),
    format("     ~w - ~w -- ~w -- ~w - ~w~n", [I2M, I1M, MM, D1M, D2M]),
    format("      \\  |  / |  \\ | /~n"),
    format("         ~w -- ~w -- ~w~n", [I1B, MB, D1B]).

simbolo(Celda, estado(Liebre, Sabuesos, _), Simbolo) :-
    ( Liebre == Celda ->
    Simbolo = '\033[34mL\033[0m' % L azul para la liebre
    ; member(Celda, Sabuesos) ->
    Simbolo = '\033[32mS\033[0m' % S roja para los sabuesos
    ; Simbolo = 'o' % o sin color si celda vacía
    ).

pertenece(Elem, [Elem|_]).
pertenece(Elem, [H|T]) :- pertenece(Elem, T).

% Comprobamos en el movimiento que el jugador sea o liebre o sabuesos, en ambos comprobamos que el destino
% pertenece a las posiciones a las cuales la posición inicial es adyacente, y en adición a los sabuesos,
% comprobamos que el valor de la columna de la posición inicial sea menor que la del destino para que no retrocedan.
movimiento(Jugador, Desde, Hacia) :- 
    (Jugador = liebre, adyacentes(Desde, Posiciones), 
    pertenece(Hacia, Posiciones);
    Jugador = sabuesos, adyacentes(Desde, Posiciones), 
    pertenece(Hacia, Posiciones),
    nodo(Desde, Col1, _), nodo(Hacia, Col2, _), Col1 =< Col2). 


movimiento_valido(Estado, Desde, Hacia) :- 
    Estado = estado(Liebre, Sabuesos, Turno),
% Caso de liebre, los movimientos que puede realizar junto con que su destino no coincida con la posición
% de los sabuesos.
    (Turno = liebre, Desde = Liebre,
    movimiento(Liebre, Desde, Hacia), 
    \+ pertenece(Hacia, Sabuesos)
    ;
% Caso de sabuesos, los movimientos que pueden realizar los sabuesos junto con que su destino no 
% coincida con la posición de otros sabuesos ni con la de la liebre.
    Turno = sabuesos, pertenece(Desde, Sabuesos), 
    movimiento(Sabuesos, Desde, Hacia),
    Hacia \= Liebre, \+ pertenece(Hacia, Sabuesos)).

% Movemos la ficha de la liebre, como es una única posición no tiene más que el movimiento sea un nodo 
% y actualizamos el estado con este.
mover_ficha(Estado, Movimiento, NuevoEstado) :-
    Estado = estado(Liebre, Sabuesos, liebre),
    movimiento_valido(Estado, Liebre, Movimiento), 
    NuevoEstado = estado(Movimiento, Sabuesos, liebre).

% Movemos la ficha del sabueso a mover, cómo son varios, la posición en la que se encuentra el sabueso 
% a mover la retiramos de la lista de posiciones y se actualiza con su destino con las otras igual.
mover_ficha(Estado, (Desde, Hacia), NuevoEstado) :-
    Estado = estado(Liebre, Sabuesos, sabuesos),
    movimiento_valido(Estado, Desde, Hacia),
    sustituir(Sabuesos, Desde, Hacia, NuevosSabuesos),
    NuevoEstado = estado(Liebre, NuevosSabuesos, sabuesos).

% Predicado para sustituir posiciones de la lista.
sustituir([PosicionAQuitar|T1], PosicionAQuitar, PosicionNueva, [PosicionNueva|T1]).
sustituir([H1|T1], PosicionAQuitar, PosicionNueva, [H1|T2]) :- 
    sustituir(T1, PosicionAQuitar, PosicionNueva, T2).

% Aplicamos el movimiento elegido, moviendo la ficha y cambiando el turno que corresponde
aplicar_movimiento(Estado, Movimiento, NuevoEstado) :- 
    Estado = estado(Liebre, Sabuesos, Turno),
    mover_ficha(Estado, Movimiento, Estado2),
    cambiar_turno(Estado2, NuevoEstado).

% Predicado para cambiar el turno de un estado
cambiar_turno(Estado, NuevoEstado) :- 
    Estado = estado(Liebre, Sabuesos, Turno),
    (Turno = liebre, 
    NuevoEstado = estado(Liebre, Sabuesos, sabuesos)
    ;
    Turno = sabuesos,
    NuevoEstado = estado(Liebre, Sabuesos, liebre)
    ).
