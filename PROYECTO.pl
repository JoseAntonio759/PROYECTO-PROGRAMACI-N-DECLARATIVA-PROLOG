nodo(i2m, -2, 1).
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
    format("Turno: ~w~n~n", [Turno]),
    pintar_tablero(estado(Liebre, Sabuesos, Turno)).




pintar_tablero(Estado) :-
    simbolo(i1a, Estado, I1A), simbolo(ma, Estado, MA), simbolo(d1a, Estado, D1A),
    simbolo(i2m, Estado, I2M), simbolo(i1m, Estado, I1M), simbolo(mm, Estado, MM),
    simbolo(d1m, Estado, D1M), simbolo(d2m, Estado, D2M),
    simbolo(i1b, Estado, I1B), simbolo(mb, Estado, MB), simbolo(d1b, Estado, D1B),

    format("    ~w -- ~w -- ~w~n", [I1A, MA, D1A]),
    format("   / | \\ | / | \\~n"),
    format(" ~w - ~w - ~w - ~w - ~w~n", [I2M, I1M, MM, D1M, D2M]),
    format("   \\ | / | \\ | /~n"),
    format("    ~w -- ~w -- ~w~n", [I1B, MB, D1B]).

simbolo(Celda, estado(Liebre, Sabuesos, _), Simbolo) :-
    ( Liebre == Celda ->
    Simbolo = '\033[34mL\033[0m' % L azul para la liebre
    ; member(Celda, Sabuesos) ->
    Simbolo = '\033[32mS\033[0m' % S roja para los sabuesos
    ; Simbolo = 'o' % o sin color si celda vacía
    ).

