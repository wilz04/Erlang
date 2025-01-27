-module(anillo).

-compile(export_all).

proc(Primero, Siguiente) ->
	receive
		{siguiente, P} -> proc(Primero, P), io:format("yo soy ~w el siguiente es ~w~n",[self(), P]);
		{empieze, Token} ->
			Siguiente ! {token, Token},
			proc(true, Siguiente);
		{token, Token} ->
			if
			   Primero ->
				if 
				  Token == 0 -> io:format("~w termina~n", [self()]), nada;
				  true -> 
					Siguiente ! {token, Token-1},
					proc(Primero, Siguiente)
				end;
			   true ->
				Siguiente ! {token, Token},
				if Token > 0 -> proc(Primero, Siguiente);
				   true -> io:format("~w termina ~n", [self()]), nada
				end
			end
	end.

genereProcesos(0) -> [];
genereProcesos(N) when N > 0 ->
	[spawn(fun() -> proc(false, nada) end)|genereProcesos(N-1)].

linkProcesos(Primero, [Ultimo]) -> 
	Ultimo ! {siguiente, Primero};
linkProcesos(Primero, [P1,P2|Resto]) ->
	P1 ! {siguiente, P2},
	linkProcesos(Primero, [P2|Resto]).
	
start(NumProcs, Token) ->
	[P|Resto] = genereProcesos(NumProcs),
	io:format("Lista de procesos ~w~n", [[P|Resto]]),
	linkProcesos(P,[P|Resto]),
	P ! {empieze, Token}.
