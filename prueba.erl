%%% File    : prueba.erl
%%% Author  : JCastro <>
%%% Description : 
%%% Created : 21 Mar 2014 by JCastro <>

-module(prueba).

-compile(export_all).

% --- Ejemplos sencillos de factorial y miembro
% > c(prueba).
% > prueba:fact(10).

fact(0) -> 1;
fact(N) when N > 0 ->
    N * fact(N-1).

miembro(_,[]) -> false;
miembro(X,[X|_]) -> true;
miembro(X,[_|Y]) -> miembro(X,Y).

% --- proceso que dialoga con la consola
%
% > c(prueba).
% A = spawn(prueba, proc, []).
% A ! hola.
% que tal?
% ok
% > _
%
proc() ->
    receive
	   hola ->
	       io:format("que tal?\n"),
	       proc();
	   chao ->
	       io:format("hasta luego\n")
    end.

% -- dialog entre 2 procesos
%
% > c(prueba).
% > B = spawn(prueba, procB, []).
% spawn(prueba, procA, [B]).
% ...

procA(B) ->
    io:format("A: hola\n"),
    B ! {self(), hola},
    receive
	   que_tal ->
	       io:format("bien y ud?\n"),
	       B ! {self(), bien_y_usted}
    end,
    receive
	   bien_ciao ->
	       io:format("ok, hasta luego\n"),
	       B ! ok_hasta_luego
    end.    

procB() ->
    receive
	   {A, hola} ->
	        A ! que_tal,
	       io:format("que tal?\n")
    end,
    receive 
	   {A, bien_y_usted} ->
	       io:format("bien ciao\n"),
	       A ! bien_ciao
    end,
    receive _ ->
	    ok
    end.

% --- calculo distribuido de numeros primos
% > c(prueba).
% > prueba:primos(1000). % genera 999 procesos
%

primo(N) -> primoAux(N,2).

primoAux(N,I) when I*I > N -> true;
primoAux(N,I) ->
    if 
	   N rem I == 0 -> false;
	   true -> primoAux(N,I+1)
    end.

range(N,M) when N > M -> [];
range(N,M) -> [N | range(N+1, M)].

primos(N) ->
    genereTrabajadores(2,N),
    recolecteResultados([], N-1).

genereTrabajadores(I,N) when I > N -> nada;
genereTrabajadores(I,N) ->
    Self = self(),
    spawn(fun() -> trabajador(Self,I) end),
    genereTrabajadores(I+1,N).

trabajador(Master, I) -> Master ! {I, primo(I)}.

recolecteResultados(Primos, 0) -> Primos;
recolecteResultados(Primos, N) ->
    receive
	   {P, true}  -> recolecteResultados([P|Primos], N-1);
	   {_, false} -> recolecteResultados(Primos, N-1)
    end.
