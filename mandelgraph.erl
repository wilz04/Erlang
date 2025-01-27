%%% File    : mandelpgm.erl
%%% Author  : Jose Castro <>
%%% Description : 
%%% Created : 20 Aug 2009 by Jose Castro <>

-module(mandelgraph).
-compile(export_all).

-define(IMAGE_SIZE,512).
-define(MAX_ITER,255).
-define(NUM_WORKERS,8).
-define(BLOCK_SIZE,64).
-define(NUM_COLORS,8).
-record(bounds, {left=-2.0, right=2.0, up=2.0, down=-2.0}).

% compute the mandelbrot-set function for a given
% point in the complex plane where
% c(i+1) = c(i)^2 + Z.

compute({complex, X, Y}, {complex, Real, Im}) -> {complex, X*X - Y*Y + Real, 2*X*Y + Im}.

mandelbrotLoop(_,_,Iter) when Iter > ?MAX_ITER -> Iter;
mandelbrotLoop(X,Z,Iter) ->
    Norm = norm(X),
    if
	Norm >= 4 -> Iter;
	true      -> mandelbrotLoop(compute(X,Z),Z,Iter+1)
    end.

norm({complex, X, Y}) -> X*X + Y*Y.

convertToWorld(Ix, Iy, ImgSize, Xmin, Xmax, Ymin, Ymax) ->
    X = (Ix/ImgSize),
    Y = (ImgSize - Iy)/ImgSize,
    {complex, X*(Xmax-Xmin)+Xmin, Y*(Ymax-Ymin)+Ymin}.

worker(Boss, Gatherer, Bounds=#bounds{left=Rmin, right=Rmax, up=Imax, down=Imin}) ->

    Boss ! {self(), mas_trabajo},
    receive
        no_hay       -> termine;
	{MinY, MaxY} ->
	    Range      = lists:seq(0, ?IMAGE_SIZE-1),
	    RangeIm    = lists:seq(MinY, MaxY),
	    PGMData = lists:foldl(
			fun(Iy,AccumY) ->
				io:format("."),
				lists:foldl(
				  fun(Ix,Accum) ->
					  Complex = convertToWorld(Ix, Iy, ?IMAGE_SIZE, Rmin, Rmax, Imin, Imax),
					  Iter    = mandelbrotLoop({complex, 0, 0}, Complex, 0),

					  if
					      Iter < ?MAX_ITER -> 
						  [(Iter div ?NUM_COLORS) + ((256 div ?NUM_COLORS) * (Iter rem ?NUM_COLORS)) | Accum];

					      true -> 
						  [0 | Accum]
					  end
				  end,
				  AccumY,
				  Range
				 )
			end,
			[],
			RangeIm
		       ),
	    BinData = list_to_binary(lists:reverse(PGMData)),
	    Gatherer ! {MinY,BinData},

	    worker(Boss, Gatherer, Bounds)
    end.

pgmHeader(Width, Height, MaxVal) ->
    "P5\n" ++ integer_to_list(Width) ++ " " ++ integer_to_list(Height) ++ "\n" ++ integer_to_list(MaxVal) ++ "\n".

gatherer(0, Packets) ->
    io:format("escribiendo archivo..."),
    SortedList = lists:map(fun({_,Y}) -> Y end, lists:sort(fun sort/2, Packets)),
    {ok, File} = file:open("mandelbrotErl.pgm", [raw, binary, write]),
    PGMHeader  = list_to_binary(pgmHeader(?IMAGE_SIZE, ?IMAGE_SIZE, 255)),
    BinData    = list_to_binary(SortedList),
    file:write(File, PGMHeader),
    file:write(File, BinData),
    file:close(File),
    io:format("listo\n"),
    main ! continue;

gatherer(Pending, Packets) when Pending > 0 ->
    receive
	{Key, Data} ->
	    gatherer(Pending-1, [{Key,Data} | Packets])
    end.

sort({MinA,_}, {MinB,_}) -> MinA < MinB.

boss(Bounds, WorkerList, NumStrips) ->
    NumWorkers = lists:sum(lists:map(fun({_,X})-> X end, WorkerList)),
    io:format("aqui esta el jefe, tengo ~w trabajadores y ~w franjas...\n", [NumWorkers, NumStrips]),
    Gatherer = spawn(fun() -> gatherer(NumStrips,[]) end),
    Boss     = self(),
    spawn(fun() -> spawn_workers(Boss, Gatherer, Bounds, WorkerList) end),
    loop_pending(0, ?IMAGE_SIZE, 0, NumStrips, NumWorkers).

worker_list(N) when is_integer(N) -> [{node(), N}];
worker_list(L) when is_list(L)    -> L.

spawn_workers(_, _, _, []) -> done;
spawn_workers(Boss, Gatherer, Bounds, [{Host,Num}|Rest]) ->
    create_workers_node(Host, Num, Boss, Gatherer, Bounds),
    spawn_workers(Boss, Gatherer, Bounds, Rest).

create_workers_node(_, 0, _, _, _) -> ok;
create_workers_node(Host, N, Boss, Gatherer, Bounds) ->
    rpc:call(Host, mandelgraph, worker, [Boss, Gatherer, Bounds]),
    create_workers_node(Host, N-1, Boss, Gatherer, Bounds).

loop_pending(_, _, _, _, 0) ->
    receive continue -> ok end;
    
loop_pending(I, Max, Strip, NumStrips, NumWorkers) ->
    Rem  = Max rem NumStrips,
    if
	Strip < Rem -> Size = Max div NumStrips + 1;
	true        -> Size = Max div NumStrips
    end,
    receive
	{Worker, mas_trabajo} ->
	    if 
		I >= Max -> 
		    Worker ! no_hay,
		    loop_pending(I, Max, Strip, NumStrips, NumWorkers-1);
		true ->
		    Worker ! {I, I+Size-1},
		    loop_pending(I+Size, Max, Strip+1, NumStrips, NumWorkers)
	    end
    end.

start() -> start(8, 16).

start(WorkersSpec, NumStrips) -> 
    register(main, self()),
    Bounds = #bounds{},
    WorkerList = worker_list(WorkersSpec),
    boss(Bounds, WorkerList, NumStrips),
    os:cmd("convert mandelbrotErl.pgm mandelbrotErl.gif"),
    GS     = gs:start(),
    Win    = gs:create(window, GS, [{width, ?IMAGE_SIZE+250}, {height, ?IMAGE_SIZE+80}, {title, "Mandelbrot"}, {map, true}]),
    Canvas = gs:create(canvas, mandelbrot, Win, [{x,50},{y,35},{width, ?IMAGE_SIZE},{height,?IMAGE_SIZE},{bg, white},{buttonpress, true}]),
    Image  = gs:create(image, Canvas, [{load_gif, "mandelbrotErl.gif"}]),
    Plus   = gs:create(button, Win, [{x,10}, {y,40+?IMAGE_SIZE}, {width, 30}, {label, {text,"+"}}]),
    Minus  = gs:create(button, Win, [{x,40}, {y,40+?IMAGE_SIZE}, {width, 30}, {label, {text,"-"}}]),
    Left   = gs:create(button, Win, [{x,10},   {y,35}, {width, 30},  {height, ?IMAGE_SIZE}, {label, {text,"<"}}]),
    Right  = gs:create(button, Win, [{x,?IMAGE_SIZE+55}, {y,35}, {width, 30},  {height, ?IMAGE_SIZE}, {label, {text, ">"}}]),
    Up     = gs:create(button, Win, [{x,50},   {y,2}, {width, ?IMAGE_SIZE}, {height, 26},  {label, {text, "^"}}]),
    Down   = gs:create(button, Win, [{x,80},   {y,40+?IMAGE_SIZE},{width,?IMAGE_SIZE-30},{height, 30},  {label, {text, "v"}}]),
    gs:create(label, Win, [{x,?IMAGE_SIZE+90}, {y, 35}, {label, {text, "X:"}}]),
    gs:create(label, Win, [{x,?IMAGE_SIZE+90}, {y, 75}, {label, {text, "Y:"}}]),
    gs:create(entry, xval, [{width,50},{x,?IMAGE_SIZE+100},{y,35}]),
    gs:create(entry, yval, [{width,50},{x,?IMAGE_SIZE+100},{y,75}]),
    loop(Canvas, Image, Plus, Minus, Left, Right, Up, Down, Bounds, WorkerList, NumStrips).

loop(Canvas, Image, Plus, Minus, Left, Right, Up, Down, Bounds, WorkerList, NumStrips) ->
    receive
	{gs, mandelbrot, buttonpress, [], [1,Ix,Iy|_]} ->
	    #bounds{left=Xmin, right=Xmax, up=Ymax, down=Ymin} = Bounds,
	    {complex, X,Y} = convertToWorld(Ix, Iy, ?IMAGE_SIZE, Xmin, Xmax, Ymin, Ymax),
	    NewBounds = focus_on(X,Y,Bounds);
	{gs, Plus,  click, _, _} -> NewBounds = zoom_in(Bounds);
	{gs, Minus, click, _, _} -> NewBounds = zoom_out(Bounds);
	{gs, Left,  click, _, _} -> NewBounds = shift_left(Bounds);
	{gs, Right, click, _, _} -> NewBounds = shift_right(Bounds);
	{gs, Up,    click, _, _} -> NewBounds = shift_up(Bounds);
	{gs, Down,  click, _, _} -> NewBounds = shift_down(Bounds);
	{gs, _, destroy,   _, _} -> NewBounds = none;
	_ -> NewBounds = Bounds
    end,
    if
	NewBounds =:= none ->
	    exit;
	Bounds =:= NewBounds ->
	    loop(Canvas, Image, Plus, Minus, Left, Right, Up, Down, Bounds, WorkerList, NumStrips);
	true ->
	    io:format("calculando imagen...\n"),
	    boss(NewBounds, WorkerList, NumStrips),
	    %% io:format("conviertiendo archivo..."),
	    %% os:cmd("convert mandelbrotErl.pgm mandelbrotErl.gif"),
	    io:format("listo.\n"),
	    gs:destroy(Image),
	    NewImage = gs:create(image, Canvas, [{load_gif, "mandelbrotErl.pgm"}]),
	    loop(Canvas, NewImage, Plus, Minus, Left, Right, Up, Down, NewBounds, WorkerList, NumStrips)
    end.

focus_on(X, Y, #bounds{left=Left, right=Right}) ->
    io:format("focus_on\n"),
    Delta  = (Right-Left)/2,
    #bounds{left=X-Delta, right=X+Delta, up=Y+Delta, down=Y-Delta}.

zoom_in(#bounds{left=Left, right=Right, up=Up, down=Down}) ->
    io:format("zoom_in\n"),
    Size    = Right - Left,
    NewSize = Size * 0.5,
    Delta   = (Size - NewSize)/2.0,
    #bounds{left=Left+Delta, right=Right-Delta, up=Up-Delta, down=Down+Delta}.

zoom_out(#bounds{left=Left, right=Right, up=Up, down=Down}) ->
    io:format("zoom_out\n"),
    Size    = Right - Left,
    NewSize = Size / 0.5,
    Delta   = (NewSize - Size)/2.0,
    #bounds{left=Left-Delta, right=Right+Delta, up=Up+Delta, down=Down-Delta}.

shift_left(Bounds=#bounds{left=Left, right=Right}) ->
    io:format("shilft_left\n"),
    Delta = (Right-Left)/4.0,
    Bounds#bounds{left=Left-Delta, right=Right-Delta}.

shift_right(Bounds=#bounds{left=Left, right=Right}) ->
    io:format("shift_right\n"),
    Delta = (Right-Left)/4.0,
    Bounds#bounds{left=Left+Delta, right=Right+Delta}.

shift_up(Bounds=#bounds{up=Up, down=Down}) ->
    io:format("shift_up\n"),
    Delta = (Up-Down)/4.0,
    Bounds#bounds{up=Up+Delta,down=Down+Delta}.

shift_down(Bounds=#bounds{up=Up, down=Down}) ->
    io:format("shift_down\n"),
    Delta = (Up-Down)/4.0,
    Bounds#bounds{up=Up-Delta,down=Down-Delta}.


