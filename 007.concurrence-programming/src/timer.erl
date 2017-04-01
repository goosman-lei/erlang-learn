-module(timer).

-export([start/0]).

start() ->
    Pid_SayHi = timer_init(1000, fun say_hi/0),
    Pid_SayHello = timer_init(2000, fun say_hello/0),
    Pid_SayHello ! cancel,
    receive after 4000 -> true end,
    init:stop().

timer_init(T, F) ->
    spawn(fun() -> timer_loop(T, F) end).

timer_loop(T, F) ->
    receive
        cancel -> void
    after
        T -> F()
    end.
    
say_hi() ->
    io:format("[~p] Hi~n", [calendar:local_time()]).

say_hello() ->
    io:format("[~p] Hello~n", [calendar:local_time()]).
