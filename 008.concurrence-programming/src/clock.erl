-module(clock).

-export([start/0]).

start() ->
    clock_start(1000, fun() -> io:format("~p~n", [calendar:local_time()]) end),
    receive after 5000 -> void end,
    init:stop().

clock_start(Time, Fun) ->
    register(clock, spawn(fun() -> tick(Time, Fun) end)).

clock_stop() -> clock ! stop.

tick(Time, Fun) ->
    receive
        stop -> void
    after Time -> 
        Fun(),
        tick(Time, Fun)
    end.
