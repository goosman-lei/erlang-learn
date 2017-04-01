-module(area_server).

-export([start/0, area/2]).

start() ->
    spawn(fun loop/0).

area(Pid, What) ->
    rpc(Pid, What).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive Response -> Response end.

loop() ->
    receive
        {From, {rectangle, W, H}} -> From ! {self(), W * H}, loop();
        {From, {circle, R}} -> From ! {self(), 3.1415926 * R * R}, loop();
        {From, Other} -> From ! {self(), {error, Other}}, loop()
    end.
