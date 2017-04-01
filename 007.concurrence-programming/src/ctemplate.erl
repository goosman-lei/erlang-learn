-module(ctemplate).
-compile(export_all).

start() ->
    P = spawn(fun() -> loop([]) end),
    P ! hi,
    init:stop().

rpc(P, Req) ->
    P ! {self(), Req},
    receive
        {P, Resp} -> Resp
    end.

loop(X) ->
    receive
        Any ->
            io:format("Received: ~p~n", [Any]),
            loop(X)
    end.
