-module(exercise_1).

-compile(export_all).

start() ->
    F1 = fun() ->
        receive
            hi -> io:format("[~p] Hi~n", [self()])
        end
    end,
    F2 = fun() ->
        receive
            hi -> io:format("[~p] Hello~n", [self()])
        end
    end,
    spawn(fun() ->
        case register_and_start(say_hi, F1) of
            ok ->
                io:format("[~p] register success[F1 Message: Hi]~n", [self()]),
                say_hi ! hi;
            {error, Trace} ->
                io:format("[~p] register failed: ~p~n", [self(), Trace])
        end
    end),
    spawn(fun() ->
        case register_and_start(say_hi, F2) of
            ok ->
                io:format("[~p] register success[F2 Message: Hello]~n", [self()]),
                say_hi ! hi;
            {error, Trace} ->
                io:format("[~p] register failed: ~p~n", [self(), Trace])
        end
    end),
    init:stop().

register_and_start(N, F) ->
    try
        register(N, spawn(fun() -> F() end)),
        ok
    catch
        error:_ ->
            {error, erlang:get_stacktrace()}
    end.
