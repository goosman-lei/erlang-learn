
main(_) ->
	code:add_patha("./lib"),
	code:add_patha("../lib"),

	demo_ColorBegin(95), io:format("~p:\n", [demo_ThrowException]), demo_ColorEndBegin(34), demo_ThrowException(),
	demo_ColorBegin(95), io:format("~p:\n", [demo_ErrorAndOk]), demo_ColorEndBegin(34), demo_ErrorAndOk(),
	demo_ColorBegin(95), io:format("~p:\n", [demo_CatchAll]), demo_ColorEndBegin(34), demo_CatchAll(),

	demo_ColorEnd().

demo_ThrowException() ->
    try
        misc:throw_exception(1)
    of
        X -> io:format("normal: ~p~n", [X])
    catch
        throw: X -> io:format("throw: ~p~n", [X]);
        exit:  X -> io:format("exit: ~p~n", [X]);
        error: X -> io:format("error: ~p~n", [X])
    after
        io:format("try ... catch ... after~n", [])
    end,
    io:format("list parse for exception: ~p~n", [[{I, (catch misc:throw_exception(I))} || I <- [0, 1, 2, 3, 4]]]),
	io:format("~n").

demo_ErrorAndOk() ->
    io:format("error_and_ok: ~p~n", [case misc:error_and_ok() of
        {error, Why}  -> Why;
        {ok, Val}  -> Val
    end]),

	io:format("~n").

demo_CatchAll() ->
    io:format("list: ~p~n", [
        [(
            try
                misc:throw_exception(I)
            of
                N -> {ok, N}
            catch
                ErrorType:Why -> {ErrorType, Why, erlang:get_stacktrace()}
            end
        ) || I <- [0, 1, 2, 3, 4]]
    ]),

	io:format("~n").


demo_ColorBegin(Color) ->
	io:format("\e[~wm", [Color]).
demo_ColorEnd() ->
	io:format("\e[0m").
demo_ColorEndBegin(Color) ->
	demo_ColorEnd(),
	demo_ColorBegin(Color).
