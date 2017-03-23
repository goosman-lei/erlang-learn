
main(_) ->
	code:add_patha("./lib"),
	code:add_patha("../lib"),

	demo_ColorBegin(95), io:format("~p:\n", [demo]), demo_ColorEndBegin(34), demo(),

	demo_ColorEnd().

demo() ->
    io:format("demo: ~p~n", ["demo"]),
	io:format("~n").

demo_ColorBegin(Color) ->
	io:format("\e[~wm", [Color]).
demo_ColorEnd() ->
	io:format("\e[0m").
demo_ColorEndBegin(Color) ->
	demo_ColorEnd(),
	demo_ColorBegin(Color).
