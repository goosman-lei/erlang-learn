
main(_) ->
	code:add_patha("./lib"),
	code:add_patha("../lib"),

    AreaServer = area_server:start(),
    io:format("area({rectangle, 3, 4}): ~p~n", [area_server:area(AreaServer, {rectangle, 3, 4})]),
    io:format("area({circle, 3}): ~p~n", [area_server:area(AreaServer, {circle, 3})]),
    io:format("area({other}): ~p~n", [area_server:area(AreaServer, {other})]),
	io:format("~n").
