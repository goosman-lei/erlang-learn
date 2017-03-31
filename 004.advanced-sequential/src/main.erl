
main(_) ->
	code:add_patha("./lib"),
	code:add_patha("../lib"),

	demo_ColorBegin(95), io:format("~p:\n", [demo_Binary]), demo_ColorEndBegin(34), demo_Binary(),
	demo_ColorBegin(95), io:format("~p:\n", [demo_ListToBinary]), demo_ColorEndBegin(34), demo_ListToBinary(),
	demo_ColorBegin(95), io:format("~p:\n", [demo_PackAndUnpack]), demo_ColorEndBegin(34), demo_PackAndUnpack(),
	demo_ColorBegin(95), io:format("~p:\n", [demo_MpegUnpack]), demo_ColorEndBegin(34), demo_MpegUnpack(),
	demo_ColorBegin(95), io:format("~p:\n", [demo_MysqlTcpdumpUnpack]), demo_ColorEndBegin(34), demo_MysqlTcpdumpUnpack(),

	demo_ColorBegin(95), io:format("~p:\n", [demo_Apply]), demo_ColorEndBegin(34), demo_Apply(),

	demo_ColorEnd().

demo_Binary() ->
    io:format("<<5, 10, 20>>: ~p~n", [<<5, 10, 20>>]),
    io:format("<<\"Hello\">>: ~p~n", [<<"Hello">>]),
    io:format("<<\"a\">> == <<97>>: ~p~n", [<<"a">> == <<97>>]),
    io:format("<<\"a\">> == <<96>>: ~p~n", [<<"a">> == <<96>>]),

	io:format("~n").

demo_ListToBinary() ->
    L_1 = [1, 2, 3],
    L_2 = [4, 5],
    L_3 = [6],
    io:format("L_1: ~p~nL_2: ~p~nL_3: ~p~n", [L_1, L_2, L_3]),
    io:format("[L_1, 1, [2, 3, L_2], 4|L_3]: ~p~n", [[L_1, 1, [2, 3, L_2], 4|L_3]]),
    io:format("ltob([L_1, 1, [2, 3, L_2], 4|L_3]): ~p~n", [misc:ltob([L_1, 1, [2, 3, L_2], 4|L_3])]),

    B_1 = misc:ltob([L_1, 1, [2, 3, L_2], 4|L_3]),
    io:format("B_1: ~p~n", [B_1]),
    io:format("split_binary(B_1, 3): ~p~n", [split_binary(B_1, 3)]),

    T_1 = abcdefg,
    io:format("T_1: ~p~n", [T_1]),
    io:format("term_to_binary(T_1): ~p~n", [term_to_binary(T_1)]),
    B_2 = term_to_binary(T_1),
    io:format("B_2: ~p~n", [B_2]),
    io:format("binary_to_term(B_2): ~p~n", [binary_to_term(B_2)]),
    io:format("size(B_2): ~p~n", [size(B_2)]),

	io:format("~n").

demo_PackAndUnpack() ->
    Red = 2, Green = 61, Blue = 20,
    Mem = <<Red: 5, Green: 6, Blue: 5>>,
    <<R1: 5, G1: 6, B1: 5>> = Mem,
    io:format("Red: ~p, Green: ~p, Blue: ~p~n", [Red, Green, Blue]),
    io:format("Mem: ~p~n", [Mem]),
    io:format("R1: ~p, G1: ~p, B1: ~p~n", [R1, G1, B1]),

	io:format("~n").

demo_MpegUnpack() ->
    MpegPack = <<
        1:8, 2:8,
        2#11111111111:11, 2:2, 3:2, 1:1, 10:4, 3:2, 1:1, 0:1, 2:2, 3:2, 1:1, 1:1, 2:2,
        2#11111111111:11, 1:2, 3:2, 1:1, 10:4, 3:2, 1:1, 0:1, 2:2, 3:2, 1:1, 1:1, 2:2,
        2#11111111111:11, 2:2, 3:2, 1:1, 10:4, 3:2, 1:1, 0:1, 2:2, 3:2, 1:1, 1:1, 2:2
        >>,
    io:format("MpegPack: "), [io:format(" ~.2B", [X]) || X <- binary_to_list(MpegPack)], io:format("~n"),
    io:format("mpeg:find_sync(MpegPack, 0): ~p~n", [mpeg:find_sync(MpegPack, 0)]),

	io:format("~n").

demo_MysqlTcpdumpUnpack() ->
    {ok, Content} = file:read_file("mysql.pcap"),

    pcap:unpack(Content),

    io:format("~n").

demo_Apply() ->
    apply(io, format, ["Hello World From apply call", []]),

    io:format("~n").

demo_ColorBegin(Color) ->
	io:format("\e[~wm", [Color]).
demo_ColorEnd() ->
	io:format("\e[0m").
demo_ColorEndBegin(Color) ->
	demo_ColorEnd(),
	demo_ColorBegin(Color).
