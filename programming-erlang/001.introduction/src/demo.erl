#!/usr/bin/env escript
main(_) ->
	demo_ColorBegin(95), io:format("demo_HelloWorld~n"), demo_ColorEndBegin(34), demo_HelloWorld(),

	demo_ColorEndBegin(95), io:format("~ndemo_Variables~n"), demo_ColorEndBegin(34), demo_Variables(),

	demo_ColorEnd().

demo_HelloWorld() ->
	io:format("Hello World!~n").

demo_Variables() ->
	% 整型
	D_1 = 123456789,
	D_2 = 987654321,
	io:format("D_1: ~w, D_2: ~w, (D_1 * D_2): ~w~n", [D_1, D_2, D_1 * D_2]),
	io:format("~n"),

	% 浮点型
	D_3 = 5,
	D_4 = 3,
	io:format("D_3: ~w, D_4: ~w, (D_3 / D_4): ~w, (D_3 div D_4): ~w, (D_3 rem D_4): ~w~n", [D_3, D_4, D_3 / D_4, D_3 div D_4, D_3 rem D_4]),
	io:format("~n"),

	% 原子
	Atom_1 = monday, Atom_2 = tuesday, Atom_3 = wednesday, Atom_4 = thursday, Atom_5 = friday, Atom_6 = saturday, Atom_7 = 'sunday',
	Atom_Upper_1 = 'Monday', Atom_Upper_2 = 'Tuesday', Atom_Upper_3 = 'Wednesday', Atom_Upper_4 = 'Thursday', Atom_Upper_5 = 'Friday', Atom_Upper_6 = 'Saturday', Atom_Upper_7 = 'Sunday',
	io:format("Atoms: ~w ~w ~w ~w ~w ~w ~w, Atom_Uppers: ~w ~w ~w ~w ~w ~w ~w~n",
		[
			Atom_1, Atom_2, Atom_3, Atom_4, Atom_5, Atom_6, Atom_7,
			Atom_Upper_1, Atom_Upper_2, Atom_Upper_3, Atom_Upper_4, Atom_Upper_5, Atom_Upper_6, Atom_Upper_7
		]),
	io:format("~n"),

	% 元组
	Tuple_Point = {point, 10, 45},
	Tuple_Person = {person, {name, joe}, {height, 1.82}, {footsize, 42}, {eyecolor, brown}},
	{point, Tuple_Point_X, Tuple_Point_Y} = Tuple_Point,
	{_, {_, Tuple_Person_Name}, {_, _}, {_, _}, {_, _}} = Tuple_Person,
	io:format("Tuple_Point: ~w, Tuple_Person: ~w~n", [Tuple_Point, Tuple_Person]),
	io:format("Tuple_Point_X: ~w, Tuple_Point_Y: ~w, Tuple_Person_Name: ~w~n", [Tuple_Point_X, Tuple_Point_Y, Tuple_Person_Name]),
	io:format("~n"),

	% 列表
	List_ThingsToBuy = [{apples, 10}, {pears, 6}, {milk, 3}],
	List_Mixed = [1 + 7, hello, 2 - 2, {const, apple, 30 - 20}, 3],
	[List_ThingsToBuy_Head | _] = List_ThingsToBuy,
	[List_ThingsToBuy_First, List_ThingsToBuy_Second | _] = List_ThingsToBuy,
	io:format("List_ThingsToBuy: ~w~nList_Mixed: ~w~n", [List_ThingsToBuy, List_Mixed]),
	io:format("List_ThingsToBuy_Head: ~w~n", [List_ThingsToBuy_Head]),
	io:format("List_ThingsToBuy_First: ~w, List_ThingsToBuy_Second: ~w~n", [List_ThingsToBuy_First, List_ThingsToBuy_Second]),
	io:format("~n"),

	String_Name = "Hello",
	String_Seq = [65, 66, 67, 68, 69, 70, 71],
	String_Name_2 = [$H, $e, $l, $l, $o],
	io:format("String_Name: ~s~n", [String_Name]),
	io:format("String_Seq: ~s~n", [String_Seq]),
	io:format("String_Name_2: ~s~n", [String_Name_2]),
	io:format("~n"),

	nil.

demo_ColorBegin(Color) ->
	io:format("\e[~wm", [Color]).
demo_ColorEnd() ->
	io:format("\e[0m").
demo_ColorEndBegin(Color) ->
	demo_ColorEnd(),
	demo_ColorBegin(Color).
