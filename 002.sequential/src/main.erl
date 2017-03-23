
main(_) ->
	code:add_patha("./lib"),
	code:add_patha("../lib"),

	demo_ColorBegin(95), io:format("~p:\n", [demo_Area]), demo_ColorEndBegin(34), demo_Area(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Shop]), demo_ColorEndBegin(34), demo_Shop(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_AnonymousFunc]), demo_ColorEndBegin(34), demo_AnonymousFunc(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_For]), demo_ColorEndBegin(34), demo_For(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_ListResolve]), demo_ColorEndBegin(34), demo_ListResolve(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Qsort]), demo_ColorEndBegin(34), demo_Qsort(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Pythag]), demo_ColorEndBegin(34), demo_Pythag(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Perms]), demo_ColorEndBegin(34), demo_Perms(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Max]), demo_ColorEndBegin(34), demo_Max(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Record]), demo_ColorEndBegin(34), demo_Record(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_Case]), demo_ColorEndBegin(34), demo_Case(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_If]), demo_ColorEndBegin(34), demo_If(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_CreateSequence]), demo_ColorEndBegin(34), demo_CreateSequence(),
	demo_ColorEndBegin(95), io:format("~p:\n", [demo_DistributeOddEven]), demo_ColorEndBegin(34), demo_DistributeOddEven(),

	demo_ColorEnd().

demo_Area() ->
    Rectange_A = {rectangle, 10, 5},
    Rectange_A_Area = misc:area({rectangle, 10, 5}),
    io:format("Rectangle_A: ~w, Rectange_A_Area: ~w~n", [Rectange_A, Rectange_A_Area]),
	io:format("~n").

demo_Shop() ->
    Items_Of_WantToBuy = [{oranges, 4}, {newspaper, 1}, {apples, 10}, {pears, 6}, {milk, 3}],
    Cost_Of_WantToBuy = misc:total(Items_Of_WantToBuy),
    io:format("Items_Of_WantToBuy: ~w, Cost_Of_WantToBuy: ~w~n", [Items_Of_WantToBuy, Cost_Of_WantToBuy]),
	io:format("~n").

demo_AnonymousFunc() ->
    Fun_Hypot = fun(X, Y) -> math:sqrt(X * X + Y * Y) end,
    io:format("Fun_Hypot: ~w~n", [Fun_Hypot]),

    Fun_Double = fun(X) -> X * X end,
    io:format("Fun_Double: ~w~n", [Fun_Double]),

    L_1 = [1, 2, 3, 4],
    L_2 = lists:map(Fun_Double, L_1),
    io:format("L_1: ~w~n", [L_1]),
    io:format("L_2: ~w~n", [L_2]),

	Fruit = [apple, pear, orange],
	MakeTest = fun(L) -> (fun(X) -> lists:member(X, L) end) end,
	IsFruit = MakeTest(Fruit),
	io:format("Fruit: ~p~n", [Fruit]),
	io:format("IsFruit(apple): ~p, IsFruit(orange): ~p, IsFruit(banana): ~p~n", [IsFruit(apple), IsFruit(orange), IsFruit(banana)]),
	io:format("lists:filter(IsFruit, [dog, orange, cat, apple, bear]): ~p~n", [lists:filter(IsFruit, [dog, orange, cat, apple, bear])]),

	io:format("~n").

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I + 1, Max, F)].

demo_For() ->
	io:format("for(1, 10, fun(I) -> I end): ~p~n", [for(1, 10, fun(I) -> I end)]),
	io:format("for(1, 10, fun(I) -> I * I end): ~p~n", [for(1, 10, fun(I) -> I * I end)]),
	io:format("misc:sum(for(1, 10, fun(I) -> I end)): ~p~n", [misc:sum(for(1, 10, fun(I) -> I end))]),

	L = [1, 2, 3, 4, 5],
	io:format("misc:map(fun(X) -> 2 * X end, L): ~p~n", [misc:map(fun(X) -> 2 * X end, L)]),

	io:format("~n").

% [ F(X) || Qualifier1, Qualifier2, ... ]
% Qualifier := Generator | Filter
demo_ListResolve() ->
	L = [1, 2, 3, 4, 5],
	io:format("L: ~p~n", [L]),
	io:format("[2 * X || X <- L ]: ~p~n", [[2 * X || X <- L]]),
	Buy = [{oranges, 4}, {newspaper, 1}, {apples, 10}, {pears, 6}, {milk, 3}],
	io:format("Buy: ~p~n", [Buy]),
	io:format("[{Name, 2 * Number} || {Name, Number} <- Buy]: ~p~n", [[{Name, 2 * Number} || {Name, Number} <- Buy]]),
	io:format("[misc:cost(A) * B || {A, B} <- Buy]: ~p~n", [[misc:cost(A) * B || {A, B} <- Buy]]),
	io:format("lists:sum([misc:cost(A) * B || {A, B} <- Buy]): ~p~n", [lists:sum([misc:cost(A) * B || {A, B} <- Buy])]),
	io:format("[X || {a, X} <- [{a, 1}, {b, 2}, {c, 3}, {a, 4}, hello, \"wow\"]]: ~p~n", [[X || {a, X} <- [{a, 1}, {b, 2}, {c, 3}, {a, 4}, hello, "wow"]]]),

	io:format("~n").

demo_Qsort() ->
	L = [3, 73, 28, 36, 38, 16, 66, 22, 87, 35, 1, 2, 6],
	io:format("L: ~p~n", [L]),
	io:format("misc:qsort(L): ~p~n", [misc:qsort(L)]),

	io:format("~n").

demo_Pythag() ->
	io:format("misc:pythag(30): ~p~n", [misc:pythag(30)]),

	io:format("~n").

demo_Perms() ->
	io:format("misc:perms(\"1234\"): ~p~n", [misc:perms("1234")]),

	io:format("~n").

demo_Max() ->
	io:format("misc:max(1, 2): ~p~n", [misc:max(1, 2)]),
	io:format("misc:max(3, 4): ~p~n", [misc:max(3, 4)]),
	io:format("misc:max(5, 4): ~p~n", [misc:max(5, 4)]),
	io:format("misc:max(8, 4): ~p~n", [misc:max(8, 4)]),
	io:format("misc:max(8, 8): ~p~n", [misc:max(8, 8)]),

	io:format("~n").

-record(todo, {status = remind, who = joe, text}).
demo_Record() ->
	Record_1 = #todo{},
	Record_2 = #todo{who = 'Jack'},
	Record_3 = Record_2#todo{status = done},
	#todo{who = Who_From_Record_2} = Record_2,
	io:format("Record_1: ~p~nRecord_2: ~p~nRecord_3: ~p~n", [Record_1, Record_2, Record_3]),
	io:format("Who_From_Record_2: ~p~n", [Who_From_Record_2]),
	io:format("Record_3#todo.status: ~p~n", [Record_3#todo.status]),
	Record_4 = misc:clear_status(Record_3),
	io:format("Record_4#todo.status: ~p~n", [Record_4#todo.status]),
	
	io:format("~n").


demo_Case() ->
	L_1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	io:format("L_1: ~p~n", [L_1]),
	Filter_Even = fun(X) when X rem 2 == 0 -> true;
			         (_) -> false end,
	Filter_Odd = fun(X) when X rem 2 /= 0 -> true;
			         (_) -> false end,
	io:format("misc:filter(Filter_Even, L_1): ~p~n", [misc:filter(Filter_Even, L_1)]),
	io:format("misc:filter(Filter_Odd, L_1): ~p~n", [misc:filter(Filter_Odd, L_1)]),

	io:format("~n").

demo_If() ->
	io:format("misc:score(30): ~p~n", [misc:score(30)]),
	io:format("misc:score(65): ~p~n", [misc:score(65)]),
	io:format("misc:score(85): ~p~n", [misc:score(85)]),
	io:format("misc:score(95): ~p~n", [misc:score(95)]),

	io:format("~n").

demo_CreateSequence() ->
	L_1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	L_2 = [],

	io:format("L_1: ~p~n", [L_1]),
	io:format("misc:create_sequence(L_1): ~p~n", [misc:create_sequence(L_1, L_2)]),
	io:format("L_2: ~p~n", [L_2]),

	io:format("~n").

demo_DistributeOddEven() ->
	L_1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	io:format("L_1: ~p~n", [L_1]),
	io:format("misc:distribute_odd_even(L_1): ~p~n", [misc:distribute_odd_even(L_1)]),

	io:format("~n").

demo_ColorBegin(Color) ->
	io:format("\e[~wm", [Color]).
demo_ColorEnd() ->
	io:format("\e[0m").
demo_ColorEndBegin(Color) ->
	demo_ColorEnd(),
	demo_ColorBegin(Color).
