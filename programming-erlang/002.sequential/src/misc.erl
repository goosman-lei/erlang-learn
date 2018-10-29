-module(misc).

-export([distribute_odd_even/1, create_sequence/2, score/1, filter/2, clear_status/1, max/2, perms/1, pythag/1, qsort/1, sum/1, map/2, cost/1, total/1, area/1]).

distribute_odd_even(L) ->
    distribute_odd_even(L, [], []).

distribute_odd_even([H|T], Odds, Evens) ->
    case (H rem 2) of
        0 -> distribute_odd_even(T, [H|Odds], Evens);
        1 -> distribute_odd_even(T, Odds, [H|Evens])
    end;
distribute_odd_even([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)}.

create_sequence([H|T], Result) ->
    H1 = H * H,
    create_sequence(T, [H1|Result]);
create_sequence([], Result) ->
    lists:reverse(Result).

score(N) ->
    if
        N < 60 ->
            "D";
        N < 80 ->
            "C";
        N < 90 ->
            "B";
        true ->
            "A"
    end.

filter(P, [H | T]) ->
    case P(H) of
        true -> [H | filter(P, T)];
        false -> filter(P, T)
    end;
filter(_, []) -> [].

-record(todo, {status = remind, who = joe, text}).

clear_status(R) when is_record(R, todo) ->
    R#todo{status = finished}.

max(A, B) when A > B -> A;
max(_, B) -> B.

% 以集合的每个元素为头, 连接集合的剩余元素的变位词集合.
perms([]) -> [[]];
perms(L)  -> [[H | T] || H <- L, T <- perms(L -- [H])].

pythag(N) ->
    [{A, B, C} ||
        A <- lists:seq(1, N),
        B <- lists:seq(1, N),
        C <- lists:seq(1, N),
        A + B + C =< N,
        A * A + B * B =:= C * C
    ].

total(L) -> sum(map(fun({What, N}) -> cost(What) * N end, L)).

qsort([]) -> [];
qsort([Pivot | T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).

sum([H|T]) -> H + sum(T);
sum([])    -> 0.

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)];
map(F, L) -> [F(X) || X <- L].

cost(oranges)   -> 5;
cost(newspaper) -> 8;
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.

area({rectangle, Width, Height}) -> Width * Height;
area({circle, R})                -> 3.14159 * R * R;
area({square, X})                -> X * X.
