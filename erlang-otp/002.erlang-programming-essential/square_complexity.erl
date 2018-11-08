-module(square_complexity).

head_seq(F, E) when not is_number(F) or not is_number(E) -> [];
head_seq(F, E) when F =< E -> [ F | head_seq(F + 1, E) ];
head_seq(F, E) when F > E -> [].

tail_seq(F, E) when not is_number(F) or not is_number(E) -> [];
tail_seq(F, E) -> tail_seq_do(F, E, []).

tail_seq_do(F, E, Acc) when F =< E -> tail_seq_do(F, E - 1, [ E | Acc ]);
tail_seq_do(F, E, Acc) when F > E -> Acc.

head_rev([X | Rest]) -> head_rev(Rest) ++ [X];
head_rev([]) -> [].

tail_rev(X) when is_list(X) -> tail_rev_do(X, []).

tail_rev_do([X | Rest], Acc) -> tail_rev_do(Rest, [X | Acc]);
tail_rev_do([], Acc) -> Acc.

time_minus({B_MegaSec, B_Sec, B_MicroSec}, {E_MegaSec, E_Sec, E_MicroSec}) -> (E_MegaSec * 1000000000000 + E_Sec * 1000000 + E_MicroSec - B_MegaSec * 1000000000000 - B_Sec * 1000000 - B_MicroSec).

main(_) ->
    {F, E} = {1, 30000},
    Make_head_begin = now(),
    head_seq(F, E),
    Make_head_end = now(),
    io:format("make head use time: ~p~n", [time_minus(Make_head_begin, Make_head_end) / 1000]),
    Make_tail_begin = now(),
    Seq = tail_seq(F, E),
    Make_tail_end = now(),
    io:format("make tail use time: ~p~n", [time_minus(Make_tail_begin, Make_tail_end) / 1000]),
    Head_sort_begin = now(),
    head_rev(Seq),
    Head_sort_end = now(),
    io:format("sort head use time: ~p~n", [time_minus(Head_sort_begin, Head_sort_end) / 1000]),
    Tail_sort_begin = now(),
    tail_rev(Seq),
    Tail_sort_end = now(),
    io:format("sort tail use time: ~p~n", [time_minus(Tail_sort_begin, Tail_sort_end) / 1000]).