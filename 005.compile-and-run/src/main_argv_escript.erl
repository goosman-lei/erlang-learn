#!/usr/bin/env escript
% escript src/main_argv_escript.erl 25

main([A]) ->
    I = list_to_integer(A),
    F = fac(I),
    io:format("factorial ~w = ~w~n", [I, F]),
    init:stop().

fac(0) -> 1;
fac(N) -> N * fac(N - 1).
