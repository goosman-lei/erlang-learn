% erlc src/main_argv_compile.erl && erl -noshell -s main_argv_compile main 25

-module(main_argv_compile).

-export([main/1]).

main([A]) ->
    I = list_to_integer(atom_to_list(A)),
    F = fac(I),
    io:format("factorial ~w = ~w~n", [I, F]),
    init:stop().

fac(0) -> 1;
fac(N) -> N * fac(N - 1).
