-module(misc).

-export([error_and_ok/0, throw_exception/1]).

throw_exception(1) -> throw("Throw exception");
throw_exception(2) -> exit("Exit");
throw_exception(3) -> error("Error");
throw_exception(N) -> N.

error_and_ok() ->
    case rand:uniform(2) of
        1 -> {error, exception};
        2 -> {ok, hello}
    end.
