-module(misc).

-export([ltob/1]).

ltob(List) -> list_to_binary(List).
