-module(customer).

-include("customer.hrl").

-define(DEBUG, true).

-ifdef(DEBUG).
-define(DEBUG_PRINTF(F, L), io:format(F, L)).
-else.
-define(DEBUG_PRINTF(F, L), ok).
-endif.

-export([print_customer/1, customer_with_name/2]).

customer_with_name(C, Name) when Name =/= undefined ->
    C#customer{name = Name}.

print_customer(#customer{name=_Name, address=_, phone=Phone}) when Phone =/= undefined ->
    ?DEBUG_PRINTF("Concat: ~s at ~s.~n", [_Name, Phone]);
print_customer(#customer{name=_Name, address=_, phone=Phone}) when Phone =:= undefined ->
    ?DEBUG_PRINTF("Concat: ~s.~n", [_Name]).
