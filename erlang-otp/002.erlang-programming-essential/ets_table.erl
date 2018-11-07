-module(ets_table).

-ifdef(DEBUG).
-define(DEBUG_INFO(F, L), io:format(F, L)).
-else.
-define(DEBUG_INFO(F, L), ok).
-endif.

main(_) ->
    PPID = self(),
    PID_A = spawn(fun() ->
        PPID ! {public, ets:new(table_public, [public])},
        PPID ! {protected, ets:new(table_protected, [protected])},
        PPID ! {private, ets:new(table_private, [protected])},
        PPID ! {public_set, ets:new(table_public_set, [public, set])},
        PPID ! {public_orderedset, ets:new(table_public_orderset, [public, ordered_set])},
        PPID ! {public_bag, ets:new(table_public_orderset, [public, bag])},
        PPID ! {public_duplicate_bag, ets:new(table_public_orderset, [public, duplicate_bag])},
        PPID ! completed,
        receive done -> ok end % Hold ets's table util test completed
        end),
    PID_B = spawn(fun() ->
        Recv_util_completed = fun(F, Fn) ->
            receive
                {Type, Table} -> Fn(Type, Table), F(F, Fn);
                completed -> ok
            end
        end,
        Recv_util_completed(Recv_util_completed, fun(Type, Table) ->
            case Type of
                public ->
                    try
                        ?DEBUG_INFO("public table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("public| first: ~p~n", [ets:first(Table)]),
                        io:format("public| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("public| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("public| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of public: ~p:~p~n", [C, A])
                    end;
                protected ->
                    try
                        ?DEBUG_INFO("protected table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("protected| first: ~p~n", [ets:first(Table)]),
                        io:format("protected| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("protected| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("protected| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of protected: ~p:~p~n", [C, A])
                    end;
                private ->
                    try
                        ?DEBUG_INFO("private table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("private| first: ~p~n", [ets:first(Table)]),
                        io:format("private| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("private| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("private| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of private: ~p:~p~n", [C, A])
                    end;
                public_set ->
                    try
                        ?DEBUG_INFO("private table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("public_set| first: ~p~n", [ets:first(Table)]),
                        io:format("public_set| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("public_set| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("public_set| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of public_set: ~p:~p~n", [C, A])
                    end;
                public_orderedset ->
                    try
                        ?DEBUG_INFO("private table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("public_orderedset| first: ~p~n", [ets:first(Table)]),
                        io:format("public_orderedset| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("public_orderedset| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("public_orderedset| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of public_orderedset: ~p:~p~n", [C, A])
                    end;
                public_bag ->
                    try
                        ?DEBUG_INFO("private table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("public_bag| first: ~p~n", [ets:first(Table)]),
                        io:format("public_bag| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("public_bag| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("public_bag| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of public_bag: ~p:~p~n", [C, A])
                    end;
                public_duplicate_bag ->
                    try
                        ?DEBUG_INFO("private table info: ~p~n", [ets:info(Table)]),
                        ets:insert(Table, {3, "c"}),
                        ets:insert(Table, {1, a}),
                        ets:insert(Table, {4, 3.14}),
                        ets:insert(Table, {9, d}),
                        ets:insert(Table, {2, b}),
                        ets:insert(Table, {4, 3.15}),
                        ets:insert(Table, {4, 3.15}),
                        io:format("public_duplicate_bag| first: ~p~n", [ets:first(Table)]),
                        io:format("public_duplicate_bag| lookup 2: ~p~n", [ets:lookup(Table, 2)]),
                        io:format("public_duplicate_bag| lookup 3: ~p~n", [ets:lookup(Table, 3)]),
                        io:format("public_duplicate_bag| lookup 4: ~p~n", [ets:lookup(Table, 4)])
                    catch
                        C:A -> io:format("Exception Of public_duplicate_bag: ~p:~p~n", [C, A])
                    end
            end
        end),
        PPID ! done
        end),
    Wait_done = fun(F, P) ->
            receive
                done -> ok;
                M -> P ! M, F(F, P)
            end
    end,
    Wait_done(Wait_done, PID_B),
    PID_A ! done.