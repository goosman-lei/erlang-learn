-module(process).

for(M, N, S, Fn) when M < N ->
    Fn(M),
    for(M + S, N, S, Fn);
for(_, _, _, _) -> ok.

for(M, N, Fn) when M < N ->
    Fn(M),
    for(M + 1, N, Fn);
for(_, _, _) -> ok.

main(_) ->
    Pid_A = spawn_link(fun() ->
        Pid_A = self(),
        Pid_B = spawn_link(fun() ->
            for(1, 10, 2, fun(M) ->
                receive continue -> ok end,
                io:format("~p ~p~n", [self(), M]),
                timer:sleep(100),
                Pid_A ! continue
            end)
        end),
        for(0, 10, 2, fun(M) ->
            receive continue -> ok end,
            io:format("~p ~p~n", [self(), M]),
            timer:sleep(100),
            Pid_B ! continue
        end)
    end),
    Pid_A ! continue,
    process_flag(trap_exit, true),
    exit(Pid_A, kill),
    receive Message -> io:format("Receive msg: ~p~n", [Message]) end.