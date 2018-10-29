-module(exercise_2).

-export([start/1]).

% 生成一个环状消息分发的长度为N的进程组
% 向其中发送一条消息, 使消息被分发M圈

start([_N, _M]) ->
    N = list_to_integer(atom_to_list(_N)),
    M = list_to_integer(atom_to_list(_M)),
    setup_ring(N),
    head ! {self(), msg, M},
    init:stop().

setup_ring(N) ->
    register(head, spawn(fun() -> ring_loop(N) end)).

ring_loop(N) ->
    io:format("setup ring: ~p~n", [N]),
    case N of
        1 ->
            receive_and_pass(head);
        _ ->
            Next = spawn(fun() -> ring_loop(N - 1) end),
            receive_and_pass(Next)
    end.

receive_and_pass(Next) ->
    receive
        {From, Msg, 1} when Next == head -> 
            ok;
        {From, Msg, M} when Next == head ->
            io:format("[~p ===> ~p] Msg: ~p M: ~p~n", [From, self(), Msg, M]),
            Next ! {self(), Msg, M - 1},
            receive_and_pass(Next);
        {From, Msg, M} ->
            io:format("[~p ===> ~p] Msg: ~p M: ~p~n", [From, self(), Msg, M]),
            Next ! {self(), Msg, M},
            receive_and_pass(Next)
    end.
