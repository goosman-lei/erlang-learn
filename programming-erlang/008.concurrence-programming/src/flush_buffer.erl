-module(flush_buffer).

-export([start/0]).

start() ->
    Pid_Self = self(),
    Pid_Self ! {msg_1},
    Pid_Self ! {msg_2},
    Pid_Self ! {msg_3},
    Pid_Self ! {msg_4},
    Pid_Self ! {msg_5},
    receive_and_dump(),
    receive_and_dump(),
    flush_buffer(),
    receive_and_dump(),

    Pid_Self ! {jack},
    Pid_Self ! {tom},
    Pid_Self ! {alarm, what},
    Pid_Self ! {jobs},
    Pid_Self ! {alarm, why},
    priority_receive(),
    priority_receive(),
    priority_receive(),
    priority_receive(),
    priority_receive(),

    init:stop(),
    void.

flush_buffer() ->
    io:format("\tTry flush buffer~n"),
    % 当有消息时, 接收并丢弃消息
    receive
        _Any -> flush_buffer()
    % 当没有消息时, 则直接返回
    after 0 ->
        true
    end.

receive_and_dump() ->
    io:format("\tTry receive~n"),
    receive
        Msg -> io:format("~p~n", [Msg])
    after 1 ->
        io:format("No message~n")
    end,
    io:format("\tDumped~n")
    .

priority_receive() ->
    % 优先处理alarm消息
    receive
        {alarm, X} -> io:format("Receive Alarm Msg: ~p~n", [X])
    % 没有alarm消息再处理其他消息
    % NOTICE: 如果信箱中有大量pending消息, 这种方式非常低效
    after 0 ->
        receive
            Any -> io:format("Receive Other Msg: ~p~n", [Any])
        end
    end.
