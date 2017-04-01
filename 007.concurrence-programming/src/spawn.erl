-module(spawn).

-export([loop_area/0, start/0]).

loop_area() ->
    receive
        {rectangle, Width, Ht} ->
            io:format("[~p] Area of rectangle is ~p~n", [self(), Width * Ht]),
            loop_area();
        {circle, R} ->
            io:format("[~p] Area of circle is ~p~n", [self(), 3.1415926 * R * R]),
            loop_area();
        {quit} ->
            io:format("[~p] Received quit command~n", [self()]),
            init:stop();
        Other ->
            io:format("[~p] I don't know what the area of a ~p is ~n", [self(), Other]),
            loop_area()
    end.

loop_reply() ->
    receive
        {Pid, {msg, MsgBody}} ->
            io:format("[~p] Receive message from ~p: ~p~n", [self(), Pid, MsgBody]),
            Pid ! {self(), ok},
            loop_reply();
        {quit} ->
            io:format("[~p] Received quit command~n", [self()]),
            init:stop();
        _ ->
            io:format("[~p] wrong~n", [self()]),
            loop_reply()
    end.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive Response -> Response end.

start() ->
    Pid_Area = spawn(fun loop_area/0),
    Pid_Area!{rectangle, 4, 5},
    Pid_Area!{rectangle, 2, 8},
    Pid_Area!{circle, 7},
    Pid_Area!{line, 2, 9},
    Pid_Area!{quit},

    Pid_Reply = spawn(fun loop_reply/0),
    Pid_Reply ! {self(), {msg, "Hello, Guy!"}},
    Response = rpc(Pid_Reply, {msg, "This message is send by rpc function"}),
    io:format("[~p] rpc response: ~p~n", [self(), Response]),
    Pid_Reply ! {quit},


    init:stop().
