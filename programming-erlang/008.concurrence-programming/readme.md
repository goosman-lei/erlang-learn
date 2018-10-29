# 并发原语

```erl
% 创建一个新的并发进程, 用于对Fun求值. 新进程和当前进程并发运行.

Pid = spawn(Fun).

% 向进程发送消息. (发送过程是异步的, 发送者无需等待返回结果).
% ! 称为是发送操作符

Pid ! Message

% P!M的返回值是消息本身. 因此可以

Pid_1 ! Pid_2 ! Pid_3 ! Message

% 接收一个发送给当前进程的消息.
% 有消息到达时, 与Pattern匹配(也可能是Guard), 如果成功, 则对Expressions求值.

receive
    Pattern1 [when Guard1] ->
        Expressions1;
    Pattern2 [when Guard2] ->
        Expressions2;
    ...
after Time ->
    Expressions
end.

% 只有超时的receive, 可以用来模拟sleep

sleep(T) ->
    receive after T -> true end.
```

# receive的工作机制

1) 进入receive语句时, 启动一个计时器(表达式中有after子句时)

2) 从信箱中读取出第一条消息, 尝试匹配所有Pattern, 如果匹配成功, 从信息删除消息, 并对模式后的表达式求值

3) 如果信箱中的第一条消息不能匹配receive语句中任何一个模式, 则将其从信箱删除, 并放入一个"保存队列", 然后继续尝试信箱中第二个消息. 这个过程一直重复, 知道找到匹配的消息或检查完信箱所有消息.

4) 如果信箱中所有消息都不能匹配, 那么挂起进程, 直到下一次又有新消息进入信箱时, 再对该进程进行重新调度执行.

>> 注意: 当有新消息到达时, 只会匹配新消息, 而不会对保存队列中的消息进行匹配

5) 如果一个消息被匹配, 那么存入保存队列的所有消息, 就会按照它们到达进程的时间先后顺序重新被放回到信箱中. 此时, 如果之前有开启计时器, 则清空计时器.

6) 如果在等待一个消息时, 触发了计时器, 那么对表达式ExpressionsTimeout求值, 然后把存入保存队列中的所有消息按照它们到达进程的时间先后顺序重新放回到信箱中.

# 注册进程

```erl
% 将进程Pid注册一个名为AnAtom的原子, 如果AnAtom被其他进程注册, 则会注册失败

register(AnAtom, Pid).

% 移除与AnAtom对应的进程的所有注册信息

unregister(AnAtom).

% 查找AnAtom是否已被进程注册, 如果已注册则返回对应Pid

whereis(AnAtom) -> Pid | undefined.

% 返回当前进程中已经注册的所有的名称列表

registered() -> [AnAtom::atom()].

% 对于已经注册了名字的进程, 可以使用名字发送消息

> Pid = spawn(fun area_server:loop/0).
> register(area, Pid).
> area ! {rectangle, 4, 5}.
Area of rectangle is 20
{rectangle, 4, 5}
```

# 尾递归

```erl
loop() ->
    receive
        Pattern_1 ->
            ...
            loop(),
            % 非尾递归.
            % loop()实际上永远都不会执行结束.
            % 所以, 后面的代码没有意义, 只是会导致编译器认为后面代码还有用, 为其压栈
            % 编写此类代码, 给loop()调用之后, 不要有任何其他代码/
            % 编译器在处理尾递归函数时, 可以将其优化为一系列指令执行完毕后, 直接一个跳转指令, 指向被调用函数的开头. 这样避免消耗栈空间.
            ...
        Pattern_2 ->
            ...
            loop() % 尾递归
    end.
```

# MFA

```erl
% MFA是指spawn调用的另外一种形式.
% 这种形式, 显式的执行了模块, 函数名, 参数列表.
% 这种方式, 可以确保代码在编译后, 处于运行状态时, 仍然可以用新版本代码进行升级.
% 而直接使用spawn(Fun).的方式, 无法获得动态代码更新的特性.

spawn(Mode, Fun, Args).
```
