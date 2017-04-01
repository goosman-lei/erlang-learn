# 开启和停止Erlang

在erlang shell中, 退出有几种方式

```erl
% 使用快捷键 Ctrl + C

> ^C (Ctrl + C)
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution

% BIF halt可以即刻停止系统运行.
% 这种方法会使得系统可能在某种不预期状态下被停止

erlang:halt().

% q().
% 这个操作会刷新所有开启的文件, 如果数据库开启, 则会停止数据库, 按顺序关闭所有OTP应用程序.

% q()是init:stop()在shell中的别名.
```

# 开发环境相关

## 加载路径

```erl
% 获取当前加载路径

code:get_path().

% 向前或向后追加加载路径

@spec code:add_patha(Dir) -> true | {error, bad_directory}.

@spec code:add_pathz(Dir) -> true | {error, bad_directory}.

% 命令行中提供选项方式增加

> erl -pa Dir1 -pa Dir2 ... -pz DirK1 -pz DirK2
```

## 初始化命令

```erl
% $HOME/.erlang文件中的代码, 会被自动执行

% 当前目录下的.erlang文件, 会比$HOME下的优先级更高

% init:get_argument(home). 可以用来获取当前的$HOME路径.
```

## 运行程序的不同方式

```bash
% 准备好用来Demo的示例代码
echo '-module(hello).
-export([start/0]).
start() ->
    io:format("Hello World!~n").' > hello.erl

% 1) 命令行方式直接以交互命令方式运行

$ erl
> c(hello).
{ok,hello}
> hello:start().
Hello World!
ok

% 2) 编译运行

% 2.1) 编译源文件

$ erlc hello.erl

% 2.2) 非交互命令方式运行

$ erl -noshell -s hello start -s init stop

% 3) eval方式直接运行代码

$ erl -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]).' \
    -noshell -s init stop

% 4) 把程序作为escript运行

$ echo '#!/usr/bin/env escript
main(_) ->
    io:format("Hello World!~n").' > hello
$ chmod +x hello
$ ./hello
```

# 调试相关

## 调试环境

```erl
> help().
** shell internal commands **
b()         -- display all variable bindings
e(N)        -- repeat the expression in query <N>
f()         -- forget all variable bindings
f(X)        -- forget the binding of variable X
h()         -- history
...
```

## 崩溃转储

```erl
> webtool:start().
WebTool is avariable at http://localhost:8888/
Or http://127.0.0.1:8888/
{ok, <0.34.0>}
```