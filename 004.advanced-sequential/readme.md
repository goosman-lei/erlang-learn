# BIF

BIF是erlang的组成部分, 是erlang虚拟机中实现的, 不过在erlang的发布中, 提供了对应的erlang实现版本供参考.

所有的BIF都包含在erlang模块中. [http://www.erlang.org/doc/man/erlang.html](BIF文档)

# 比特语法

```erl
<<5, 10, 20>>.
<<"Hello">>.
```

比特语法, 是一种模式匹配语法, 主要用于对二进制数据中的比特进行封包和解包.

```erl
% 比特语法表达式

Ei = Value |
     Value:Size |
     Value/TypeSpecifierList |
     Value:Size/TypeSpecifierList

TypeSpecifierList = [End] [ "-" Sign ] [ "-" Type ] [ "-" Unit ]
End = big | little | native % 字节序[大小端]
Sign = signed | unsigned % 符号位
Type = integer | float | binary % 类型. binary默认Unit为8, 其他默认Unit为1
Unit = 1 | 2 | ... | 255 % 占用空间为Size * Unit
```

# erlang小问题集锦

## 模块属性

### 预定义模块属性

```erl
% 模块声明, modname必须是一个原子.
% 这个属性必须是文件中第一个属性.
% modname的代码, 都应存储在一个名为modname.erl中, 否则代码自动加载就会出错.

-module(modname).

% import属性指定编译期从Mod模块中导入参数为Arity1且名为Name1的函数
% 使用import导入的函数, 使用时不需要指明模块名

-import(Mod, [Name1/Arity1, Name2/Arity2, ...]).

% export指定从当前模块中, 导出Name1/Arity1等函数, 只有被导出的函数才能在模块外被调用

-export(Mod, [Name1/Arity1, Name2/Arity2, ...]).

% compile属性, 用来给编译器选项列表添加Options.
% Options的选项列表, 包含在[compile模块手册](http://erlang.org/doc/man/compile.html#file-2)中.

-compile(Options).

% vsn用来指定模块的版本.
% Version值可以是任何形式的文字项

-vsn(Version).
```

### 用户定义属性

```erl
% 用户定义模块属性的语法规则
% SomeTag必须是一个原子, Value必须是一个文字项

-SmoeTag(Value).
```

**Demo**

```erl
% attrs.erl文件
-module(attrs).
-vsn(1234).
-author({joe, armstrong}).
-purpose("example of attributes").
-export([fac/1]).

fac(1) -> 1;
fac(N) -> N * fac(N-1).

% erl交互命令环境
> attrs.module_info().
[
    {exports, [
        {fac, 1},
        {module_info, 0},
        {module_info, 1}
    ]},
    {imports, []},
    {attributes, [
        {vsn, [1234]},
        {author, [{joe, armstrong}]},
        {purpose, "example of attributes"}
    ]},
    {compile, [
        {options, [
            {cwd, "/home/joe/2006/book/JAERLANG/Book/code"},
            {outdir, "/home/joe/2006/book/JAERLANG/Book/code"}
        ]},
        {version, "4.4.3"},
        {time, {2007, 2, 21, 19, 23, 48}},
        {source, "/home/joe/2006/book/JAERLANG/Book/code/attrs.erl"}
    ]}
]

> attrs.module_info(attributes).
[
    {vsn, [1234]},
    {author, [{joe, armstrong}]},
    {purpose, "example of attributes"}
]

> beam_lib:chunks("attrs.beam", [attributes]).
{ok, {attrs, [
    {vsn, [1234]},
    {author, [{joe, armstrong}]},
    {purpose, "example of attributes"}
]}}
```

## 块表达式

```erl
% 把一串表达式组织成一个类似子句的实体
% 块表达式的返回值是最后一个表达式的值

begin
    Expr1,
    ...,
    ExprN
end
```

## 布尔表达式

```erl
not XXX

A and B

A or B

A xor B
```

## 预处理器epp

在erlang模块被编译之前, 先被epp进行自动处理.

这个预处理器会扩展源文件中的宏, 插入任何必须的包含文件.

```erl
compile:file(M, ['p']).可以将预处理器的输出保存到文件.

另外, 命令行工具, 也可以使用
erlc -E src/misc.erl
来产生预处理后的文件
```

## 函数引用

```erl
% 引用本地函数

double(L) -> lists:map(fun square/1, L).

% 引用其他模块函数

double(L) -> lists:map(fun x1:square/1, L).
```

## 包含文件

```erl
-include(Filename).

-include_lib(Name).
```

## 列表操作符

```erl
> [1, 2, 3] ++ [4, 5, 6].
[1, 2, 3, 4, 5, 6]

> [a, b, c, 1, d, e, 1, x, y, 1] -- [1].
[a,b,c,d,e,1,x,y,1]

> [a, b, c, 1, d, e, 1, x, y, 1] -- [1, 1].
[a,b,c,d,e,x,y,1]

> [a, b, c, 1, d, e, 1, x, y, 1] -- [1, 1, 1].
[a,b,c,d,e,x,y]

> [a, b, c, 1, d, e, 1, x, y, 1] -- [1, 1, 1, 1].
[a,b,c,d,e,x,y]
```

## 宏

```erl
% 语法定义

-define(Constant, Replacement).
-define(Func(Var1, Var2, ..., VarN), Replacement).

% 几个预定义宏

?FILE   % 当前文件名
?MODULE % 当前模块名
?LINE   % 当前行号

% 宏的流程控制

-undef(Macro).

-ifdef(Macro).

-ifndef(Macro).

-else.

-endif.
```
