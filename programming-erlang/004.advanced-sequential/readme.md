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

% 把模块中的每个函数都导出

-compile(export_all).

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

## 模式中使用匹配操作符

```erl
% 模式匹配到的元祖, 可以赋值给临时变量

func([{tag, A, B}|T]) ->
    ...,
    f(..., {tag, A, B}, ...),
    ...
    .

func([{tag, A, B} = Z|T]) ->
    ...,
    f(..., Z, ...),
    ...
    .

% 模式匹配到的元祖内部, 仍然可以嵌套的赋值

func([{tag, {one, A}, B}|T]) ->
    ...,
    f(..., {tag, A, B}, ...),
    ...
    .

func([{tag, {one, A} = Z1, B} = Z2|T]) ->
    ...,
    f(..., Z1, ...),
    f(..., Z2, ...),
    ...
    .
```

## 数值类型

### 整型

```erl
% 整型数值的3种表示语法

% 1) 传统语法
A = 12.
B = 12375.
C = -23427.

% 2) K进制表示法

A = 2#001000010.
B = 16#AB0137F.

% 3) $语法: 语法$C表示ASCII字符C的整数值

A = $a % A = 97
A = $1 % A = 49
```

### 浮点型

```erl
% 浮点型表示法

Floag := [Sign] Integer "." Decimal [Exponent]

Sign := "+" | "-"
Integer := [0-9]+
Decimal := [0-9]+
Exponent := ("E" | "e") [Sign] Integer
```

## 操作符优先级

|操作符|结合律|
|:||
|#||
|(unary)+, (unary)-, bnot, not||
|/, *, div, rem, band, and|左结合|
|+, -, bor, bxor, bsl, bsr, or, xor|左结合|
|++, --|右结合|
|==, /=, =<, <, >=, >, =:=, =/=||
|andalso||
|orelse||

## 进程字典

```erl
% 向进程字典增加数据

@spec put(Key, Value) -> OldValue.

% 获取进程字典中的数据

@spec get(Key) -> Value.

% 获取整个进程字典中所有数据

@spec get() -> [{Key, Value}].

% 获取进程字典中的所有值为Value的Key

@spec get_keys(Value) -> [Key].

% 删除进程字典中指定Key的数据

@spec erase(Key) -> Value.

% 清空进程字典

@spec erase() -> [{Key, Value}].
```

## 引用

引用是全局唯一的erlang值.

使用BIF erlang:make_ref()创建引用.

## 短路布尔表达式

```erl
Expr1 orelse Expr2

Expr1 andalso Expr2
```

## 比较表达式

```erl
X > Y       % 大于
X < Y       % 小于
X =< Y      % 小于等于
X >= Y      % 大于等于
X == Y      % 等于(0.0 == 0)
X /= Y      % 不等于
X =:= Y     % 全等于(全等意味着有相同的值, 不存在引用问题)
X =/= Y     % 不全等于(0.0 =/= 0)

% 模式匹配中, 对类型不同的不会予以通过

fun(12) -> 1.

% 使用下面代码调用则不会通过

fun(12.0).

% erlang中类型的大小顺序

number < atom < reference < fun < port < pid < tuple < list < binary
```

## 下划线变量

主要用途: 命名一个不准备使用的变量(抑制编译器警告), 但同时提供更高的可读性.
