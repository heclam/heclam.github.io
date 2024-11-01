---
layout: article
title: erlang基础
date: 2024-03-11 14:30:41
toc: true
categories:
	- erlang
tags:
	- erlang
---

#### 1、模块的命名

`-moudle(Name)`

Name为模块名称也是这个文件的名称,模块名必须与文件名一致



#### 2、数据类型

1. 元组（`tuple`）
   一个元组可以容纳不同类型的值，一对花括号括起来，以若干个逗号分割的值 就形成了一个元组，如`{test,18}`，\_（下划线）作为占位符，表示那些我们不关心的变量，符号（\_）称为匿名变量

   <!--more-->

2. 列表（`list`）
   将若干个以逗号分割的值用一对中括号括起来，就形成了一个列表，比如`[{name,test},{age,19}]`

3. 浮点数(`float`)
   5/3       1.66667(正常取值)（注意： “/”永远返回浮点型 比如 4/2 结果为2.0）
   5 div 3   1（取整除数）
   5 rem 3   2（取余数）

4. 原子（`atom` 表示不同的非数字常量值）
   是一串以小写字母开头，后跟数字、字母或下划线（_）或邮件符号（@）的字符
   使用单引号引起来的字符也是原子，使用这种形式，原子就可以用大写字母作为开头或者包含非数字字符，如 `'Monday'、'+'、'*'`

5. 字符串(`str`)
   严格地讲，Erlang并没有字符串，字符串实际上就是一个整数列表，用双引号（“）将一串字符串括起来就是一个字符串。
   可以使用$符号来表示字符的整数值，例如，$a实际上就是一个整数，表示字符a
   	
   注：**命令f()会让shell释放它所绑定过的所有变量，执行这个命令后，所有的变量都变成自由变量**

6. 记录（`record`）
   声明一个记录：
   `-record(Name,{key1 = Default1,key2 = Default2,key3})`
   使用一个记录
   `#Name{key1 = 1,Key3 = 0}`
   注意：记录是元组的另一种形式

7. 映射组（`map`）：关联式键-值存储
   创建一个映射
   `M = #{a => 1, b => 2}`
   更新一个映射
   `M2 = M#{a ：= 3, b :=4}`
   表达式 `K => V`有两种用途，一种是将现有的键K的值更新为新值V,另一种是给映射组添加一个全新的K-V对，这个操作总是成功的

   表达式`K := V`的作用是将现有键K的值更新为V。如果被更新的映射组不包含键K,这个操作就会失败                 



#### 3、比较运算符

| 运算符 | 描述       |
| ------ | ---------- |
| ==     | 等于       |
| /=     | 不等于     |
| =:=    | 精确等于   |
| =/=    | 精确不等于 |
| =<     | 小于或等于 |
| <      | 小于       |
| \>=    | 大于或等于 |
| >      | 大于       |

**注：** 

等级排序： `number < atom(基元) < reference < fun < port < pid < tuple < list < binary`

从上可知： **任何的数字都比任何的基元小，而任何的元组也都小于任何的列表**



#### 4、模式匹配

Erlang的模式匹配可以用于：

* 变量赋值
* 控制程序的执行流程
* 从复合数据类型中提取值



#### 5、函数和模块

1. 函数

   函数的**名称是一个基元**，一个函数的头包括名字，随后是一对括号，在里面包含多个形式的参数或者没有参数。在Erlang中，函数参数的数量叫做**元数**。使用箭头 `->` 来分割函数头和函数体。

2. 模块

   模块文件通常存放在以 `.erl` 为扩展名的文件中，要运行一个模块，首先需要编译它，编译成功后的模块文件其扩展名为`.beam`

3. 模块指令

   每个模块都拥有一个 -attribute(Value)格式的属性列表。它们通常放在模块的开始 , 如

   ```erlang
   % Name 为模块名称
   -module(Name)  
   
   % Function为函数的名称，Arity为元数即参数的个数
   -export([Function/Arity])  
   
   % 这个指令会在编译阶段导出模块中的所有函数，
   % 另一种方法是在编译这个文件的时候加上特定的选项  c(Mod，[export_all]).
   -compile(export_all)     
   
   % 允许你从其他的模块中导入函数然后在本地调用
   -import(Module,[Function/Arity])  				
   ```



#### 6、Erlang顺序编程

1. case结构

   case 表达式有如下的形式

   ```erlang
   case conditional-expression of
   	Pattern1 -> expression1,expression2,...;
   	Pattern2 -> expression1,expression2,...;
   	...
   	Pattern ->  expression1,expression2,...
   end
   ```

   使用的**关键字是case、of和end** ,对`conditional-expression`求值，然后和`Pattern1,...,Pattern`进行匹配（模式匹配），直到一个模式匹配成功

   下面给个例子：

   ```erlang
   case lists:member(foo,List) of
   	true -> ok;
   	false -> {error,unknown_element}
   	end
   
   %% lists:member函数检查基元foo是否是列表List的一个成员元素，如果是返回ok,不是则返回{error,unknown_element}
   ```

2. if结构

   if结构就像一个没有`conditional-express`和`of`关键字的`case`语句

   ```erlang
   if
   	Guard1 -> expression1,expression2,..;
   	Guard2 -> expression3,expression4,..;
   	...;
   	GuardN -> expressionN,expressN2,..
   end
   ```

   

   保护元表达式`Guard1,...,GuardN`按次序进行计算，直到其中的一个计算结果为true.

   如果为true则执行对应下的语句。

3. 保护元

   保护元是一个额外的限制条件，它应用于函数的`case`或者`receive`语句中，保护元应该放在`->`之前来分隔语句的主体和头部

   保护元由`when`关键字和紧跟其后的一个保护元表达式组成，只有在模式匹配和保护元表达式求值结果为基元true的情况下，语句才会执行

   下面给个例子：

   ```erlang
   %% 阶乘
   factorial(N) when N >0 ->  N * factorial(N-1);
   factorial(0) ->1.
   ```

4. 内置函数

   Erlang中有很多内置函数来处理内置类型，例如列表和元组

   ```erlang
   List = [one,two,three,four,five].
   
   Tuple = {1,2,3,4,5}.
   ```

   | 函数名称                | 函数描述                                                     |
   | ----------------------- | ------------------------------------------------------------ |
   | hd/1                    | 返回列表的第一个元素 ，例如：hd(List)                        |
   | tl/1                    | 返回删除第一个元素后的其余部分                               |
   | length/1                | 返回一个列表的长度                                           |
   | tuple_size/1            | 返回元组元素的数目 ,例如：tuple_size(Tuple)                  |
   | element/2               | 返回元组的第n个元素,例子：element(2,Tuple) 返回2             |
   | setelement/3            | 替换元组中的第一个元素，并返回新的元组，例子：setelement(3,Tuple,three) 返回{1，2，three，4，5} |
   | erlang:append_element/2 | 向元组添加一个元素作为最后的元素,并返回新的元组，例子：erlang:append_element(Tuple,6) 返回{1，2，3，4，5，6} |

5. 类型转换

   类型转换必须是内置函数

   | 函数名称                                                | 函数描述                                                     |
   | ------------------------------------------------------- | ------------------------------------------------------------ |
   | atom_to_list/1, list_to_atom/1, list_to_existing_atom/1 | 它们实现基元和字符串的相互转换，如果基元在运行时系统中的当前会话里没有使用过，那么调用函数list_to_existing_atom/1将会失败 |
   | list_to_tuple/1, tuple_to_list/1                        | 这两个函数实现元组类型和列表类型的相互转换                   |
   | float/1,                      list_to_float/1           | 这两个函数都产生一个float类型，一个是把一个整数参数转换成一个浮点数，另外一个是把一个字符串转换成一个浮点数 |
   | float_to_list/1, integer_to_list/1                      | 这两个函数都返回字符串                                       |
   | round/1, trunc/1, list_to_integer/1                     | 它们都返回整数 ，例子： round(10.5).返回11，trunc(10.5).返回10 |

6. 采用debugger:start()命令启动调试器

   要想追踪一个模块，需要使用`debug_info`标志来编译它

   在Erlang终端中，使用以下两个命令中的一个

   ```erlang
   c(Module,[debug_info]).
   
   compile:file(exception,[debug_info]).
   ```

7. guard(断言)

   当元组的元素数量变得非常庞大时，我们会很难记住元组中每个元素的确切含义，记录（record）就提供了一种方法把一个名称与元组中的一个元素对应起来，从而解决了这个烦恼

   注意：**record不是一个shell命令,记录的声明只能用于Erlang源代码而不能用于shell**

   ```erlang
   record语法：
   -record(Name,{
   		key1 = Default1,
   		key2 = Default2,
   		...
   }).
   Name是记录的名字，key1,key2...是记录中的字段名，这些名字必须是原子
   
   在shell中读取记录的定义，可以用rr(read record的缩写)这个命令
   例如：
   rr("records.hrl")
   
   在shell中，rd的使用方法，直接在里面定义一个记录，并读取
   rd（person,{name = "abc", age = 12}）
   ```

   ```erlang
   -import (lists,[map/2,sum/1])意味着函数map/2是从lists模块中导入的，然后在程序中可以直接使用map(Fun,...)而不必去写list:map(Fun,...)
   -export([total/1])意味着函数total/1能够在模块本模块之外调用，只有从一个模块中导出的函数才能在模块之外调用
   ```

   

8. 异常

   

9. 抛出异常

   通过调用下面几个内建的异常产生函数来显示地产生一个错误

   `exit(Why)`

   当想要终止当前进程时，就需要用到这个函数。如果是这个异常未被捕获，那么系统会向所有与当前进程相连接的进程广播`{'EXIT',Pid,Why}`消息

   `throw(Why)`

   这个函数用于抛出一个调用者可能会捕获的异常

   `erlang:error(Why)`

   这个函数用于抛出那些“崩溃错误”，这些异常应该是那些调用者不会真正意识到要去处理的致命错误，可以将它等同于内部产生的系统错误

   

10. 捕获异常（`try...catch`）

   Erlang捕获异常的语法结构

   ```erlang
   try FuncOrExpressionSequence of
   	Pattern1 [when Guard1] -> Expression1;
   	Pattern2 [when Guard2] -> Expression2;
   	...
   catch
   	ExceptionType: ExPattern1 [when ExGuard1] -> ExExpression1;
   	ExceptionType: ExPattern2 [when ExGuard2] -> ExExpression2;
   	...
   after
   	AfterExpressions
   end
   ```

   `try...catch`就像一个`case`表达式的增强版本。基本上它就是一个在尾部带有`catch`和`after`块的`case`表达式

   `try..catch`的执行流程：先对`FuncOrExpressionSequence`求值，如果不产生异常，那么函数的返回值就对`Pattern1`(连同可选的断言`Guard1`)、`Pattern2`等进行模式匹配

   如果在`FuncOrExpressionSequence`中有异常抛出，那么就会逐个匹配`catch`下的`ExPattern1`等模式，

   `ExceptionType`是原子`throw`、`exit`或`error`中的一个，告诉我们异常是以什么方式产生的。若未标明`ExceptionType`,那么默认的值就是`throw`。
