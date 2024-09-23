---
layout: article
title: erlang并发
date: 2024-03-12 14:30:41
toc: true
categories:
	- erlang
tags:
	- erlang
---

#### 1、进程的生命周期（从创建到销毁的过程）

`Pid = spawn(Fun)`	创建一个新的并发进程（Pid  (`process identifier `进程标识符)）

进程间通过发送信息进行通信，所以必须双方必须知道对方的Pid才可以发起通信



如何注册一个进程？

Erlang中有4个BIF用于管理注册进程

<!--more-->

| 操作                                | 说明                                                         |
| ----------------------------------- | ------------------------------------------------------------ |
| register(AnAtom,Pid)                | 将一个进程Pid注册一个名为AnAtom的原子，如果原子AnAtom已经被另外一个注册进程所使用，那么注册就会失败。 |
| unregister(Antom)                   | 移除与AnAtom相对应进程的所有注册信息（如果一个注册进程死亡，那么它也会被自动取消注册） |
| whereis(AnAtom) -> Pid \| undefined | 判断AnAtom是否已经被其他进程注册，如果成功，返回进程标识符Pid,没有成功则返回原子undefined |
| registered() -> [AnAtom :: atom()]  | 返回一个系统中所有已经注册的名称列表                         |



两个进程之间如何相关联?

两个进程A和B，它们之间建立链接，是其中一个进程调用了BIF(Built In Function 内置函数) link(Pid)。

一旦两个进程之间建立链接，它们就会相互监视，若A消亡，系统就会向B发送一个叫做退出的信号，反之也一样。

**process_flag(trap_exit,true)可以把创建的进程变为一个系统进程**



```
-spec 表示这是对一个函数进行规范，人话就是：说明函数的参数或者返回值的类型
@spec spawn_link(Fun) -> Pid
这个原语非常像spawn(Fun),但它除了创建进程之外还会在这个两个进程之间建立链接。
spawn_link是一个原子操作，它和spawn+link操作并不等价，因为在执行spawn和link的间隙之间，所创建的进程有可能就已经消亡了

@spec process_flag(trap_exit,true)
这个原语把当前进程转换为系统进程，系统进程可以接收和处理退出信号
```

| 操作                                       | 说明                                                     |
| ------------------------------------------ | -------------------------------------------------------- |
| link(Pid) -> true                          | 将当前进程与Pid进程之间链接起来                          |
| unlink(Pid) -> true                        | 取消链接                                                 |
| exit(why) -> none()                        | 终止当前进程                                             |
| exit(Pid,why) -> true                      | 向Pid所指示的进程发出一个退出信号，并把退出原因设置为why |
| erlang:monitor(process,Item) -> MonitorRef | 建立一个监视器，Item为一个PId或者一个进程的注册名        |



#### 2、并发编程

3个原语：**spawn、send(使用！操作符)和receive**



1. 创建进程

   通过使用内置函数`spawn(Module, Function, Arguments)`可以生成一个新的进程，并对`Module`模块中导出函数`Function`以列表`Arguments`作为参数进行求值，内置函数`spawn/3`会返回一个进程标识符（`process identifier）pid`

**可以使用终端命令i()来查看当前运行时系统正在执行的进程**

**终端崩溃会自动产生一个新终端进程**



2. 接收一个发给当前进程的消息，它的语法如下

`receive... end`

```erlang
receive
	Pattern1 [when Guard1] -> Expression1;
	Pattern2 [when Guard2] -> Expression2;
	...
end
```

带超时的`receive`

```erlang
receive
	Pattern1 [when Guard1] -> Expression1;
	Pattern2 [when Guard2] -> Expression2;
	...
after Time ->
    Expressions
end.

%% 如果在进入receive表达式后在Time所规定的毫秒数内没有接收到能够匹配的消息，那么进程就会停止等待，并对Expression进行求值。
```

Erlang的每个进程都有与之对应的邮箱。当向进程发送消息时，消息就被送入邮箱之中。当系统对receive语句进行求值时，就是对进程邮箱进行检查的唯一机会。

**receive的内部工作机制**

![receive内部工作机制](/images/erlang_receive.png)



3. 注册进程

   如果想要向一个进程发送消息，那么就需要知道它的`Pid`。Erlang有一种机制可以用于发布一个进程的标识符以便其他进程可以与之通信，这种进程就叫注册进程。

   ```erlang
   %% 将一个进程Pid注册一个名为AnAtom的原子，如果原子AnAtom已经被另一个注册进程所使用，那么注册就会失败
   register(AnAtom, Pid)
   
   %% 移除与AnAtom相对应进程的所有注册信息
   unregister(AnAtom)
   ```

   注：如果一个注册进程死亡，那么它也会被自动取消注册



#### 3、并发编程中的错误处理

​	链接（`link`）、退出信号（`exit signal`）、系统进程

1. 链接进程

   如果一个进程在某种程度上依赖于另一个进程，那么它就需要时刻紧盯着第二个进程的运行状态。

   > 现有A、B两个进程，如果其中的一个进程调用了BIF link(P)(P的值是另一个进程的PID),那么这两个进程就建立了链接。一旦两个进程建立链接，它们就会自动互相监视。此时，若A消亡，系统就会向B发送一个叫做退出信号的东西，反之，若B消亡，则A也会接受到这个信号

​	如果一个进程接受到退出信号，若没有对这个接受进行特殊处理，那么这个退出信号的默认处理就是让该进程也一并退出。但也可以让这个**进程捕获退出信号**，当进程进入这个捕获状态时，那么我们称其为系统进程



#### 4、gen_server

gen_server行为模式的接口包含六个函数：`init/1、handle_call/3、handle_cast/2、handle_info/2、terminate/2、code_change/3`

gen_server行为模式最精简的实现模块：

```erlang
-module(...).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-record(state,{}).

init([]) ->
	{ok, #state{}}.
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.
handle_cast(_Msg, State) ->
	{noreply, State}
handle_info(_Info, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
```



**gen_server实现API的库函数**

| 库函数                  | 对应的回调函数       | 描述                                                         |
| ----------------------- | -------------------- | ------------------------------------------------------------ |
| gen_server:start_link/4 | Module:init/1        | 启动并链接一个gen_server容器进程                             |
| gen_server:call/2       | Module:handle_call/3 | 向gen_server进程发送同步消息并等待应答                       |
| gen_server:cast/2       | Module:handle_cast/2 | 向gen_server进程发送异步消息                                 |
| N/A                     | Module:handle_info/2 | 处理通过call或cast函数以外的手段发送给gen_server容器的消息，这些都是带外（out_of_band）消息 |

`gen_server:call/2 ` 调用方会等待应答（同步消息）

`gen_server:cast/2` 无须坐等应答（异步消息）

`handle_info/2`没有对应的gen_server库函数，这个回调是一个重要的特例，所有未经call或cast库函数发送至gen_server信箱的消息都由它处理（通常是直接用！运算符发送的裸消息）

**带外消息处理：**采用call或cast以外的手段发送消息给gen_server进程的所有消息都由`handle_info/2`回调函数处理。这些消息都被归类为带外消息

`tr_server:start_link/1    ---对用户屏蔽调用---->   gen_server:start_link/4    -----------然后新进程随后会回调--------->tr_server:init/1`



`gen_server:start `函数可以四个参数（`ServerName,Module,Args, Options`)

- 第一个参数`ServerName`是服务名，可以省略掉。具有相同服务名的模块在一个节点中只能启动一次，重复启动会报错，为`{error, {already_started, Pid}}`。具有服务名的服务进程可以使用服务名来调用，没有服务名的只能通过进程号`Pid`来调用。通常有名字的服务进程会使用模块名作为服务名称，即代码中定义的宏 `-define(SERVER, ?MODULE)`,然后再需要使用服务名的地方填入`?SERVER`
- 第二个参数`Module`是模块名，一般而言API和回调函数是写在同一个文件里，所以就用`?MODULE`,表示本模块的模块名
- 第三个参数`Args`是回调函数`init/1`的参数，会原封不动地传给`init/1`
- 第四个参数`Options`是一些选项，可以设置debug、超时等东西



#### 5、杂项

`Module:Func(Args) `与`Func(Args)`的区别

1. Erlang函数有`local call`和`external call`的区别，`Local call `就是在函数在被定义的模块里面被调用,可以直接被调用`Func(Args)`; `External call` 就是显式的使用`Module:Func(Args)`来调用或import别的模块进来的调用.

   当同一个模块有2个版本被加载里，所有的`local call`都可以工作在当前版本状态，但是：`external call`只能调用到最新的版本！(**代码热更的时候**)

2. 用link进行关联的进程组，只要其中一个进程终止，其他进程也会同时终止

3. 通过`process_flag(trap_exit, true)` 将进程设置为系统进程，不会随着其他进程退出而退出，同时还能监控其他进程

