---
layout: article
title: erlang套接字编程
date: 2024-03-12 14:30:43
toc: true
categories:
	- erlang
tags:
	- erlang
---

gen_tcp:controlling_process(Socket, NewPid) 函数来把一个套接字的控制进程改为新的控制进程NewPid

Erlang的套接字可以以3种模式打开：active、active once 或 passive。

在调用gen_tcp:connect(Address, Port, Options) 或者 gen_tcp:listen(Port, Options) 时通过选项{active, true | false | once} 设置套接字的参数来实现

<!--more-->

{active, true} 那么程序会创建一个主动套接字

{active, false} 则会创建一个被动套接字

{acitve, once} 也会创建一个主动套接字，但这个套接字仅接收一条消息，当它接收完这条消息后，如果想让它接收下一条消息，那么就必须再次激活它



主动套接字和被动套接字的区别在于套接字接收到消息之后的工作机制：

- 建立主动套接字之后，当数据到达时系统会向控制进程发送`{tcp, Socket, Data}` 消息。

而控制进程无法控制这些消息流。一个独立的客户机有可能会向系统发送成千上万条消息，而这些消息都会被转送到控制进程。但控制进程却无法停掉这个消息流。

- 如果套接字以被动模式打开，那么控制进程必须调用`gen_tcp:recv(Socket, N)` 来接收来自于套接字的数据。它会尝试从套接字接收N字节的数据。如果N为0，那么所有可用的字节数据都会返回。在这种情况下，服务器可以通过选择调用gen_tcp:recv的时机来控制来自客户机的消息流

#### 3种方式编写一个服务器的消息接收循环

主动型消息接收（非阻塞）

被动型消息接收（阻塞）

混合型消息接收（半阻塞）

##### 1、主动型消息接收（非阻塞）

```erlang
{ok, Listen} = gen_tcp:listen(Port, [..., {active, true}, ...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).

loop(Sokcet) ->
    receive
        {tcp, Socket, Data} ->
            ... do something with the data ...
		{tcp_closed, Socket} ->
    		...
	end.
```

这个进程无法控制服务器循环中的消息流。如果客户端发送的数据快过服务器可以处理的速度，那么系统就会被消息淹没 ，消息缓冲区会被塞满，系统可能就会莫名奇妙的崩溃。

这种类型的服务器被称为异步服务器，因为它不会阻塞客户端。只有在我们可以确信服务端的性能能够跟上客户机的需求时，才应该选择使用异步服务器

##### 2、被动型消息接收（阻塞）

服务器通过设置`{active, false}` 选项来以被动模式打开一个套接字，它不会因为一个过于活跃的客户机通过发送大量数据的攻击而崩溃。

每次程序想要接收数据的地方都会调用`gen_tcp:recv`。调用recv函数时客户端会被阻塞

```erlang
{ok, Listen} = gen_tcp:listen(Port, [..., {active, false}, ...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).

loop(Sokcet) ->
    case gen_tcp:recv(Socket, N) of
        {ok, Bin} ->
            ... do something with the data ...
			loop(Socket);
		{tcp_closed, Socket} ->
    		...
	end.
```

##### 3、混合型消息接收（半阻塞）

在这个模式中，套接字是主动的但是仅仅针对一个消息。在控制进程发过一个消息后，必须显示地调用函数`inet:setopts`来把它重新激活以便接收下一个消息。在此之前，系统会处于阻塞状态。

```erlang
{ok, Listen} = gen_tcp:listen(Port, [..., {active, once}, ...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).

loop(Sokcet) ->
    receive
        {tcp, Socket, Data} ->
            ... do something with the data ...
			inet:setopts(sock, [{active, once}]),
    		loop(Socket);
		{tcp_closed, Socket} ->
    		...
	end.
```