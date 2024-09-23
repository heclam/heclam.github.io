---
layout: article
title: erlang节点
date: 2024-03-13 14:30:41
toc: true
categories:
	- erlang
tags:
	- erlang
---



两个重要的概念：

	1. 复制式进程间通信
	2. 位置透明性



#### 1、节点与集群：

Erlang VM 实例称为节点

两个或两个以上的Erlang节点能够相互感知，它们就形成了一个集群

<!--more-->

#### 2、节点的启动

只要给erl（或werl）加上命令行参数`-name`或`-sname`,就可以以分布式模式启动Erlang节点

第一种形式适用于配有DNS的普通环境，需要给出节点的完全限定域名

> erl -name simple_cache

第二种形式适用于完全限定域名不可用的情况,可以使用短节点名

> erl -sname simple_cache

**只有采用相同模式的节点才能互联**

windows下启动节点，可以直接找到Erlang图标，可以复制多个快捷方式，然后右击属性，在目标地址栏添加参数如` 'url' -sname a,` 随后直接点击图标就可以启动a节点了



#### 3、节点互联

​	在节点内可以通过nodes()，检查节点间的互联情况

```erlang
% 对一个节点建立连接
% 成功返回pong,失败返回pang
% net_adm:ping('节点名称') 
net_adm:ping('B@xxxx').    

% 默认情况下，每个节点都会假定所有与自己打交道的节点都拥有和自己一样的cookie
auth:get_cookie()	% 获取cookie
set_cookie(Node,Cookie)	% 设置cookie
```



**epmd**(Erlang Port Mapper Daemon) 是Erlang的端口映射守护进程，这是一个操作系统级的守护进程，当erlang的节点启动时，会将节点名称和地址发送给epmd, epmd会记录当前机器下节点的信息，主机间可以通过epmd交换双方机器下的节点信息并进行连接。



不管运行了多少个Erlang节点，每个计算机只运行一个epmd，它随Erlang节点的第一次启动而启动，不随Erlang节点的结束而结束。

epmd负责监听**4369**端口上的连接请求,然后将其映射给相应节点的监听端口

```erlang
% 使用 erl_epmd:names() 查看epmd注册的节点

% 查看本机的epmd下注册的节点
erl_epmd:names()

% 查看其他机器下的epmd注册的节点
erl_epmd:names('ip地址')

% 若双方能通过erl_epmd:name('对方的ip') 看到对方epmd下注册的节点，说明双方的epmd端口至少是互通的
```





**节点互联要素**

>1、要有相同的cookie
>
>2、命名（节点可以使用短命名(-sname) 或长命名(-name)启动，短命名和长命名的节点不能进行通信
>
>3、开放端口 (在不同主机进行通信时，需要开放节点通信所需要的端口, 开放epmd端口4369)，其次节点的动态端口是可以访问的
>
>版本： erlang允许不同操作系统的erlang节点进行互联，但是不允许主版本不同的erlang节点进行连接。例如erlang R16 不能与erlang R17互联(查看主版本指令  erlang:system_info(otp_release))



通过nodes(hidden) 或 nodes(connected) 查看hidden nodes

hidden node 的一个用法是作为“网关”将许多小的分布式集群连接起来。另外一个用法是用来做维护，它不会影响流量

```erlang
%% 查看允许被访问的节点
net_kernel:allowed()
%% 如果为空则允许所有节点访问，不为空则只允许列表中的节点访问

%% 添加允许被访问的节点
net_kernel:allow(节点名称列表)
```



#### 4、远程Shell

```
1、在节点终端
ctrl+G % 打开任务控制接口

2、开始远程shell
r ‘b@xxx’

3、j查看任务

4、c 执行相应的任务

5、退出远程shell
第一步通过c  直接切换任务
第二部 k 任务编号
```



另外一种接入控制台的方法

```erlang
%% 这样直接进入了node_1节点
werl -setcookie abc -name node_2@127.0.0.1 -remsh node_1@127.0.0.1
```



