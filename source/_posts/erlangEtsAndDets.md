---
layout: article
title: erlang Ets and Dets
date: 2024-03-12 14:30:42
toc: true
categories:
	- erlang
tags:
	- erlang
---



#### ETS和EDTS

ETS 和 DETS 表保存的是元组。

ETS 和 DETS 基本上做同一件事情：它们提供大量的“键-值” 搜索表，所不同的是，ETS驻留在内存，而DETS驻留在磁盘。

表的4种类型： set、ordered set、bag、 duplidate bag，

<!--more-->

在set类型下，表中每一个元组的键值都不能相同。

在ordered set 类型下，元组会进行排序。

在bag类型下，多个元组可以有相同的键值，但不能有两个完成相同的元组。

在duplicate bag 类型下，不仅多个元组可以有相同的键值，同一个元组也可以在表中出现多次

```erlang
ets:new(Name, [Opt]) -> TableId
% Name 是一个原子
% [Opt] 是一个选项列表，取值范围
%     表类型 set| ordered_set | bag | duplicate_bag 
% 	  private 创建私有表，只有所有者进程可以读写这个表
%	  public  创建公开表，所有知道这个表标识的进程都可以对这个表进行读写操作
%     protected 创建受保护的表，所有知道这个表的标识的进程都可以对这个表进行读操作，但只有这个表的所有者进程可以对这个表进行写操作
%    named_table 命名表，如果存在这个选项，则可以在后续操作中使用Name来操作表             %	   {keypos, K} 使用K作为键的位置            
```





