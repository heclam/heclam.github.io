---
layout: article
title: mnesia
date: 2024-03-19 10:30:31
toc: true
categories:
	- erlang
tags:
	- erlang
	- mnesia
---



#### Mnesia

Mnesia是一套轻量级的软实时分布式数据存储系统，支持冗余复制和事务。

建立项目数据库

```erlang
建立数据库的过程可分为以下几个步骤：
1、初始化mnesia
2、启动节点
3、建立数据库模式（schema)
4、启动Mnesia
5、建立数据库表
6、向新建的表中录入数据
7、对数据做一些基本查询
```

<!--more-->

```erlang
1、启动节点
% 启动时告诉节点应该将Mnesia的相关信息写到文件系统的某个地方，由dir指定路径
werl -mnesia dir '"F:/erl/gen_server/mnesia/mnesia_store"' -sname nodename

2、建立数据库模式 (在本节点建立数据库模式)
mnesia:create_schema([node()]).

3、启动Mnesia
mnesia:start().

% 查看信息
mnesia:info().

4、建表
-record(user,{
	id,
	name
}).

-record(project,{
	title,
	description
}).

-record(contributor,{
	user_id,
	project_title
}).

init_tables() ->
	mnesia:create_table(user,[{attributes,record_info(fields,user)}]),
	mnesia:create_table(project,[{attributes,record_info(fields,project)}]),
	mnesia:create_table(contributor,[{type,bag},{attributes,record_info(fields,contributor)}]).
	
	
insert_user(Id,Name,ProjectTitles) when ProjectTitles =/= [] ->
	User = #user{id =Id,name = Name},
	F = fun() ->
		mnesia:write(User),
		lists:foreach(fun(Title) ->
			[#project{title = Title}] = mnesia:read(project,Title),
			mnesia:write(#contributor{user_id = Id,project_title = Title})
		end,ProjectTitles)
	 end,
	mnesia:transaction(F);	%% 设置事务
insert_user(_,_,_) -> ok.
	
insert_project(Title,Desc) ->
	% 脏操作在执行时不会考虑事务或数据库锁
	mnesia:dirty_write(#project{title = Title,description = Desc}).

5、向表中录入数据
mnesia:write(#user{id= Id,name = Name})

% 脏操作
mnesia:dirty_write(#project{title = Title,description = Desc})


6、执行基本查询
mnesia:read(project,Title).
mnesia:dirty_read(user,1).


F = fun() ->
        mnesia:select(user,[{#user{id = '$1',name = abc},[],['$1']}])
  end,      
mnesia:transaction(F).

% mnesia:select/2
% 第一个参数是待查询的表
% 第二个参数是所有的匹配规范，每条匹配规范都是一个形如{Head,Conditions,Results}的三元组
% Head是一个Erlang项式，用于描述查询模式，其中‘$1’,'$2'等原子用于表示变量。
% Conditions部分用于罗列作用于该匹配条件上的额外约束条件
% Results,可以描述要从匹配到的每条记录中生成什么样的结果项式

% 一些具有特殊含义的原子：
% ’_‘ (仅限于在Head部分使用) ------无所谓，任意值都可以
% '$_' (仅限于在Results和Conditions中使用) --------与查询条件项匹配的整条记录
% ’$$‘ (仅限于在Results和Conditions中使用) --------等价于依次罗列出在Head部分匹配的所有变量’$1‘,'$2'...等


% 使用查询列表速构（QLC)
mnesia:transaction(fun() ->
                  Table = mnesia:table(user),
                  QueryHandle = qlc:q([U#user.id||U <- Table，U#user.name =:= abc]),
                  qlc:eval(QueryHandle)
                  end)。

```



Mnesia表可分为set型，ordered_set型和bag型：

set型：set型表中的建是唯一的，如果插入新的记录与先存的某个表项的主键相同，则新的记录会覆盖旧记录。

ordered_set型：ordered_set型与set型的行为相同，但set型表和bag型表都采用哈希表实现，ordered_set表则可以按主键的顺序保存记录。

bag型：bag型表可以容纳多个具有相同主键的记录，但这些记录至少要有一个字段的值不相等，同一条记录插入多次是没用的





#### 创建发布镜像

1、确定需要包含哪些应用

2、创建用于描述发布镜像内容的元数据（.rel）文件

3、创建启动脚本

4、创建系统配置文件（可选，但一般都需要）

5、将所有内容打包成单个文件



I、发布镜像的元数据文件（如：mnesia_sc.rel）

{release,{发布镜像名,版本号},{erts,版本号},[{依赖的应用名,版本号}|...]}

```erlang
{release,
 {"mnesia_sc", "0.1.0"},
 {erts, "8.3"},
 [{kernel, "5.2"},
  {stdlib, "3.3"},
  {sasl, "3.0.3"},
  {mnesia, "4.14.3"},
  {resource_discovery, "0.1.0"},
  {mnesia_sc, "0.3.0"}
 ]}.
```



II、脚本与启动文件

.script文件内包含一份完整的规范，所有应用的内容明细全部都罗列在内，包括应用的路径、需要加载的模块，以及其他信息。

.boot文件则是.script文件的二进制形式，可供Erlang运行时系统在启动时直接读取。

生成这两个文件的操作：

```
% 在.rel同目录下 执行
erl -pa ./mnesia_sc/ebin -pa ./resource_discovery/ebin

% 调用systools模块来生成真正的.boot和.script文件
systools:make_script("mnesia_sc",[local])

% 执行完命令后，当前目录下就会生成两个文件：mnesia_sc.script、mnesia_sc.boot
% local参数主要用于测试，在调用make_script/2函数时，传入的local选项的作用在于将所有应用的绝对路径写入.script和.boot文件
% 如果不采用local选项，生成的.script和.boot文件会认为所有应用都位于文件系统的某个名为lib的目录下,该目录的具体位置由系统变量$ROOT指定
```

III、系统配置

在.rel同目录下，穿件xxx.config文件 （xxx可以随便起名）

[{应用名,[应用选项键值对]}|...]

```
[
	{sasl,
		[
			{sasl_error_logger,{file,"F:/erl/gen_server/test_pack/tmp/mnesia_sc.sasl_log"}}
		]
	},
	{mnesia_sc,
		[
			{contact_nodes,['a@SD-20190726VNMA','b@SD-20190726VNMA']}
		]
	}
].
```

III、启动柜目标系统

```
% 按照之前的缓存系统，至少启动一个联络节点a,然后再执行

erl -sanme cache -boot ./mnesia_sc -config ./sys

% -boot指定使用哪个.boot文件，这里是当前目录下的 mnesia_sc.boot
% -config指定使用哪个.config文件，这里是当前目录下的 sys.config
```



可使用

observer:start() 查看起来的应用



```
erl -sanme cache -boot ./mnesia_sc -config ./sys -detatched
% 采用这种方式启动的系统将在后台运行，不会打开shell.在联络节点的shell内调用nodes()，仍然可以在结果中看到分离的节点，可以针对该节点启动一个远程shell来对它执行各种操作。
```

