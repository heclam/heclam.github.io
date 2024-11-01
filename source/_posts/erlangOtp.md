---
layout: article
title: erlang Otp
date: 2024-03-14 14:30:41
toc: true
categories:
	- erlang
tags:
	- erlang
---



#### 1、OTP 应用

OTP(Open Telecom PlatForm) 开放电信平台

应用的目录结构如下

```
<application-name>[-<version>]
			- doc
			- ebin
			- include
			- priv
			- src
```

<!--more-->

![erlangOtpDir](/images/erlangOtpDir.png)

创建OPT应用时的主要工作集中于**标准目录结构的创建和应用元数据的编写**。元数据的作用在于让系统获悉应该如何**启动和停止应用**，还可以用于指定应用的依赖项，也就是应用启动前必须预先安装或启动哪些其他应用。

1. 应用元数据

   元数据一般为`<application-name>.app` 文件存放在`server/ebin/`文件夹下

   ```erlang
   %% tcp_rpc.app
   %% _*_ mode: Erlang; fill-column: 75; comment-column: 50; _*_
   
   {application, tcp_rpc,  %% tcp_rpc为应用名称
   	[{description, "RPC server for Erlang and OTP in action"},
   		{vsn, "0.1.0"}, %% 应用版本 主版本号.次版本号.修订版本号
   		{modules, [tr_app,
   				   tr_sup,
   				   tr_server]},% 模块列表
   		{registered, [tr_sup]},	%% 注册进程名
   		{applications, [kernel, stdlib]}, %% 依赖其他应用的列表
   		{mod, {tr_app, []}}	%% 告知OTP系统应该如何启动应用
   	]}.
   ```

   元数据文件`<application-name>.app` 文件的格式很简明。除去注释，只剩下一个由句号结尾的Erlang项式：三元组 `{application, ..., ...}`, 其中第二个元素是应用名称所对应的原子，此处即是`tcp_rpc`。第三个元素是一个参数列表，其中每一个参数都是`{Key, Value}`对的形式。

   | 参数         | 描述                                                         |
   | ------------ | ------------------------------------------------------------ |
   | description  | 针对应用的简短描述                                           |
   | vsn          | 应用的版本                                                   |
   | modules      | 应用中的模块列表。模块在列表中的顺序无关紧要                 |
   | registered   | 在.app文件中罗列出所有进程注册名并不会促使系统执行实际的注册操作，但这可以告知OTP系统哪个进程注册了哪个名字，从而为系统升级等操作提供便利，同时也可以尽早发现重复的注册名并给出警告 |
   | applications | 必须在该应用启动前先启动的所有应用。应用往往依赖于其他应用。主动应用要求自己所依赖的所有应用在自己的生命周期开始之前先行启动并就绪。列表中各应用的顺序无关紧要 |
   | mod          | 告知OTP系统应该如何启动应用。该参数的值是一个元组，其内容为一个模块名以及一些可选的启动参数。 这个模块必须实现application行为模式 |

2. 应用行为模式

   ```erlang
   -module(tr_app).
   -behaviour(application).	% 行为模式声明
   
   % 应用行为模式的回调函数
   -export([
            start/2,
            stop/1
           ]).
   
   start(_Type, _StartArgs) ->
       case tr_sup:start_link() of	% 启动根监督者
           {ok, Pid} -> {ok, Pid};
           Other ->
               {error, Other}
       end.
   
   stop(_State) ->
       ok.
   ```

   start/2`的输入参数，`Type`一般取值为`normal`,但也可能是`{failover, ...}` 或`{takeover, ...}`, `StartArgs` 则是在元数据文件`<application-name>.app` 文件中给mod的参数

3. 监督者行为模式

   ```erlang
   -module(tr_sup).
   -behaviour(supervisor).
   
   %% API
   -export([start_link/0]).
   %% Supervisor callbacks
   -export([init/1]).
   -define(SERVER, ?MODULE).
   
   start_link() ->
       supervisor:start_link({local, ?SERVER}, ?MODULE, []).
   
   init([]) ->
       Server = {tr_server, {tr_server, start_link, []},
                permanent, 2000, worker, [tr_server]},
       Children = [Server],
       RestartStrategy = {one_for_one, 0, 1},
       {ok, {RestartStrategy, Children}}.
   ```

   > supervisor:start_link({local, ?SERVER}, ?MODULE, [])

   第一个参数是二元组{local, ?SERVER}, 用于让OTP库在本地节点上以?SERVER(如宏定义为tr_sup)为注册名自动注册监督进程

   ##### 3.1、监督者重启策略

   `init/1` 回调函数的返回值的格式为 `{ok, {RestartStrategy, Children}}`, 其中`Children`是若干子进程规范组成的一个列表。

   `RestartStrategy`是一个三元组 `{How, Max, Within}` 

   ##### 3.2、子进程规范

   ```erlang
   Server = {tr_server, {tr_server, start_link, []},
                permanent, 2000, worker, [tr_server]},
   ```

   由6个元素组成：`{ID, Start, Restart, Shutdown, Type, Modules}`

   第一个元素ID, 是一个用于在系统内部标识规范的项式。

   第二个元素Start, 是一个用于启动进程的三元组`Module, Function, Arguments`。与调用内置函数`spawn/3` 时是一样的，其中一个元素是模块名，第二个元素是函数名，第三个元素是函数的调用参数列表。

   第三个元素Restart,用于指明子进程发生故障时是否需要重启。此处指定为`permanent`,无论出于任何原因导致进程终止都应重启进程。`temporary` 表示永不重启进程，`transient` 表示仅在进程意外终止时重启进程。

   第四个元素Shutdown, 用于指明如何终止进程。此处取值为一个整数（2000），表示终止进程时采用软关闭策略，给进程留出一段自我了断的时间（以毫秒为单位），如果进程未能在指定时间内自行退出，将被无条件终止。该选项还可以取值为brutal_kill,表示在关闭监督进程时立即终止子进程；以及infinity, 主要用于子进程本身也同为监督者的情况，表示应给予子进程充分的时间自行退出

   第五个元素Type，用于表示进程是监督者（supervisor)还是工作者（worker)

   第六个元素Modules,选项列出了该进程所依赖的模块。这部分信息仅用于在代码热升级时告知系统该以何种顺序升级各个模块。一般来说，只需要列出子进程的主模块

4. 启动应用

   上面的`tr_server.erl` 模块就是一个`gen_server` 行为模式的实现为一个工作进程

   ```erlang
   # 1、可以先进行代码编译
   # -o 表示指定编译后的.beam文件存放位置
   # src/*.erl表示需要编译的源代码
   erlc -o ebin src/*.erl
   
   # 2、代码执行的环境,(-pa 是path add 的缩写，用于添加单个目录到代码路径的最前方)
   erl -pa ebin
   
   # 3、Erlang shell 启动后，只需一个命令便可启动应用:
   # 以应用名tcp_rpc为参数调用标准库函数 application:start/1
   application:start(tcp_rpc).
   
   %%%====================================
   %   这里讲解一下如何运行这个项目
   %     将.beam文件放入ebin文件夹下
   %       1. windows：进入项目路径下然后在cmd下输入for %f in (src/*.erl) do erlc -o ebin src/%f
   %       2. linux：输入：erl -o ebin src/*.erl
   %     启动Erlang并将ebin目录纳入代码路径
   %       3. erl -pa ebin
   %       4. application:start(tcp_rpc)
   %%%====================================
   ```

   Erlang会在代码路径中搜索`.beam`文件来加载模块，`application:start/1`函数也会在代码路径中搜索`.app`文件。由于`ebin`目录已经位于代码路径之中，shell便可以顺利找到元数据文件`ebin/tcp_rpc.app`，该文件包含了所需的一切信息（尤其是该调用哪个模块（tr_app)来启用整个应用）

5. 生成EDoc文档

   ```erlang
   %% 在项目目录下打开erl,输入
   edoc:application(tcp_rpc, ".", []).
   ```

   

#### 2、应用结构小结

​	建立OPT应用要做的3件事

```
1. 遵循标准目录结构
2. 添加用于存放应用元数据的.app文件
3. 创建一个application行为模式实现模块，负责启动应用
```



#### 3、监督树例子

```erlang
-module(bank_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

% 定义一个启动本监督树的API
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % 启动两个子进程
    BankCenterSpec = {
        center, % 指定本进程（在子进程中唯一）的名称
        {bank_center, start_link,[]}, % 进程启动函数：{M,F,A}
        transient,  % 重启策略： permanent | transient | temporary
        5000,   % 关闭方式： brutal_kill | init() > 0 | infinity
        worker, % 进程类型： worker | supervisor
        [bank_center] % 回调模块名称： [Module] |dynamic
    },
    BankCenterSpec2 = {
        center2,
        {bank_center2,start_link, []},
        transient,
        5000,
        worker,
        [bank_center2]
    },
    % {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}
    {ok,{{one_for_one, 5, 30}, [BankCenterSpec,BankCenterSpec2]}}.
```

##### 重启策略

```
permanent : 终止后总是会被重启
transient : 意外终止后会被重启（就是进程退出的Reason不是 normal | shutdown | {shutdown, Term}） 
temporary : 终止后不会被重启 

如果重启策略为permanent，那么发生异常的系统进程就会不断地重启，这可如何是好？
```

`{ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}{ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}`可以很好的解决这个问题

`RestartStrategy `: 重启策略

1. `one_for_one`: 当某个进程终止时，只启动这个子进程
2. `simple_for_one`: 和`one_for_one`一样，只是子进程是监督树启动后在动态添加的，比如游戏中，一个玩家就是一个进程
3. `one_for_all`: 当某个进程终止时，全部的子进程都重新启动
4. `rest_for_one`: 但某个进程终止时，之前比它晚启动的进程都将重新启动

* 例如`MaxR  = 5` 与`MaxT = 30` 表示在30秒内该进程总允许重启数为5，如果超过了，那么该进程将终止，整个监督树下的进程都将会终止，整个applicaiton也就此停止



##### 子规范

```erlang
{Id, StartFunc, Restart, Shutdown, Type, Modules}

%------------------------------------
{Id, StartFunc, Restart, Shutdown, Type, Modules}
    Id = term()
    StartFunc = {M, F, A}
        M = F = atom()
        A = [term()]
    Restart = permanent | transient | temporary
    Shutdown = brutal_kill | integer() >=0 | infinity
    Type = worker | supervisor
    Modules = [Module] | dynamic
        Module = atom()
%-----------------------------------
```

* `Id` 用来内部标识子规范
* `StartFunc`是启动子进程时调用的函数，它将成为对`supervisor:start_link, gen_server:start_link,gen_fsm:start_link,gen_event:start_link`的调用
* `Restart`标识一个进程终止后将怎样重启，一个`permanent进程`总会被重启；一个`temporary进程`不会被重启；一个`transient进程`仅仅当时不正常的被终止后才重启，例如非`normal`的退出原因
* `Shutdown`定义一个进程怎样被终止，`brutal_kill`意味着子进程被`exit(Child,kill) `无条件的终止；一个整数值的超时时间意味着监督者告诉子进程通过`exit(Child, shutdown)`而被终止，然后等待一个返回的退出信号，假如在指定时间里没有收到退出信号，那么子进程用`exit(Child,kill)`被无条件终止。
* `Type` 指定子进程是`supervisor`还是`worker`
* `Modules` 是有一个元素的列表[Module],假如子进程时supervisor、gen_server或gen_fsm那么Module是回调模块的名称；假如子进程时gen_event,那么Modules应该是dynamic



#### 4、另外一种编译方式

在项目根目录下，新建一个Emakefile文件

```
{
	[
		'src/*',		
		'src/*/*',		
		'src/*/*/*',
		'src/*/*/*/*',
		'src/*/*/*/*/*'
	],
	[
		{i, "include/"},
		{outdir,'ebin'}
	]
}.
```

上面的结构一目了然，源码路径，头文件路径，输出目录

```
% 在项目根目录下调用cmd窗口，执行一下命令编译
erl -make
```



#### 5、OTP中的缓存系统例子

​												**简易缓存应用中的模块**

----

​		模块																用途

----

​		simple_cache											用户API,应用的外部接口

​		sc_app														应用行为模式实现模块

​		sc_sup														根监督者实现模块

​		sc_store													 用于封装键和pid之间映射关系的模块

​		sc_element												缓存数据存储进程

---



1. 搭建基本的骨架

   ```
   1. 创建标准应用目录布局（上面可见）；
   2. 编写.app文件；
   3. 编写应用行为模式实现，即sc_app;
   4. 实现顶层监督者，即sc_sup
   ```

2.  创建应用元数据(ebin/simple_cache.app)

   ```erlang
   {application, simple_cache,
   	[{description, "A simple caching system"},
   		{vsn,"0.1.0"},
   		{modules,[
   			sc_app,
   			sc_sup
   		]},
   		{registered, [sc_sup]},
   		{applications, [kernel, stdlib]},
   		{mod, {sc_app, []}}
   	]}.
   ```

3.  实现应用行为模式(src/sc_app.erl)

   ```erlang
   %%%-------------------------------------------------------------------
   %%% @author Administrator
   %%% @copyright (C) 2019, <COMPANY>
   %%% @doc
   %%%
   %%% @end
   %%% Created : 04. 十二月 2019 18:16
   %%%-------------------------------------------------------------------
   -module(sc_app).
   -author("Administrator").
   % 实现应用行为模式
   -behaviour(application).
   %% API
   % 导出的行为模式回调函数
   -export([start/2, stop/1]).
   
   start(_StartType, _StartArgs) ->
       sc_store:init(),		% 存储初始化
   	case sc_sup:start_link() of % 启动根监督者
   		{ok, Pid} ->
   			{ok, Pid};
   		Other ->
   			{error, Other}
   	end.
   
   stop(_State) ->
   	ok.
   
   
   %%%====================================
   %   这里讲解一下如何运行这个项目
   %     将.beam文件放入ebin文件夹下
   %       1. windows：进入项目路径下然后在cmd下输入for %f in (src/*.erl) do erlc -o ebin src/%f
   %       2. linux：输入：erl -o ebin src/*.erl
   %     启动Erlang并将ebin目录纳入代码路径
   %       3. erl -pa ebin
   %       4. application:start(simple_cache)
   %		5. simple_cache:insert(a,man).  % 插入数据
   %%%====================================
   ```

4. 实现监督者（src/sc_sup.erl）

   ```erlang
   %%%-------------------------------------------------------------------
   %%% @author Administrator
   %%% @copyright (C) 2019, <COMPANY>
   %%% @doc
   %%%
   %%% @end
   %%% Created : 04. 十二月 2019 18:21
   %%%-------------------------------------------------------------------
   -module(sc_element_sup).
   -author("Administrator").
   % 实现监督者
   -behaviour(supervisor).
   %% API
   -export([start_link/0,
            start_child/2]). %% start_child/2 动态启动子进程
   -export([init/1]).
   
   -define(SERVER, ?MODULE).
   
   start_link() ->
   	supervisor:start_link({local, ?SERVER}, ?MODULE, []).
   
   start_child(Value, LeaseTime) ->
       % sc_element:start_link/2的参数
   	supervisor:start_child(?SERVER, [Value, LeaseTime]).
   
   init([]) ->
       %% 关闭策略设置为brutal_kill,表示子进程应随监督者的关闭而立即终止
   	Element = {sc_element, {sc_element, start_link, []},
   				temporary, brutal_kill, worker, [sc_element]},
   	Children = [Element],
       %% 监督策略，（简易一对一监督，只能启动一种子进程，但可以启动任意个）
       %% 它所有的子进程都是运行时动态添加的，子进程不会随监督者一同启动
       %% 这里设置的重启频率为每秒0次（即不执行重启）
   	RestartStrategy = {simple_one_for_one, 0,1},
   	{ok, {RestartStrategy, Children}}.
   ```

   

5. 编写sc_element进程（src/sc_element.erl）

   ```erlang
   %%%-------------------------------------------------------------------
   %%% @author Administrator
   %%% @copyright (C) 2019, <COMPANY>
   %%% @doc
   %%%
   %%% @end
   %%% Created : 04. 十二月 2019 19:44
   %%%-------------------------------------------------------------------
   -module(sc_element).
   -author("Administrator").
   -behaviour(gen_server).
   %% API
   -export([
   		start_link/2,
   		create/2,
   		create/1,
   		fetch/1,
   		replace/2,
   		delete/1
   ]).
   -export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
   -define(SERVER, ?MODULE).
   -define(DEFAULT_LEASE_TIME, (60*60*24)).    % 一天的总秒数 , 默认的淘汰时间
   -record(state, {value, lease_time, start_time}).    % 状态记录：进程持有值，淘汰时间，进程启动的时间戳
   
   
   start_link(Value, LeaseTime) ->
   	gen_server:start_link(?MODULE, [Value, LeaseTime],[]).
   
   create(Value, LeaseTime) ->
   	sc_sup:start_child(Value, LeaseTime). % 将启动委托给sc_sup
   
   create(Value) ->
   	create(Value, ?DEFAULT_LEASE_TIME).
   
   fetch(Pid) ->
   	gen_server:call(Pid, fetch).
   
   replace(Pid, Value) ->
   	gen_server:cast(Pid, {replace, Value}).
   
   delete(Pid) ->
   	gen_server:cast(Pid, delete).
   
   
   %% gen_server回调
   init([Value, LeaseTime]) ->
   	Now = calendar:local_time(),
   	StartTime = calendar:datetime_to_gregorian_seconds(Now),
   	{ok,
   		#state{value = Value,       % 初始化进程状态
   				lease_time = LeaseTime,
   				start_time = StartTime},
   		time_left(StartTime, LeaseTime)}.   % 初始化超时设置
   
   time_left(_StartTime, infinity) ->
   	infinity;
   time_left(StartTime, LeaseTime) ->
   	Now = calendar:local_time(),
   	CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
   	TimeElapsed = CurrentTime - StartTime,
   	case LeaseTime - TimeElapsed of
   		Time when Time =< 0 -> 0;
   		Time                -> Time*1000
   	end.
   
   handle_call(fetch, _From, State) ->
   	#state{value = Value,
   			lease_time = LeaseTime,
   			start_time = StartTime} = State,
   	TimeLeft = time_left(StartTime, LeaseTime),
   	{reply, {ok, Value}, State, TimeLeft}. % 取出进程状态中的值
   
   handle_cast({replace, Value}, State) ->
   	#state{lease_time = LeaseTime,
   		start_time = StartTime} = State,
   	TimeLeft = time_left(StartTime, LeaseTime),
   	{noreply, State#state{value = Value}, TimeLeft};
   handle_cast(delete, State) ->
   	{stop, normal, State}.          % 发出关闭信号
   
   handle_info(timeout, State) ->
   	{stop, normal, State}.
   
   terminate(_Reason, _State) ->
   	sc_store:delete(self()),        % 删除进程的键
   	ok.
   
   code_change(_OldVsn, State, _Extra) ->
   	{ok, State}.
   
   ```

6. 实现sc_store模块（src/sc_store.erl）

   ```erlang
   %%%-------------------------------------------------------------------
   %%% @author Administrator
   %%% @copyright (C) 2019, <COMPANY>
   %%% @doc
   %%%
   %%% @end
   %%% Created : 04. 十二月 2019 20:34
   %%%-------------------------------------------------------------------
   -module(sc_store).
   -author("Administrator").
   
   %% API
   -export([
   		init/0,
   		insert/2,
   		delete/1,
   		lookup/1
   ]).
   -define(TABLE_ID, ?MODULE).
   
   init() ->
   	ets:new(?TABLE_ID, [public, named_table]),
   	ok.
   
   insert(Key, Pid) ->
   	ets:insert(?TABLE_ID,  {Key, Pid}).
   
   lookup(Key) ->
   	case ets:lookup(?TABLE_ID, Key) of
   		[{Key, Pid}] -> {ok, Pid};
   		[]          ->  {error, not_found}
   	end.
   
   % 利用模式匹配删除表项
   delete(Pid) ->
   	ets:match_delete(?TABLE_ID, {'_', Pid}).
   
   ```

7. 打造应用层API模块(src/simple_cache.erl)

   ```erlang
   %%%-------------------------------------------------------------------
   %%% @author Administrator
   %%% @copyright (C) 2019, <COMPANY>
   %%% @doc
   %%%
   %%% @end
   %%% Created : 04. 十二月 2019 20:44
   %%%-------------------------------------------------------------------
   -module(simple_cache).
   -author("Administrator").
   % 打造应用层API模块
   %% API
   -export([
   		insert/2,
   		lookup/1,
   		delete/1
   ]).
   
   insert(Key, Value) ->
   	case sc_store:lookup(Key) of
   		{ok, Pid} ->
   			sc_element:replace(Pid, Value);
   		{error, _} ->
   			{ok, Pid} = sc_element:create(Value),
   			sc_store:insert(Key, Pid),
   			sc_event:create(Key, Value)
   	end.
   
   lookup(Key) ->
   	try
   		{ok, Pid} = sc_store:lookup(Key),
   		{ok, Value} = sc_element:fetch(Pid),
   		{ok, Value}
   	catch
   	    _Class:_Exception  ->
   		    {error, not_found}
   	end.
   
   delete(Key) ->
   	case sc_store:lookup(Key) of
   		{ok, Pid} ->
   			sc_element:delete(Pid);
   		{error, _Reason} ->
   			ok
   	end.
   ```

   




#### 进阶需要学习的内容

##### rebar

​	 rebar是一个附带许多标准功能的便携Erlang脚本 

##### erlang.mk

​	 erlang.mk则是一个很小但却可以是非常快编译文件的神奇makefile 文件 

##### OPT applications 和 OTP release

​	 OTP applications和OTP release项目结构是不同的，一个OTP application 可以看成一个拥有最高级监控树(如果有的话)，并且下面可能有一大堆的依赖项(a bunch of dependencies).一个OTP release通常是多个OTP applications的组合，这些application之间可能会有依赖关系，也可能没有。这就导致了两种主要的部署applicaitons的方式 

