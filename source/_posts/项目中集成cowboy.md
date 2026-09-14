---
title: 项目中集成cowboy
date: 2026-08-18 11:35:55
toc: true
categories:
  - erlang
  - cowboy
tags:
  - cowboy
---



### 第一步、项目中添加依赖:

如果有内部镜像或需要指定git仓库

```erlang
{deps,
  [
    {cowboy, "2.9.0"}
  ]}.
```

或者:

获取将cowboy应用里面的源码添加进来(不用每次编译的话,将编译后的代码添加进ebin目录即可)



### 第二步、实现WebSocket处理逻辑

编写一个回调模块来处理游戏客户端的WebSocket消息。这个模块要实现`cowboy_websocket`行为。

创建一个文件 `src/game_ws_handler.erl`

```erlang
-module(game_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
    player_id :: integer()
}).

%% 初始化HTTP升级请求
init(Req, _Opts) ->
    %% 执行升级到WebSocket协议
    {cowboy_websocket, Req, #state{}}.

%% WebSocket连接建立成功后的初始化
websocket_init(State) ->
    %% 生成临时玩家ID，实际项目中这里可能做token验证
    PlayerId = rand:uniform(9999),
    io:format("~p 玩家 ~p 已连接~n", [self(), PlayerId]),
    %% 将当前进程PID存入ETS表，用于后续广播（需提前建表）
    ets:insert(online_players, {PlayerId, self()}),
    %% 向客户端发送欢迎消息
    {reply, {text, jiffy:encode(#{type => <<"welcome">>, player_id => PlayerId})}, State#state{player_id = PlayerId}}.

%% 处理客户端发来的文本消息
websocket_handle({text, Msg}, State) ->
    %% 解码JSON
    try jiffy:decode(Msg, [return_maps]) of
        #{<<"action">> := <<"move">>, <<"x">> := X, <<"y">> := Y} ->
            %% 处理移动逻辑（这里只是回显）
            io:format("玩家 ~p 移动到了 (~p, ~p)~n", [State#state.player_id, X, Y]),
            %% 广播位置给所有玩家（简单示例）
            broadcast_to_all(#{type => <<"move">>, player => State#state.player_id, x => X, y => Y}),
            {ok, State};
        _ ->
            %% 未知指令
            {reply, {text, jiffy:encode(#{type => <<"error">>, msg => <<"unknown action">>})}, State}
    catch
        _:_ ->
            {reply, {text, jiffy:encode(#{type => <<"error">>, msg => <<"invalid json">>})}, State}
    end;
%% 处理二进制消息（如Protobuf），这里忽略
websocket_handle(_Data, State) ->
    {ok, State}.

%% 处理来自其他Erlang进程的消息（用于广播等）
websocket_info({broadcast, Msg}, State) ->
    {reply, {text, jiffy:encode(Msg)}, State};
%% 心跳超时
websocket_info(timeout, State) ->
    {reply, {text, jiffy:encode(#{type => <<"ping">>})}, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% 连接关闭回调
terminate(_Reason, _Req, State) ->
    case State#state.player_id of
        undefined -> ok;
        PlayerId ->
            ets:delete(online_players, PlayerId),
            io:format("玩家 ~p 已断开~n", [PlayerId])
    end.

%% 广播消息给所有在线玩家（通过遍历ETS表）
broadcast_to_all(Msg) ->
    Encoded = jiffy:encode(Msg),
    %% 获取所有在线玩家进程
    AllPlayers = ets:tab2list(online_players),
    lists:foreach(fun({_Id, Pid}) ->
        Pid ! {broadcast, Encoded}
    end, AllPlayers).
```



### 第三步、启动Cowboy并集成到监督树

这是最关键的一步。需要创建一个`gen_server`（或`application`的`start/2`）来负责Cowboy的启动和停止，并将其挂载到项目的监督树上。

创建 `src/game_cowboy_sup.erl`：

```erlang
-module(game_cowboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% 1. 创建ETS表用于存储在线玩家
    %%    注意：如果表已存在，不要重复创建，这里我们用public以便调试
    case ets:info(online_players) of
        undefined ->
            ets:new(online_players, [set, public, named_table, {read_concurrency, true}]);
        _ -> ok
    end,

    %% 2. 配置路由：将所有WebSocket请求指向我们的处理模块
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", game_ws_handler, []}  %% 客户端连接 ws://host/websocket
        ]}
    ]),

    %% 3. 启动Cowboy监听器
    {ok, _} = cowboy:start_clear(
        game_http_listener,        %% 监听器名称
        [{port, 8080}],            %% 监听端口
        #{env => #{dispatch => Dispatch}}
    ),

    %% 4. 监督树策略：这是一个一次性启动的worker
    {ok, {{one_for_one, 10, 10}, []}}.
```



### 第四步、将Cowboy监督者加入到主监督树

```erlang
-module(game_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% 启动顶层监督者
    game_sup:start_link().

%% 在 game_sup.erl 中：
init([]) ->
    %% 确保子进程列表包含 Cowboy 监督者
    Children = [
        %% 你的其他gen_server...
        game_cowboy_sup  %% 加入这一行
    ],
    {ok, {{one_for_one, 10, 10}, Children}}.
```





## 验证：(是否可以正常工作)

Q :  如何确保cowboy已集成到Erlang游戏节点？



### 一、节点内确认：Cowboy 真的在游戏节点里活着

节点终端执行：查看应用是否已起来

```erlang
application:which_applications().


## 检查监听
ranch:get_addr(http).

## 预期返回
{{0,0,0,0}, 9621}
```



### 二、节点外确认：端口通、HTTP通、WS通

假设游戏节点的端口为9621、 ws路由为/websockt、HTTP路由为/background

#### 1、判断端口在不在

```
# linux检查端口
ss -ltnp | grep 9621		// 检测是否有beam.smp监听，有则说明Ranch拿到了socket

或 mac
lsof -i :9621
```



#### 2、HTTP接口冒烟测试

```
curl -i http://localhost:9621/background // 这里注意如果是在部署机器上可以用localhost,否则需要指定ip
或：curl -i http://192.168.100.204:9621/background 
```

预期：返回server: Cowboy

例如：

```bash
huang@huang-VirtualBox:~/xnh5/server/script$ curl -i http://localhost:9621/background/
HTTP/1.1 200 OK
content-length: 72
content-type: application/json
date: Tue, 18 Aug 2026 16:24:33 GMT
server: Cowboy

{"code":1,"data":"\u670d\u52a1\u5668\u5e73\u53f0\u548cID\u4e0d\u5bf9!!"}
```

```bash
C:\Users\admin>curl -i http://192.168.100.204:9621/background/
HTTP/1.1 200 OK
content-length: 42
content-type: application/json
date: Tue, 18 Aug 2026 16:13:23 GMT
server: Cowboy

{"code":1,"data":"no access permission!!"}
```



#### 3、ws测试 (若为wss里面的http替换成https)

```shell
curl -i --http1.1 --max-time 5 \
  -H "Connection: Upgrade" \
  -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Version: 13" \
  -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
  http://192.168.100.204:9621/
```

预期返回：

```shell
huang@huang-VirtualBox:~/xnh5/server/script$ curl -i --http1.1 --max-time 5 \
>   -H "Connection: Upgrade" \
>   -H "Upgrade: websocket" \
>   -H "Sec-WebSocket-Version: 13" \
>   -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
>   http://127.0.0.1:9621/
HTTP/1.1 101 Switching Protocols
connection: Upgrade
date: Tue, 18 Aug 2026 16:41:07 GMT
sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
server: Cowboy
upgrade: websocket



curl: (28) Operation timed out after 5001 milliseconds with 0 bytes received
```



windows下的cmd

```bash
curl -i --http1.1 --max-time 5 -H "Connection: Upgrade" -H "Upgrade: websocket" -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" http://192.168.100.204:9621/


# windows换行输入用 ^
curl -i --http1.1 --max-time 5 ^
  -H "Connection: Upgrade" ^
  -H "Upgrade: websocket" ^
  -H "Sec-WebSocket-Version: 13" ^
  -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" ^
  http://192.168.100.204:9621/
```

预期返回：

```
C:\Users\admin>curl -i --http1.1 --max-time 5 -H "Connection: Upgrade" -H "Upgrade: websocket" -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" http://192.168.100.204:9621/
HTTP/1.1 101 Switching Protocols
connection: Upgrade
date: Tue, 18 Aug 2026 16:43:15 GMT
sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
server: Cowboy
upgrade: websocket

curl: (28) Operation timed out after 5001 milliseconds with 0 bytes received
```



返回 `101` 就证明整条集成链已生效：

```text
Ranch TCP/TLS
  -> Cowboy HTTP
  -> Cowboy Router
  -> websocket_handler:init/2
  -> cowboy_websocket 升级
```

如果返回：

- `404`：路由或访问路径不正确，WebSocket 路径必须是 `/`
- `400`：Upgrade Header 不完整
- TLS 错误：证书、私钥或协议配置异常
- Connection refused：Listener 没启动或端口错误
- `500`/连接立即断开：检查 `websocket_handler:init/2` 初始化失败





## 原因以及流程

Q: 为什么添加cowboy的依赖，并且实现某些接口就可以工作了，其流程是如何实现的?

A: 之所以「加个依赖、写几个 handler 就能工作」，是因为 **Cowboy + Ranch 把「TCP 监听 → HTTP 解析 → 路由分发 → 升级 WebSocket → 回调你的模块」这条链路上所有脏活都封装好了**，你只填了最上层的一小块业务逻辑



### 一、Cowboy 本质是一套 **基于 Ranch（TCP 连接管理库）的 Erlang HTTP 服务框架**。

HTTP、WebSocket 底层都是 TCP 长连接；

**你只需要实现特定行为（behaviour）回调函数，Cowboy 在合适时机自动调用你的代码，这就是 “实现接口就能处理请求” 的本质。**



整个链条分层：

```text
操作系统TCP → Ranch(acceptor接收连接) → Cowboy(HTTP解析/协议升级) → 你的Handler模块
```



### 二、分层拆解完整流程

1、底层：Ranch 负责 TCP 连接

Cowboy 2.x 依赖 ranch：

1. `cowboy:start_clear()` 内部会创建 ranch listener；
2. 启动一批 acceptor 进程，持续 `accept()` 等待客户端 TCP 连接；
3. 客户端发起 TCP 握手 → 操作系统建立 socket → ranch 拿到连接，交给 cowboy 协议处理进程。

   此时只是裸 TCP，还不是 HTTP，更不是 WebSocket。



2、Cowboy 处理 HTTP 协议（普通 HTTP 接口流程）

客户端发送标准 HTTP 请求报文：

```http
GET /api/hello HTTP/1.1
Host: 127.0.0.1:8080
```

流程：

1. Cowboy 进程读取 TCP 数据流，**解析 HTTP 请求头、method、path、query、body**；
2. 拿请求路径去匹配 `cowboy_router` 路由表；
3. 找到对应的模块：`game_http_handler`；
4. Cowboy 检测到该模块实现了 `cowboy_handler` behaviour；
5. **自动调用：`game_http_handler:init(Req0, State)`**

```erlang
-behaviour(cowboy_handler).
-export([init/2]). % 这就是必须实现的唯一接口
```

- `Req0`：Cowboy 封装的请求对象（路径、header、body、响应工具）
- 你在 init 里调用 `cowboy_req:reply()` 构造响应；
- Cowboy 把响应序列化成 HTTP 二进制，通过 TCP 发回客户端；
- HTTP 短连接默认：响应发送完成 → TCP 连接关闭。

这就是：**实现 cowboy_handler 就能写 HTTP 接口**。



3、Cowboy 处理 HTTP 协议（普通 HTTP 接口流程）

重点：**WebSocket 建立连接第一步依然是 HTTP 请求！**

客户端先发送一条特殊 HTTP 请求，带上升级头：

```http
GET /ws HTTP/1.1
Host: 127.0.0.1:8080
Connection: Upgrade
Upgrade: websocket
Sec-WebSocket-Key: xxxxx
```



### 完整升级流程

1. 客户端发起 HTTP Upgrade 请求；

2. Cowboy 路由匹配到 `game_ws_handler`；

3. 调用 `game_ws_handler:init(Req0, State)`；

4. 你的代码返回：

   ```erlang
   {cowboy_websocket, Req0, State}
   ```

   这是告诉 Cowboy：不要走普通 HTTP 响应，执行协议升级！

5. Cowboy 组装 `101 Switching Protocols` 响应发回客户端；

6. TCP 通道**不再使用 HTTP 协议**，切换成 WebSocket 二进制 / 文本帧协议；

7. 协议升级成功后，Cowboy 开始进入 WebSocket 生命周期，持续调用你实现的 WS 相关回调：

```erlang
-behaviour(cowboy_websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
```



### WebSocket 建立之后的消息流转

1. `websocket_init(State)`：升级成功后第一时间调用（初始化玩家会话）

2. 客户端发 WebSocket 帧 → Cowboy 解析帧 → 调用 `websocket_handle({text,Data}, State)`

3. Erlang 服务内部进程（游戏逻辑进程）向 WS 进程发普通 Erlang 消息

   比如 

   ```erlang
   Pid ! {push, <<"xxx">>
   ```

   → 触发 

   ```erlang
   websocket_info({push, Bin}, State)
   ```

   ，你可以在这里主动推送消息给前端

4. 客户端断开 / 异常 → `terminate/3` 执行，清理资源

> 关键点区分：
>
> - `websocket_handle`：**来自客户端的消息**
> - `websocket_info`：**Erlang 集群内部其他进程发给这个 WS 进程的消息**（游戏推送核心）



### 执行顺序直观流程：

###### HTTP 流程

````text
客户端 TCP 连接 → Cowboy 解析 HTTP → 路由匹配 → `Mod:init(Req, State)` → 回复 reply → 连接关闭
````

###### WebSocket 流程

```text
客户端 TCP 连接 → Cowboy 解析 HTTP (Upgrade 请求) → 路由匹配 → `Mod:init()` 返回 `{cowboy_websocket,...}`

→ 回复 101 切换协议 →

→ `websocket_init()`

→ 循环：

客户端消息 → `websocket_handle/2`

内部 Erlang 消息 → `websocket_info/2`

→ 断开 → `terminate/3`
```

极简时序图（文本版 WebSocket）

```
【客户端】                     【Cowboy】                  【你的WS Handler】
    |                            |                           |
    |---- TCP三次握手 ----------> |                           |
    |                            |                           |
    |---- HTTP Upgrade请求 -----> |                           |
    |                            | 路由匹配找到模块           |
    |                            |------> init(Req0, State)   |
    |                            | <------ {cowboy_websocket, ...}
    |<---- HTTP/1.1 101响应 -----|                           |
    |                            |------> websocket_init(State)
    | (协议切换完成)              |                           |
    |---- WS文本帧消息 ---------> |                           |
    |                            |------> websocket_handle() |
    |                            |                           |
    |                            | 其他游戏进程 ! {push, Bin} |
    |                            |------> websocket_info()   |
    |<---- WS推送帧 --------------|                           |
    | 关闭连接                   |                           |
    |                            |------> terminate()        |
```



### Erlang项目集成cowboy工作流程

之所以「加个依赖、写几个 handler 就能工作」，是因为 **Cowboy + Ranch 把「TCP 监听 → HTTP 解析 → 路由分发 → 升级 WebSocket → 回调你的模块」这条链路上所有脏活都封装好了**，你只填了最上层的一小块业务逻辑。下面把黑盒拆开，按时间顺序走一遍。



#### 一、为什么「加依赖」就够了：Application 启动链

在 `rebar.config` 里加 `{cowboy, "2.x"}`，cowboy 又依赖 `ranch` 和 `cowlib`。

当你游戏节点 `application:start(your_game_app)`（或 release 自动启动）时：

1. OTP 读 `your_game_app.app`，看到 `applications: [kernel, stdlib, ranch, cowboy, ...]`
2. 先启动 `ranch` → 起 `ranch_sup` → 起 `ranch_server`（用 ETS 存 listener 配置）
3. 再启动 `cowboy` → 起 `cowboy_sup`（很薄，主要挂 `cowboy_clock`）
4. 最后执行你 `your_game_app:start/2` 里的 `cowboy:start_clear(...)`

> 所以「加依赖」的本质是：**把 ranch 的 TCP 池和 cowboy 的 HTTP/WS 状态机送进同一个 VM**，你不用自己写 `gen_tcp:listen/accept`。



#### 二、`cowboy:start_clear` 背后发生了什么

你这行代码：

```erlang
cowboy:start_clear(game_listener, [{port,9000}], #{env => #{dispatch => Dispatch}}).
```

内部等价于：

```text
cowboy:start_clear
  └─ ranch:start_listener(game_listener, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts)
       └─ supervisor:start_child(ranch_sup, ranch_listener_sup)
            ├─ ranch_acceptors_sup  （默认 10 个 ranch_acceptor 进程）
            └─ ranch_conns_sup      （管连接进程）
```

- `ranch_tcp`：实现 `ranch_transport`，包了 `gen_tcp`
- `cowboy_clear`：实现 `ranch_protocol`，代表「用 HTTP 说话」
- `ranch_acceptor`：阻塞在 `Transport:accept(LSocket)`，等客户端连

此时 **端口 9000 已经被 Ranch 拿着了**，但还没人解析 HTTP。



#### 三、一个客户端连进来：从 socket 到你的 `init/2`

1. TCP 接受阶段（Ranch 管）

- 客户端 TCP 握手
- 某个 `ranch_acceptor` 拿到 `Socket`，把 socket 控制权 `controlling_process` 转给 `ranch_conns_sup`
- 发 `{?MODULE, start_protocol, self(), Socket}` 给 conns_sup
- acceptor 自身继续回去 `accept` 下一个连接（不阻塞业务）



2. 起连接进程（Cowboy 接手）

```
ranch_conns_sup` 调 `cowboy_clear:start_link(Ref, Socket, ranch_tcp, ProtoOpts)
```

→ `proc_lib:spawn_link` 出 **一个连接进程**

→ `ranch:handshake` 完成后，`cowboy_http:init` 进入接收循环：

```erlang
Transport:setopts(Socket, [{active, once}]),
loop(State)
```

`socket` 上有数据 → 以 `{tcp, Socket, Data}` 消息形式发到这个连接进程。



3. HTTP 请求解析

`cowboy_http` 状态机：

- 解析请求行 + 头 → 构造 `#{method, host, path, headers, ...}` 的 `Req`

- 走 

  Middleware 管道

  （默认两条）：

  1. `cowboy_router`：拿 `path` 去匹配你 `cowboy_router:compile` 出来的 `Dispatch`，查出 `{Handler, Opts}`
  2. `cowboy_handler`：动态起一个**临时请求进程**，调 `Handler:init(Req, Opts)`

到这里，**终于进到你写的代码**。



#### 四、普通 HTTP 接口为什么「写个 handler 就行」

```erlang
-module(game_http_handler).
-export([init/2]).
init(Req0, State) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => "application/json"}, <<"{\"ok\":1}">>, Req0),
    {ok, Req, State}.
```

流程是：

- 请求进程跑 `init/2`
- 你调 `cowboy_req:reply/4` → 不是直接发 socket，而是给**连接进程**发消息 `{response, ...}`
- 连接进程（还在 `cowboy_http:loop`）收消息 → `cow_http` 拼报文 → `Transport:send(Socket, ...)`
- 请求进程退出，连接进程保持 keep-alive 等下一个请求 

**你完全没碰 socket、没碰 TCP、没碰 HTTP 报文格式**——这些 cowlib/cowboy_http 全做了。



#### 五、WebSocket 为什么「返回 `{cowboy_websocket, Req, State}` 就升级了」

WS 的特殊点在：`init/2` 不是终点，而是**协议切换申请**。

```erlang
init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.
```

连接进程里的 `cowboy_http` 看到这个返回值：

1. 检查请求头是不是 `Connection: Upgrade` + `Upgrade: websocket` + `Sec-WebSocket-Key`
2. 算 `Sec-WebSocket-Accept`（SHA1+base64 那个魔法串）
3. 发 `101 Switching Protocols`
4. **把同一个连接进程从 `cowboy_http` 状态机切到 `cowboy_websocket` 状态机**（不建新 socket，不新建 TCP）
5. 之后这个进程不再走 router/middleware，而是直接调：
   - `websocket_init/1`（在连接进程里，所以 `self()` 就是 WS 进程）
   - `websocket_handle/2`（收到客户端帧）
   - `websocket_info/2`（收到 Erlang 消息，比如你 `player_server` 发来的 `{push, Bin}`）
   - `terminate/3`

所以你前面写的 `game_ws_handler` 能直接 `WsPid ! {push, Bin}` 推数据，是因为 **WS 进程本身就是个普通 Erlang 进程，只是被 cowboy_websocket 接管了收发包逻辑**。



#### 六、一张总图（你填的和 Cowboy 填的）

```text
你写的代码                     Cowboy/Ranch 写的代码
─────────────                  ──────────────────────────────
rebar.config                   ranch_sup / ranch_server(ETS)
  ↓ deps                       cowboy_sup
your_app:start/2               ranch_listener_sup
  └─ cowboy:start_clear         ├─ ranch_acceptors_sup ×10  (gen_tcp:listen/accept)
      └─ Dispatch 编译好         ├─ ranch_conns_sup
                                 └─ 每连接一个 cowboy_clear/cowboy_http 进程
cowboy_router:compile            cow_http 解析报文
game_http_handler:init/2  ◀──── cowboy_router 匹配路径
game_ws_handler:init/2          cowboy_http 做 101 握手
  └─ {cowboy_websocket,...} ──▶ cowboy_websocket 状态机
game_ws_handler:websocket_*     cow_ws 帧编解码
player_server ──cast──▶ WsPid   Transport:send/recv
```

**你只写了：路由表 + handler 回调 + 游戏侧 gen_server。**

**Cowboy 写了：TCP 池、HTTP/1.1 解析、router 中间件、WS 握手与帧解析、背压/超时/keepalive。**

