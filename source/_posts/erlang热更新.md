---
title: erlang热更新
date: 2026-08-19 18:34:25
toc: true
categories:
  - erlang
tags:
  - erlang
---





项目热更新原理

#### 1、节点启动的时候,启动一个代码管理进程

```erlang
ets:new(sys_code, [named_table, public, set]),
```

创建了一个ets表保存每个源码文件的对应的md5,保存格式为

```
{filename, 文件md5值}
例如
[{yy_pay_new_year_dao,"cd472001938028a778ad82231ea2e104"},
 {db_mnesia_info,"2a440fe51e5b1519c6f7e7f80ee39672"},
 ....
 ]
```

获取文件md5的方法

先获取/ebin/mod目录下所有的xxx.beam文件，通过以下方法即可获取文件md5的值

```Erlang
do_beam_hash([], List) -> List;
do_beam_hash([N | T], List) ->
    L = case beam_lib:md5(config:get_code_path() ++ "/ebin/mod/" ++ N) of
            {ok, {M, Md5}} ->
                [{M, util:md5(Md5)} | List];
            Err ->
                ?ERR("无法获取文件[~s]的hash值:~w", [N, Err]),
                List
        end,
    do_beam_hash(T, L)
```



#### 2、热更新分为两种

热更的原理是获取文件最新的hash值跟ets里面的hash值对比，若不相同则执行热更操作

##### 2.1 软更新

```erlang
load_beam_soft_purge(Mod, Hash) ->
    case code:soft_purge(Mod) of
        true ->
            case code:load_file(Mod) of
                {module, _} ->
                    ets:insert(sys_code, {Mod, Hash}),
                    ?INFO("# 加载模块成功: ~p~n", [Mod]),
                    true;
                {error, Why} ->
                    ?ERR("* 加载模块失败[~p], Why:~p~n", [Mod, Why]),
                    false
            end;
        _ ->
            ?ERR("* 加载模块失败[~p], Hash:~p~n", [Mod, Hash]),
            false
    end.
```

##### 2.2 硬更新

```erlang
load_beam_purge(Mod, Hash) ->
    case code:purge(Mod) of
        true ->
            case code:load_file(Mod) of
                {module, _} ->
                    ets:insert(sys_code, {Mod, Hash}),
                    ?INFO("# 加载模块成功: ~p~n", [Mod]),
                    true;
                {error, Why} ->
                    ?ERR("* 加载模块失败[~p], Why:~p~n", [Mod, Why]),
                    false
            end;
        _ ->
            ?ERR("* 加载模块失败[~p], Hash:~p~n", [Mod, Hash]),
            false
    end.
```

