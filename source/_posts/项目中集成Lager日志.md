---
title: 项目中集成Lager日志
date: 2026-08-11 16:46:40
toc: true
categories:
  - erlang
  - lager
tags:
  - lager
---



#### 1、项目中添加依赖:

如果有内部镜像或需要指定gif仓库

```erlang
{deps,
  [
    {lager, "2.13.0", {git, "https://github.com/basho/lager.git", {tag, "2.13.0"}}}
  ]}.
```

或者:

获取将lager里面的源码添加进来(不用每次编译的话,将编译后的代码添加进ebin目录即可)



#### 2、配置lager（如config/sys.config文件）

```erlang
[
...

{sasl, [
        %% 关闭SASL日志
        {sasl_error_logger, false}]
	},

    {lager, [
        {log_root, "./logs"},
        {handlers, [
                %%是否显示在console
                {lager_console_backend, [
                    {level, debug},
                    {formatter, lager_default_formatter},
                    {formatter_config,["## ", severity, " ", date," ",time," ","[",{module,"none"},":",{line,"0"},"]",message,"\n"]}
                    ]},
                %% 奔溃日志
                {lager_file_backend, [
                    {file, "critical.log"},
                    {level, critical},
                    %% 单个文件最大200M，每天00:00滚动，最多5个文件
                    {size, 209715200}, {date, "$D0"}, {count, 5},
                    {formatter, lager_default_formatter},
                    {formatter_config,["## ", severity, " ", date," ",time," ","[",{module,"none"},":",{line,"0"},"]",message,"\n"]}
                    ]},
                %% 错误日志
                {lager_file_backend, [
                    {file, "error.log"}, 
                    {level, error},
                    %% 单个文件最大200M，每天00:00滚动，最多5个文件
                    {size, 209715200}, {date, "$D0"}, {count, 5},
                    {formatter, lager_default_formatter},
                    {formatter_config,["## ", severity, " ", date," ",time," ","[",{module,"none"},":",{line,"0"},"]",message,"\n"]}
                    ]},
                %% 打印日志
                {lager_file_backend, [
                    {file, "info.log"}, 
                    {level, info},
                    %% 单个文件最大200M，每天00:00滚动，最多5个文件
                    {size, 209715200}, {date, "$D0"}, {count, 5},
                    {formatter, lager_default_formatter},
                    {formatter_config,["## ", severity, " ", date," ",time," ","[",{module,"none"},":",{line,"0"},"]",message,"\n"]}
                    ]}
            ]},

            %% Crash日志
            {crash_log, "crash.log"},
            {crash_log_msg_size, 65536},
            {crash_log_size, 209715200},
            {crash_log_date, "$D0"},
            {crash_log_count, 5},
            %% Mailbox中累计多少消息时切换到同步方式
            {async_threshold, 20},
            %% 每秒钟最多从error_logger接收多少条消息
            {error_logger_hwm, 200},
            %% 重定向error_logger的错误消息
            {error_logger_redirect, true}
        ]},
    
    ...
]
```





#### 3、项目里面启动lager日志

```erlang
lager:start().

lager:set_loglevel(lager_console_backend, debug). % 设置日志级别为debug
```



#### 4、使用

```erlang
% 记录不同级别的日志
lager:debug("This is a debug log"),
lager:info("This is an info log"),
lager:error("This is an error log").
```

