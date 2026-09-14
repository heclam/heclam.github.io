---
title: erlang多进程编译
date: 2026-08-19 19:24:45
toc: true
categories:
  - erlang
tags:
  - erlang
---





#### 多进程编译代码

windows脚本下打开./ctl.bat脚本执行mmake (实际执行的流程)

```bat
:fun_mmake
	echo 编译中......
    rem echo A | xcopy %DIR_TPL_APP%\* /d /r /k %DIR_EBIN%\
    rem if not exist %DIR_LOG% md %DIR_LOG%
	rem svn update 
	echo %cd%
	erl -pa "%DIR_EBIN%" -pa "%DIR_EBIN%\mod" -noshell -eval "mmake:all(50), init:stop()"
	goto where_to_go
```

注意脚本的`-pa` 路径附加不能用下面的`%DIR_EBIN%` ,因为指向的是`F:\xnh5\server\ebin`,里面还有两个文件夹`deps`和`mod`， 如果用下面的附加路径，实际只是加入了`ebin`, 不会递归搜索`ebin/mod`

```shell
erl -pa %DIR_EBIN% -noshell -eval "mmake:all(50), init:stop()"
```

如果使用下面执行错误的附加路径,那么每次用的都是系统自带的来编译源码

```tex
D:/Program Files/erl8.3/lib/tools-2.9.1/ebin/mmake.beam
```



#### 编译脚本执行过程如下：

1、启动Erlang VM

2、`%DIR_EBIN%`中找不到`mmake.beam`

3、Erlang继续在 OTP 默认 code path 中查找

4、找到 OTP 自带的 `tools-2.9.1/ebin/mmake.beam`

5、OTP 的 `mmake:all(50)` 读取项目根目录的 `Emakefile`

6、编译所有源码，包括项目中`mmake.erl` 

7、重新生成 `ebin/mod/mmake.beam`



#### mmake.erl多进程的编译原理：

1、结合Emakefile配置文件，里面

```
{
	[
		"src/*",
		"src/*/*",
		"src/*/*/*",
		"src/*/*/*/*",
		"src/*/*/*/*/*",
		"src/*/*/*/*/*/*"
	]
	,[
		debug_info
		,show_errors
		,show_warnings
		%% ,nowarn_deprecated_function
		,{hipe, [o3]}
		%%,{debug_info_key, "abc"}
		%% ,encrypt_debug_info
		%%,{d, debug}
		%%,{d, wwc_debug}
		%%,{d, debug_socket_save}
		%%,{d, maps_support}
		%%,{d, debug_socket}
		%% ,{d, debug_log_print}
		,{i, "include"}
		,{outdir, "ebin/mod"}
	]
}.
```

包含两部分的内容,

​	第一部分为源码的通配路径列表

​	第二部分为编译的参数列表,编译源码(xxx.beam)输出目录



2、先根据配置获取所所有的源码xxx.erl文件,判断编译后的文件(xxx.beam)是否存在配的outdir, 如ebin/mod/player.beam

* 存在 

  判断player.beam与player.erl文件的最后写入时间是否有差异，有差异则重新编译(若没有差异还需要判断Emakefile的编译参数是否有变更,有也需要重新编译），否则不编译

* 不存在

  则编译

