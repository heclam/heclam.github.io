---
layout: article
title: erlang日志
date: 2024-03-14 14:30:42
toc: true
categories:
	- erlang
tags:
	- erlang
---



#### 日志与事件处理

SASL（System Architectrue Support Libraries (系统架构支持库))，它负责错误日志、过载保护等。

它表示的是身为Erlang/OTP五大基础应用(erts、kernel、stdlib、sasl和compiler)之一的系统架构支持库。

日志级别通常分为5个，分别是：critical(或server)、error、warning、info和debug

Erlang/OTP的基本发行版本便提供了日志功能支持。其主要功能由标准库中kernel应用的`error_logger`模块提供，供OTP行为模式使用的扩展日志功能则由SASL应用提供。

<!--more-->

##### 1、标准日志函数

```erlang
error_logger:error_msg(Format) -> ok.
error_logger:error_msg(Format, Data) -> ok.

error_logger:warning_msg(Format) -> ok.
error_logger:warning_msg(Format, Data) -> ok.

error_logger:info_msg(Format) -> ok.
error_logger:info_msg(Format, Data) -> ok.
```



##### 2、用gen_event编写自定义事件处理器

gen_event行为模式接口与gen_server类似：如其中的init、code_change 和 terminate 回调函数以及handle_call 和handle_info回调（在参数和返回值方面有所差异），gen_event 接口用 handle_event/2取代了handle_cast/2这个正是接收错误日志事件的地方。

gen_event 和 gen_server 之间的一个重要区别在于当启动新的gen_server容器时，需要告诉它应该使用那个回调模块；但在启动gen_event容器（也称为事件管理器）时，起初是无须任何回调模块的。相反，在容器完成初始化之后，可以动态添加或删除一个或多个处理器。当事件被投递至事件管理器时，事件管理器会调用当前已注册的所有处理器模块来处理事件。

