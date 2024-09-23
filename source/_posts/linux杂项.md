---
layout: article
title: linux杂项
date: 2024-03-18 14:30:41
toc: true
categories:
	- linux
tags:
	- linux
    - screen
---

1、linux绑定ip地址：
https://blog.csdn.net/nyist_zxp/article/details/131338441

查看可绑定的网段ip

<!--more-->

2、screen 使用（idserver的启动）

```
# 显示目前所有的screen终端
screen -ls
或
screen -list

#新建终端
screen -S screenName(终端名称)

#执行完指定命令后

#使用快捷键Ctrl+A+D,关闭窗口即可实现程序的后台运行

# 重现窗口(回到终端)
screen -r screenName(终端名称)

#


例子：（idServer后台运行）
cd /data/idServer
screen  -S idServer		// 新建终端
./idserver &			// 执行启动命令
使用快捷键Ctrl+A+D,进行退出

screen -list 可查看
```

