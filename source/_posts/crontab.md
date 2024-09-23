---
layout: article
title: contab基础用法
date: 2024-03-16 14:30:41
toc: true
categories:
	- contab
tags:
	- contab
---

 

语法：

```
crontab [-u <用户名称>][配置文件] 或 crontab { -l | -r | -e }
-u   #<用户名称> 是指设定指定<用户名称>的定时任务，这个前提是你必须要有其权限(比如说是 root)才能够指定他人的时程表。如果不使用 -u user 的话，就是表示设定自己的定时任务。
-l 　#列出该用户的定时任务设置。
-r 　#删除该用户的定时任务设置。
-e 　#编辑该用户的定时任务设置。
```

<!--more-->

命令时间格式 :

```
*     * 　  *　  *　  *　　command
分　  时　  日　  月　 周　  命令
第1列表示分钟1～59 每分钟用*或者 */1表示
第2列表示小时1～23（0表示0点）
第3列表示日期1～31
第4列表示月份1～12
第5列标识号星期0～6（0表示星期天）
第6列要运行的命令
 
```

一些Crontab定时任务例子:

```
30 21 * * * /usr/local/etc/rc.d/lighttpd restart  #每晚的21:30 重启apache
45 4 1,10,22 * * /usr/local/etc/rc.d/lighttpd restart  #每月1、10、22日的4 : 45重启apache
10 1 * * 6,0 /usr/local/etc/rc.d/lighttpd restart  #每周六、周日的1 : 10重启apache
0,30 18-23 * * * /usr/local/etc/rc.d/lighttpd restart  #每天18 : 00至23 : 00之间每隔30分钟重启apache
0 23 * * 6 /usr/local/etc/rc.d/lighttpd restart  #每星期六的11 : 00 pm重启apache
* 23-7/1 * * * /usr/local/etc/rc.d/lighttpd restart  #晚上11点到早上7点之间，每隔一小时重启apache
* */1 * * * /usr/local/etc/rc.d/lighttpd restart  #每一小时重启apache
0 11 4 * mon-wed /usr/local/etc/rc.d/lighttpd restart  #每月的4号与每周一到周三的11点重启apache
0 4 1 jan * /usr/local/etc/rc.d/lighttpd restart  #一月一号的4点重启apache
 
*/30 * * * * /usr/sbin/ntpdate cn.pool.ntp.org  #每半小时同步一下时间
0 */2 * * * /sbin/service httpd restart  #每两个小时重启一次apache 
50 7 * * * /sbin/service sshd start  #每天7：50开启ssh服务 
50 22 * * * /sbin/service sshd stop  #每天22：50关闭ssh服务 
0 0 1,15 * * fsck /home  #每月1号和15号检查/home 磁盘 
1 * * * * /home/bruce/backup  #每小时的第一分执行 /home/bruce/backup这个文件 
00 03 * * 1-5 find /home "*.xxx" -mtime +4 -exec rm {} \;  #每周一至周五3点钟，在目录/home中，查找文件名为*.xxx的文件，并删除4天前的文件。
30 6 */10 * * ls  #每月的1、11、21、31日是的6：30执行一次ls命令
```

