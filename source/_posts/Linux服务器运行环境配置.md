---
layout: article
title: Linux服务器运行环境配置
date: 2019-09-7 18:19:37
toc: true
categories:
	- 教程
tags:
	- JDk
	- MySQL
	- Tomcat
	- Nginx
---



一台云服务器需要搭建的运行环境，包括JDK、MySQL、Tomcat、Nginx等，该服务器（CentOS7）用于部署JavaEE项目

#### 一、JDK安装

##### 卸载自带的openJDK

​		因为centos有自带的jdk，所以在安装自己的JDK时需要卸载自带的openJDK

1. 使用***rpm -qa | grep java***命令查看系统中是存在有java
2. 使用***rpm -e --nodeps 相关应用名称***来进行卸载

```
[root@localhost ~]# rpm -qa | grep java
java-1.8.0-openjdk-headless-1.8.0.161-2.b14.el7.x86_64
tzdata-java-2018c-1.el7.noarch
python-javapackages-3.4.1-11.el7.noarch
javapackages-tools-3.4.1-11.el7.noarch
java-1.8.0-openjdk-1.8.0.161-2.b14.el7.x86_64
[root@localhost ~]# rpm -e --nodeps java-1.8.0-openjdk-headless-1.8.0.161-2.b14.el7.x86_64
[root@localhost ~]# rpm -e --nodeps java-1.8.0-openjdk-1.8.0.161-2.b14.el7.x86_64
```

##### 下载并安装JDK

1. 下载ORAClE所提供的JDK,[传送门](https://www.oracle.com/java/technologies/jdk8-downloads.html)可根据自己的系统来进行选择相应的版本，我这选择的是***jdk-8u221-linux-x64.tar.gz***
2. 从本地上传文件到服务器，需要安装***Irzsz***: yum -y install lrzsz (注：参数-y中"y"的意思是：当安装过程提示选择全部为"yes")
3. rz中的r意为received（接收），输入rz时、意为服务器接收文件，既将文件从本地上传到服务器。sz中的s意为send（发送），输入sz时、意为服务器要发送文件，既从服务器发送文件到本地，或是说本地从服务器上下载文件。注：不论是send还是received，动作都是在服务器上发起的。
4. 将文件上传到***/usr/java***目录下，然后使用***tar -zxvf jdk-8u221-linux-x64.tar.gz***进行解压

##### 修改profile配置文件

使用***vi /etc/profile***打开，并加入以下内容（注：[传送门](https://vimjc.com/)不熟悉vi可以先去学习以下基本操作）

```java
export JAVA_HOME=/usr/java/jdk1.8.0_221(这个根据你解压之后的名称来定)
export CLASSPATH=.:$JAVA_HOME/jre/lib/rt.jar:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar
export PATH=$PATH:$JAVA_HOME/bin
```

保存退出之后需要运行***source /etc/profile***使配置生效

最后执行***java -version***验证是否安装成功

```
java version "1.8.0_221"
Java(TM) SE Runtime Environment (build 1.8.0_221-b11)
Java HotSpot(TM) 64-Bit Server VM (build 25.221-b11, mixed mode)
```



#### 二、MySQL安装

##### 卸载之前安装的mysql

1. 使用以下命令查看当前安装mysql的情况，看是否有安装mysql

    ```
      rpm -qa | grep -i mysql
    ```

2. 停止mysql服务，删除之前安装的mysql

    ```
      rpm -e --nodeps mysql(mysql表示为相关的应用名称)
    ```

3. 查找之前老版本mysql的目录、并且删除老版本的文件和库

   ```
      find / -name mysql
      
      删除对应的mysql目录
      rm -rf  /var/lib/mysql(这个是由上述命令查询出来的)
      ...
      
      还要手动删除/etc/my.cnf文件
      rm -rf /etc/my.cnf
   ```

4. 再次验证机器是否由安装mysql

    ```
      rpm -qa | grep -i mysql
    ```

##### 安装mysql

```
wget http://repo.mysql.com/mysql-community-release-el7-5.noarch.rpm
rpm -ivh mysql-community-release-el7-5.noarch.rpm
yum install mysql-server
```

权限设置：

```
chown mysql:mysql -R /var/lib/mysql
```

初始化MySQL:

```
mysqld --initialize

如果报 Fatal error: Please read "Security" section of the manual to find out how to run mysqld as root!错误信息则在/etc/my.cnf中添加一个用户，比如user=mysql
那么在启动的时候就需要：mysqld --initialize --user=mysql来初始化
```

启动MySQL:

```
systemctl start mysqld
```

查看MySQL运行状态：

```
systemctl status mysqld
```

##### 验证MySQL是否安装

```
[root@localhost ~]# mysqladmin --version
如果出现以下内容则证明安装成功，否则则为失败
mysqladmin  Ver 8.42 Distrib 5.6.45, for Linux on x86_64
```

##### 使用MySQL Client(Mysql客户端)执行简单的SQL命令

```
[root@localhost ~]# mysql
就会出现
mysql>  这里输入平常的sql命令即可
```

##### 为root用户添加密码

```
[root@localhost ~]# mysqladmin -u root password "新密码";
那么就可以通过以下命令来连接Mysql服务器
[root@localhost ~]# mysql -u root -p
```



#### 三、Tomcat安装

1. 需要先下载tomcat,[传送门](http://tomcat.apache.org/)，我下载的是apache-tomcat-9.0.24.tar.gz

2. 将这个文件上传到/usr/local/tomcat里面进行解压

3. 启动tomcat

   ```
   切换到以下目录
   cd /usr/local/tomcat/apache-tomcat-9.0.24/bin
   执行
    ./startup.sh
   ```

可以通过***ip addr***查看linux的ip地址，然后在本机上输入*** ip地址：8080 ***



#### 四、nginx安装

需要安装几个必备的库

```
yum install gcc-c++   //安装gcc环境
yum install -y pcre pcre-devel
yum install -y openssl openssl-devel
```

##### 安装

上传本地的压缩包到/usr/local目录进行解压，我使用的是***nginx-1.12.2.tar.gz***,可以到[传送门](https://nginx.org/download/)下载

```
解压：
tar -zxvf nginx-1.12.2.tar.gz
创建Makefile文件：
[root@localhost ~]# cd nginx-1.12.2
[root@localhost nginx-1.12.2]# ./configure
编译：
[root@localhost nginx-1.12.2]# make
安装：
[root@localhost nginx-1.12.2]# make install
```

##### Nginx启动与访问

编译安装的Nginx其实默认被安装在CentOS7系统的***/usr/local/nginx***目录

```
启动Nginx(到/usr/local/nginx目录下)
[root@localhost nginx]# cd sbin
[root@localhost sbin]# ./nginx
```

在本地输入服务器的地址 即可访问到Nginx欢迎界面，可以通过 ***ip addr***获取到地址

如果访问不到则关闭CentOS7的防火墙，开放80端口

```
[root@localhost sbin]# systemctl stop firewalld.service #停止firewall
[root@localhost sbin]# systemctl disable firewalld.service #禁止firewall开机启动
[root@localhost sbin]# firewall-cmd --state #查看默认防火墙状态（关闭后显示notrunning，开启后显示running）
```

##### Nginx的基本操作

```
查看Nginx进程：
[root@localhost sbin]# ps aux|grep nginx

关闭Nginx:
[root@localhost sbin]# ./nginx -s stop
或：[root@localhost sbin]# ./nginx -s quit

重启Nginx:
[root@localhost sbin]# ./nginx -s reload

检查Nginx配置文件是否正确：
[root@localhost sbin]# ./nginx -t
```

