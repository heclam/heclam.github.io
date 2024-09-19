---
layout: article
title: linux基础服务搭建
date: 2024-05-11 11:19:37
toc: true
categories:
	- linux
tags:
	- linux
---


```shell
# 基础环境
CentOS7

# 网络工具 （虚拟机初始安装的时候没有这个，需要安装）
yum search ifconfig
yum install net-tools.x86_64
# 然后才可以使用 ifconfig	（网卡）

# 安装远程同步工具
yum install -y rsync

yum install -y libnuma.so.1 numactl 

# 创建用户  
groupadd www
useradd www -s /sbin/nologin -M -g www
  
```

<!--more-->

# 多实例MySql(5.6.13   linux-Generic)

```shell
# 安装依赖
yum install libaio
# mysql下载地址
https://downloads.mysql.com/archives/community/

# 将下载的文件上传到 /usr/local/src/ 目录下
mysql-5.6.13-linux-glibc2.5-x86_64.tar.gz

# 不需要卸载自带的mariadb

#创建用户和用户组
groupadd mysql
useradd mysql -s /sbin/nologin -M -g mysql

# 解压安装包（将安装包解压到/usr/local/mysql56/ 目录下）
tar -zxvf mysql-5.6.13-linux-glibc2.5-x86_64.tar.gz -C /usr/local

# 重新命名
cd /usr/local/
mv mysql-5.6.13-linux-glibc2.5-x86_64/ mysql56

# 建立一个软连接
ln -s /usr/local/mysql56 /usr/local/mysql

```

#  安装 Mongodb（3.6.12） (使用tar包方式)

```shell
# 去官网地址
https://www.mongodb.com/download-center/community/releases/archive

# 选择3.6.12版本下载
https://fastdl.mongodb.org/linux/mongodb-linux-x86_64-rhel70-3.6.12.tgz

# 上传到/usr/local/src/ 目录
# 执行解压命令 (一下合成一步)
tar -xvzf mongodb-linux-x86_64-rhel70-3.6.12.tgz  -C /usr/local/

# 重新命名
cd /usr/local/
mv mongodb-linux-x86_64-rhel70-3.6.12 mongodb-3.6.12
```



# 安装Nginx

```shell
# 查看nginx的configure
/usr/local/nginx/sbin/nginx -V

# 输出的内容为
nginx version: nginx/1.17.9
built by gcc 4.8.5 20150623 (Red Hat 4.8.5-44) (GCC) 
built with OpenSSL 1.0.2k-fips  26 Jan 2017
TLS SNI support enabled		
# 支持一个服务器使用多个域名证书的TLS扩展，支持用户配置多个域名证书
# 支持单IP多域名SSL证书需要OpenSSL支持
configure arguments: 
 --user=www		# 指定Nginx woker进程运行时所属的用户 
 --group=www	# 指定Nginx woker进程运行时所属的组
 --prefix=/usr/local/nginx		# Nginx安装部署后的根目录，默认为/usr/local/nginx
 # 安装http stub_status模块。该模块可以让运行中的Nginx提供性能统计页面，获取相关的并发链接
 --with-http_stub_status_module
 # 安装http ssl 模块。该模块使Nginx支持SSL协议，提供HTTPS服务。注意：该模块依赖openssl
 --with-http_ssl_module
 # 安装http realip模块。该模块可以从客户端请求的header信息（如X-Real-IP或者X-Forwarder-For)获取真正客户端的IP地址
 --with-http_realip_module
 # 安装http image_filter模块。这个模块将符合配置的图片实时压缩为指定大小的缩略图再发送给用户，目前支持JPEG、PNG、GIF格式。注意：该模块依赖libgb库
 --with-http_image_filter_module
 # 用来集成Google的高性能工具集(Google Performance tools 简称gperftools),这个工具集可以提供更好的内存和CPU性能分析。需要安装依赖 gperftools-libs gperftools-devel
 --with-google_perftools_module	
 --with-stream	# 开启TCP/IP代理模块
 --with-stream_ssl_module

# 安装前的准备
# 如果指定用户和用户组需要提前创建，如WWW用户

# 依赖包的安装
yum -y install gcc gcc-c++ autoconf automake libtool make cmake
yum -y install zlib zlib-devel openssl openssl-devel pcre-devel gd-devel google-perftools google-perftools-devel

# 有些参数设置需要的依赖包 (需要安装依赖才能configure成功)
# gd-devel (--with-http_image_filter_module 需要的依赖)
# google-perftools google-perftools-devel (--with-google_perftools_module 需要的依赖)

# 从官网下载nginx 稳定版本 并上传到服务器 /usr/local/src
https://nginx.org/en/download.html

# 解压
tar -zxvf nginx-1.26.0.tar.gz

# 进入nginx-1.26.0目录并执行 (编译配置、编译、安装)
./configure --prefix=/usr/local/nginx --user=www --group=www --with-http_stub_status_module --with-http_ssl_module --with-http_realip_module --with-http_image_filter_module --with-google_perftools_module --with-stream --with-stream_ssl_module

# 编译
make

# 安装
make install

# 查看安装后的程序版本
/usr/local/nginx/sbin/nginx -v

# 如果修改nginx配置,验证配置是否合法
/usr/local/nginx/sbin/nginx -t

# 启动nginx
/usr/local/nginx/sbin/nginx

# 重启nginx
/usr/local/nginx/sbin/nginx -s reload

# 停止nginx
/usr/local/nginx/sbin/nginx -s quit
/usr/local/nginx/sbin/nginx -s stop


# curl 访问localhost测试
curl localhost

# 对外访问需要开放端口（按需开放）
firewall-cmd --add-port=80/tcp --permanent 
firewall-cmd --add-port=443/tcp --permanent 
firewall-cmd --reload   #重载防火墙配置
```







# 安装php

```shell
# 查看php的configure (以下为配置路径文件)
/usr/local/php7.3.10/bin/php-config

# 安装前的准备
# 如果指定用户和用户组需要提前创建，如WWW用户 (configure会用到)

# 从官网上下载
https://www.php.net/releases/ 	# 选择7.3.10版本

# 下载链接
https://www.php.net/distributions/php-7.3.10.tar.gz

# 安装依赖库
yum -y install libxml2 libxml2-devel openssl openssl-devel curl-devel libjpeg-devel libpng-devel freetype-devel libmcrypt-devel bzip2-devel readline-devel
 
# 大部分依赖包(此项不执行)
 yum install -y wget gcc gcc-c++ autoconf libjpeg libjpeg-devel perl perl* perl-CPAN libpng libpng-devel freetype freetype-devel libxml2 libxml2-devel zlib zlib-devel glibc glibc-devel glib2 glib2-devel bzip2 bzip2-devel ncurses ncurses-devel curl curl-devel e2fsprogs e2fsprogs-devel krb5 krb5-devel libidn libidn-devel openssl openssl-devel openldap openldap-devel nss_ldap openldap-clients openldap-servers png jpeg autoconf gcc cmake make gcc-c++ gcc ladp ldap* ncurses ncurses-devel zlib zlib-devel zlib-static pcre pcre-devel pcre-static openssl openssl-devel perl libtoolt openldap-devel libxml2-devel ntpdate cmake gd* gd2 ImageMagick-devel jpeg jpeg* pcre-dev* fontconfig libpng libxml2 zip unzip gzip
 
 
# 上传源码安装包到/usr/local/src/目录,并解压
tar -zxvf php-7.3.10.tar.gz 
cd php-7.3.10

# 源码编译、安装
./configure --prefix=/usr/local/php7.3.10 --with-config-file-path=/usr/local/php7.3.10/etc/ --enable-fpm --enable-cli --enable-mbstring --enable-soap --enable-opcache --enable-pcntl --enable-shmop --enable-sysvmsg --enable-sysvsem --enable-sysvshm --enable-sockets --enable-zip --with-bz2 --with-readline --disable-fileinfo --disable-rpath --with-mysqli --with-pdo-mysql --with-iconv-dir --with-fpm-user=www --with-fpm-group=www --with-mhash --with-gd --with-jpeg-dir --with-png-dir --with-freetype-dir --with-zlib --enable-simplexml --with-libxml-dir --with-openssl --with-curl --enable-maintainer-zts
# 这里configure报错，看下面 configure报错解决

# 编译
make

# 对编译结果测试
make test

# 安装
make install

# 查看安装成功后的版本信息
/usr/local/php7.3.10/bin/php -v

# 修改配置
cp /usr/local/src/php-7.3.10/php.ini-production  /usr/local/php7.3.10/etc/php.ini
修改php.ini文件
display_errors = Off  修改为 display_errors = on

# 因为后续中央服需要mongodb的支持，所以这里需要添加响应的扩展
# 具体操作看以下php扩展
文件的最后面加上以下代码：
extension=redis.so
extension=fileinfo.so
extension=mongodb.so
;extension=sockets.so
;extension=openssl.so

# 启用php-fpm服务
cd /usr/local/php7.3.10/etc
cp php-fpm.conf.default php-fpm.conf
cp php-fpm.d/www.conf.default php-fpm.d/www.conf	# 基本这个配置不用改 注意user与group为www 安装php的时候就已指定

#启动php-fpm
/usr/local/php7.3.10/sbin/php-fpm

https://blog.csdn.net/ziqibit/article/details/129562734
```

#### configure报错解决

```shell
# ./configure会报错 (如果安装yum install libzip-devel -y)
# configure: error: Please reinstall the libzip distribution
# checking for libzip... configure: error: system libzip must be upgraded to version >= 0.11
# 如果用yum安装libzip-devel的话，安装的版本时0.10，版本达不到要求，所以需要卸载libzip后手动安装新版本
# 1、移除旧版本
yum remove libzip libzip-devel

# 2、到https://libzip.org/news/ 下载1.5.2版本
https://libzip.org/download/libzip-1.5.2.tar.gz

# 3、解压
tar -zxvf libzip-1.5.2.tar.gz
# cd libzip-1.5.2
# mkdir build
# cd build
# cmake ..	(#注意：cmake后面有两个小数点, 有可能这一步会报cmake版本过低，以下有升级的方法)]
# make -j4
# make test
# make install

# 4、执行完上面后，configure还是会报错
# configure: error: off_t undefined; check your library configuration
#在/etc/ld.so.conf.d/路劲下创建usr_local_lib.conf 文件，并添加以下内容
/usr/local/lib
/usr/local/lib64
/usr/lib
/usr/lib64

# 5、保存,让修改的文件生效
ldconfig -v

# 改完后configure还是报错，参考
https://blog.csdn.net/weixin_43493122/article/details/96647432
```

#### Cmake版本太低了（升级Cmake）

```shell
# 报错
 CMake 3.0.2 or higher is required.  You are running version 2.8.12.2
#升级Cmake
#1、移除旧版本
yum remove cmake -y

#2、从官网上下载https://cmake.org/files/v3.17/  (选择3.17.5版本)
https://cmake.org/files/v3.17/cmake-3.17.5-Linux-x86_64.tar.gz
#上传到/usr/local/src目录中

#3、进行一个解压：
tar -zxvf cmake-3.17.5-Linux-x86_64.tar.gz

#4、增加环境变量
vim /etc/profile
#在文件末尾添加以下代码 并保存：
export PATH=$PATH:/usr/local/src/cmake-3.17.5-Linux-x86_64/bin
#使修改的文件生效：
source /etc/profile

#5、验证
echo $PATH
cmake --version
```



#### php安装扩展

```shell
# cd /usr/local/src/
wget http://pecl.php.net/get/mongodb-1.5.2.tgz

# 解压
tar -zxvf mongodb-1.5.2.tgz

# 切换目录 
cd mongodb-1.5.2

# 执行 （以下就是php的安装目录)
/usr/local/php7.3.10/bin/phpize

./configure --with-php-config=/usr/local/php7.3.10/bin/php-config

make && make install
# 安装成功后，会有类似以下安装目录信息输出：
#Installing shared extensions:     /usr/local/php7.3.10/lib/php/extensions/no-debug-zts-20180731

# 修改配置
vim  /usr/local/php7.3.10/etc/php.ini

# 在文档末尾加入
extension-mongodb.so

#然后重启php-fpm

```





#### 安装Redis

```shell
# 下载安装包：(下载至/usr/local/src/ 目录下)
wget http://download.redis.io/releases/redis-7.0.11.tar.gz
tar xzf redis-7.0.11.tar.gz -C /usr/local/
#mv redis-7.0.11 ../	# 移动解压文件到上一层，也就是/usr/local/目录下
cd redis-7.0.11
make PREFIX=/usr/local/redis-7.0.11/ -j4 install


# 创建目录 (中央服依赖这个)
mkdir /data/redis/33379/{conf,data,logs} -p

# 配置
cp -a /usr/local/redis-7.0.11/redis.conf /data/redis/33379/conf/

# 修改/data/redis/33379/conf/redis.conf配置
----   33379  ----
bind 127.0.0.1
protected-mode yes		#开启保护模式
port 33379   #端口
pidfile /var/run/redis_33379.pid   #进程id
logfile /data/redis/33379/logs/redis-server.log   #日志
dir /data/redis/33379/data/   #数据目录
repl-diskless-sync no
appendonly yes   #开启持久化
requirepass zusI9fuCMI5eHP2X   #设置密码  
rename-command FLUSHALL ""
rename-command CONFIG ""
rename-command EVAL ""

# 修改/usr/local/redis-7.0.11/redis.conf配置 (应该用不到这个配置启动redis的,先不做处理)
-------
/usr/local/redis-7.0.11/redis.conf
bind 0.0.0.0
dir /data/redis/ #数据目录
appendonly yes



# 测试（在/root/目录下创建脚本start-redis.sh)
#!/bin/bash
/usr/local/redis-7.0.11/bin/redis-server  /data/redis/33379/conf/redis.conf

#启动redis
sh /root/start-redis.sh

#连接（因为上面设置了密码、所以也需要密码连接）
/usr/local/redis-7.0.11/bin/redis-cli -h 127.0.0.1 -p 33379 -a zusI9fuCMI5eHP2X ping
# 返回pong则为成功
```



#### 安装Erlang

```shell
# 从官网上下载erlnag25
https://github.com/erlang/otp/releases/download/OTP-25.2.1/otp_src_25.2.1.tar.gz

# 安装依赖
yum -y install make gcc gcc-c++ kernel-devel m4 ncurses-devel openssl-devel

# 上传文件到/usr/local/src/目录下 并解压
tar -zxvf otp_src_25.1.2.tar.gz
# 编译
cd otp_str_25.1.2
./configure --prefix=/usr/local/erlang25
make -j2 && make install

```





-------------------------

```
# centos7 安装使用rz/sz命令
yum install -y lrzsz


# 创建软链接
ln -s [源文件] [软链接文件]
ln -s /usr/lib64/libtcmalloc.so /usr/local/lib/libtcmalloc.so
# 创建的软链接文件为/usr/local/lib/libtcmalloc.so
```





```
需要开放的端口：
80		// nginx
```

