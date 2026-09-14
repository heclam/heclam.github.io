---
title: windows开发环境搭建
date: 2024-11-26 19:50:06
toc: true
categories:
  - windows
tags:
  - windows
---



### 基础开发工具



#### 1、Office

```
使用 Office Tool Plus安装激活Office2021
1、下载Office Tool Plus
	从百度云盘中下载
	或从官网上下载：https://otp.landian.vip/zh-cn/download.html

2、解压下载的文件（xxx.zip or xxx.rar  最好先把病毒查杀给关闭，否则加压的时候 解压内容被当成病毒删除了）

3、安装office
	双击运行 Office Tool Plus.exe
	先卸载旧版本的Office(如果之前没有安装过 可忽略)
		点击工具箱可选择卸载已安装的office
	安装office
		点击部署，选择需要安装office版本(安装内容选择：Excel、World、PowerPoint常用的即可)
		语言选择简体中文
		部署设置选择下载后再部署
		点击开始部署（然后耐心等待部署完成即可）
		
4、激活Office
	选择激活按钮
	安装许可证
	设置KMS管理(使用下列任意一个即可)
		kms.loli.beer
		kms.luochenzhimu.com
		kms9.MSGuides.com
	最后点击激活即可(激活显示报错也没事，下面验证正常激活即可)

5、验证激活
	随便打开一个world或excel然后点击
	文件->账户 查看是否已是激活状态
		
ref:https://baijiahao.baidu.com/s?id=174926385039398754&wfr=spider&for=pc
```

![office_active.png](/images/office_active.png)



#### 2、SecureCRT

```
# 使用secureCrt连接ubuntu，虚拟机需要添加以下依赖
sudo apt-get install ssh
sudo apt-get install openssh-server

官网下载安装程序：
程序破解：
https://blog.csdn.net/z321267178193/article/details/135992991

生成秘钥流程：
SecureCRT界面 -> Tools -> Create Public Key... 
选择：
Key type: RSA
Key length in bits: 2048
Choose a director and filename for the private key:  OpenSSH Key format

```



#### 3、Typora

```
官网下载安装Typora即可

激活教程：
https://blog.csdn.net/qq_61621323/article/details/141036982
```



#### 4、NotePad++



#### 5、Internet Download Manager

```
IDM激活：
https://www.bilibili.com/opus/940113253860638720
```



#### 6、Vscode开发Erlang下载的扩展

```
1、Chinese (Simplified) (简体中文) Language Pack for Visual Studio Code
2、efmt
3、erlang
4、Project Manager
-------------------以上为公用依赖模块
```

如果OTP版本低再安装以下依赖

```
erlang-symbols
```

如果OTP版本为OTP21及以上，则可以安装

```
Erlang Ls (需要特定的otp版本才能支持(如otp24)，以及源码目录配置erlang_ls.config 文件)
```

使用`Erlang Ls`插件还需要，在项目根路径下添加配置文件

erlang_ls.config

```yaml
otp_path: "D:/Program Files/Erlang OTP"

deps_dirs:
  - "src/apps/*"

diagnostics:
  enabled:
    - crossref
  disabled:
    - dialyzer

include_dirs:
  - "include"
  - "src/apps"
  - "src/apps/*/include"
  - "src/apps/cowlib/src"
  - "src/apps/cowboy/src"

lenses:
  enabled:
    - function-references
  disabled:
    - show-behaviour-usages

```

./vscode/setting.json

```json
{
  "files.associations": {
    "*.erl": "erlang",
    "*.hrl": "erlang"
  },
  "erlang-ls.enabled": true,
  "erlang.erlPath": "D:\\Program Files\\Erlang OTP\\bin\\erl.exe",
  "erlang_ls.escriptPath": "D:\\Program Files\\Erlang OTP\\bin\\escript.exe",
  "erlang-ls.otpPath": "D:\\Program Files\\Erlang OTP",
  "erlang_ls.serverPath": "C:/Users/admin/.local/share/erlang_ls-1.1.0-otp28/erlang_ls",
  "editor.codeLens": true,
  "[erlang]": {
      "editor.defaultFormatter": "truqu.tqformat",
      "editor.formatOnSave": true
  },
  "workbench.colorTheme": "Dark+",
  "erlang.codeLensEnabled": true,
  "python-envs.defaultEnvManager": "ms-python.python:system",
  "python-envs.pythonProjects": []
}
```



如果`erlang-symbols`和`Erlang Ls` 同时安装可能会造成引用冲突，所以如果环境没有安装多个版本的OTP则根据安装的OTP版本安装需要的依赖即可

```
启用Workspace级启停扩展(某个项目不需要启动某个插件的直接停用)
点击插件卡片右下角齿轮图标：选 启用（工作区） | 禁用（工作区）
```

如果有多个版本OTP：

例如：

```
OTP 19：D:\Program Files\Erlang\19 （老项目默认）
OTP 28：D:\Program Files\Erlang OTP 28
```

系统环境变量 **PATH 里只保留 OTP 19 的 `bin`**， 其他的项目根据需求如果需要其他的版本的OTP则根据安装路径写绝对路径调用



### 7、Ubuntu下的环境搭建

#### 一、ubuntu源码安装mysql5.6

##### 1、安装依赖

>sudo apt-get update
>sudo apt-get install build-essential cmake ncurses-dev

##### 2、下载MYSQL源码

>cd /usr/local/src
>
>sudo wget https://dev.mysql.com/get/Downloads/MySQL-5.6/mysql-5.6.45.tar.gz
>
>sudo tar zxvf mysql-5.6.45.tar.gz
>
>cd mysql-5.6.45

##### 3、配置编译项

>sudo cmake . -DWITH_BOOST=boost/boost_1_59_0/ \
>-DCMAKE_BUILD_TYPE=Release \
>-DCMAKE_INSTALL_PREFIX=/usr/local/mysql \
>-DMYSQL_DATADIR=/usr/local/mysql/data \
>-DSYSCONFDIR=/etc \
>-DWITH_MYISAM_STORAGE_ENGINE=1 \
>-DWITH_INNOBASE_STORAGE_ENGINE=1 \
>-DWITH_PARTITION_STORAGE_ENGINE=1 \
>-DWITH_FEDERATED_STORAGE_ENGINE=1 \
>-DWITH_BLACKHOLE_STORAGE_ENGINE=1 \
>-DWITH_ARCHIVE_STORAGE_ENGINE=1 \
>-DWITH_MEMORY_STORAGE_ENGINE=1 \
>-DENABLED_LOCAL_INFILE=1 \
>-DENABLE_DTRACE=0 \
>-DDEFAULT_CHARSET=utf8 \
>-DDEFAULT_COLLATION=utf8_general_ci

##### 4、编译和安装

>sudo make
>
>sudo make install

##### 5、配置MySQL用户和权限

>sudo groupadd mysql
>sudo useradd -r -g mysql -s /bin/false mysql
>sudo chown -R mysql:mysql /usr/local/mysql

##### 6、初始化数据库

>cd /usr/local/mysql
>sudo scripts/mysql_install_db --user=mysql --basedir=/usr/local/mysql --datadir=/usr/local/mysql/data
>sudo chown -R root .
>sudo chown -R mysql data

##### 7 、配置MySQL服务

>sudo cp support-files/mysql.server /etc/init.d/mysql
>
>sudo chmod +x /etc/init.d/mysql
>
>sudo update-rc.d mysql defaults

##### 8、启动MySQL服务

>sudo service mysql start

##### 9 、安全设置(设置root密码)

>sudo /usr/local/mysql/bin/mysql_secure_installation

执行之后回车，因为刚创建的MySQL root是没有密码的，执行此命令后才开始设置密码

##### 10、root授权允许远程访问

>use mysql;
>GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' IDENTIFIED BY 'root' WITH GRANT OPTION;
>flush privileges;
>
>GRANT ALL PRIVILEGES ON *.* TO 'root'@'localhost' IDENTIFIED BY 'root' WITH GRANT OPTION;
>FLUSH PRIVILEGES;





#### 二、ubuntu源码安装Erlang

#### 1、下载源码

>cd /usr/local/src
>
>wget https://github.com/erlang/otp/releases/download/OTP-25.2.1/otp_src_25.2.1.tar.gz

##### 2、安装依赖

>sudo apt-get install build-essential   
>
>sudo apt-get install libncurses5-dev   
>
>sudo apt-get install libssl-dev  

##### 3、解压编译安装

>sudo tar -zxvf erlang.xxx.tar.gz
>
>cd erlang..解压后的文件
>
>sudo ./configure --prefix=/usr/local/erlang19 && make && make install
>
>
>
>
>
>// 可能以上执行权限有问题可分开执行
>
>sudo ./configure --prefix=/usr/local/erlang19 
>
>sudo make
>
>sudo make install



#### 三、ubuntu添加环境变量

```
方法一：

export PATH=命令行路径:$PATH
#配置完后可以通过echo $PATH查看配置结果。
 
#生效方法：立即生效
#有效期限：临时改变，只能在当前的终端窗口中有效，当前窗口关闭后就会恢复原有的path配置
#用户局限：仅对当前用户

方法二：

#通过修改.bashrc文件:
sudo vim ~/.bashrc 
#在最后一行添上：
export PATH=命令行路径:$PATH
 
#生效方法：（有以下两种）
#1、关闭当前终端窗口，重新打开一个新终端窗口就能生效
#2、输入“source ~/.bashrc”命令，立即生效
#有效期限：永久有效
#用户局限：仅对当前用户

方法三：
 
#通过修改profile文件:
sudo vim /etc/profile
export PATH=命令行路径:$PATH
 
#生效方法（有以下两种）
#1：source /etc/profile
#有效性：只在当前终端生效
#2：系统重启
#有效期限：永久有效
#用户局限：对所有用户

方法四：
 
#通过修改environment文件:
sudo vim /etc/environment
在PATH="/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin"中加入 
":命令行路径"
 
#生效方法：系统重启
#有效期限：永久有效
#用户局限：对所有用户


```



#### 四、VirtualBox Ubuntu文件夹共享

```
1、确定好windows下需要共享的文件夹

2、virtualbox打开共享文件夹选项
设备-> 共享文件夹 -> 共享文件夹->添加新的共享
	共享文件夹路径
	共享文件夹名称（跟上面最后的文件夹同名）
	（选择固定分配，不用勾选自动挂载，不然后面开机自动挂载启动不了虚拟机）
	
3、启动虚拟机在虚拟机里面的终端操作
# 新建共享文件夹
sudo mkdir /mnt/shared/

# 挂载命令
# sudo mount -t vboxsf [windows共享目录] [ubuntu共享目录]
sudo mount -t vboxsf xnh5 /mnt/shared/

4、实现开机自动挂载
# sudo gedit /etc/fstab
sudo vim /etc/fstab
# 在fstab文件末添加一项
# <共享名称> <Ubuntu共享名称> vboxsf defaults 0 0
xnh5 /mnt/shared/ vboxsf defaults 0 0
```



#### MISC

```
Erlang相关：

使用remsh进入控制节点，退出方式：
Ctrl + g , q	# 在节点命令行下，先键入Ctrl+g, 然后再键入q
	
	
screen 相关命令：
查看screen列表：screen -ls
新建screen: screen -S YourScreenName
恢复制定screen: screen -r YourScreenName

Ctrl+D # 在当前screen下输入Ctrl+D，删除该screen
Ctrl+A,Ctrl+D # 在当前screen下，输入先后Ctrl+A,Ctrl+D,退出该screen

# 连接状态为[Attached]的screen
screen -D -r YourScreenName	# -D -r 先踢掉前一用户，再登录

# 删除制定screen
screen -S YourScreenName -X quit




```





#### 虚拟机时间不跟随主机时间

```
VirtualBox 虚拟机与主机时间同步问题
(注意：需要先关闭虚拟机再操作)
1、在主机找到虚拟机安装目录（如：D:\Program Files\Oracle\VirtualBox）
然后在该目录下命令行窗口下执行VBoxManage.exe list vms 查看已安装的虚拟机

2、打开或关闭时间同步(同样在主机命令行下执行此操作，此操作需要关闭虚拟机才会生效）
VBoxManage setextradata "虚拟机名称" "VBoxInternal/Devices/VMMDev/0/Config/GetHostTimeDisabled" 1(是否与主机时间同步：0表示不同 1表示不同步)
VBoxManage setextradata "game" "VBoxInternal/Devices/VMMDev/0/Config/GetHostTimeDisabled" "1"	// 关闭时间同步
VBoxManage setextradata "game" "VBoxInternal/Devices/VMMDev/0/Config/GetHostTimeDisabled" "0"	// 打开时间同步

3、启动虚拟机，在虚拟机使用以下命令修改虚拟机时间
sudo date -s "2024-11-30 20:55:00"

4、如果需要虚拟机设置回正常时间（从互联网获取时间）
安装：
sudo apt-get install ntp
sudo apt-get install ntpdate

执行即可恢复互联网时间
sudo ntpdate -s time.nist.gov

tips:如果以上指令无法恢复则重新启动ntp
sudo systemctl restart ntp
或者是
sudo service ntp restart
```

