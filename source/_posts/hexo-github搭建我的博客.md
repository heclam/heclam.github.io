---
title: hexo+github搭建我的博客
date: 2019-06-14 11:52:00
toc: true
categories:
	- hexo
tags:
	- hexo
---

#### 1、本地环境搭建

搭建需要的环境

* node
* github
* hexo

首先在自己的磁盘目录下新建一个文件夹，如heclam.github.io，然后cd进入该目录下 <!--more-->

```shell
# 切换目录到 D:\heclam.github.io, 在git bash下

# 设置npm的源为阿里的源
npm config set registry https://registry.npmmirror.com

# 安装hexo
npm install hexo-cli -g

# 初始化博客
hexo init

# 安装依赖
npm install

# 生成静态文件
hexo g

# 启动hexo服务
hexo s

# 然后在浏览器上输入 http://localhost:4000/即可看到运行成功
```



#### 2、部署到github上

##### 2.1 在github上面新建一个仓库, 名为**用户名.github.io**



##### 2.2 本地设置git账号

```shell
# 在git bash下
git config --global user.name "github用户名"
git config --global user.email "github注册邮箱"
```



##### 2.3 本地创建SSH秘钥

> ssh-keygen -t rsa -C "github注册邮箱"

然后在本地找到公钥的位置，一般在**c:\\Users\\用户名\\.ssh** 目录下，打开id_rsa.pub文件，拷贝里面的内容。



在github上面新建ssh key

>github主页 -> 头像 -> settings -> SSH and GPG keys -> SSH Keys -> New SSH key

点击New SSH key, Title随便注释即可，将上面的公钥复制粘贴到key下然后点击Add SSH key按钮即可



测试是否可连接

> ssh git@github.com

出现连接成功则成功



##### 2.4 将博客部署到GitHub上

安装hexo-deployer-git插件

>npm install hexo-deployer-git --save

修改一下_config.yml文件,如

```shell
deploy:
  type: 'git'
  repo: git@github.com:heclam/heclam.github.io.git
  # 这个不一定是master，旧的可能为master,现在新建的分支默认可能都为main了，可是具体情况而定
  branch: master
```

执行部署

```shell
hexo g -d

# 或者拆分以上指令
hexo g
hexo d
```

如果成功则可以通过

https://<github用户名>.github.io/ 访问了

如https://heclam.github.io/



#### 3、多台电脑共同操作博客

上面的设置有点鸡肋，如果换了设备则无法再对博客进行一个修改更新操作，因为保存在github上面的内容仅仅只是hexo生成的静态资源文件，而并没有源码文件，那么接下来将处理这个问题，使得我们在家里以及公司或则其他的设备都可以对这个博客进行一个修改更新操作



##### 3.1 新建hexo分支，存放源码

​	首先我们在同一个仓库下新建一个hexo分支存放源码，这样我们在其他设备也可以获取源码,这样子不管在哪个设备上我们只需要更新hexo分支的源码即可共同管理博客里面的内容。

新建一个hexo分支，并设置为默认分支。这个逻辑就很清楚了hexo分支保存博客的源码文件，当我们执行hexo d的时候，hexo都会帮我们把生成好的静态页面文件推送到master分支上



#### 3.2 本地创建仓库跟远程新建的hexo分支产生关联

将博客源码推送到hexo分支上

```shell
# clone hexo分支内容
# clone的是hexo默认分支内容
git clone https://github.com/heclam/heclam.github.io.git

# 可以将.git文件夹以外的所有内容删除

# 把原来的hexo源码除了.deploy_git以外的都复制到本目录中，通过.gitignore文件可以忽略不需要版本控制的文件
# 如果themes主题文件夹里面有.git文件，也需要将其删除，因为git不能嵌套上传，否则上传的时候会出错导致主题文件无法上传，这样配置的主题在其他电脑也无法使用

# 将源码推送到hexo分支
git add .
git commit -m '添加博客源码'
git push
```

这里完成之后，之前的源码文件已经没有任何用处了，可以选择直接删除



##### 3.3 在新设备的操作

```shell
# 首先需要生成ssh key添加到github,具体流程可以参考上面的2.3

# clone hexo分支代码
git clone https://github.com/heclam/heclam.github.io.git

# 进入clone下来的目录,执行相应的指令
# 这里不再需要执行hexo init了
npm install hexo
# 读取packages.json里面的信息，自动安装依赖
npm install
npm install hexo-deployer-git --save

# 最后执行
# 生成静态文件
hexo g
# 本地服务启动
hexo s
# 部署到master分支
hexo d
```

在设备准备提交内容的时候，为了保证分支同步，最好先git pull一下



------------------------------------



[hexo文档](https://hexo.io/zh-cn/docs/)

[icarus主题设置](!https://ppoffice.github.io/hexo-theme-icarus/)

