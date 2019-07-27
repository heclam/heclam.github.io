---
title: hexo+github搭建我的博客
date: 2019-06-14 11:52:00
categories:
	- 笔记
	- 教程
tags:
	- hexo
---

##### 经过两天的搭建终于搭成了  在多终端操作博客的搭建中遇到的坑比较多

[hexo文档](https://hexo.io/zh-cn/docs/)

> 搭建完基本的博客时，实现多终端操作博客需要注意的点：
>
> ​	1.本地博客的配置文件(\_config.yml)中的deply:  branch:master(必须为主分支，因为master就是用来部署上去的     hexo g -d）<!--more-->
>
> ​	2.需要在github上新建一个分支（比如 hexo）,这时候master分支与hexo分支的内容是一样的（为什么需要新建分支呢？ hexo用来存放原来博客的配置文件，这样才能实现多终端的操作而master分支用来存放静态文件的展示）
>
> ​	3.在本地的博客目录中，找到主题（theme）目录下的主题，如果是clone下来的主题必须把里面的.git(一般是隐藏的，要开启隐藏文件可见)删除，不然待会的push到github会出错（因为一个仓库只能有一个.git文件）
>
> ​	4.在本地的博客目录中找到 .gitignore文件，里面添加（public/  、  .deploy*/  、\_config.yml）一行一个，public文件里面是hexo -g（根据source目录生成的）生成的文件所以不必要添加，.deploy_git是hexo默认的.git配置文件夹，不需要同步， \_config.yml是一个博客的配置文件也不应该上传（但我嫌麻烦就上传了 ）
>
> ​	5.到这一步就是在本地目录打开终端（cmd）,再初始化仓库，重新对代码进行版本控制

> ​		git init
>
> ​		git remote add origin <server>   
>
> ​		//<serve>为远程仓库的地址，origin是本地分支,remote add操作会将本地仓库映射到云端

##### 将博客源文件上传到hexo分支

> ​		git add .
>
> ​		git commit -m "更新内容"
>
> ​		git push origin hexo (如果这一步有错，可以先执行git push -u origin hexo -f  但建议不要这么做，因为有数据丢失)，出现这个问题就是本地分支与hexo分支内容不同，可以先 pull 然后再push, 如果push出现错误，可以在本地新建一个分支hexo对应github上的hexo分支（这时候master与hexo分支的内容是一致的），这是后push就可以，我也不知道为什么（苦笑）

##### 在其他电脑的操作

可以现在其他电脑生成ssh密钥，这样就不用频繁（提交的时候）输入账号密码了

###### 生成新的ssh key

> 打开终端输入一下命令：
>
> ssh-keygen -t rsa -c "注册时的邮箱"
>
> 输入命令后, 然后连续回车, 默认会在相应路径下（~/.ssh/id_rsa.pub）生成id_rsa和id_rsa.pub两个文件
>
> 将id_rsa.pub这里面的内容复制

将ssh key添加到Github中

打开你的github主页，进入个人设置 -> SSH and GPG keys -> New SSH key：     title随便起，key填入刚刚复制的内容

将博客项目克隆下来

> git clone  url   //url表示的是项目地址

克隆下来的仓库和你在个人PC中的目录是一模一样的，所以可以在这基础上继续写博客了。但是由于`.gitignore`文件中过滤了`node_modules\`，所以克隆下来的目录里没有`node_modules\`，这是hexo所需要的组件，所以要在该目录中重新安装hexo，**但不需要hexo init**。

> npm install hexo
>
> npm install    //安装依赖
>
> npm install hexo-deployer-git --save   //安装部署插件

###### 新建文章

> hexo new article //article表示的是你的文章名称

###### 推送到hexo分支

>git add .
>
>git commit -m "新增内容"
>
>git push origin hexo

###### 部署到master分支

> hexo g -d