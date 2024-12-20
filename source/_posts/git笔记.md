---
layout: article
title: git笔记
date: 2019-06-15 16:02:33
toc: true
tags:
	- git
---

##### 1、git命令的介绍：

| 指令                      | 描述                                                         |
| ------------------------- | ------------------------------------------------------------ |
| git init                  | 将某目录变成git可以管理的仓库（在此文件夹下生成了.git文件夹） |
| git add xxx.md            | 添加xxx.md文件到暂存区（从工作区-->暂存区）                  |
| git add .                 | 这是添加所新建的所有文件到暂存区                             |
| git  commit -m "提交注释" | 一次性提交多个文件（add进来的文件），将文件提交到本地仓库（暂存区-->本地仓库） |
| git status                | 可以时刻知道仓库的当前状态                                   |
| git diff xxx.md           | 查看修改的内容（与提交进去的文件相比）                       |
| git log                   | 查看仓库历史记录 ，命令显示从最近到最远的提交日志            |
| git reflog                | 查看命令历史                                                 |

<!--more-->

##### 2、版本回退：

git reset  --hard  HEAD^

将会重置HEAD返回到另外一个commit,这个是个比较危险的动作，具有破坏性，数据因此可能会丢失，如果想找回原来的数据就使用 git reflog命令

Git中，用HEAD表示是当前的版本，上一个版本就是HEAD^,上上个版本就是HEAD^^,

当然往上n个版本写n个^比较容易数不过来，所以写成HEAD~n,如HEAD-10

```shell
# 回退到上一个版本
git reset --hard HEAD^  

# 回退到上10个版本
git reset --hard HEAD~10
```

也可以指定回滚的版本号，版本号用git log查看commit id前7位

如果我们忘记回退前的版本号，`git reflog` 命令记录了每一次的命令操作，包含了所有版本的版本号



`--hard`参数含义：

`--hard`会回退到上个版本的已提交状态

`--soft`会回退到上个版本的未提交状态

`--mixed`会回退到上个版本已添加但未提交的状态



##### 3、撤销操作

| 指令                       | 描述                                                         |
| -------------------------- | ------------------------------------------------------------ |
| git checkout \-\- filename | 把文件在工作区的修改全部撤销，回到最近一次git commit 或git add 时的状态。 |
| git reset HEAD filename    | 把暂存区的修改撤销掉，重新放回到工作区中                     |
| git rm filename            | 删除版本库中的文件，比如执行 git rm test.txt ,然后再执行commit操作则成功删除 |
| git rm dir -r              | 删除版本库中的文件夹                                         |
| git checkout \-\- test.txt | 撤销删除                                                     |



##### 4、远程仓库

首先需要拥有github的账号

创建SSH Key密钥

```shell
ssh-keygen -t rsa -C "注册github的email"
```

然后在用户主目录里找到.ssh目录，里面有id_rsa和id_rsa.pub两个文件，然后将id_rsa.pub里面的内容添加到github上面

```shell
# 先在github上面创建仓库,如heclam.github.io.git

# 关联远程仓库
# origin 为本地仓库
# https://github.com/heclam/heclam.github.io.git为仓库的地址
git remote add origin https://github.com/heclam/heclam.github.io.git

# 一般push之前先pull一下 把远程仓库的内容更新到本地仓库
# 将本地仓库推送到远程分支上
git push -u origin master

# or
git push


#由于远程库是空的，我们第一次推送 master 分支时，加上了 `-u` 参数，Git不但会把本地的master分支内容推送的远程新的 master 分支，还会把本地的 master 分支和远程的 master 分支关联起来，在以后的推送或者拉取时就可以不使用 `-u`参数。

#列出已存在的远程分支
git remote

# 删除添加的远程库
git remote remove name

# 从远程克隆仓库
git clone https://github.com/heclam/heclam.github.io.git

# 修改远程仓库地址
git remote set-url origin <remote-url>

```



##### 5、分支管理

master：默认为主分支，Head 指向哪个分支，则表明那个分支为当前分支

| 指令                              | 描述                                                         |
| --------------------------------- | ------------------------------------------------------------ |
| git branch test                   | 创建分支test                                                 |
| git checkout test                 | 切换至test分支                                               |
| git checkout -b  test             | 创建了一个test分支并且切换到了test分支中                     |
| git branch                        | 查看当前分支                                                 |
| git merge name                    | 合并指定分支与当前分支，name为分支的名称<br>（如master分支需要合并test分支的内容，则在master分支下执行 git merge test） |
| git merge -no-ff -m "..."  分支名 | 禁用git合并时默认的fast-forward模式，生成新的commit加入分支信息 |
| git branch -d 分支名              | 删除分支                                                     |



##### 6、工作现场存储

| 指令            | 描述                                                         |
| --------------- | ------------------------------------------------------------ |
| git stash       | 将当前工作现场储藏起来，等以后恢复现场后继续工作             |
| git stash list  | 查看储藏的工作现场                                           |
| git stash apply | 恢复储藏的工作现场,stash内容并不删除，你需要用git stash drop来删除 |
| git stash pop   | 恢复的同事把stash内容也删除了                                |



##### 7、多人协作

```shell
# 从远程库克隆到本地仓库，克隆对应的分支，比如这里为test分支
git checkout -b test  origin/test  

# 用git push origin test（分支名） 推送自己的修改
# 如果失败，因为远程分支比本地分支内容新，需要先git pull 合并一下
# 如果有冲突则解决冲突后在提交
# 使用git push origin test 推送到远程库
# 如果 git pull 提示 “no tracking information”，则需要创建本地分支和远程分支的关联
git branch --set-upstream test origin/test
```



##### 8、标签管理

```shell
# 打标签
git tag  tagName 

# 例子：git tag  v0.1 忘记在某次commit时打标签，可以使用以下的方式
# 将commit id为2893423的提交打上v1.0标签
git  tag v1.0  2893423 

# 查看标签
git tag  
# 查看标签信息
git show <tagname>
# 创建带有说明的标签，-a 指定标签名，-m指定说明
git tag -a v2.0 -m "记录信息" 23498222（commit id）

# 删除指定标签
git tag -d tagName

# 因为创建的标签都只存储在本地，不会自动推送到远程。所以，打错的标签可以在本地安全删除。
# 如果要推送某个标签到远程，使用命令 
git push origin <tagname>

#一次性推送全部尚未推送到远程的本地标签
git push origin --tags

# 删除已推送到远程库的标签，先从本地删除，再从远程删除
git tag -d tagname & git push origin :refs/tags/tagname
```



##### 9、忽略特殊文件

Git仓库也可以有多个.gitignore文件，.gitignore文件放在哪个目录下，就对哪个目录（包括子目录）起作用