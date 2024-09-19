本博客采用github+Hexo+icarus主题搭建

#### 1、hexo基本用法

| 命令                      | 简写     | 注释                                                         |
| ------------------------- | -------- | ------------------------------------------------------------ |
| hexo init [folder]        |          | 新建一个网站。如果没有设置folder,Hexo默认在当前的文件夹建立网站 |
| hexo new [layout]         |          | 新建一篇文章。<br>如果没有设置layout的话，默认使用_config.yml中的default_layout参数代替 |
| hexo publish [layout]     |          | 发布草稿                                                     |
| hexo render [file2] ...   |          | 渲染文件                                                     |
| hexo migrate              |          | 从其他博客系统迁移内容                                       |
| hexo list                 |          | 列出所有路由                                                 |
| hexo config [key] [value] |          | 列出网站的配置(\_config.yml)<br>如果指定了key,则只展示对应key的值；如果同时指定了key和value,则将配置中对应key的值修改为value |
| hexo version              |          | 显示版本信息                                                 |
| hexo clean                |          | 清除缓存和已经生成的静态文件                                 |
| hexo generate             | hexo   g | 生成静态文件                                                 |
| hexo server               | hexo s   | 本地启动，一般用于本地测试                                   |
| hexo deploy               | hexo d   | 部署网站，可以用于部署到github                               |



#### 2、目录文件解析

* _config.yml: 网站的配置信息，如网站的title、描述、关键词、图标等
* _config.xxx.yml: (如\_config.icarus.yml文件)主题配置文件，用于设置该主题的网站信息，比如网站的title、网站的导航栏、网站的footer等
* package.json: 应用程序信息
* scaffolds: 模板文件夹。当新建文章时，Hexo会根据scaffold来创建文件。Hexo的模板是指在新建文章时在文件中默认填充的内容
* source: 资源文件夹是存放用户资源的地方。除\_posts文件夹外，开头命令为\_(下划线)的文件或文件夹和隐藏的文件都会被忽略。Markdown和HTML文件会被解析并放到public文件，而其他文件会被拷贝过去
* themes: 主题文件夹，Hexo会根据主题来生成静态页面
* public: 这个文件夹就是Hexo根据主题和source文件下的内容生成的静态文件内容，也就是整个网站展示的内容



#### 3、新加文章

可通过以下指令添加文件

> hexo new [layout] <title>

可以在指令中指定文章的布局(layout),默认为post,可以修改\_config.yml中的default_layout参数来指定默认布局

也可以直接在source/_posts文件加下新建markdown文件来直接生成新加的文章



#### 4、主题icarus使用

##### 4.1 下载主题

>```
>git clone https://github.com/ppoffice/hexo-theme-icarus.git themes/icarus
>```

##### 4.2 安装主题

> npm install hexo-theme-icarus

##### 4.3 切换主题

>hexo config theme icarus

或者直接修改_config.yml中的theme字段对应值

>theme: icarus

##### 4.4 icarus相应的设置参考文档

> https://ppoffice.github.io/hexo-theme-icarus/

#### 杂项

hexo分支存放源代码分支

master分支存放静态文件分支，也就是hexo分支生成的静态文件



#### q&a

windows启动服务后无法通过Ctrl+C来停止服务，导致端口被占用

```
# 1、在命令行模式下，查找端口被占用进程
netstat -ano | findstr 4000

# 2、kill被占用端口的进程
taskkill /F /PID 进程pid
```



