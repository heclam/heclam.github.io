---
layout: article
title: JSP笔记
date: 2019-07-08 17:20:20
toc: true
categories:
	- javaWeb
tags:
	- JSP
---



##### 何为JSP ？

* JSP全称是Java Server Pages，它和servle技术一样，都是SUN公司定义的一种用于开发动态web资源的技术。<!--more-->
* JSP这门技术的最大的特点在于，写jsp就像在写html，但它相比html而言，html只能为用户提供静态数据，而Jsp技术允许在页面中嵌套java代码，为用户提供动态数据。

##### JSP原理

浏览器向服务器发请求，不管访问的是什么资源，其实都是在访问Servlet，所以当访问一个jsp页面时，其实也是在访问一个Servlet，服务器在执行jsp的时候，首先把jsp翻译成一个Servlet，所以我们访问jsp时，其实不是在访问jsp，而是在访问jsp翻译过后的那个Servlet

#####　为什么有了Servlet了还需要JSP呢？

其实JSP的出现是简化了Servlet，之前我们要展示一个页面要在Servlet中写  

```java
out.write("<html>");
out.write("<body>");
out.write("</body>");
out.write("</html>");
```

之类的代码，显得太过麻烦了，所以就出现了JSP。

##### JSP的生命周期

jsp的生命周期类似于servlet的生命周期，区别在于JSP生命周期还包括将JSP文件编译成servlet。

JSP生命周期的阶段：

1. 编译阶段(解析JSP文件，将JSP文件转换为servlet，然后编译servlet)
   *　servlet容器编译servlet源文件，生成servlet类
2. 初始化(执行 jspInit()方法)
   * 加载与JSP对应的servlet类，创建其实例，并调用它的初始化方法
3. 执行阶段（执行 _jspService(HttpServletRequest request,HttpServletResponse response) 方法）
   * 调用与JSP对应的servlet实例的服务方法
4. 销毁阶段（执行jspDestroy()方法 ）
   * 调用与JSP对应的servlet实例的销毁方法，然后销毁servlet实例

```
 编译阶段 --> jspInit()  -->   _jspServcie()  -->  jspDestroy()
```