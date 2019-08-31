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



##### jsp获取绝对路径

​	代码” ${pageContext.request.contextPath}”的作用是取出部署的应用程序名，这样不管如何部署，所用路径都是正确的

```
<!--使用绝对路径的方式引入CSS文件-->
2 <link rel="stylesheet" href="${pageContext.request.contextPath}/css/xxx.css" type="text/css"/>
3 <!--使用绝对路径的方式引入JavaScript脚本-->
4 <script type="text/javascript" src="${pageContext.request.contextPath}/js/xxx.js"></script>
```

使用<%=request.getContextPath()%>和使用${pageContext.request.contextPath}达到同样的效果

##### 静态包含与动态包含

``<%@include file=”name.jsp”%>``

include指令是静态包含。静态包含的意思就是：把文件的代码内容都包含进来，再编译！

``<%jsp:include page=”name.jsp”%>``

jsp行为包含文件就是先编译被包含的页面，再将页面的结果写入到包含的页面中

动态包含可以向被包含的页面传递参数（用处不大）**，并且是**分别处理包含页面的（将被包含页面编译后得出的结果再写进包含页面）【如果有相同名称的参数，使用静态包含就会报错！】！



##### 防止表单重复提交

- 利用javascript防止表单重复提交

  ```
  <form onsubmit="return dosubmit()">
  	这是表单内容
  </form>
  
  <script type="text/javascript">
  	 var isCommitted = false;//表单是否已经提交标识，默认为false
   8         function dosubmit(){
   9             if(isCommitted==false){
  10                 isCommitted = true;//提交表单后，将表单是否已经提交标识设置为true
  11                 return true;//返回true让表单正常提交
  12             }else{
  13                 return false;//返回false那么表单将不提交
  14             }
  15         }
  </script>
  ```

  还有一种方法是，当表单提交之后，设置提交按钮不可用

  ```
  function dosubmit(){
  2     //获取表单提交按钮
  3     var btnSubmit = document.getElementById("submit");
  4     //将表单提交按钮设置为不可用，这样就可以避免用户再次点击提交按钮
  5     btnSubmit.disabled= "disabled";
  6     //返回true让表单可以正常提交
  7     return true;
  8 }
  ```



