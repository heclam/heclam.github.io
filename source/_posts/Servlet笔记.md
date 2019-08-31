---
layout: article
title: Servlet笔记
date: 2019-07-08 10:19:20
toc: true
categories:
	- javaWeb
tags:
	- Servlet
---

##### 何为Servlet

Servlet是sun公司提供的一门用于开发动态web资源的技术

用户若想用发一个动态web资源(即开发一个Java程序向浏览器输出数据)，需要完成以下2个步骤：<!--more-->

  　　1. 编写一个Java类，实现servlet接口。
        　　2. 把开发好的Java类部署到web服务器中。

##### Servlet的运行过程

Servlet程序室友Web服务器调用，Web服务器接受到客户端的Servelt访问请求后：

1. Web服务器首先检查是否已经装载并创建了该Servlet的实例对象。如果是，则直接执行第4步，否则，执行第2步。

　2. 装载并创建该Servlet的一个实例对象。
 　3. 调用Servlet实例对象的init()方法。
 　4. 创建一个用于封装HTTP请求消息的HttpServletRequest对象和一个代表HTTP响应消息的HttpServletResponse对象，然后调用Servlet的service()方法并将请求和响应对象作为参数传递进去。
 　5. WEB应用程序被停止或重新启动之前，Servlet引擎将卸载Servlet，并在卸载之前调用Servlet的destroy()方法。 

Servlet生命周期：

```
1.被创建，执行且只执行一次init方法
2.提供服务，执行service方法，执行多次
3.被销毁，当Servlet服务器正常关闭时，执行destroy方法，只执行一次。     
```

**注意：只要有首次访问Servlet才创建目标**

##### Servlet调用图

![servlet_init.png](https://i.loli.net/2019/08/18/pQAkLvMUXGbDFsy.png)

##### 一个Servlet的创建例子

```java 
public class ServletDemo extends HttpServlet {
  
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
 		 response.setContentType("text/html");
         PrintWriter out = response.getWriter();
         out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
         out.println("<HTML>");
         out.println("  <HEAD><TITLE>A Servlet</TITLE></HEAD>");
         out.println("  <BODY>");
         out.print("Hello World!!!");
        out.println("  </BODY>");
         out.println("</HTML>");
         out.flush();
        out.close();
     }
   public void doPost(HttpServletRequest request, HttpServletResponse response)
             throws ServletException, IOException {
		this.doGet(request,response);
		
     }

 }
```

web.xml需要配置上servlet的映射

```xml
	<servlet>
     <servlet-name>ServletDemo</servlet-name>
     <servlet-class>com.heclam.ServletDemo</servlet-class><!--这里写类的全限定名-->
   </servlet>
 
   <servlet-mapping>
  	 <!--这里需要与上面Servlet-name一致 -->
     <servlet-name>ServletDemo</servlet-name>
     <!--这里是要输入什么才能访问到ServletDemo的映射-->
     <url-pattern>/ServletDemo</url-pattern>
   </servlet-mapping>
   注意：<servlet-mapping>可以设置多个，这个我们就有多个映射路径可以访问到ServletDemo了
```

##### 用servletContext实现请求转发

```java
public class ServletDemo extends HttpServlet {
  
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
            //获取ServletContext对象
 			ServletContext context = this.getServletContext();
 			//获取请求转发对象
 			RequestDispatcher rd = context.getRequestDispatcher("/ServletDemo2");
 			//调用forward方法实现请求转发
            rd.forward(request, response);
     }
   public void doPost(HttpServletRequest request, HttpServletResponse response)
             throws ServletException, IOException {
		...
     }

 }
```

