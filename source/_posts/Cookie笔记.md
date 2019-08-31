---
layout: article
title: Cookie笔记
date: 2019-07-08 11:12:20
toc: true
categories:
	- javaWeb
tags:
	- Cookie
---

##### 何为会话

用户开一个浏览器，点击多个超链接，访问服务器多个web资源，然后关闭浏览器，整个过程称之为一个会话。<!--more-->

##### Cookie的流程：

浏览器访问服务器，如果服务器需要记录该用户的状态，就使用response向浏览器发送一个Cookie，浏览器会把Cookie保存起来。当浏览器再次访问服务器的时候，浏览器会把请求的网址连同Cookie一同交给服务器。

​	创建一个Cookie对象，发送给浏览器

​	Cookie cookie = new Cookie(key,valus); //创建Cookie对象，指定名称和值

​	Cookie.setAge(秒);  //设置时间

​	response.addCookie(cookie);//向浏览器发送一个cookie

##### 保存会话数据的两种技术

Cookie和Session

##### Cookie API

| 方法                                   | 类型     | 描述                                                         |
| :------------------------------------- | -------- | ------------------------------------------------------------ |
| Cookie(String name ,String value)      | 构造方法 | 实例化Cookie对象，传入Cookie名称和Cookie值                   |
| public String getName()                | 普通方法 | 获取Cookie的名称                                             |
| public String getValue()               | 普通方法 | 获取Cookie的值                                               |
| public String setValue()               | 普通方法 | 设置Cookie的值                                               |
| public void setMaxAge(int expiry)      | 普通方法 | 设置Cookie的有效期，如果expiry为正数，浏览器会把Cookie写入到硬盘中，如果为负数，Cookie是临时的 |
| public int getMaxAge()                 | 普通方法 | 获取Cookie的有效期                                           |
| setPaht(String uri) 与getPath()        | 普通方法 | 有效路径                                                     |
| setDomain(String pattern)与getDomain() | 普通方法 | 有效域                                                       |

##### 一个Cookie例子

```java
public class ServletDemo extends HttpServlet {
  
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
 		 //设置服务器端以UTF-8编码进行输出
         response.setCharacterEncoding("UTF-8");
         //设置浏览器以UTF-8编码进行接收,解决中文乱码问题
        response.setContentType("text/html;charset=UTF-8");
        //创建一个Cookie对象,并指定名称和值
        Cookie cookie = new Cookie("username","abc");
        //设置cookie有效期
        cookie.setMaxAge(1000);
        //向浏览器添加一个Cookie
        response.addCookie(cookie);
     }
     
   public void doPost(HttpServletRequest request, HttpServletResponse response)
             throws ServletException, IOException {
		this.doGet(request,response);
		
     }

 }
```

获取Cookie

```java
 Cookie[] cookies = request.getCookies();
        for (int i = 0;cookies! = null && i < cookies.length; i++) {
                String name = cookies[i].getName();
                String Value = cookies[i].getValue();
            }
```

##### 一些Cookie方法的解析

Cookie有效期：

- 如果MaxAge为**正数**，**浏览器会把Cookie写到硬盘中，只要还在MaxAge秒之前，登陆网站时该Cookie就有效**【不论关闭了浏览器还是电脑】
- 如果MaxAge为**负数**，**Cookie是临时性的，仅在本浏览器内有效**，关闭浏览器Cookie就失效了，Cookie不会写到硬盘中。Cookie默认值就是-1。
- 如果MaxAge为**0**，则表示**删除该Cookie**。Cookie机制没有提供删除Cookie对应的方法，把MaxAge设置为0等同于删除Cookie

##### Cookie中存取中文

```java
//Cookie存储中文的时候需要使用URLEncoder类里面的encode(String s,String enc)方法进行中文转码
Cookie cookie = new Cookie("userName", URLEncoder.encode("孤傲苍狼", "UTF-8"));
response.addCookie(cookie);

//获取Cookie中的中文，需要使用URLEncoder类里面的decode(String s,String enc)进行解码
URLDecoder.decode(cookies[i].getValue(), "UTF-8")
```

