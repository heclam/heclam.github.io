---
layout: article
title: Session笔记
date: 2019-07-08 15:30:31
toc: true
categories:
	- javaWeb
tags:
	- Session
---

##### 何为Session

在WEB开发中，服务器可以为每个用户浏览器创建一个会话对象（session对象），用户使用浏览器访问服务器的时候，服务器把用户的信息以某种的形式记录在服务器中，这就是Session。<!--more-->

##### Session与Cookie的区别

* Cookie是把用户的数据写给用户的浏览器

* Session技术把用户的数据写到用户独占的Session中

* Session对象由服务器创建，开发人员可以调用request对象的getSession方法得到session对象

* Cookie保存在浏览器中，Session保存在服务器中

##### 有了Cookie为何还要使用Session呢

**Session比Cookie使用方便，Session可以解决Cookie解决不了的事情，比如：Session可以存储对象而Cookie只能存储字符串。**

##### Session的实现原理

服务器创建session出来后，会把session的id号，以cookie的形式回写给客户机，这样，只要客户机的浏览器不关，再去访问服务器时，都会带着session的id号去，服务器发现客户机浏览器带session id过来了，就会使用内存中与之对应的session为之服务

##### SessionAPI

| 方法                                        | 描述                      |
| ------------------------------------------- | ------------------------- |
| long getCreationTime()                      | 获取Session被创建时间     |
| String getId()                              | 获取SessionId             |
| long getLastAccessedTime()                  | 返回Session最后活跃的时间 |
| ServletContext getServletContext()          | 获取ServletContext对象    |
| void setMaxInactiveInterval(int var)        | 设置Session超时时间       |
| int getMaxInactiveInterval()                | 获取Session超时时间       |
| Object getAttribute(String var)             | 获取Session属性           |
| Enumeration getAttributeNames()             | 获取Session所有的属性名   |
| void setAttribute(String var1, Object var2) | 设置Session属性           |
| void removeAttribute(String var)            | 移除Session属性           |
| void invalidate()                           | 销毁该Session             |
| boolean isNew();                            | 该Session是否为新的       |

##### 一个Session的例子

```
public class SessionDemo1 extends HttpServlet {
 
     public void doGet(HttpServletRequest request, HttpServletResponse response)
             throws ServletException, IOException {
 
         response.setCharacterEncoding("UTF=8");
         response.setContentType("text/html;charset=UTF-8");
         //使用request对象的getSession()获取session，如果session不存在则创建一个
         HttpSession session = request.getSession();
         //将数据存储到session中
         session.setAttribute("data", "heclam");
         //获取session的Id
       String sessionId = session.getId();
         //判断session是不是新创建的
         if (session.isNew()) {
             response.getWriter().print("session创建成功，session的id是："+sessionId);
         }else {
             response.getWriter().print("服务器已经存在该session了，session的id是："+sessionId);
         }
     }
 
     public void doPost(HttpServletRequest request, HttpServletResponse response)
             throws ServletException, IOException {
        doGet(request, response);
    }
 }
```

**Session也是一个域对象，Session作为一种记录浏览器状态的机制，只要Session对象没有被销毁，Servlet之间就可以通过Session对象实现通信**

```
//使用request对象的getSession()获取session，如果session不存在则创建一个
         HttpSession session = request.getSession();
//获取存入Session的属性
		String val = session.getAttribute("data");
```

