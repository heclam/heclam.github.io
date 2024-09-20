---
layout: article
title: Ajax
date: 2019-06-26 11:19:37
toc: true
tags:
	- ajax
---

>当我们使用AJAX之后，浏览器是先把请求发送到XMLHttpRequest异步对象之中，异步对象对请求进行封装，然后再与发送给服务器。服务器并不是以转发的方式响应，而是以流的方式把数据返回给浏览器
>
>XMLHttpRequest异步对象会不停监听服务器状态的变化，得到服务器返回的数据，就写到浏览器上【因为不是转发的方式，所以是无刷新就能够获取服务器端的数据】<!--more-->



##### XMLHttpRequest对象

```javascript
function getXHR() {
    //根据对象判断浏览器
    if(window.XMLHttpRequest) {
        //不是IE
        return new XMLHttpRequest();
    }else{
        //IE
        return new ActiveXObject("Microsoft.XMLHttp");
    }
}
var xhr = getXHR();
```



##### get方式请求

```javascript
//参数解析：
	第一个参数：表示请求的方式
	第二个参数：表示请求的地址
	第三个参数：表示是否为异步请求（true表示发送异步请求）
xhr.open("get", url, true);
```



##### 发送请求

```javascript
xhr.send(null);
```



##### 回调函数

XHR 对象的`readyState` 属性可取的值：

0：未初始化。尚未调用open()方法。

1：启动。已经调用open()方法，但尚未调用send()方法。

2：发送。已经调用send()方法，但尚未接收到响应。

3：接收。已经接收到部分响应数据。

4：完成。已经接收到全部响应数据，而且已经可以在客户端使用了。

只要readyState 属性的值由一个值变成另一个值，都会触发一次readystatechange 事件。可以利用这个事件来检测每次状态变化后readyState 的值。

````javascript
xhr.onreadystatechange = function(){
	 //当请求结束且没报错时
     if(xhr.readyState==4 && xhr.status==200) {
         var txt = xhr.responseText;
         console.log(txt);
     }
};

````

responseText:作为响应主体被返回的文本

responseXML：如果响应的内容类型是”text/xml”或”application/xml”，这个属性中将保存包含着响应数据的XML DOM 文档。



##### post请求

```javascript
xhr.open("post",url, true);
//设置请求头
xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

//发送form表单数据
var form = document.getElementById("form");
//发送序列化的表单
xhr.send(serialize(form));

//发送普通数据
xhr.send("name = abc");
xhr.send("book = JavaScript");
```



##### $.ajax()

语法：

$.ajax({key:value,key:value})

例如：

```javascript
$.ajax({
	type: "GET", //String类型参数，请求方式POST或GET,默认为GET,请求方式

	url: "xxx", //String类型参数，发送请求的地址
	
	async: true,  //Boolean类型参数，表示为异步请求，默认为true

	cache: false,  //Boolean类型参数，是否设置缓存,默认为false

	contentType: xxx, //发送信息至服务器时内容编码类型

	data: String , //Object或String类型参数，发送到服务器的数据，比如为"json"
	
	dataType: String, //String类型参数，服务器返回的数据类型

	success: function(data){}//成功回调的函数
	
	error:function(data){} //失败回调函数

});
```