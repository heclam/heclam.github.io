---
layout: article
title: java反射机制
date: 2019-06-26 14:41:36
toc: true
categories: 
	- java
tags:
	- java
---

Java反射机制是指在<font color=red> 运行状态中 </font>，对于任意一个 **类**，都能够知道这个类的所有属性和方法；对于任意一个**对象**，都能够调用它的任意一个方法和属性；这种动态获取的信息以及动态调用对象的方法的功能称为java语言的反射机制。
用一句话总结就是反射可以实现在运行时可以知道任意一个类的属性和方法。<!--more-->

#### 获取class的三种方法

```获取class
package com.luo.test;
public class TestDemo{
    public static void main(String[] args){
        //第一种
        Class class1 = TestDemo.class;
        System.out.pritnln(class1.getName（）);
        
        //第二种
        TestDemo demo = new TestDemo();
        Class class2 = demo.getClass();
        System.out.pritnln(class2.getName（）);
        
        //第三种
        Class class3 = Class.forName("com.luo.test.TestDemo");//这里面的参数为类的全量限定名
        System.out.pritnln(class2.getName（）);
    }
}

执行结果为：
		com.luo.test.TestDemo
		com.luo.test.TestDemo
		com.luo.test.TestDemo
```



#### 获取这个Class有什么作用？

- 获取成员方法Method
- 获取成员变量Field
- 获取构造函数Constructor

#### 获取成员方法

单独获取某一个方法是通过Class类的以下方法获得的：

```huo
//参数：第一个参数为方法名，后面的参数为 参数类型 比如 String.class int.class(可以看下方的例子)
public Method getDeclaredMethod(String name, Class<?>... parameterTypes) // 得到该类所有的方法，不包括父类的
public Method getMethod(String name, Class<?>... parameterTypes) // 得到该类所有的public方法，包括父类的
```

例如有个一类

```
public class Person {
    private String name;
    private int age;
    private String msg="hello wrold";
 public String getName() {
        return name;
  }

    public void setName(String name) {
        this.name = name;
  }

    public int getAge() {
        return age;
  }

    public void setAge(int age) {
        this.age = age;
  }

    public Person() {
    }

    private Person(String name) {
        this.name = name;
  System.out.println(name);
  }

    public void fun() {
        System.out.println("fun");
  }

    public void fun(String name,int age) {
        System.out.println("我叫"+name+",今年"+age+"岁");
  }
}

public class TestDemo{
    public static void main(String[] args){
        try {
            Class c = Class.forName("com.luo.Person");
            Object o = c.newInstance();
            Method method = c.getMethod("fun", String.class, int.class);
            method.invoke(o, "abc", 18);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

```
执行结果：
	我叫abc,今年18岁
```

获取所有的成员方法：

```
Class c = Class.forName("com.luo.Person");
//不带参数则获取全部的方法
Method[] methods = c.getDeclaredMethods(); // 得到该类所有的方法，不包括父类的
或者：
Method[] methods = c.getMethods();// 得到该类所有的public方法，包括父类的

遍历数组：
for (Method method : methods)

```

#### 获取成员变量

单独获取某个成员变量，通过Class类的以下方法实现：

```
public Field getDeclaredField(String name) // 获得该类自身声明的所有变量，不包括其父类的变量
public Field getField(String name) // 获得该类自所有的public成员变量，包括其父类变量

注意：field.setAccessible(true);//如果变量的访问属性为private,需设置是否允许访问
```

获取全部的成员变量：

```
Field[] fields = c.getDeclaredFields();

然后遍历变量数组,获得某个成员变量：
for(Field field : fields)
```



#### 获取构造方法

获取单个构造函数（传入参数）

```
public Constructor<T> getDeclaredConstructor(Class<?>... parameterTypes) //  获得该类所有的构造器，不包括其父类的构造器
public Constructor<T> getConstructor(Class<?>... parameterTypes) // 获得该类所以public构造器，包括父类

注意： constructor.setAccessible(true);//如果构造方法为private，需设置是否允许访问

```

获取该类的所有构造函数

```
Constructor[] constructors = c.getDeclaredConstructors();

然后遍历：
for (Constructor constructor : constructors)
```