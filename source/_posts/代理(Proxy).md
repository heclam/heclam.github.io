---
layout: article
title: 代理（Proxy）
date: 2019-07-15 15:01:21
toc: true
tags:
	- java
---



动态代理技术是整个java技术中最重要的一个技术，它是学java框架的基础，在Spring框架中广泛使用

动态代理技术就是用来产生一个对象的代理对象<!--more-->

1. 代理对象存在的价值主要用于拦截对真实业务对象的访问
2. 代理对象应该具有和目标对象（真实业务对象）相同的方法

##### java中的代理

###### 通过java.lang.reflect.Proxy 类提供的一个newProxyInstance方法来串讲一个对象的代理对象

想要生成某一个对象的代理对象，这个代理对象也要编写一个类来生成

``` java
参数解析：
	first arg:ClassLoader loader用来指明生成代理对象使用哪个类加载器
	second arg:Class<?>[] interfaces用来指明生成哪个对象的代理对象，通过接口指定
	third arg:InvocationHandler h用来指明产生的这个dialing对象要做什么事情
static Object newProxyInstance(ClassLoader loader,class<?>[] interfaces,InvocationHandler h)
```

###### 编写生成代理对象的类

在java中规定，要想产生一个对象的代理对象，那么这个对象必须要有一个接口

例子：(要想为一个类生成一个代理对象，前提是这个类必须要有一个接口，比如下面的AbcMan就有一个Person接口)

```java
public interface Person{
    String sing(String name);
    String dance(String name);
}
```

```java
public class AbcMan implements Person{
    public String sing(String name){
        return "sing a"+name+"song";
    }
    
    public String dance(String name){
        return "dance"+name;
    }
}
```

###### 创建生成代理对象的代理类

```java
public class AbcManProxy{
	//设计一个类变量记住代理类要代理的目标对象
    private Person abc = new AbcMan();
    
    //设计一个方法生成代理对象
    public Person getProxy(){
        return (Person)Proxy.newProxyInstance(AbcManProxy.class.getClassLoader(),abc.getClass().getInterfaces(),new InvocationHandler(){
        	/*
        		参数解析：
        			first arg:把代理对象自己传递进来
        			second arg:把代理对象当前调用的方法传递进来
        			third arg:把方法参数传递进来
        	*/
            @Override
            public Object invoke(Object proxy,Method method,Object[] args) throws Throwable{
                //如果调用的是代理对象的sing方法
                if(method.getName().equals("sing")){
                    System.out.println("这里由代理类做一些操作sing");
                    return method.invoke(abc,args);//代理对象调用真实目标对象的sing方法去处理用户请求
                }
                if(method.getName().equals("dance")){
                    System.out.println("由代理类做一些操作dance");
                    return method.invoke(abc,args);
                }
                return null；
            }
        })；
    }
    
}
```

测试：

````java
public Class Test{
    public static void main(String[] args){
        AbcManProxy proxy = new AbcManProxy();
        //获取代理对象
        Person p = proxy.getProxy();
        //调用代理对象的sing方法
        String singstr = p.sing("aaa");
        System.out.println(singstr);
        //调用代理对象的dance方法
        String dancestr = p.dance("bbb");
        System.out.println(dancestr);
    }
}
-----
输出的结果为：
这里由代理类做一些操作sing
sing a aaa song  //目标方法执行
由代理类做一些操作dance
dance bbb 

````



##### 静态代理模式

```java
public interface Person{
    void sing();
}
```

静态代理需要创建接口实现类和代理类，这两个类分别实现这个接口

```java
public class Abc implements Person{
    @Override
    public void sing(){
        System.out.println("Abc高歌一曲");
    }
}
```

```java
public class ProxyAbc implements Person{
    //接收实现类对象
    private Person p;
    //通过构造方法传进来实现类对象
    public ProxyAbc(Person p){
        this.p = p;
    }
     @Override
    public void sing(){
        System.out.println("代理类做的操作");
        this.p.sing();
    }
}
```

静态代理测试：

```java
public class Test{
    public static void main(String [] args){
        Person abc = new Abc();
        Person proxy = new ProxyAbc(abc);
        proxy.sing();
    }
}
```

##### JDK动态代理

* 动态代理不需要创建代理类，只需要编写一个动态处理器即可

```java
public class JdkProxyHandler{
	//用来接收实现类对象
    private Object abc;
    
    //通过构造方法传进来实现类对象
    public JdkProxyHandler(Person p){
        super();
        this.abc = p;
    }
    
    //为实现类对象创建一个代理对象的实例
    public Object getProxyInstance(){
    		/*
        		参数解析：
        		first arg:指定当前目标对象使用的类加载器
        		second arg: 指定目标对象实现的接口的类型
        		third arg: 指定动态处理器
        	*/
        return Proxy.newProxyInstance(abc.getClass().getClassLoader(),abc.getClass().getInterfaces(),new InvocationHandler(){
            @Override
            public Object invoke(Object proxy,Method method,Object[] args) throws Throwable{
               System.out.printn("代理做一些相应的操作");
               Object object = method.invoke(abc,args);
                return obejct；
            }
        });
    }
}
```

JDK动态代理测试：

```java
public class Test{
    public static void main(String [] args){
        Person abc = new Abc();
        Person proxy = new JdkProxyHandler(abc).getProxyInstance();
        proxy.sing();
    }
}
```

##### CGLIB动态代理

JDK动态代理需要实现类通过接口定义方法，那对于没有实现接口的类就使用了CGLIB动态代理来实现

```java
public class CglibProxyHandler implements MethodInterceptor{
	//维护目标对象
    private Object target;
    public Object getProxyInstance(final Object target){
        this.target = target;
        // Enhancer类是CGLIB中的一个字节码增强器，它可以方便的对你想要处理的类进行扩展
       Enhancer enhancer = new Enhancer();
       // 将被代理的对象设置成父类
       enhancer.setSuperclass(this.target.getClass());
       // 回调方法，设置拦截器
       enhancer.setCallback(this);
       // 动态创建一个代理类
       return enhancer.create();
    }
     @Override
   public Object intercept(Object object, Method method, Object[] args,
           MethodProxy methodProxy) throws Throwable {

       System.out.println("代理做一些操作");
       Object result = methodProxy.invokeSuper(object, args);
       return result;
   }
}
```

使用 CGLIB 需要实现 MethodInterceptor 接口，并重写intercept 方法，在该方法中对原始要执行的方法前后做增强处理。该类的代理对象可以使用代码中的字节码增强器来获取

JDK动态代理测试：

```java
public class Test{
    public static void main(String [] args){
        Person abc = new Abc();
        Person proxy = new CglibProxyHandler().getProxyInstance(abc);
        proxy.sing();
    }
}
```



JDK 动态代理和 CGLIB 动态代理均是实现 Spring AOP 的基础

如果目标对象实现了接口，默认情况下会采用 JDK 的动态代理实现 AOP；如果目标对象没有实现了接口，则采用 CGLIB 库，Spring 会自动在 JDK 动态代理和 CGLIB 动态代理之间转换。



