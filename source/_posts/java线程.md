---
layout: article
title: java线程
date: 2019-07-07 16:11:28
categories:
	- java
tags:
	- java线程
---

#### 进程与线程的区别

* **进程:** 进程简单的来说就是在内存中运行的应用程序，一个进程可以启动多个线程。
  ​	比如在windows中一个运行( xxx.EXE文件 )就是一个进程。进程是系统运行程序的基本单位<!--more-->
* **线程:**  线程是是一个比进程更小的执行单位，一个线程执行过程中可以产生多个线程。

​	同一个线程中的进程共用相同的地址空间，同时共享进程所拥有的内存和其他资源。

#### 并行与并发

* 并行：同一时间段，多个任务都在执行（单位时间内不一定同时执行）
* 并发：单位时间内，多个任务同时执行

使用多线程可能带来的问题：内存泄露、上下文切换、死锁

##### 何为死锁

例子：线程A持有资源2，线程B持有资源1，在线程A、B都没有释放自己所持有资源的情况下（锁未释放），他们有必需获取相互的资源才能完成线程，比如线程A需要获取资源1（线程B也需要获取资源2）才能完成线程操作，所才会释放，现在各自抱着其他线程所需要的资源而形成死锁。

#### 创建多线程有两种方法

 * 继承Thread,重写run方法

   ```
   public class MyThread extends Thread {
       @Override
       public void run() {
           for (int x = 0; x < 200; x++) {
               System.out.println(x);
           }
       }
   }
   ----
   调用
   public static void main（String [] args）{
       MyThread my1 = new MyThread();
       my1.start();
   }
   ```

 * 实现Runnable接口，重写run方法（这个比较常用，避免java单继承的限制）

   ```
   public class MyRunnable implements Runnable {
   
       @Override
       public void run() {
           for (int x = 0; x < 100; x++) {
               System.out.println(x);
           }
       }
   
   }
   ----
   调用
       public static void main(String[] args) {
           MyRunnable my = new MyRunnable();
           Thread t1 = new Thread(my);
           t1.start();
       }
   
   ```

线程有3个基本状态：执行、就绪、阻塞

##### sleep()和wait()的区别

两者的区别在于：sleep方法没有释放锁，而wait方法释放了锁

* 两者都可以暂停线程的执行
* wait()通常用于线程间交互/通信，sleep()通常用于用户暂停执行
* wait()方法被调用后，线程不会自动苏醒，需要别的线程调用同一对象上的notify()或者notifyAll()方法。sleep方法执行完成后，线程会自动苏醒。

##### synchronized关键字

synchronized关键字解决多个线程之间访问资源的同步性，它可以保证由它休息的方法或代码块再任意时刻只有一个线程执行。

**用法：**

 * 修饰实力方法：给当前对象加锁，进入同步代码块前要获取当前对象实例的锁

   ```
   public synchronized void add(){}
   ```

 * 修饰静态方法：给当前类加锁（因为静态方法没有this），会作用于当前类的所有对象实例，因为静态成员不属于任何一个实例对象，是一个类成员。

    ```
      public synchronized static void add(){}
    ```

 * 修饰代码块：指定加锁对象，对给定对象加锁，进入同步代码块之前要获取给定对象的锁

   ```
   synchronized(object){  }
   ```

#### 线程中常用的方法

##### 方法详解

 * public static void sleep(long mills)
    * 当调用`sleep()`方法时，必须捕获异常或者向上层抛出异常。当线程休眠时间满时，并不一定会马上执行，因为此时有可能CPU正在执行其他的任务，所以调用了`sleep()`方法相当于线程进入了阻塞状态。
* public static void yield()
  * 调用`yield()`方法是为了让当前线程交出CPU权限，让CPU去执行其他线程。它和`sleep()`方法类似同样是不会释放锁。但是`yield()`不能控制具体的交出CUP的时间。并且它只能让相同优先级的线程获得CPU执行时间的机会。
  * 调用`yield()`方法不会让线程进入阻塞状态，而是进入就绪状态，它只需要等待重新获取CPU的时间，这一点和`sleep()`方法是不一样的。
* public final void join()
  * `join()`方法的作用是等待线程对象销毁，如果子线程执行了这个方法，那么主线程就要等待子线程执行完毕之后才会销毁
* setDaemon(boolean on),getDaemon()
  * 用来设置是否为守护线程和判断是否为守护线程
  * 守护线程依赖于创建他的线程，而用户线程则不需要。如果在`main()`方法中创建了一个守护线程，那么当main方法执行完毕之后守护线程也会关闭。而用户线程则不会，在JVM中垃圾收集器的线程就是守护线程。

#### 有返回值得多线程

​	**简单的实现有返回值得线程**


	 import java.util.concurrent.ExecutionException;
	 import java.util.concurrent.ExecutorService;
	 import java.util.concurrent.Executors;
	 import java.util.concurrent.Future;
	 public class CallableFuture {
	 	public static void main(String[] args) {
	 		//创建一个线程池
	 		ExecutorService pool = Executors.newFixedThreadPool(3) ;
	 		
	 		//创建三个有返回值的任务
	 		CallableTest2 c1 = new CallableTest2("线程1") ;
	 		CallableTest2 c2 = new CallableTest2("线程2") ;
	 		CallableTest2 c3 = new CallableTest2("线程3") ;
	 		
			Future f1 = pool.submit(c1) ;
	 		Future f2 = pool.submit(c2) ;
	 		Future f3 = pool.submit(c3) ;
	 		
	 		try {
	 			System.out.println(f1.get().toString());
	 			System.out.println(f2.get().toString());
	 			System.out.println(f3.get().toString());
	 		} catch (InterruptedException e) {
	 			e.printStackTrace();
	 		} catch (ExecutionException e) {
	 			e.printStackTrace();
	 		}finally{
	 			pool.shutdown();
	 		}
	 		
	 	}
	 }


​	
​	 import java.util.concurrent.Callable;
​	 
	 public class CallableTest2 implements Callable {
	 	private String name ;
	 
	 	public CallableTest2(String name) {
	 		this.name = name;
	 	}
	 
	 	@Override
		public Object call() throws Exception {
	 		return name+"返回了东西";
		}
	   }
