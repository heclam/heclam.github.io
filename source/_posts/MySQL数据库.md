---
title: MySQL数据库
date: 2019-8-16 14:19:19
toc: true
categories:
	- MySQL
tags:
	- MySQL
---

#### 数据库的范式

​     我们常见的也就三范式：

​	一个数据库设计如果符合第二范式（2NF），一定也符合第一范式(1NF)。如果符合第三范式(3NF)，一定也符合第二范式(2NF)。<!--more-->

​	那么下面我们就来了解什么叫做数据库的范式吧~

 * 1NF：属性不可再分（强调的是列的原子性，即列不能够再分成其他几列）
 * 2NF：属性完全依赖于主键（表中必须要有一个主键，表中的其他列（除主键列外）必须**完全依赖**于主键，而不能只依赖于主键的部分）
 * 3NF  : 属性不依赖于其他非主属性（消除传递依赖，确保每列都和主键列直接相关，而不是间接相关）



#### 数据库ACID

1. 原子性
   * 原子性是指事务是一个不可分割的工作单位，事务中的操作要么全部成功，要么全部失败。比如在同一个事务中的SQL语句，要么全部执行成功，要么全部执行失败。
2. 一致性
   * 事务必须使数据库从一个一致性状态变换到另外一个一致性状态。以转账为例子，A向B转账，假设转账之前这两个用户的钱加起来总共是2000，那么A向B转账之后，不管这两个账户怎么转，A用户的钱和B用户的钱加起来的总额还是2000，这个就是事务的一致性。
3. 隔离性
   * 隔离性是当多个用户并发访问数据库时，比如操作同一张表时，数据库为每一个用户开启的事务，不能被其他事务的操作所干扰，多个并发事务之间要相互隔离。
4. 持久性
   * 一旦事务提交，则其所做的修改将会永远保存到数据库中。即使系统发生崩溃，事务执行的结果也不能丢失。可以通过数据库备份和恢复来实现，在系统发生奔溃时，使用备份的数据库进行数据恢复



#### 并发一致性问题

1. 丢失修改
   * T1 和 T2 两个事务都对一个数据进行修改，T1 先修改，T2 随后修改，T2 的修改覆盖了 T1 的修改。
2. 脏读
   * （针对未提交数据）脏读又称无效数据读出。一个事务读取另外一个事务还没有提交的数据叫脏读。
   * **解决方法：**把数据库的事务隔离级别调整到 READ_COMMITTED
3. 不可重复读
   * 同时操作，事务1分别读取事务2操作时和提交后的数据，读取的记录内容不一致。不可重复读是指在同一个事务内，两个相同的查询返回了不同的结果。
   * **解决方法：**如果只有在修改事务完全提交之后才可以读取数据，则可以避免该问题。把数据库的事务隔离级别调整到REPEATABLE_READ
4. 幻读
   * 事务 T1 读取一条指定的 Where 子句所返回的结果集，然后 T2 事务新插入一行记录，这行记录恰好可以满足T1 所使用的查询条件。然后 T1 再次对表进行检索，但又看到了 T2 插入的数据。 **（和可重复读类似，但是事务 T2 的数据操作仅仅是插入和删除，不是修改数据，读取的记录数量前后不一致）**
   * **解决办法：**如果在操作事务完成数据处理之前，任何其他事务都不可以添加新数据，则可避免该问题。把数据库的事务隔离级别调整到 SERIALIZABLE_READ



#### mysql事务

  事务例子：A向B转账，转账这个流程中如果出现问题，事务可以让数据恢复成原来一样【A账户的钱没变，B账户的钱也没变】

​	如果没有添加事务，当A的钱减少的时候出现异常，而B并没有接受到A的转账

​	解决的方法：

``` java
try{
        //开启事务，对数据的操作就不会立即生效
        connection.setAutoCommit(false);
            ...a执行减操作
            ....出现问题 int a = 10/0;
            ...b执行加操作
        //如果执行到这里，没有抛出异常，就提交数据
        connection.commit();
        //关闭事务，设置回原始的状态
        connection.setAutoCommit(true);
    }catch(SQLException e){
        try{
        	//如果出现了异常，就会进到这里来，我们就把事务回滚【将数据变成原来那样】
            connection.rollback();
            //关闭事务，设置回原来的状态
            connection.setAutoCommit(true);
        }catch(SQLException e1){
           	e1.printStackTrace();
        }
    }
```

注意：当Connection遇到一个未处理的SQLException时，系统会非正常退出，事务也会自动回滚，但**如果程序捕获到了异常，是需要在catch中显式回滚事务的**



##### 事务的隔离级别

 * 数据库定义了4个隔离级别：
     * Serializable（串行化）【可避免脏读，不可重复读，虚读】
     * Repeatable read（可重复读）【可避免脏读，不可重复读】
     * Read committed（读已提交）【可避免脏读】
     * Read uncommitted（读未提交）【级别最低，什么都避免不了】
 * 分别对应
     * TRANSACTION_SERIALIZABLE
     * TRANSACTION_REPEATABLE_READ
     * TRANSACTION_READ_COMMITTED
     * TRANSACTION_READ_UNCOMMITTED

****



本文参考[https://frank-lam.github.io/fullstack-tutorial/#/MySQL](<https://frank-lam.github.io/fullstack-tutorial/#/MySQL>)

