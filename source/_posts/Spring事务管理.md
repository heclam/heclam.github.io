---
title: Spring事务管理
date: 2019-8-14 14:15:19
toc: true
categories:
	- Spring
tags:
	- Spring事务
---



##### 事务的四个特性（ACID）：

* **原子性：**原子性是指事务是一个不可分割的工作单位，事务里面的操作要么都发生，要么都不发生，如果有一个操作失败，事务将回滚<!--more-->

- **一致性：**一致性指事务前后数据的完整性必须保持一致（事务执行的前后都是合法的数据状态，不会违背任何的数据完整性）

- **隔离性：**隔离性指多个用户并发访问数据库时，一个用户的事务不被其他用户的事务所干扰，多个并发事务之前要相互隔离

- **持久性：**持久性是指一个事务一旦被提交，那么数据就一定会被写入到数据库中并持久储存起来（事务被提交之后无法再回滚）



##### 事务的属性

**传播行为（propagation）：**传播行为定义了客户端与被调方法见的事务边界

* PROPAGATION_REQUIRED：默认传播行为，指的是若当前存在事务，则加入该事务；如果当前没有事务，则创建一个新事务
* PROPAGATION_REQUIRES_NEW：需要创建一个新的，若当前有事务，则将当前事务挂起。
* PROPAGATION_SUPPORTS： 当前存在事务，就在事务中运行；当前不存在事务，则不在事务中运行。
* PROPAGATION_NOT_SUPPORTED: 不运行在事务中，当前有事务，则挂掉当前事务。
* PROPAGATION_NEVER：不运行在事务中，如果当前有事务，则抛出异常。
* PROPAGATION_MANDARORY: 必须运行在事务中，如果当前方法没有事务，则抛出异常。
* PROPAGATION_NESTED: 当前存在事务，则创建一个事务作为当前事务的嵌套事务运行，如果当前没有事务，则创建一个新的事务。

**隔离级别（isolation）:**指若干个并发事务之间的隔离程度

* 默认（DEFAULT）

* READ_UNCOMMITTED：读未提交，一个事务可以读取到另一个事务未提交的数据。（脏读，不可重复读，幻读）。

*  READ_COMMITTED：读已提交，一个事务只能读取另一个事务已经提交的数据。（不可重复读，幻读）。

* REPEATABLE_READ：可重复读一个事务在整个过程中多次重复执行某个查询，每次返回的结果都相同。（幻读）

* SERIALIZABLE：序列化所有事务依次逐个执行，这样事务之间不可能存在干扰。

隔离引发的一些问题：

* 脏读：一个事务读取了另一个事务改写但未提交的数据
* 不可重复读（侧重于修改）：一个事务执行多次的读操作，每次得到不同数据。（并发访问造成的，可能另外一个事务已经对数据进行了更改）
* 幻读（侧重于新增或删除）：类似不可重复读。一个事务读取时遇到另一个事务的插入，则这个事务就会读取到一些原本不存在的记录。
  只读：事务只读时，数据库就可以对其进行一些特定的优化。
  事务超时：事务运行时间过长。
  回滚原则：定义那些异常会导致事务回滚。（默认情况只有运行时异常才事务回滚）



##### Spring基于XML的声明式事务

  * Spring的声明式事务的支持是通过Spring AOP框架来实现的

```
<!-- 1.加载配置文件 --> 
<context:property-placeholder location="classpath:jdbc.properties"/> 

<!--2.配置数据源：有两个选择 c3p0,阿里的连接池druid-->
<bean id="dataSource" class="com.mchange.v2.c3p0.ComboPooledDataSource">
         <!--配置链接属性-->
         <property name="driverClass" value="${jdbc.driver}"/>
         <property name="jdbcUrl" value="${jdbc.url}"/>
         <property name="user" value="${jdbc.username}"/>
         <property name="password" value="${jdbc.username}"/>
  </bean>
  
  <!--3.配置事务管理器-->
<bean id="transactionManager" class="...DataSourceTransactionManager">
     <property name="dataSource" ref="dataSource"/>
</bean>

<!--4.配置事务通知，根据方法名，指定事务的属性--><!--这是需要tx和aop的命名空间-->
<tx:advice id="txAdvice" transaction-manager="transactionManager">
    <tx:attributes>
        <tx:method name="get*" read-only="true" propagation='"SUPPORTS">
        <tx:method name="transferAc" protagation="REQUIRED">
    </tx:attributes>
</tx:advice>

<!--5.配置aop,将事务和切入点关联起来-->
<aop:config>
    <aop:pointcut expresssion="execution(* com.heclam.*(...))" id="txPointcut">
    <aop:advisor advice-ref="txAdvice" pointcut-ref="txPointcut"/>
</aop:config>
```

\<tx:advice\>实现声明事务性策略，所有的事务配置都在这里面，

* isolation:指定事务的隔离级别

* propagation:指定事务的传播规则

* read-only:指定事务为只读

使用\<tx:advice\>声明事务，需要一个事务管理器，这里定义了一个AOP的通知，然后还需要使用\<aop:config\>定义一个通知器（advisor）,里面还需定义一个切入点



##### Spring基于注解的声明式事务配置

```
<!-- 1.加载配置文件 --> 
<context:property-placeholder location="classpath:jdbc.properties"/> 

<!--2.配置数据源：有两个选择 c3p0,阿里的连接池druid-->
<bean id="dataSource" class="com.mchange.v2.c3p0.ComboPooledDataSource">
         <!--配置链接属性-->
         <property name="driverClass" value="${jdbc.driver}"/>
         <property name="jdbcUrl" value="${jdbc.url}"/>
         <property name="user" value="${jdbc.username}"/>
         <property name="password" value="${jdbc.username}"/>
  </bean>
  
  <!--3.配置事务管理器-->
<bean id="transactionManager" class="...DataSourceTransactionManager">
     <property name="dataSource" ref="dataSource"/>
</bean>

<!--4.启动事务注解-->
<tx:annotation-driven transaction-manager="transactionManager">

以下的配置是在类中
<!--5.可以在对应的方法或类上添加Transaction注解-->
@Transaction
public void transferAc(){
}
或者
@Transaction
public class Demo{
    
}
```

个人觉得还是基于注解的声明式事务比较简单些,不用做很多的配置

