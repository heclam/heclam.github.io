---
title: java集合总结
date: 2019-06-29 11:52:00
categories:
	- java基础
	- java集合
tags:
	- java
---

#### **List:**(有序（存储顺序和取出顺序一致），可重复)

> - ArrayList底层数据结构是数组。线程不安全J(如果想要ArrayList是实现同步，可以使用Collections的方法：List list = Collections.synchronizedList(new ArrayList()),就可以实现同步了)
> - LinkedList底层数据结构是链表。线程不安全(LinkedList底层是**双向链表**)
> - Vector底层数据结构是数组。线程安全<!--more-->

​	#### 注：查询多用Arraylist（底层实现为数组），增删多则用LinkedList(底层实现为双向链表（双向链表方便往前遍历）)



#### Set（元素不可重复）

- HashSet集合
  - 底层数据结构是哈希表（是一个元素为链表的数组）+红黑树
- TreeSet集合
  - 底层数据结构是红黑树（是一个自平衡的二叉树）
  - 保证元素的排序方式
- LinkedHashSet集合
  - 底层数据结构是由哈希表（是一个元素为链表的数组）+双向链表组成



#### HashSet

- 实现Set接口
- 不保证迭代顺序
- 允许元素为空
- 底层实际上是一个HashMap实例
- 非同步
- 初始容量非常影响迭代性能



#### TreeSet

- 实现NavigableSet接口
- 可以实现排序功能
- 有序，不允许为null
- 底层实际是一个TreeMap实例
- 非同步



#### LinkedHashSet

- 迭代是有序的
- 允许为空
- 底层实际是一个HashMap+双向链表实例（其实就是LinkedHashMap）
- 非同步
- 性能比HashSet差一点，因为要维护一个双向链表
- 初始容量与迭代无关，LinkedHashSet迭代的是双向链表



#### 散列表（数组+链表）

- **不在意元素的顺序，能够快速的查找元素的数据**



#### 红黑树

- 红黑树是二叉搜索树。
- **根节点是黑色**。
- **每个叶子节点都是黑色的空节点（NIL节点）**。
- **每个红色节点的两个子节点都是黑色。(从每个叶子到根的所有路径上不能有两个连续的红色节点)**
- **从任一节点到其每个叶子的所有路径都包含相同数目的黑色节点(每一条树链上的黑色节点数量（称之为“黑高”）必须相等)**。



#### Map

- Set<Map.Entry<k key,v value> entrySet();返回的是键值对对象的集合
- Set<K> keySet(); 获取集合中所有的键的集合
- Collection<V> values(); 获取集合中所有值得集合

Map集合的特点：将键映射到值得对象，一个映射不能包含重复的键，每个键最多只能映射到一个值

Map和Collection集合的区别：

​	1.Map集合存储元素师成对出现的，Map的键是唯一的，值是可以重复的

​	2.Collection集合存储元素是单独出现的，collection的儿子set是唯一的，List是可重复的



#### HashMap

```
特点：  
 * 无序，允许为null,非同步
 * 底层由散列表（哈希表）实现
 * 初始容量和装载因子对HashMap影响挺大的，设置小了不好，设置大了也不好
```



#### HashMap与Hashtable比较

> 从存储结构和实现来讲基本上都是相同的。Hashtable和HashMap的最大的不同是它是线程安全的，另外它不允许key和value为null。Hashtable是个过时的集合类，不建议在新代码中使用，不需要线程安全的场合可以用HashMap替换，需要线程安全的场合可以用ConcurrentHashMap替换



#### LinkedHashMap（继承HashMap）

> - 底层是散列表和双向链表
> - 允许为null,不同步
> - 插入的顺序是有序的（底层链表致使有序）
> - 装载因子和初始容量对LinkedHashMap影响很大



#### TreeMap

> - TreeMap实现了NavigableMap接口，而NavigableMap接口继承着SortedMap接口，致使我们的TreeMap是有序的！
> - TreeMap底层是红黑树，它方法的时间复杂度不会太高
> - 非同步，想要同步可以使用Collections来进行封装
> - 使用Comparator或者Comparable来比较key是否相等与排序的问题
> - key不能为null，为null抛出NullPointException的异常



#### ConCurrentHashMap

> - 底层实现是：散列表和红黑树，与hashMap是一样的
> - ConcurrentHashMap支持高并发的访问和更新，他的线程是安全的
> - 检索操作不用加锁，get方法是非阻塞的
> - key和value都不允许为空

