---
layout: article
title: php笔记
date: 2024-02-12 16:40:41
toc: true
categories:
	- php
tags:
	- php
---

PHP是一门弱类型语言(PHP: Hypertext Preprocessor)

php基本语法：

 ```
<?php
	//php代码
	//echo与print的区别都是打印语句，echo可以同时打印多个字符串，prin只能打印一个
	//echo打印之后没有返回值，print始终会返回一个1
	echo "first line code";	// echo "test1","test2"
	print "hello world";
?>

php 脚本以<?php 开始， 以 ?> 结束
php文件通常包含HTML标签和一些PHP脚本代码
 ```

<!--more-->

注释：

	// 单行
	/*
	* 
	多行注释
	*/



大小写敏感：

```
在PHP中，所有用户定义的函数、类和关键字（例如if、else、echo等）都对大小写不敏感
不过在PHP中，所有变量都会对大小写敏感
```

变量：

	* 以$符号开始，后面接变量名称：$age (变量名称区分大小写 ,$Age 是另一个变量)
	* 变量名只能包含字母、数字和下划线



变量作用域：

 * local (局部)		// 如在方法内定义的变量即为局部变量（变量前面若有 global则为全局变量）

 * global（全局）          //  函数内调用函数外定义的全局变量，需要在函数内变量前加上global关键字

   ```
   <?php
   	$x = 1;
   	$y = 2;
   	
   	function test(){
   		global $x,$y;
   		$y = $x + $y;	//对全局变量$y 进行了新的赋值
   	}
   ?>
   
   另一种写法
   // PHP将所有全局变量存储在一个名为$GLOBALS[index]的数组中，index保存变量的名称， 这个数组可以在函数内部访问，也可以直接用来更新全局变量
   <?php
   	$x = 1;
   	$y = 2;
   	function test(){
   		$GOLBALS['y'] = $GLOBALS['x'] + $GLOBALS['y'];	//对全局变量$y 进行了新的赋值
   	}
   ?>
   ```

   

 * static （静态）     

   ```
   <?php
   	function test(){
   		static $x = 0;
   		echo $x;
   		$x++;			//静态变量进行加1
   		echo PHP_EOL	//换行符
   	}
   	test();		// echo 0
   	test();		// echo 1
   	test();		// echo 2
   ?>
   ```

   如在一个方法内定义了一个静态变量(对变量进加1），那么每一次调用这个方法的时候，都是从之前的基础上进行加 1

 * parameter(参数作用域)  //  方法的形参与实参

   参数是通过调用代码将值传递给函数的局部变量。

   参数是在参数列表中声明的，作为参数声明的一部分



数据类型：

	* String(字符串)   //使用"."（并置运算符）用于把两个字符串值连接起来。字符串可以使用单引号或双引号
	* Integer(整型)
	* Float(浮点型)
	* Boolean(布尔型)
	* Array(数组)
	* Object(对象)
	* NULL(空值)



类型比较	(注意 0 、false、null 这三者的比较)

	* 使用两个等号'=='比较，只比较值，不比较类型
	* 使用三个等号'==='比较，比较值，也比较类型



常量：

 常量值被定义后，在脚本的其他任何地方都不能被改变

**常量的命令规则与变量一样，但不需要在前面加上$ 符号**

设置常量使用define()函数

```
bool define(string $name, mixed $value [, bool $case_insensitive = false])

name :必选参数，常量名称
value：必选参数，常量值
case_insensitive: 可选参数，如果设置为TRUE,该常量大小写不敏感。默认是大小写敏感

例子：
define("GREETING", "Welcome");
```



顺序语句：

	* if.....elseif....else语句
	* switch语句



循环语句：

	* while语句
	* do....while语句
	* for 语句
	* foreach语句



数组（Array）（详解）：

1、在PHP中，有三种类型的数组：

 * 数值数组 - 带有数字索引的数组 

   ```
   <?php
   $cars = array("Volvo", "BMW", "Toyota");
   ?>
   // 可通过下标访问,获取数组长度count($cars)
   ```

 * 关联数组 - 带有指定键的数组，每个键关联一个值

   ```
   <?php
   	$age = array("Peter" => "35", "Ben" => "37", "Joe" => 18);
   	//遍历数组
   	foreach($age as $x => $x_value){
   		echo "key=". $x .", value= ". $x_value;
   		echo "<br>"
   	}
   ?>
   $age['Peter']		// 可通过键去访问
   ```

   

 * 多维数组 - 包含一个或多个数组的数组

   	* todo



2、数组排序

	* sort()  //对数组进行升序排序
	* rsort()  //对数组进行降序排序
	* asort() //根据关联数组的值，对数组进行升序排序
	* ksort() //根据关联数组的键，对数组进行升序排序
	* arsort() //根据关联数组的值，对数组进行降序排序
	* krsort() //根据关联数组的值，对数组进行降序排序



PHP超级全局变量：（在全部作用域中始终可用的内置变量）

PHP中预定义了几个超级全局变量（superglobals）, 在一个脚本的全部作用域都可以用，不需要特别说明，就可以在函数及类中使用

* $GLOBALS

* $_SERVER

  //用于保存关于报头、路径和脚本位置的信息

  ```
  如在表单中
  <form method="post" action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>">
  
  $_SERVER["PHP_SELF"]是超级全局变量，返回当前正在执行脚本的文件名
  
  htmlspecialchars()函数把一些预定义的字符转换为HTML实体
  	* &（和号） 成为 &amp;
  	* "（双引号） 成为 &quot;
  	* ' (单引号) 成为 &#039;
  	* < (小于)  成为  &lt;
  	* > (大于)  成为  &gt;
  ```

  

* $_REQUEST  

  //用于收集HTML表单提交的数据 如：$name = $_REQUEST['name'];

  预定义的$_REQUEST变量包含了$\_GET、$\_POST、$\_COOKIE的内容。

  $_REQUEST变量可用来收集通过GET和POST方法发送过来的表单数据。

* $_POST

  // 用于收集表单数据（post方法提交的表单）

* $_GET

  用于收集表单数据（get方法提交的表单）

* $_FILES

* $_ENV

* $_COOKIE

* $_SESSION



PHP 魔术常量：

这些常量是由不同的扩展库定义的，只有在加载这些扩展库时才会出现，或者动态加载后，或者在编译时已经包括进去了

**\_\_LINE\_\_**    //文件中的当前行号

**\_\_FILE\_\_**    //文件的完整路径和文件名，如果用在被包含文件中，则返回被包含的文件名

**\_\_DIR\_\_**    // 文件所在的目录。如果用在被包括文件中，则返回被包括的文件所在的目录

**\_\_FUNCTION\_\_**    // 显示方法名

**\_\_CLASS\_\_**    // 显示类名

**\_\_METHOD\_\_**    // 显示方法名

**\_\_NAMESPACE\_\_**    // 显示命令空间



命名空间：

 * todo





#### PHP面向对象

* 类使用class关键字后加上类名定义
* 类名后一对大括号，里面定义成员变量以及成员方法
* 类的变量使用var进行声明，变量也可以初始化值

```
<?php
	class Site{
		/* 成员变量 */
		var $url
		var $title;
		
		/*成员函数*/
		function setUrl($par){
			$this->url = $par;
		}
		
		function getUrl(){
			echo $this.url.PHP_EOL;  //输入url并拼接一个换行符
		}
		....
		....
	}
?>

//创建对象
$mysite = new Site()

//调用方法
$mysite->getUrl()
```

* 构造方法

  ```
  void __construct(...)	//可以设置多个参数
  ```

* 析构函数

  析构函数与构造函数相反，当对象结束其生命周期时（例如对象所在的函数已调用完毕），系统自动执行析构函数

  ```
  void __destruct(void)
  ```

* 继承

  PHP跟java一行不支持多继承（关键字 extends）

* 方法重写

  从父类继承的方法不满足子类的需求，对其方法进行修改，这个过程称为覆盖（override）

* 访问控制

  PHP对属性或方法的访问控制，是通过在前面添加关键字public（共有）， protected(受保护)，private(私有)

  * public : 共有的类成员可以在任何地方被访问

  * protected: 受保护的类成员则可以被其自身以及其子类和父类访问

  * private: 私有的类成员则只能被其定义所在的类访问

    **被protected及private声明的变量、方法无法通过对象直接调用，会抛出错误，这是没有访问权限**

    可以在对象内部中定义一个普通的共有方法，在该方法里面调用**protected、private**修饰的变量、方法

* 接口

  使用接口（interface）, 可以指定某个类必须实现哪些方法，但不需要定义这些方法的具体内容

  要实现一个接口，使用implements操作符，类中必须实现接口中定义的所有方法，类可以实现多个接口，用逗号来分隔多个接口的名称

  ```
  <?php
  	声明一个接口
  	interface iTemplate{
  		public function setVariable($name,$var);
  		public function getHtml($template);
  	}
  	
  	//实现接口
  	class Template implements iTemplate
  {
      private $vars = array();	//定义一个空数组
    
      public function setVariable($name, $var)
      {
          $this->vars[$name] = $var;	// 将传进来的值，存入关联数组中
      }
    
      public function getHtml($template)
      {
          foreach($this->vars as $name => $value) { //关联数组获取值
              $template = str_replace('{' . $name . '}', $value, $template);
          }
   
          return $template;
      }
  ?>
  ```

* 常量

  ```
  类中声明一个常量
  const constant = '常量值'	 //不可变的值
  
  可通过类名或对象直接调用
  类名::constant	// :: 这个符号用来调用常量
  ```

* 抽象类

  如果一个类里面至少有一个方法被声明为抽象的，那么这个类就必须声明为抽象的

  定义为抽象的类不能被实例化

  继承一个抽象类的时候，子类必须定义父类中的所有抽象方法，另外，这些方法的访问控制必须和父类中一样（或者更为宽松）

  ```
  <?php
  	abstract class AbstractClass{
  		abstract protected function getvalue();
  		abstract protected function prefixValue($prefix);
  		//普通的方法
  		public function printOut(){
  			print $this->getvalue().PHP_EOL;
  		}
  	}
  	
  	class ConcreteClass extends AbstractClass{
  		protected function getValue(){
  			return "xxxxxxx"
  		}
  		public function prefixValue($prefix){
  			return "xxxxxxxxxx"
  		}
  	}
  ?>
  ```

  子类方法可以包含父类抽象方法中不存在的可选参数。

  子类定义了一个可选参数，而父类抽象方法的声明里没有，则也是可以正常运行的

* Static关键字

  声明类属性或方法为static(静态)，就可以不实例化类而直接访问

  静态属性不能通过一个类已实例化的对象来访问（但静态方法可以）

  由于静态方法不需要通过对象即可直接调用，所以伪变量$this在静态方法中不可用

  静态属性不可以有对象通过->来访问

  ```
  <?php
  	class Foo {
  		public static $my_static = 'foo';
  		
  		public function staticValue(){
  			return self::$my_static;
  		}
  	}
  	
  	print Foo::$my_static.PHP_EOL;
  ?>
  ```

* Final关键字

  如果父类中的方法被声明为final，则子类无法覆盖该方法，如果一个类被声明为final，则不能被继承

* 调用父类构造方法

  PHP不会在子类的构造方法中调用父类的构造方法。要执行父类的构造方法，需在子类的构造方法中调用**parent::__construct()**



#### 高级

​	PHP include 或 require语句，可以将PHP文件的内容插入另一个PHP文件

​	include 和 require语句是相同的，除了错误处理方面：

   * require会生成致命错误并停止脚本
   * include只会生成警告，并且脚本会继续运行



​	PHP文件：

  * readfile()函数读取文件，并把它写入输出缓存

  * 文件打开/读取

    ```
    <?php
    	$myfile = fopen("filename.txt","r") or die("Unable to open file!");
    	echo fread($myfile, filesize("filename.txt"))
    	fclose($myfile);
    ?>
    
    读取文件-fread()
    	fread()的第一个参数包含待读文件名，第二个参数规定待读取的最大字节数
    	
    关闭文件-fclose()
    
    读取单行文件- fgets()
    
    读取单字符-fgetc()
    
    检查 End-Of_file - feof()
    	feof()函数检查是否已到达"end-of-file" (EOF)
    	feof()对于遍历未知长度的数据很有用
    ```

* 文件创建/写入

  ```
  # 创建文件，并写入
  $myfile = fopen("filename.text", "w") or die("Unable to open file");
  $txt = "string";
  fwrite($myfile, $txt);
  fclose($myfile);
  
  
  写入文件-fwrite()
  	fwrite()的第一个参数：要写入文件的文件名，第二个参数：被写的字符串
  ```

* 文件上传

  ```
  <?php
  	if($_FILES["file"]["error"]>0){
  		echo "Error:" . $_FILES["file"]["error"] . "<br/>";
  	}else{
  		echo "Upload:" . $_FILES["file"]["name"] . "<br/>";
  		echo "Type:" . $_FILES["file"]["type"] . "<br/>";
  		echo "Size" . ($_FILEs["file"]["size"] /1024) . "Kb<br/>";
  		echo "store in:" . $_FILES["file"]["tmp_name"];
  	}
  
  ?>
  ```



cookie and session

```
设置cookie
	setcookie(name,value,expire, path, domain);
获取cookie
	$_COOKIE[name]
	
php 用到sessioin必须先开启会话(session)
<?php session_start(); ?>

存储Session变量
<?php
	session_start();
	//store session data
	$_SESSION['views'] = 1;
?>
<html>
	<body>
		<?php
			//retrieve session data
			echo "pageviews=". $_SESSION['views'];
		?>
	</body>
</html>

销毁session
<?php
	unset($_SESSION['views']);
	//or
	session_destroy(); // lost all session
?>
```



php 发送电子邮件

```
mail(to(接受者), subject（主题）, message（消息）, headers, parameters)
```



过滤器（Filter）:

	* filter_var() 通过一个指定的过滤器来过滤单一的变量
	* filter_var_array() 通过相同的或不同的过滤器来过滤多个变量
	* filter_input 获取一个输入变量，并对它进行过滤变量
	* filter_input_array() 获取多个输入变量，并通过相同的或不同的过滤器对它们进行过滤



#### PHP连接MySQL数据库

```
语法
mysqli_connect(host, username, password, dbname)

创建数据库
<?php
$con = mysqli_connect("127.0.0.1", "abc", "123456");

if(mysqli_connect_errno($con)){
	echo "Failed to connect to MySQL:". mysqli_connect_error();
}

$sql = "CREATE DATABASE test_db";
if(mysqli_query($con,$sql)){
	echo "Database my_db created successfully"
}else{
	echo "fail:" . mysqli_error($conn);
}

//关闭连接
mysqli_close($con);

?>
===========================

<?php
$con = mysqli_connect("127.0.0.1", "abc", "123456", "test_db");

if(mysqli_connect_errno($con)){
	echo "Failed to connect to MySQL:". mysqli_connect_error();
}
//创建数据表
$sql = "create table Persons(FirstName char(30), LastName char(30), Age INT)";

if(mysqli_query($con,$sql)){
	echo "crate table success";
}else{
	echo "fail" . mysqli_error($con);
}

//关闭连接
mysqli_close($con);

?>
```



#### 杂项

htmlspecialchars()函数把特殊字符转换为HTML实体。这意味着<  和  > 之类的HTML字符会被替换为$lt;和$gt;

基本的错误处理：使用die()函数







