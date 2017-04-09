//package scala_note
import math.Ordering
/*
  In this worksheet we will talk about collections in Scala:
  1. sequence
  2. higher-order function on sequence
*/
object collection1 {
  //1. List
  // First we introduce some predefined operators.
	
	val l1 = List(4, 2, 5, 1, 3)              //> l1  : List[Int] = List(4, 2, 5, 1, 3)
	
	// append
	var l2 = 6 :: l1                          //> l2  : List[Int] = List(6, 4, 2, 5, 1, 3)
	// Note here the parameter is appended to the right of the list. It's equivalent to 3 :: 2 :: List(1)
	val l3 = List(1).::(2).::(3)              //> l3  : List[Int] = List(3, 2, 1)
	
	// concat
	var l4 = l1 ::: l2                        //> l4  : List[Int] = List(4, 2, 5, 1, 3, 6, 4, 2, 5, 1, 3)
	
	// the first |l1|-1 elements (excluding the last element)
	l1.init                                   //> res0: List[Int] = List(4, 2, 5, 1)
	// the last element
	l1.last                                   //> res1: Int = 3
	// take out the first n elements
	l1 take 2                                 //> res2: List[Int] = List(4, 2)
	// drop the first n elements
	l1 drop 3                                 //> res3: List[Int] = List(1, 3)
	// reverse a list
	l1.reverse                                //> res4: List[Int] = List(3, 1, 5, 2, 4)
	// update element at given index
	l1 updated (2, 99)                        //> res5: List[Int] = List(4, 2, 99, 1, 3)
	// find element in list and return index
	l1 indexOf 5                              //> res6: Int = 2

	// Next we try to implement 'last' method
	def myLast[T](l: List[T]): T = l match {
		case List() => throw new NoSuchElementException("no last element in empty list")
		case List(x) => x
		case x :: xs => myLast(xs)
	}                                         //> myLast: [T](l: List[T])T
	
	def removeAt[T](n: Int, l: List[T]): List[T] = (l take n) ::: (l drop n+1)
                                                  //> removeAt: [T](n: Int, l: List[T])List[T]
                                                  
	def myLastForInt(l: List[Int]) = myLast[Int](l: List[Int])
                                                  //> myLastForInt: (l: List[Int])Int
	
	myLastForInt(l1)                          //> res7: Int = 3
	removeAt[Int](2, l1)                      //> res8: List[Int] = List(4, 2, 1, 3)
	
	// next we define a classic sort algorithm: merge sort, where we introduce pair matching
	def mergeSort[T](f: (T, T) => Boolean)(l: List[T]): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			val (fst, snd) = l splitAt n
			merge[T](f)(mergeSort[T](f)(fst), mergeSort[T](f)(snd))
		}
	}                                         //> mergeSort: [T](f: (T, T) => Boolean)(l: List[T])List[T]
	
	// We need merge method to merge two sorted sublist
	def merge[T](f: (T, T) => Boolean)(l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
		case (Nil, ys) => ys
		case (xs, Nil) => xs
		case (x :: xs, y :: ys) => {
			if (f(x, y)) x :: merge[T](f)(xs, l2)
			else y :: merge[T](f)(l1, ys)
		}
	}                                         //> merge: [T](f: (T, T) => Boolean)(l1: List[T], l2: List[T])List[T]
	
	// Note here we didn't provide the type of x and y, since the compile would refer to type of l2 element
	mergeSort[Int]((x, y) => x < y)(l2)       //> res9: List[Int] = List(1, 2, 3, 4, 5, 6)
	
	
	// Instead of passing a function, we can also pass the Scala standard library Ordering.
	// By writing implicit, Scala will refer type from the function signature and we don't need to
	// porvide ord during function call.
	def mergeSortOrdering[T](l: List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			val (fst, snd) = l splitAt n
			def mergeOrdering(l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
				case (Nil, ys) => ys
				case (xs, Nil) => xs
				case (x :: xs, y :: ys) => {
					if (ord.lt(x, y)) x :: mergeOrdering(xs, l2)
					else y :: mergeOrdering(l1, ys)
				}
			}
			mergeOrdering(mergeSortOrdering[T](fst), mergeSortOrdering(snd))
		}
	}                                         //> mergeSortOrdering: [T](l: List[T])(implicit ord: scala.math.Ordering[T])Lis
                                                  //| t[T]
	
	val chars = List("a", "c", "z", "q", "t") //> chars  : List[String] = List(a, c, z, q, t)
	mergeSortOrdering[String](chars)          //> res10: List[String] = List(a, c, q, t, z)
	
	// Also List has a built-in method to do sorting. sortWith can take any self-defined order
	// while sorted takes the system defined order.
	chars sortWith (_.length < _.length)      //> res11: List[String] = List(a, c, z, q, t)
	chars.sorted                              //> res12: List[String] = List(a, c, q, t, z)
	
	// Another Scala featured method is groupBy. It takes a condition to group the elements and
	// returns a map.
	chars groupBy (_.head)                    //> res13: scala.collection.immutable.Map[Char,List[String]] = Map(t -> List(t)
                                                  //| , a -> List(a), q -> List(q), c -> List(c), z -> List(z))
	
	//2. higher-order function on list
	
	//map will have an operation on each element of a list
	
	def sqList(l: List[Int]): List[Int] = {
		l map (x => x * x)
	}                                         //> sqList: (l: List[Int])List[Int]
	
	sqList(l1)                                //> res14: List[Int] = List(16, 4, 25, 1, 9)
	
	//filter will pick out the elements satisfying given condition
	//filterNot is similar
	def biggerThan(l: List[Int], n: Int): List[Int] = {
		l filter (x => x > n)
	}                                         //> biggerThan: (l: List[Int], n: Int)List[Int]
	
	biggerThan(l1, 3)                         //> res15: List[Int] = List(4, 5)
	// combining filter and filterNot into a pair we may get partition
	l1 partition (x => x > 3)                 //> res16: (List[Int], List[Int]) = (List(4, 5),List(2, 1, 3))
	
	// If you want to take the first several elements satisfying the give condition, try takeWhile.
	// the opposite is dropWhile. Just mind that here we take out the first several elements satisfying
	// the condition until we meet with the first one not satisfying.
	l1 takeWhile (x => x < 5)                 //> res17: List[Int] = List(4, 2)
	
	// just like partition, a combination of takeWhile and dropWhile is span
	
	def pack[T](l: List[T]): List[List[T]] = l match {
		case Nil => Nil
		case x :: xs =>
			val (first, rest) = l partition (y => y == x)
			first :: pack(rest)
	}                                         //> pack: [T](l: List[T])List[List[T]]
	
	
	def encode[T](l: List[T]): List[(T, Int)] = {
		pack[T](l) map (xs => (xs.head, xs.length))
	}                                         //> encode: [T](l: List[T])List[(T, Int)]
	
	val data = List("a", "a", "a", "b", "b", "c", "c", "d", "a")
                                                  //> data  : List[String] = List(a, a, a, b, b, c, c, d, a)
	encode[String](data)                      //> res18: List[(String, Int)] = List((a,4), (b,2), (c,2), (d,1))
	
	pack[String](data)                        //> res19: List[List[String]] = List(List(a, a, a, a), List(b, b), List(c, c), 
                                                  //| List(d))
	
	// Another important higher-order function is reduce.
	def sum(l: List[Int]): Int = {
		(0 :: l) reduceLeft (_ + _)
	}                                         //> sum: (l: List[Int])Int
	sum(l1)                                   //> res20: Int = 15
	// Here you may put _ to represent new parameters. Also 0 is appended as the base.
	// A more general function is foldLeft which requires a base passed explicitly as a parameter.
	// foldLeft follows the pattern: (List foldLeft base)(binary operator)
	def sum1(l: List[Int]): Int = {
		(l foldLeft 0)(_ + _)
	}                                         //> sum1: (l: List[Int])Int
	sum1(l1)                                  //> res21: Int = 15
	
	/*
		foldLeft forms a left-lean tree which basically does operation from left to right,
		while foldRight operates from right to left.
		
		(List(x1, x2, ..., xn) foldLeft base)(op) = (List(x2, x3, ..., xn) foldLeft op(x1, base))(op) =
		(List(x3, ..., xn) foldLeft op(x2, op(x1, base)))(op) = ...
		
		(List(x1, x2, ..., xn) foldRight base)(op) = (List(x1, x2, x3, ..., xn-1) foldRight op(xn, base))(op) =
		(List(x1, x2, x3, ..., xn-2) foldRight op(xn-1, op(xn, base)))(op) = ...
		
		For operators that are associative and commutative, foldLeft and foldRight are equivalent.
	*/

	//3. vector
	/*
  	Vector is not implemented as a linear structure in Scala. For operations like head and tail, it's not
  	constant in time complexity. However, the scalable feature of vector makes it prefered when the task
  	involves large number of elements.
	*/
	// Most of the methods overlap between vector and list, except ::.
	val v1 = Vector(1, 2, 3)                  //> v1  : scala.collection.immutable.Vector[Int] = Vector(1, 2, 3)
	v1 :+ 2                                   //> res22: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 2)
	2 +: v1                                   //> res23: scala.collection.immutable.Vector[Int] = Vector(2, 1, 2, 3)
	
	/*
	 	Hierachy of collection
	 																____________
	 											----------|   range  |
	 											|					------------
	 											|
		____________   ____________   ____________
  	| iterable |---| sequence |---|   list   |
  	------------	 ------------   ------------
  			 |		 					|					____________
  			 |							----------|  vector  |
  			 |												------------
				 |         ____________
				 |---------|   set    |
				 |				 ------------
				 |
				 |  		   ____________
				 |---------|   map    |
				 					 ------------
	
	*/
	
	// another widely used sequence: Range
	1 to 5  // to inclusive                   //> res24: scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5)
	1 until 5 // until exclusive              //> res25: scala.collection.immutable.Range = Range(1, 2, 3, 4)
	1 to 10 by 2                              //> res26: scala.collection.immutable.Range = Range(1, 3, 5, 7, 9)
	
	//4. functions on sequence
	/*
		Next we have more exposure to sequence operations.
	*/
	// true if any/every one element satisfies property p
	l1 exists (x => x > 3)                    //> res27: Boolean = true
	l1 forall (x => x > 3)                    //> res28: Boolean = false
	
	// pair each element of x with each element of y
	val lzip = l1 zip l1                      //> lzip  : List[(Int, Int)] = List((4,4), (2,2), (5,5), (1,1), (3,3))
	// opposite operation of zip
	val (ll, lr) = lzip.unzip                 //> ll  : List[Int] = List(4, 2, 5, 1, 3)
                                                  //| lr  : List[Int] = List(4, 2, 5, 1, 3)
	// map each element and concatenate into one single collection
	l1 flatMap (x => List(x, x+1))            //> res29: List[Int] = List(4, 5, 2, 3, 5, 6, 1, 2, 3, 4)
	// sum, product, min, max are also available
	
	//e.g. list all combination of (x, y) where 1<=x<=n, 1<=y<=m
	def comb(n: Int, m: Int) = {
		(1 to n) flatMap (x => (1 to m) map (y => (x, y)))
	}                                         //> comb: (n: Int, m: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	comb(5, 4)                                //> res30: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1
                                                  //| ,2), (1,3), (1,4), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (3,4), 
                                                  //| (4,1), (4,2), (4,3), (4,4), (5,1), (5,2), (5,3), (5,4))
  
  // Now with more options, we can write more elegant functions.
  // The following is a tail recursive way to define inner product.
  def innerProd(xs: List[Double], ys: List[Double]): Double = {
  	def acc(sum: Double, x: List[Double], y: List[Double]): Double = x match {
  		case Nil => sum
  		case t :: ts => acc(sum + x.head * y.head, x.tail, y.tail)
  	}
  	acc(0, xs, ys)
  }                                               //> innerProd: (xs: List[Double], ys: List[Double])Double
  val ld = List(1.0,2.0)                          //> ld  : List[Double] = List(1.0, 2.0)
  innerProd(ld, ld)                               //> res31: Double = 5.0
  
  // As comparison, the follow is a higher-order function version.
  def innerProd1(xs: List[Double], ys: List[Double]): Double = {
  	(xs zip ys) map (t => t._1 * t._2) sum
  }                                               //> innerProd1: (xs: List[Double], ys: List[Double])Double
	
	innerProd1(ld, ld)                        //> res32: Double = 5.0
	
	
	
}