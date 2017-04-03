//package scala_note
import math.Ordering
/*
  In this worksheet we will talk about collections in Scala:
  1. list
  2. higher-order function on list
*/
object collection {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(268); 
  //1. List
  // First we introduce some predefined operators.
	
	val l1 = List(4, 2, 5, 1, 3);System.out.println("""l1  : List[Int] = """ + $show(l1 ));$skip(31); 
	
	// append
	var l2 = 6 :: l1;System.out.println("""l2  : List[Int] = """ + $show(l2 ));$skip(133); 
	// Note here the parameter is appended to the right of the list. It's equivalent to 3 :: 2 :: List(1)
	val l3 = List(1).::(2).::(3);System.out.println("""l3  : List[Int] = """ + $show(l3 ));$skip(33); 
	
	// concat
	var l4 = l1 ::: l2;System.out.println("""l4  : List[Int] = """ + $show(l4 ));$skip(70); val res$0 = 
	
	// the first |l1|-1 elements (excluding the last element)
	l1.init;System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(30); val res$1 = 
	// the last element
	l1.last;System.out.println("""res1: Int = """ + $show(res$1));$skip(45); val res$2 = 
	// take out the first n elements
	l1 take 2;System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(41); val res$3 = 
	// drop the first n elements
	l1 drop 3;System.out.println("""res3: List[Int] = """ + $show(res$3));$skip(31); val res$4 = 
	// reverse a list
	l1.reverse;System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(54); val res$5 = 
	// update element at given index
	l1 updated (2, 99);System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(56); val res$6 = 
	// find element in list and return index
	l1 indexOf 5;System.out.println("""res6: Int = """ + $show(res$6));$skip(221); 

	// Next we try to implement 'last' method
	def myLast[T](l: List[T]): T = l match {
		case List() => throw new NoSuchElementException("no last element in empty list")
		case List(x) => x
		case x :: xs => myLast(xs)
	};System.out.println("""myLast: [T](l: List[T])T""");$skip(78); 
	
	def removeAt[T](n: Int, l: List[T]): List[T] = (l take n) ::: (l drop n+1);System.out.println("""removeAt: [T](n: Int, l: List[T])List[T]""");$skip(111); 
                                                  
	def myLastForInt(l: List[Int]) = myLast[Int](l: List[Int]);System.out.println("""myLastForInt: (l: List[Int])Int""");$skip(20); val res$7 = 
	
	myLastForInt(l1);System.out.println("""res7: Int = """ + $show(res$7));$skip(22); val res$8 = 
	removeAt[Int](2, l1);System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(303); 
	
	// next we define a classic sort algorithm: merge sort, where we introduce pair matching
	def mergeSort[T](f: (T, T) => Boolean)(l: List[T]): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			val (fst, snd) = l splitAt n
			merge[T](f)(mergeSort[T](f)(fst), mergeSort[T](f)(snd))
		}
	};System.out.println("""mergeSort: [T](f: (T, T) => Boolean)(l: List[T])List[T]""");$skip(303); 
	
	// We need merge method to merge two sorted sublist
	def merge[T](f: (T, T) => Boolean)(l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
		case (Nil, ys) => ys
		case (xs, Nil) => xs
		case (x :: xs, y :: ys) => {
			if (f(x, y)) x :: merge[T](f)(xs, l2)
			else y :: merge[T](f)(l1, ys)
		}
	};System.out.println("""merge: [T](f: (T, T) => Boolean)(l1: List[T], l2: List[T])List[T]""");$skip(144); val res$9 = 
	
	// Note here we didn't provide the type of x and y, since the compile would refer to type of l2 element
	mergeSort[Int]((x, y) => x < y)(l2);System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(716); 
	
	
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
	};System.out.println("""mergeSortOrdering: [T](l: List[T])(implicit ord: scala.math.Ordering[T])List[T]""");$skip(45); 
	
	val chars = List("a", "c", "z", "q", "t");System.out.println("""chars  : List[String] = """ + $show(chars ));$skip(34); val res$10 = 
	mergeSortOrdering[String](chars);System.out.println("""res10: List[String] = """ + $show(res$10));$skip(163); 
	
	//2. higher-order function on list
	
	//map will have an operation on each element of a list
	
	def sqList(l: List[Int]): List[Int] = {
		l map (x => x * x)
	};System.out.println("""sqList: (l: List[Int])List[Int]""");$skip(14); val res$11 = 
	
	sqList(l1);System.out.println("""res11: List[Int] = """ + $show(res$11));$skip(170); 
	
	//filter will pick out the elements satisfying given condition
	//filterNot is similar
	def biggerThan(l: List[Int], n: Int): List[Int] = {
		l filter (x => x > n)
	};System.out.println("""biggerThan: (l: List[Int], n: Int)List[Int]""");$skip(21); val res$12 = 
	
	biggerThan(l1, 3);System.out.println("""res12: List[Int] = """ + $show(res$12));$skip(95); val res$13 = 
	// combining filter and filterNot into a pair we may get partition
	l1 partition (x => x > 3);System.out.println("""res13: (List[Int], List[Int]) = """ + $show(res$13));$skip(156); val res$14 = 
	
	// If you want to take the first several elements satisfying the give condition, try takeWhile.
	// the opposite is dropWhile
	l1 takeWhile (x => x < 5);System.out.println("""res14: List[Int] = """ + $show(res$14));$skip(241); 
	
	// just like partition, a combination of takeWhile and dropWhile is span
	
	def pack[T](l: List[T]): List[List[T]] = l match {
		case Nil => Nil
		case x :: xs =>
			val (first, rest) = l partition (y => y == x)
			first :: pack(rest)
	};System.out.println("""pack: [T](l: List[T])List[List[T]]""");$skip(98); 
	
	def encode[T](l: List[T]): List[(T, Int)] = {
		pack[T](l) map (xs => (xs.head, xs.length))
	};System.out.println("""encode: [T](l: List[T])List[(T, Int)]""");$skip(64); 
	
	val data = List("a", "a", "a", "b", "b", "c", "c", "d", "a");System.out.println("""data  : List[String] = """ + $show(data ));$skip(22); val res$15 = 
	encode[String](data);System.out.println("""res15: List[(String, Int)] = """ + $show(res$15));$skip(122); 
	
	// Another important higher-order function is reduce.
	def sum(l: List[Int]): Int = {
		(0 :: l) reduceLeft (_ + _)
	};System.out.println("""sum: (l: List[Int])Int""");$skip(9); val res$16 = 
	sum(l1);System.out.println("""res16: Int = """ + $show(res$16));$skip(312); 
	// Here you may put _ to represent new parameters. Also 0 is appended as the base.
	// A more general function is foldLeft which requires a base passed explicitly as a parameter.
	// foldLeft follows the pattern: (List foldLeft base)(binary operator)
	def sum1(l: List[Int]): Int = {
		(l foldLeft 0)(_ + _)
	};System.out.println("""sum1: (l: List[Int])Int""");$skip(10); val res$17 = 
	sum1(l1);System.out.println("""res17: Int = """ + $show(res$17));$skip(948); 
	
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
	val v1 = Vector(1, 2, 3);System.out.println("""v1  : scala.collection.immutable.Vector[Int] = """ + $show(v1 ));$skip(9); val res$18 = 
	v1 :+ 2;System.out.println("""res18: scala.collection.immutable.Vector[Int] = """ + $show(res$18));$skip(9); val res$19 = 
	2 +: v1;System.out.println("""res19: scala.collection.immutable.Vector[Int] = """ + $show(res$19));$skip(623); val res$20 = 
	
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
	1 to 5;System.out.println("""res20: scala.collection.immutable.Range.Inclusive = """ + $show(res$20));$skip(30); val res$21 =   // to inclusive
	1 until 5;System.out.println("""res21: scala.collection.immutable.Range = """ + $show(res$21));$skip(14); val res$22 =  // until exclusive
	1 to 10 by 2;System.out.println("""res22: scala.collection.immutable.Range = """ + $show(res$22));$skip(170); val res$23 = 
	
	//4. functions on sequence
	/*
		Next we have more exposure to sequence operations.
	*/
	// true if any/every one element satisfies property p
	l1 exists (x => x > 3);System.out.println("""res23: Boolean = """ + $show(res$23));$skip(24); val res$24 = 
	l1 forall (x => x > 3);System.out.println("""res24: Boolean = """ + $show(res$24));$skip(74); 
	
	// pair each element of x with each element of y
	val lzip = l1 zip l1;System.out.println("""lzip  : List[(Int, Int)] = """ + $show(lzip ));$skip(57); 
	// opposite operation of zip
	val (ll, lr) = lzip.unzip;System.out.println("""ll  : List[Int] = """ + $show(ll ));System.out.println("""lr  : List[Int] = """ + $show(lr ));$skip(96); val res$25 = 
	// map each element and concatenate into one single collection
	l1 flatMap (x => List(x, x+1));System.out.println("""res25: List[Int] = """ + $show(res$25));$skip(196); 
	// sum, product, min, max are also available
	
	//e.g. list all combination of (x, y) where 1<=x<=n, 1<=y<=m
	def comb(n: Int, m: Int) = {
		(1 to n) flatMap (x => (1 to m) map (y => (x, y)))
	};System.out.println("""comb: (n: Int, m: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]""");$skip(12); val res$26 = 
	comb(5, 4);System.out.println("""res26: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$26));$skip(388); 
  
  // Now with more options, we can write more elegant functions.
  // The following is a tail recursive way to define inner product.
  def innerProd(xs: List[Double], ys: List[Double]): Double = {
  	def acc(sum: Double, x: List[Double], y: List[Double]): Double = x match {
  		case Nil => sum
  		case t :: ts => acc(sum + x.head * y.head, x.tail, y.tail)
  	}
  	acc(0, xs, ys)
  };System.out.println("""innerProd: (xs: List[Double], ys: List[Double])Double""");$skip(25); 
  val ld = List(1.0,2.0);System.out.println("""ld  : List[Double] = """ + $show(ld ));$skip(20); val res$27 = 
  innerProd(ld, ld);System.out.println("""res27: Double = """ + $show(res$27));$skip(180); 
  
  // As comparison, the follow is a higher-order function version.
  def innerProd1(xs: List[Double], ys: List[Double]): Double = {
  	(xs zip ys).map(t => t._1 * t._2) sum
  };System.out.println("""innerProd1: (xs: List[Double], ys: List[Double])Double""");$skip(22); val res$28 = 
	
	innerProd1(ld, ld);System.out.println("""res28: Double = """ + $show(res$28));$skip(169); 
	
	// It's even simpler to define a function to tell whether a given number is prime or not.
	def isPrime(x: Int): Boolean = {
		(2 until x) forall (t => x % t != 0)
	};System.out.println("""isPrime: (x: Int)Boolean""");$skip(15); val res$29 = 
	
	isPrime(91);System.out.println("""res29: Boolean = """ + $show(res$29));$skip(13); val res$30 = 
	isPrime(79);System.out.println("""res30: Boolean = """ + $show(res$30))}
}
