package scala_note
import math.Ordering
/*
  In this worksheet we will talk about several collections in Scala:
  1. list
*/
object collection {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(239); 
  //1. List
  // First we introduce some predefined operators.
	
	val l1 = List(4, 2, 5, 1, 3);System.out.println("""l1  : List[Int] = """ + $show(l1 ));$skip(31); 
	
	// append
	var l2 = 6 :: l1;System.out.println("""l2  : List[Int] = """ + $show(l2 ));$skip(132); 
	// Note here the parameter is appended to the right of the list. It's equvalent to 3 :: 2 :: List(1)
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
	mergeSort[Int]((x, y) => x < y)(l2);System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(598); 
	
	
	// Instead of passing a function, we can also pass the Scala standard library Ordering.
	def mergeSortOrdering[T](ord: Ordering[T])(l: List[T]): List[T] = {
		val n = l.length / 2
		if (n == 0) l
		else {
			val (fst, snd) = l splitAt n
			def mergeOrdering[T](l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
				case (Nil, ys) => ys
				case (xs, Nil) => xs
				case (x :: xs, y :: ys) => {
					if (ord.lt(x, y)) x :: mergeOrdering[T](xs, l2)
					else y :: mergeOrdering[T](l1, ys)
				}
			}
			mergeOrdering[T](mergeSortOrdering[T](ord)(fst), mergeSortOrdering[T](ord)(snd))
		}
	};System.out.println("""mergeSortOrdering: [T](ord: scala.math.Ordering[T])(l: List[T])List[T]""")}
	
	
}
