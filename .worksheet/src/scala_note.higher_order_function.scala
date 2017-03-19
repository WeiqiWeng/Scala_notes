package scala_note
/*
  In this worksheet, we will present:
  1. higher order function
  2. currying
*/
object higher_order_function {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(780); 
	/*
		One feature of functional language is that it can take function both as paramter and return.
		Functions taking other functions as paramter and return another function are called higher order functions.
	*/
	
	// e.g.1
  /*
		The first example is a simplified mapReduce. MapReduce first maps each element with the same
		mapping function and then does some operation on the mapped value. In this example, we do
		an map and then reduce on the mapped Ints.
	*/
	
	def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, start: Int, end: Int): Int = {
		if (start > end) 0
		else reduce(map(start), mapReduce(map, reduce, start+1, end))
	};System.out.println("""mapReduce: (map: Int => Int, reduce: (Int, Int) => Int, start: Int, end: Int)Int""");$skip(393); 
	
	/*
		Now we can define a function to sum from a to b by mapReduce. Here map is just an identical map and
		reduce is just sum of two Ints. Notice that sometimes we can ignore the return type of
		the function. The compiler is powerful enough to infer from the function defination.
		However, the return type can not be omitted if the function involves recursion.
	*/
	def myMap(x: Int) = x;System.out.println("""myMap: (x: Int)Int""");$skip(38); 
	def myReduce(x: Int, y: Int) = x + y;System.out.println("""myReduce: (x: Int, y: Int)Int""");$skip(74); 
	def sumv1(start: Int, end: Int) = mapReduce(myMap, myReduce, start, end);System.out.println("""sumv1: (start: Int, end: Int)Int""");$skip(16); val res$0 = 
	
	sumv1(5, 10);System.out.println("""res0: Int = """ + $show(res$0));$skip(321); 
	/*
		Sometimes it takes time to explicitly define a function whose operation is quite simple.
		In this case, anonymous function comes to help. Note that nonymous function may not be
		helpful when defining some complicated function.
	*/
	def sumv2(start: Int, end: Int) = mapReduce(x => x, (x, y) => x + y, start, end);System.out.println("""sumv2: (start: Int, end: Int)Int""");$skip(14); val res$1 = 
	sumv2(5, 10);System.out.println("""res1: Int = """ + $show(res$1));$skip(240); 
	
	// a tail recursion version
	def mapReduceTailRec(map: Int => Int, reduce: (Int, Int) => Int, start: Int, end: Int, acc: Int): Int = {
		if (start > end) acc
		else mapReduceTailRec(map, reduce, start+1, end, reduce(acc, map(start)))
	};System.out.println("""mapReduceTailRec: (map: Int => Int, reduce: (Int, Int) => Int, start: Int, end: Int, acc: Int)Int""");$skip(94); 
	
	def sumv3(start: Int, end: Int) = mapReduceTailRec(x => x, (x, y) => x + y, start, end, 0);System.out.println("""sumv3: (start: Int, end: Int)Int""");$skip(14); val res$2 = 
	sumv3(5, 10);System.out.println("""res2: Int = """ + $show(res$2));$skip(232); 
	
	// e.g.2
  /*
		With mapReduce we can define many useful functions. It's just different combination of
		map and reduce.
	*/
	
	def sumOfSquare(start: Int, end: Int) = mapReduceTailRec(x => x * x, (x, y) => x + y, start, end, 0);System.out.println("""sumOfSquare: (start: Int, end: Int)Int""");$skip(76); 
	def factorial(x: Int) = mapReduceTailRec(x => x, (x, y) => x * y, 1, x, 1);System.out.println("""factorial: (x: Int)Int""");$skip(21); val res$3 = 
	
	sumOfSquare(1, 5);System.out.println("""res3: Int = """ + $show(res$3));$skip(14); val res$4 = 
	factorial(5);System.out.println("""res4: Int = """ + $show(res$4));$skip(483); 
	
	// e.g.3
  /*
		In this example, we will present currying, a syntax sugar in Scala. Consider the following
		function first. It takes two functions as parameters and returns a function.
	*/
	def preMapReduceTailRec(map: Int => Int, reduce: (Int, Int) => Int): (Int, Int, Int) => Int = {
		def innerMapReduceTailRec(start: Int, end: Int, acc: Int): Int = {
			if (start > end) acc
			else innerMapReduceTailRec(start+1, end, reduce(acc, map(start)))
		}
		innerMapReduceTailRec
	};System.out.println("""preMapReduceTailRec: (map: Int => Int, reduce: (Int, Int) => Int)(Int, Int, Int) => Int""");$skip(101); 
	
	// Then we think about sum from a to b.
	def sumv4 = preMapReduceTailRec(x => x, (x, y) => x + y);System.out.println("""sumv4: => (Int, Int, Int) => Int""");$skip(17); val res$5 = 
	sumv4(5, 10, 0);System.out.println("""res5: Int = """ + $show(res$5));$skip(417); 
	
	// In this way, we first form a function by passing map and reduce and then indicate
	// the start and end. Since this pattern is frequently used we can define a function
	// in the following way.
	def mapReduceTailRecCurrying(map: Int => Int, reduce: (Int, Int) => Int)(start: Int, end: Int, acc: Int): Int = {
		if (start > end) acc
		else mapReduceTailRec(map, reduce, start+1, end, reduce(acc, map(start)))
	};System.out.println("""mapReduceTailRecCurrying: (map: Int => Int, reduce: (Int, Int) => Int)(start: Int, end: Int, acc: Int)Int""");$skip(114); 
	
	def sumv5(start: Int, end: Int, acc: Int) = mapReduceTailRecCurrying(x => x, (x, y) => x + y)(start, end, acc);System.out.println("""sumv5: (start: Int, end: Int, acc: Int)Int""");$skip(19); val res$6 = 
	
	sumv5(5, 10, 0);System.out.println("""res6: Int = """ + $show(res$6))}

}
