package scala_note
/*
  In this worksheet, we will present:
  1. higher order function
  2. currying
*/
object higher_order_function {
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
	}                                         //> mapReduce: (map: Int => Int, reduce: (Int, Int) => Int, start: Int, end: Int
                                                  //| )Int
	
	/*
		Now we can define a function to sum from a to b by mapReduce. Here map is just an identical map and
		reduce is just sum of two Ints. Notice that sometimes we can ignore the return type of
		the function. The compiler is powerful enough to infer from the function defination.
		However, the return type can not be omitted if the function involves recursion.
	*/
	def myMap(x: Int) = x                     //> myMap: (x: Int)Int
	def myReduce(x: Int, y: Int) = x + y      //> myReduce: (x: Int, y: Int)Int
	def sumv1(start: Int, end: Int) = mapReduce(myMap, myReduce, start, end)
                                                  //> sumv1: (start: Int, end: Int)Int
	
	sumv1(5, 10)                              //> res0: Int = 45
	/*
		Sometimes it takes time to explicitly define a function whose operation is quite simple.
		In this case, anonymous function comes to help. Note that nonymous function may not be
		helpful when defining some complicated function.
	*/
	def sumv2(start: Int, end: Int) = mapReduce(x => x, (x, y) => x + y, start, end)
                                                  //> sumv2: (start: Int, end: Int)Int
	sumv2(5, 10)                              //> res1: Int = 45
	
	// a tail recursion version
	def mapReduceTailRec(map: Int => Int, reduce: (Int, Int) => Int, start: Int, end: Int, acc: Int): Int = {
		if (start > end) acc
		else mapReduceTailRec(map, reduce, start+1, end, reduce(acc, map(start)))
	}                                         //> mapReduceTailRec: (map: Int => Int, reduce: (Int, Int) => Int, start: Int, 
                                                  //| end: Int, acc: Int)Int
	
	def sumv3(start: Int, end: Int) = mapReduceTailRec(x => x, (x, y) => x + y, start, end, 0)
                                                  //> sumv3: (start: Int, end: Int)Int
	sumv3(5, 10)                              //> res2: Int = 45
	
	// e.g.2
  /*
		With mapReduce we can define many useful functions. It's just different combination of
		map and reduce.
	*/
	
	def sumOfSquare(start: Int, end: Int) = mapReduceTailRec(x => x * x, (x, y) => x + y, start, end, 0)
                                                  //> sumOfSquare: (start: Int, end: Int)Int
	def factorial(x: Int) = mapReduceTailRec(x => x, (x, y) => x * y, 1, x, 1)
                                                  //> factorial: (x: Int)Int
	
	sumOfSquare(1, 5)                         //> res3: Int = 55
	factorial(5)                              //> res4: Int = 120
	
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
	}                                         //> preMapReduceTailRec: (map: Int => Int, reduce: (Int, Int) => Int)(Int, Int,
                                                  //|  Int) => Int
	
	// Then we think about sum from a to b.
	def sumv4 = preMapReduceTailRec(x => x, (x, y) => x + y)
                                                  //> sumv4: => (Int, Int, Int) => Int
	sumv4(5, 10, 0)                           //> res5: Int = 45
	
	// In this way, we first form a function by passing map and reduce and then indicate
	// the start and end. Since this pattern is frequently used we can define a function
	// in the following way.
	def mapReduceTailRecCurrying(map: Int => Int, reduce: (Int, Int) => Int)(start: Int, end: Int, acc: Int): Int = {
		if (start > end) acc
		else mapReduceTailRec(map, reduce, start+1, end, reduce(acc, map(start)))
	}                                         //> mapReduceTailRecCurrying: (map: Int => Int, reduce: (Int, Int) => Int)(star
                                                  //| t: Int, end: Int, acc: Int)Int
	
	def sumv5(start: Int, end: Int, acc: Int) = mapReduceTailRecCurrying(x => x, (x, y) => x + y)(start, end, acc)
                                                  //> sumv5: (start: Int, end: Int, acc: Int)Int
	
	sumv5(5, 10, 0)                           //> res6: Int = 45
	
	// e.g.4
  /*
		In the last example, we use higher order function and currying, define a function to
		find fixed point: find x such that f(x) = x. Here we stablize by average: (next+curVal)/2.
		Otherwise, in some cases, the step will be too large for the value to converge.
	*/
	
	def findFixedPoint(f: Double => Double)(curVal: Double): Double = {
		val next = f(curVal)
		if (Math.abs(curVal - next) / next / next < 0.001) next
		else findFixedPoint(f)((next+curVal)/2)
	}                                         //> findFixedPoint: (f: Double => Double)(curVal: Double)Double
	
	findFixedPoint(x => x/2+1)(0)             //> res7: Double = 1.996828788061066
	
	// In fact, we can redefine sqrt by findFixedPoint.
	// here avergeStablizing takes a function as parameter and return a function.
	def avergeStablizing(f: Double => Double)(x: Double) = (x + f(x)) / 2
                                                  //> avergeStablizing: (f: Double => Double)(x: Double)Double
	def sqrtv2(x: Double) = findFixedPoint(avergeStablizing(y => x / y))(1)
                                                  //> sqrtv2: (x: Double)Double
	
	sqrtv2(247)                               //> res8: Double = 15.71755047534884

}