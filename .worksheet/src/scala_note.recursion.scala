package scala_note
/*
  In this worksheet we will talk about:
  1. Scala primitive and basic syntax
  2. recursion
 	with several classic examples
*/
object recursion {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(990); 
  /*
		Scala is both an objective oriented and functional programming language.
		The objective oriented part is quite similar to Java, therefore the basic syntax can be
		compared to Java except the way to define a function.
	*/

  // e.g.1
  /*
		In this example, we define a function to get arithmetic mean of two given doubles.
		Note that formulation (parameter: data type) not only applies to parameter of a function
		it also applies to a function. If compiled, the function has
	 	(x: Double, y: Double)Double as the type
	 	which means the function takes two Double paramters and return a Double
	*/

  // common primitives include
  // Byte, Short, Int, Long, Float, Double
  // common reference types include
  // Char, String, Boolean, Null, Nothing
  def arithMean(x: Double, y: Double): Double = (x + y) / 2;System.out.println("""arithMean: (x: Double, y: Double)Double""");$skip(71); 
  // define a variable is easy
  val myArithMean = arithMean(4.2, 5.8);System.out.println("""myArithMean  : Double = """ + $show(myArithMean ));$skip(299); 

  // e.g.2
  /*
		In this example, we deal with print functions. In Java, we have println and printf.
		Both works to print. However, println print out a string while printf does formatted output.
		In Scala, we can tune println to behave like printf.
	*/

  println("The value is " + myArithMean);$skip(147); 

  // 'f' means println should behave like printf here. $variable_name is followed by format control.

  println(f"The value is $myArithMean%.2f");$skip(507); 
  
  // e.g.3
  /*
		In this example, let's talk about recursion. We start with the classic Fibonacci number.
		0, 1, 1, 2, 3, ..., An, ...
		A[n] = A[n-1] + A[n-2]
	*/
	
	// Note that:
	// we can throw exception just like in Java, and
	// you must provide return type of the function for recursion, here Int must be given.
	def Fibo(n: Int): Int = {
		if (n < 1) throw new IndexOutOfBoundsException("Fibonacci index starts from 1")
		else if (n == 1) 0
		else if (n == 2) 1
		else Fibo(n-1) + Fibo(n-2)
	};System.out.println("""Fibo: (n: Int)Int""");$skip(23); val res$0 = 
	
	// Fibo(0)
	Fibo(5);System.out.println("""res0: Int = """ + $show(res$0));$skip(458); 
	
	// e.g.4
  /*
		Another example, square root through Newton's method
	*/
	
	// once again, recursion function requires return type
	// we use relative precision Math.abs(curVal * curVal - x) / x
	// to adapt to large number / very small number (overflow problem)
	def sqrtNewtonMethod(curVal: Double, x: Double): Double = {
		println(curVal)
		if (Math.abs(curVal * curVal - x) / x < 0.001) curVal
		else sqrtNewtonMethod((curVal + x / curVal) / 2, x)
	};System.out.println("""sqrtNewtonMethod: (curVal: Double, x: Double)Double""");$skip(29); val res$1 = 
		
	sqrtNewtonMethod(1, 2.0);System.out.println("""res1: Double = """ + $show(res$1));$skip(27); val res$2 = 
	sqrtNewtonMethod(1, 1e-6);System.out.println("""res2: Double = """ + $show(res$2));$skip(29); val res$3 = 
	
	sqrtNewtonMethod(1, 1e20);System.out.println("""res3: Double = """ + $show(res$3))}

}
