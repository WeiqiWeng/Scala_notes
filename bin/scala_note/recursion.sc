// command(ctrl)+shift+F to reindent on mac(windows)
package scala_note
/*
  In this worksheet we will talk about:
  1. Scala primitive and basic syntax
  2. recursion
 	with several classic examples
*/
object recursion {
  /*
		Scala is both an objective oriented and functional language.
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
  def arithMean(x: Double, y: Double): Double = (x + y) / 2
                                                  //> arithMean: (x: Double, y: Double)Double
  // define a variable is easy
  val myArithMean = arithMean(4.2, 5.8)           //> myArithMean  : Double = 5.0

  // e.g.2
  /*
		In this example, we deal with print functions. In Java, we have println and printf.
		Both works to print. However, println print out a string while printf does formatted output.
		In Scala, we can tune println to behave like printf.
	*/

  println("The value is " + myArithMean)          //> The value is 5.0

  // 'f' means println should behave like printf here. $variable_name is followed by format control.

  println(f"The value is $myArithMean%.2f")       //> The value is 5.00
  
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
	}                                         //> Fibo: (n: Int)Int
	
	// Fibo(0)
	Fibo(5)                                   //> res0: Int = 3
	
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
	}                                         //> sqrtNewtonMethod: (curVal: Double, x: Double)Double
		
	sqrtNewtonMethod(1, 2.0)                  //> 1.0
                                                  //| 1.5
                                                  //| 1.4166666666666665
                                                  //| 1.4142156862745097
                                                  //| res1: Double = 1.4142156862745097
	sqrtNewtonMethod(1, 1e-6)                 //> 1.0
                                                  //| 0.5000005
                                                  //| 0.250001249999
                                                  //| 0.12500262498950004
                                                  //| 0.06250531241075212
                                                  //| 0.031260655525445276
                                                  //| 0.015646322308953218
                                                  //| 0.007855117545897352
                                                  //| 0.003991211544161647
                                                  //| 0.0021208810160681787
                                                  //| 0.0012961915927068783
                                                  //| 0.0010338412392442034
                                                  //| 0.0010005538710539446
                                                  //| 0.0010000001533016628
                                                  //| res2: Double = 0.0010000001533016628
	
	sqrtNewtonMethod(1, 1e20)                 //> 1.0
                                                  //| 5.0E19
                                                  //| 2.5E19
                                                  //| 1.25E19
                                                  //| 6.25E18
                                                  //| 3.125E18
                                                  //| 1.5625E18
                                                  //| 7.8125E17
                                                  //| 3.9062500000000006E17
                                                  //| 1.9531250000000016E17
                                                  //| 9.7656250000000336E16
                                                  //| 4.882812500000068E16
                                                  //| 2.4414062500001364E16
                                                  //| 1.220703125000273E16
                                                  //| 6.103515625005461E15
                                                  //| 3.0517578125109225E15
                                                  //| 1.5258789062718452E15
                                                  //| 7.629394531686906E14
                                                  //| 3.814697266498813E14
                                                  //| 1.9073486345601266E14
                                                  //| 9.536743199015033E13
                                                  //| 4.7683716519363164E13
                                                  //| 2.3841859308257566E13
                                                  //| 1.192093175128066E13
                                                  //| 5.960470069943347E12
                                                  //| 2.9802434235718027E12
                                                  //| 1.4901384889389368E12
                                                  //| 7.451027983977584E11
                                                  //| 3.726185040334168E11
                                                  //| 1.8644343751597583E11
                                                  //| 9.348989660680984E10
                                                  //| 4.727976545279471E10
                                                  //| 2.4697417583458378E10
                                                  //| 1.437321195409579E10
                                                  //| 1.0665299546702797E10
                                                  //| 1.002075063550277E10
                                                  //| 1.0000021484861237E10
                                                  //| res3: Double = 1.0000021484861237E10
	// e.g.5
  /*
		An import type of recursion is tail recursion. Roughly speaking, a function (can be itself) gets
		called at last action in the recursion function. It's important since it can be done with constant
		space in memory, such as stack.
	*/
	
	// one example would be gcd
	def gcd(n: Int, m: Int): Int = {
		if (m == 0) n
		else gcd(m, n % m)
	}                                         //> gcd: (n: Int, m: Int)Int
	
	gcd(63, 49)                               //> res4: Int = 7
	
	// a tail recursive version factorial
	// the key is to maintain an accumulator
	def factorialv1(n: Int): Int = {
		def loop(acc: Int, x: Int): Int = {
			if (x == 1) acc
			else loop(acc*x, x-1)
		}
		loop(1, n)
	}                                         //> factorialv1: (n: Int)Int
	
	factorialv1(5)                            //> res5: Int = 120
	
	// a tail recursive version factorial: without subfunction
	def factorialv2(n: Int, acc: Int): Int = {
		if (n == 1) acc
		else factorialv2(n-1, acc*n)
	}                                         //> factorialv2: (n: Int, acc: Int)Int
	
	factorialv2(5, 1)                         //> res6: Int = 120
	
	
	
}