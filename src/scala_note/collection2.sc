package scala_note
/*
  In this worksheet we will continue talking about collections in Scala:
  1. sequence
  2. higher-order function on sequence
*/
object collection2 {

	// It's even simpler to define a function to tell whether a given number is prime or not.
	def isPrime(x: Int): Boolean = {
		(2 until x) forall (t => x % t != 0)
	}                                         //> isPrime: (x: Int)Boolean
	
	isPrime(91)                               //> res0: Boolean = false
	isPrime(79)                               //> res1: Boolean = true
	
	// Based on isPrime and higher-order function, we can make some complicated functions.
  //5. use of higher order functions
	
	/*
		e.g. Here we present a more complicated example. Given a positive integer N, find all the combinations
		(x, y) such that 1 <= x, y <= N and x+y is prime.
	*/
	def findPair(n: Int) = {
		((1 until n) map (x => (1 until x) map (y => (x, y)))).flatten filter (z => isPrime(z._1 + z._2))
	}                                         //> findPair: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	
	findPair(7)                               //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
	
	// the pattern map and then flatten can be replaced by flatMap
	def findPair1(n: Int) = {
		(1 until n) flatMap (x => (1 until x) map (y => (x, y))) filter (z => isPrime(z._1 + z._2))
	}                                         //> findPair1: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	
	// Also Scala provides a syntax to do filter and map in a row.
	def findPair2(n: Int) = {
		for (i <- 1 until n; j <- 1 until i; if isPrime(i + j)) yield (i, j)
	}                                         //> findPair2: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	
	findPair2(7)                              //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))
	// Similarly, scalarProjuct can be rewritten with for syntax.
	def scalarProd2(xs: List[Double], ys: List[Double]): Double = {
		(for ((x, y) <- xs zip ys) yield x*y) sum
	}                                         //> scalarProd2: (xs: List[Double], ys: List[Double])Double
	scalarProd2(List(1.0, 2.3), List(2.4, 9.2))
                                                  //> res4: Double = 23.559999999999995
	
	// 6. Set
	// We introduce the usage of Set with a classic problem: N-queen. Set can not contain duplicated elements.
	
	def nQueens(n: Int): Set[List[Int]] = {
		def placeQueen(k: Int): Set[List[Int]] = {
			if (k == 0) Set(List())
			else {
				for {queen <- placeQueen(k-1)
				col <- 0 until n
				if isSafe(col, queen)} yield col :: queen
			}
		}
		placeQueen(n)
	}                                         //> nQueens: (n: Int)Set[List[Int]]
	
	def isSafe(col: Int, queen: List[Int]): Boolean = {
		val row = queen.length
		((row-1 to 0 by -1) zip queen) forall {
			case (r, l) => col != l && math.abs(col-l) != row-r
		}
	}                                         //> isSafe: (col: Int, queen: List[Int])Boolean
	
	nQueens(4)                                //> res5: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))
	
	// 7. Map
	// x comes from X, y comes from Y, map: f x --> y
	val map1 = Map("A" -> (91, 100), "B" -> (81, 90), "C" -> (71, 80))
                                                  //> map1  : scala.collection.immutable.Map[String,(Int, Int)] = Map(A -> (91,10
                                                  //| 0), B -> (81,90), C -> (71,80))
  // We can make a function call on a map, but it will return exception if the key is not found.
	map1("A")                                 //> res6: (Int, Int) = (91,100)
	// Otherwise, we can make a method call. It will return a none if the key is not found.
	map1 get "F"                              //> res7: Option[(Int, Int)] = None
	map1 get ("B")                            //> res8: Option[(Int, Int)] = Some((81,90))
	// Another way is to set default values.
	val map2 = map1 withDefaultValue (0, 70)  //> map2  : scala.collection.immutable.Map[String,(Int, Int)] = Map(A -> (91,10
                                                  //| 0), B -> (81,90), C -> (71,80))
  map2("F")                                       //> res9: (Int, Int) = (0,70)
  map2 get "F"                                    //> res10: Option[(Int, Int)] = None
  // Note here the difference between function call and method.
  
  // Now we present a more concrete example: polynomials.
	
	val p1 = new Poly(Map(1 -> 1.0, 2 -> 2.5, 3 -> 3.1))
                                                  //> p1  : scala_note.Poly = 1.0x^1+2.5x^2+3.1x^3
	val p2 = new Poly(Map(0 -> 1.0, 1 -> 1.5, 4 -> 4.1))
                                                  //> p2  : scala_note.Poly = 1.0x^0+1.5x^1+4.1x^4
	
	p1 + p2                                   //> res11: scala_note.Poly = 1.0x^0+2.5x^1+2.5x^2+3.1x^3+4.1x^4
	p1 add p2                                 //> res12: scala_note.Poly = 1.0x^0+2.5x^1+2.5x^2+3.1x^3+4.1x^4
	
}

class Poly(term: Map[Int, Double]) {
	// * denotes an arbitrary number of parameters passed to constructor
	def this(pairs: (Int, Double)*) = this(pairs.toMap)
	val termWithDefault = term withDefaultValue 0.0
	
	def + (other: Poly) = new Poly(termWithDefault ++ (other.termWithDefault map adjustCoef))
	
	def add (other: Poly) = new Poly((other.termWithDefault foldLeft termWithDefault) (addTerm))
	
	def addTerm(a: Map[Int, Double], b: (Int, Double)): Map[Int, Double] = {
		val (exp, coef) = b
		a + (exp -> (coef + a(exp)))
	}
	
	def adjustCoef(term2: (Int, Double)): (Int, Double) = {
		val (exp, coef) = term2
		exp -> (coef + termWithDefault(exp))
	}
	
	override def toString = {
		(for ((exp, coef) <- termWithDefault.toList.sorted) yield coef + "x^" + exp) mkString "+"
	}

  	
}