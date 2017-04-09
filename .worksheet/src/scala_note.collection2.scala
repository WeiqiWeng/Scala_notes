package scala_note
/*
  In this worksheet we will continue talking about collections in Scala:
  1. sequence
  2. higher-order function on sequence
*/
object collection2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(339); 

	// It's even simpler to define a function to tell whether a given number is prime or not.
	def isPrime(x: Int): Boolean = {
		(2 until x) forall (t => x % t != 0)
	};System.out.println("""isPrime: (x: Int)Boolean""");$skip(15); val res$0 = 
	
	isPrime(91);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(13); val res$1 = 
	isPrime(79);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(423); 
	
	// Based on isPrime and higher-order function, we can make some complicated functions.
  //5. use of higher order functions
	
	/*
		e.g. Here we present a more complicated example. Given a positive integer N, find all the combinations
		(x, y) such that 1 <= x, y <= N and x+y is prime.
	*/
	def findPair(n: Int) = {
		((1 until n) map (x => (1 until x) map (y => (x, y)))).flatten filter (z => isPrime(z._1 + z._2))
	};System.out.println("""findPair: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]""");$skip(15); val res$2 = 
	
	findPair(7);System.out.println("""res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$2));$skip(190); 
	
	// the pattern map and then flatten can be replaced by flatMap
	def findPair1(n: Int) = {
		(1 until n) flatMap (x => (1 until x) map (y => (x, y))) filter (z => isPrime(z._1 + z._2))
	};System.out.println("""findPair1: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]""");$skip(167); 
	
	// Also Scala provides a syntax to do filter and map in a row.
	def findPair2(n: Int) = {
		for (i <- 1 until n; j <- 1 until i; if isPrime(i + j)) yield (i, j)
	};System.out.println("""findPair2: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]""");$skip(16); val res$3 = 
	
	findPair2(7);System.out.println("""res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$3));$skip(175); 
	// Similarly, scalarProjuct can be rewritten with for syntax.
	def scalarProd2(xs: List[Double], ys: List[Double]): Double = {
		(for ((x, y) <- xs zip ys) yield x*y) sum
	};System.out.println("""scalarProd2: (xs: List[Double], ys: List[Double])Double""");$skip(45); val res$4 = 
	scalarProd2(List(1.0, 2.3), List(2.4, 9.2));System.out.println("""res4: Double = """ + $show(res$4));$skip(375); 
	
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
	};System.out.println("""nQueens: (n: Int)Set[List[Int]]""");$skip(184); 
	
	def isSafe(col: Int, queen: List[Int]): Boolean = {
		val row = queen.length
		((row-1 to 0 by -1) zip queen) forall {
			case (r, l) => col != l && math.abs(col-l) != row-r
		}
	};System.out.println("""isSafe: (col: Int, queen: List[Int])Boolean""");$skip(14); val res$5 = 
	
	nQueens(4);System.out.println("""res5: Set[List[Int]] = """ + $show(res$5));$skip(132); 
	
	// 7. Map
	// x comes from X, y comes from Y, map: f x --> y
	val map1 = Map("A" -> (91, 100), "B" -> (81, 90), "C" -> (71, 80));System.out.println("""map1  : scala.collection.immutable.Map[String,(Int, Int)] = """ + $show(map1 ));$skip(108); val res$6 = 
  // We can make a function call on a map, but it will return exception if the key is not found.
	map1("A");System.out.println("""res6: (Int, Int) = """ + $show(res$6));$skip(103); val res$7 = 
	// Otherwise, we can make a method call. It will return a none if the key is not found.
	map1 get "F";System.out.println("""res7: Option[(Int, Int)] = """ + $show(res$7));$skip(16); val res$8 = 
	map1 get ("B");System.out.println("""res8: Option[(Int, Int)] = """ + $show(res$8));$skip(84); 
	// Another way is to set default values.
	val map2 = map1 withDefaultValue (0, 70);System.out.println("""map2  : scala.collection.immutable.Map[String,(Int, Int)] = """ + $show(map2 ));$skip(12); val res$9 = 
  map2("F");System.out.println("""res9: (Int, Int) = """ + $show(res$9));$skip(15); val res$10 = 
  map2 get "F";System.out.println("""res10: Option[(Int, Int)] = """ + $show(res$10));$skip(181); 
  // Note here the difference between function call and method.
  
  // Now we present a more concrete example: polynomials.
	
	val p1 = new Poly(Map(1 -> 1.0, 2 -> 2.5, 3 -> 3.1));System.out.println("""p1  : scala_note.Poly = """ + $show(p1 ));$skip(54); 
	val p2 = new Poly(Map(0 -> 1.0, 1 -> 1.5, 4 -> 4.1));System.out.println("""p2  : scala_note.Poly = """ + $show(p2 ));$skip(11); val res$11 = 
	
	p1 + p2;System.out.println("""res11: scala_note.Poly = """ + $show(res$11));$skip(11); val res$12 = 
	p1 add p2;System.out.println("""res12: scala_note.Poly = """ + $show(res$12))}
	
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
