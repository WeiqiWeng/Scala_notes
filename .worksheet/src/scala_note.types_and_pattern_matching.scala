package scala_note

/*
  In this worksheet we will talk about:
  1. type bounds
  2. pattern matching, a good tool for decomposition
*/

object types_and_pattern_matching {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(567); 
	/*
		1. +T <=> covariant <=> List[A] <: List[B] if A <: B, so that a List[A] object can be assigned
		to a List[B] pointer.
		2. -T <=> contravariant <=> List[A] >: List[B] if A <: B, so that a List[B] object can be assigned
		to a List[A] pointer.
		3. T <=> nonvariant <=> List[A] and List[B] are not related so that none of them can be assigned
		to another.
	*/
	val l1: List[String] = Nil;System.out.println("""l1  : scala_note.List[String] = """ + $show(l1 ));$skip(30); val res$0 = 
	l1 prepend "a1" prepend "a2";System.out.println("""res0: scala_note.List[String] = """ + $show(res$0));$skip(59); 
	
	// call the pattern matching method
	val n1 = Number(5);System.out.println("""n1  : scala_note.Number = """ + $show(n1 ));$skip(20); 
	val n2 = Number(3);System.out.println("""n2  : scala_note.Number = """ + $show(n2 ));$skip(20); 
	val n3 = Number(2);System.out.println("""n3  : scala_note.Number = """ + $show(n3 ));$skip(9); val res$1 = 
	n1.eval;System.out.println("""res1: Int = """ + $show(res$1));$skip(9); val res$2 = 
	n2.eval;System.out.println("""res2: Int = """ + $show(res$2));$skip(30); 
	val s = Sum(n3, Sum(n1, n2));System.out.println("""s  : scala_note.Sum = """ + $show(s ));$skip(8); val res$3 = 
	s.show;System.out.println("""res3: String = """ + $show(res$3));$skip(8); val res$4 = 
	s.eval;System.out.println("""res4: Int = """ + $show(res$4));$skip(70); 
	
	// call the objected-oriented method
	val n1Obj = new NumberObj(5);System.out.println("""n1Obj  : scala_note.NumberObj = """ + $show(n1Obj ));$skip(30); 
	val n2Obj = new NumberObj(3);System.out.println("""n2Obj  : scala_note.NumberObj = """ + $show(n2Obj ));$skip(30); 
	val n3Obj = new NumberObj(2);System.out.println("""n3Obj  : scala_note.NumberObj = """ + $show(n3Obj ));$skip(12); val res$5 = 
	n1Obj.eval;System.out.println("""res5: Int = """ + $show(res$5));$skip(12); val res$6 = 
	n2Obj.eval;System.out.println("""res6: Int = """ + $show(res$6));$skip(54); 
	val s1 = new SumObj(n3Obj, new SumObj(n1Obj, n2Obj));System.out.println("""s1  : scala_note.SumObj = """ + $show(s1 ));$skip(9); val res$7 = 
	s1.show;System.out.println("""res7: String = """ + $show(res$7));$skip(9); val res$8 = 
	s1.eval;System.out.println("""res8: Int = """ + $show(res$8))}
}

// e.g.1
/*
  In this example, we define a new version of list. We'll provide an implementation based
  on variance.
  Assuming we have two types A and B such that A <: B. Then a parameterized class C[T] can have
  3 different situations when parameterized by A and B:
  	1. C[A] <: C[B] <=> C is covariant
  	2. C[A] >: C[B] <=> C is contravariant
  	3. C[A] and C[B] is not related <=> C is nonvariant.
  Different situations corresponds to different function design.
  In general, functions are contravariant in argument types and covariant in result type.
*/

// Note that since List is covariant, functions should take arguments from superclass of T.
// (prepend)
trait List[+T] {
	def isEmpty(): Boolean
	def head: T
	def tail: List[T]
	def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty() = false
	override def toString() = head + " " + tail.toString()
}

/*
	Remember in Scala Nothing is a common subclass of every class living in Scala.
	Note that List is covariant so that we can assign Nil, List[Nothing] to a List[String].
  Another point is that there actually lives only one empty list so it makes sense to
 	have an object for empty list.
*/
object Nil extends List[Nothing] {
	def isEmpty() = true
	def head: Nothing = throw new NoSuchElementException("No head element in empty list.")
	def tail: Nothing = throw new NoSuchElementException("No cons element in empty list.")
	override def toString() = ""
}

// e.g.2
/*
  In this example, we will talk about the Expression Problem and show how pattern matching is going to
  solve decomposition through a simplified Expression class. Case class definition will be used here.
  The general task we're going to solve is to find a general and convinient way to access
  object in an extensive class hierachy.
*/
trait Expr {
	def eval: Int = this match {
		case Number(e) => e
		case Sum(e1, e2) => e1.eval + e2.eval
	}
	def show[T]: String = this match {
		case Number(e) => e.toString
		case Sum(e1, e2) => e1.toString + " + " + e2.toString
	}
}

case class Number(x: Int) extends Expr

case class Sum(x: Expr, y: Expr) extends Expr

/*
  To make a comparison, we present another way as follow: an objected-oriented way.
*/
trait ExprObj {
	def eval: Int
	def show: String
}

class NumberObj(x: Int) extends ExprObj {
	def value = x
	def eval = value
	def show = value.toString
	override def toString = "" + x
}

class SumObj(x: ExprObj, y: ExprObj) extends ExprObj {
	def l = x
	def r = y
	def eval = l.eval + r.eval
	def show = x.show + " + " + y.show
	override def toString = l + " + " + r
}

/*
  From e.g.2, we can see that
  1. It's more complicated to add method in objected-oriented framework than pattern matching. Each time we add
  a new method, we need to define it each each of the subclass while we just need to add one method involving
  pattern matching.
  2. It's more complicated to add subclass in pattern matching than objected-oriented framework. Each time we add
  a new subclass, we need to add case class for all methods defined in superclass.
*/
