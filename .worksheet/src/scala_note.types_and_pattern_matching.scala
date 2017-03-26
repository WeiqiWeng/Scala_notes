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
	l1 prepend "a1" prepend "a2";System.out.println("""res0: scala_note.List[String] = """ + $show(res$0))}
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
