import scala_note.hierachy_polymorphism._

/*
  In this worksheet we will talk about:
  1. type bounds
  2. pattern matching
*/

object types_and_pattern_matching {
	

}

// e.g.1
/*
  In this example, we define a new version of list. We'll provide an implementation based
  on variance.
*/

trait List[T] {
	def isEmpty(): Boolean
	def head: T
	def tail: List[T]
}

class Cons[T] extends List[T] {
	def isEmpty() = false
}

// Remember in Scala Nothing is a common subclass of every class living in Scala.
// Another point is that there actually lives only one empty list so it makes sense to
// have an object for empty list.
class Nil extends List[Nothing] {
	def isEmpty() = true
	def head: Nothing = throw new NoSuchElementException("No head element in empty list.")
	def tail: Nothing = throw new NoSuchElementException("No cons element in empty list.")
}
