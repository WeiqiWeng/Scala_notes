package scala_note
/*
  In this worksheet we will talk about:
  1. class hierarchy, how class and subclass are organized
  2. abstract class
  3. trait
  4. polymorphism
*/
object hierarchy_polymorphism {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(217); 
	println("")}
}

// First we define an abstract class to implement the coming classes
abstract class set[T] {
	def contain(x: T): Boolean
	def include(x: T): set[T]
}

class intSet extends set[Int] {
	def contain(x: Int)
}
