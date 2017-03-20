package scala_note

/*
  In this worksheet we will talk about:
  1. class hierachy, how to organize classes
  2. abstract class
  3. polymorphism
*/
object hierachy_polymorphism {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(266); 
  val l1 = new IntLinkedList(1, new IntLinkedList(2, new IntLinkedList(3, new Empty)));System.out.println("""l1  : scala_note#29.IntLinkedList#1143097 = """ + $show(l1 ));$skip(16); val res$0 = 
  l1.contain(2);System.out.println("""res0: Boolean#2531 = """ + $show(res$0));$skip(25); 
  val l2 = l1.include(1);System.out.println("""l2  : scala_note#29.IntLinkedList#1143097 = """ + $show(l2 ));$skip(15); val res$1 = 
  l1 append l2;System.out.println("""res1: scala_note#29.IntLinkedList#1143097 = """ + $show(res$1));$skip(475); 

  val s1 =
    new nonEmptyNode(
      4,
      new nonEmptyNode(
        2,
        new nonEmptyNode(
          1,
          new emptyNode,
          new emptyNode),
        new nonEmptyNode(
          3,
          new emptyNode,
          new emptyNode)),
      new nonEmptyNode(
        6,
        new nonEmptyNode(
          5,
          new emptyNode,
          new emptyNode),
        new nonEmptyNode(
          7,
          new emptyNode,
          new emptyNode)));System.out.println("""s1  : scala_note#29.nonEmptyNode#1207430 = """ + $show(s1 ));$skip(143); 

  val s2 =
  	(new emptyNode)
  		.include(4)
  		.include(2)
  		.include(1)
  		.include(3)
  		.include(6)
  		.include(5)
  		.include(7);System.out.println("""s2  : scala_note#29.nonEmptyNode#1207430 = """ + $show(s2 ));$skip(82); 
  		
  val s3 =
  (new emptyNode)
  		.include(4)
  		.include(2)
  		.include(1);System.out.println("""s3  : scala_note#29.nonEmptyNode#1207430 = """ + $show(s3 ));$skip(82); 
  		
  val s4 =
  (new emptyNode)
  		.include(6)
  		.include(5)
  		.include(7);System.out.println("""s4  : scala_note#29.nonEmptyNode#1207430 = """ + $show(s4 ));$skip(19); val res$2 = 
  		
 	s3 union s4;System.out.println("""res2: scala_note#29.simpleSet#1168783[Int#1109] = """ + $show(res$2))}
}

// e.g.1
/*
	The first example is an linked list. It's simple to implement due to its recursive property.
	First we define an abstract class for the coming class definition.
	Note that we apply a type abstraction here to allow more flexibility.
	Type T needs to be specified when creating classes extending linkedList.
*/
abstract class linkedList[T] {
  def include(x: T): linkedList[T]
  def contain(x: T): Boolean
  def append(x: linkedList[T]): linkedList[T]
}

// Next we create an Empty linked list, which is basically an empty node.
// Note that when a new value is added, an empty linked list will become non-empty.

// You can argue that there can be only one Empty list
// no need to new Empty everytime creating a new non-empty list
// this can be fixed by change class to object
class Empty extends linkedList[Int] {
  def include(x: Int) = new IntLinkedList(x, this)
  def contain(x: Int) = false
  def append(x: linkedList[Int]) = x

  override def toString() = ""
}

// The following is the definition of linked list consisting of Ints.
// Here cons is an implicit polymorphism
class IntLinkedList(x: Int, cons: linkedList[Int]) extends linkedList[Int] {
  private def head = x
  private def tail = cons

  def include(x: Int) = {
    new IntLinkedList(x, this)
  }

  def contain(x: Int) = {
    head == x || this.tail.contain(x)
  }
  
  def append(x: linkedList[Int]) = new IntLinkedList(head, tail.append(x))

  override def toString() = head + " " + this.tail.toString()

}

// e.g.2
/*
	The second one is a little more complicated.
	We will implement set with a binary tree data structure.
	In a binary tree, the value of each node in left subtree is supposed to be smaller than
	the current node and each node in the right subtree.
*/
abstract class simpleSet[T] {
  def include(x: T): simpleSet[T]
  def contain(x: T): Boolean
  def union(x: simpleSet[T]): simpleSet[T]
}

class emptyNode extends simpleSet[Int] {
  def include(x: Int) = new nonEmptyNode(x, new emptyNode, new emptyNode)
  def contain(x: Int) = false
  def union(x: simpleSet[Int]) = x

  override def toString() = "Null"
}

class nonEmptyNode(x: Int, leftSubtree: simpleSet[Int], rightSubtree: simpleSet[Int]) extends simpleSet[Int] {
  private def value = x
  private def left = leftSubtree
  private def right = rightSubtree

  def contain(x: Int): Boolean = {
    if (value < x) right contain x
    else if (value > x) left contain x
    else true
  }

  def include(x: Int): nonEmptyNode = {
    if (x < value) new nonEmptyNode(value, left include x, right)
    else if (x > value) new nonEmptyNode(value, left, right include x)
    else this
  }
  
  def union(x: simpleSet[Int]) = {
  	((left union right) union x) include value
  }

  // With recursion, it's very easy to implement preorder traversal, postorder traversal and
  // inorder traversal.
  // preorder
  override def toString() = {
    value + " " + left.toString() + " " + right.toString()
  }

  /*
  	inorder
		override def toString() = {
			left.toString() + " " + value + " " + right.toString()
		}
		postorder
		override def toString() = {
			left.toString() + " " + right.toString() + " " + value
		}
	*/
}
