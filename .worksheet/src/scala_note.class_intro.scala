package scala_note

/*
  In this worksheet we will talk about:
  1. how to define class in Scala through an example
  2. legal identifier in Scala
  3. infix
*/
object class_intro {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(212); 

  val r1 = new Rational(1, 2);System.out.println("""r1  : scala_note#29.Rational#636920 = """ + $show(r1 ));$skip(30); 
  val r2 = new Rational(2, 3);System.out.println("""r2  : scala_note#29.Rational#636920 = """ + $show(r2 ));$skip(30); 
  val r3 = new Rational(1, 4);System.out.println("""r3  : scala_note#29.Rational#636920 = """ + $show(r3 ));$skip(27); 
  val r4 = new Rational(2);System.out.println("""r4  : scala_note#29.Rational#636920 = """ + $show(r4 ));$skip(9); val res$0 = 
  r1.neg;System.out.println("""res0: scala_note#29.Rational#636920 = """ + $show(res$0));$skip(6); val res$1 = 
  -r1;System.out.println("""res1: scala_note#29.Rational#636920 = """ + $show(res$1));$skip(19); 
  println(r1 + r2);$skip(10); val res$2 = 
  r2 < r1;System.out.println("""res2: Boolean#2531 = """ + $show(res$2));$skip(12); val res$3 = 
  r1 max r2;System.out.println("""res3: scala_note#29.Rational#636920 = """ + $show(res$3));$skip(24); 
  println(r1 - r2 - r3);$skip(82); 

  // here r1 mul r2 is equivalent to r1 * r2 and r1.mul(r2)
  println(r1 mul r2)}
}

// class definition in Scala is quite similar to Java
// it's the same syntax to create an object of a class
// every time we define a class, a constructor will be created as well as 'this'
// Also like Java, every class has a toString method for printing. We just need to override
// otherwise it will return a hashcode of the object

class Rational(x: Int, y: Int) {
  // To be a rational, the denominator must not be zero
  // it will be checked whenever an object is initialized
  require(y != 0, "denominator must not be zero")

  // first we need a private function gcd to simplify our Rational, for example 2/6 = 1/3
  // client will not have access to private function
  private def gcd(n: Int, m: Int): Int = {
    if (m == 0) n
    else gcd(m, n % m)
  }
  // val in a class will be initialized and computed only once
  private val g = gcd(x, y)
  private def numer = x / g
  private def denom = y / g

  // as in Java we can have several constructors, it's also possible to have
  // several constructors in Scala
  // In the following constructor, we call the primary constructor through this
  def this(x: Int) = this(x, 1)

  // Note that Scala allows function name begins with +, -, *, etc
  // also 'this' is available in every function definition
  def +(that: Rational) =
    new Rational(this.numer * that.denom + this.denom * that.numer, this.denom * that.denom)

  def neg = new Rational(-numer, denom)
  // neg should be an unary operator
  // in Scala we have a special way to define unary operator
  // if return type is specified,
  // be careful with the ':', we need a space between '-' and ':' since ':' is allowed
  // in function name, 'unary_-:' is legal
  def unary_- = new Rational(-numer, denom)

  // any method with one parameter can be used in an infix style
  def -(that: Rational) = this + -that

  def *(that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)

  def /(that: Rational) =
    new Rational(this.numer * that.denom, this.denom * that.numer)

  def mul(that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)

  def <(that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  def max(y: Rational) = if (this < y) y else this

  // Every class has a toString method. When overriding, we need to specify
  // the keyword 'override'
  override def toString(): String = numer + "/" + denom
}
