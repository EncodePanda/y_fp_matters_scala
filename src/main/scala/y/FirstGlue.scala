package y

object FirstGlue {

  def :::[T](head: T, tail: List[T]): List[T] = head :: tail

  def simpleSum(list: List[Int]): Int = list match {
    case Nil => 0
    case num :: tail => num + sum(tail)
  }

  def reduce[T, K](op: (T, K) => K)(zero: K)(list: List[T]): K = list match {
    case Nil => zero
    case head :: tail => op(head, reduce(op)(zero)(tail))
  }

  def plus(n1: Int, n2: Int) = n1 + n2

  def multiply(n1: Int, n2: Int) = n1 * n2

  def sum: List[Int] => Int = reduce(plus)(0)

  def product: List[Int] => Int = reduce(multiply)(1)

  def or(n1: Boolean, n2: Boolean) = n1 || n2

  def and(n1: Boolean, n2: Boolean) = n1 && n2

  def anyTrue: List[Boolean] => Boolean = reduce(or)(false)

  def allTrue: List[Boolean] => Boolean = reduce(and)(true)

  //  def ++[T](a: List[T], b: List[T]): List[T] = reduce(:::)(a)(b)

}
