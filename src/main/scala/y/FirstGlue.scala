package y

object SumProblem {
  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case num :: tail => num + sum(tail)
  }
}

object Reduce {
  def reduce[T, K](op: T => K => K)(zero: K)(list: List[T]): K = list match {
    case Nil => zero
    case head :: tail => op(head)(reduce(op)(zero)(tail))
  }
}

object ArithmeticsOnReduce {

  import Reduce._

  def plus(n1: Int)(n2: Int) = n1 + n2

  def multiply(n1: Int)(n2: Int) = n1 * n2

  def sum: List[Int] => Int = reduce(plus)(0)

  def product: List[Int] => Int = reduce(multiply)(1)
}

object BooleanOnReduce {

  import Reduce._

  def or(n1: Boolean)(n2: Boolean) = n1 || n2

  def and(n1: Boolean)(n2: Boolean) = n1 && n2

  def anyTrue: List[Boolean] => Boolean = reduce(or)(false)

  def allTrue: List[Boolean] => Boolean = reduce(and)(true)
}

object ConcatenateLists {

  import Reduce._

  def :::[T](head: T)(tail: List[T]): List[T] = head :: tail

  def <++>[T](a: List[T])(b: List[T]): List[T] = reduce(:::[T])(b)(a)
}

object Length {

  import Reduce._

  def <+>[T](el: T)(len: Int) = 1 + len

  def lgth[T]: List[T] => Int = reduce(<+>)(0)
}

object DoubleAll {

  import Reduce._

  def double(x: Int) = 2 * x

  def <+>(el: Int)(list: List[Int]) = double(el) :: list

  def doubleAll: List[Int] => List[Int] = reduce(<+>)(List.empty[Int])
}

object Map {

  import Reduce._

  def map[T](f: T => T) = {
    def <+>(el: T)(list: List[T]) = f(el) :: list

    reduce(<+>)(List.empty[T])(_)
  }

  def double(x: Int) = 2 * x

  def doubleAll: List[Int] => List[Int] = map(double)
}

case class TreeOf[T](label: T, subtrees: List[TreeOf[T]])

object Tree {

  private def _redtree[T, K](op1: T => K => K)
                            (op2: K => K => K)
                            (zero: K)
                            (subtrees: List[TreeOf[T]]): K = subtrees match {
    case Nil => zero
    case subtree :: rest => op2(redtree(op1)(op2)(zero)(subtree))(_redtree(op1)(op2)(zero)(rest))
  }

  def redtree[T, K](op1: T => K => K)
                   (op2: K => K => K)
                   (zero: K)
                   (tree: TreeOf[T]): K = tree match {
    case TreeOf(label, subtrees) => op1(label)(_redtree[T, K](op1)(op2)(zero)(subtrees))
  }
}

object SumTree {

  import Tree._
  import ArithmeticsOnReduce.plus

  def sumtree = redtree(plus)(plus)(0)(_)
}


object LabelsTree {

  import Tree._
  import ConcatenateLists.:::
  import ConcatenateLists.<++>

  def labels[T]: TreeOf[T] => List[T] = {
//    def <+>(label: T)(subtrees: List[TreeOf[K]]) = TreeOf(f(label), subtrees)


    redtree(:::[T])(<++>[T])(List.empty[T])
  }

}


//object MapTree {
///
//  import Tree._
//  import ConcatenateLists.:::

 // def mapTree[T, K](f: T => K): TreeOf[T] => TreeOf[K] = {

  ///  def <+>(label: T)(subtrees: List[TreeOf[K]]) = TreeOf(f(label), subtrees)

  //  redtree(<+>)(:::[K])(List.empty[K])(_)
 // }


