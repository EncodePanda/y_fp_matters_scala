package y

import org.scalatest.{Matchers, FunSuite}

class FirstGlueTest extends FunSuite with Matchers {

  val numbers = List(1, 2, 4, 6, 8, 10)

  val booleans = List(true, false, false, true, false)

  val words1 = List("first", "second", "third")
  val words2 = List("dog", "cat", "tortoise")

  test("SumProblem") {
    import y.SumProblem._

    sum(numbers) should equal(31)
  }

  test("ArithmeticsOnReduce") {
    import y.ArithmeticsOnReduce._

    sum(numbers) should equal(31)

    product(numbers) should equal(3840)
  }

  test("BooleanOnReduce") {
    import y.BooleanOnReduce._

    allTrue(booleans) should be(false)

    anyTrue(booleans) should be(true)
  }

  test("ConcatenateLists") {
    import y.ConcatenateLists._

    <++>(words1)(words2) should equal(List("first", "second", "third", "dog", "cat", "tortoise"))
  }

  test("Length") {
    import y.Length._

    lgth(numbers) should equal(6)
    lgth(words1) should equal(3)
    lgth(words1 ++ words2) should equal(6)
  }

  test("DoubleAll") {
    import y.DoubleAll._

    doubleAll(numbers) should equal(List(2, 4, 8, 12, 16, 20))
  }

  test("Map") {
    import y.Map._

    doubleAll(numbers) should equal(List(2, 4, 8, 12, 16, 20))
  }

  test("SumTree") {
    import y.Tree._
    import y.SumTree._


    val sampleTree = TreeOf(1, List(TreeOf(2, Nil), TreeOf(3, List(TreeOf(4, Nil)))))

    sumtree(sampleTree) should equal(10)
  }

  test("LabelsTree") {
    import y.LabelsTree._

    val sampleTree = TreeOf(1, List(TreeOf(2, Nil), TreeOf(3, List(TreeOf(4, Nil)))))

    labels(sampleTree) should equal(List(1,2,3,4))
  }

  test("MapTree") {
    import y.MapTree.mapTree

    val sampleTree = TreeOf(1, List(TreeOf(2, Nil), TreeOf(3, List(TreeOf(4, Nil)))))

    (mapTree((n:Int) => n.toString)(sampleTree)) should
              equal(TreeOf("1", List(TreeOf("2", Nil), TreeOf("3", List(TreeOf("4", Nil))))))
  }

}
