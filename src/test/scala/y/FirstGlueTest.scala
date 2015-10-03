package y

import org.scalatest.{Matchers, FunSuite}

class FirstGlueTest extends FunSuite with Matchers {

  import y.FirstGlue._

  test("this and that") {

    val numbers = List(1, 2, 4, 6, 8, 10)

    simpleSum(numbers) should equal(31)

    sum(numbers) should equal(31)

    product(numbers) should equal(3840)

    val booleans = List(true, false, false, true, false)

    allTrue(booleans) should be(false)

    anyTrue(booleans) should be(true)


    val words1 = List("first", "second", "third")
    val words2 = List("dog", "cat", "tortoise")

    words1 ++ words2 should equal(List("first", "second", "third", "dog", "cat", "tortoise"))

  }

}
