package com.dataich.gettingstarted

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by dataich on 5/30/15.
  */
class GettingStartedSpec extends WordSpec with Matchers {

  import MyModule._

  "EXERCISE 2.1" should {
    "fib" in {
      fib(0) shouldBe 0
      fib(1) shouldBe 1
      fib(2) shouldBe 1
      fib(3) shouldBe 2
      fib(4) shouldBe 3
      fib(5) shouldBe 5
    }
  }

  "EXERCISE 2.2" should {
    "isSorted" in {
      def orderedInt(a: Int, b: Int): Boolean = {
        Ordering.Int.lteq(a, b)
      }
      isSorted(Array(1, 2, 3, 4, 5), orderedInt) shouldBe true
      isSorted(Array(1, 2, 3, 5, 4), orderedInt) shouldBe false
      isSorted(Array(3, 2, 1, 5, 4), orderedInt) shouldBe false


      def orderedString(a: String, b: String): Boolean = {
        Ordering.String.lteq(a, b)
      }
      isSorted(Array("a", "b", "c", "d", "e"), orderedString) shouldBe true
      isSorted(Array("a", "b", "c", "e", "d"), orderedString) shouldBe false
      isSorted(Array("c", "b", "a", "e", "d"), orderedString) shouldBe false
    }
  }

  "EXERCISE 2.3" should {
    "curry" in {
      def sum(x: Int, y: Int): Int = {
        x + y
      }
      val increment = curry(sum)(1)
      increment(1) shouldBe 2
      increment(2) shouldBe 3
    }
  }

  "EXERCISE 2.4" should {
    "uncurry" in {
      val sum = uncurry { x: Int => y: Int =>
        x + y
      }
      sum(1, 1) shouldBe 2
      sum(1, 2) shouldBe 3
    }
  }

  "EXERCISE 2.5" should {
    "compose" in {
      val ceilSqrt = compose(Math.sqrt, Math.ceil)

      ceilSqrt(4.0) shouldBe 2.0
      ceilSqrt(8.6) shouldBe 3.0
    }
  }
}
