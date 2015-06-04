package com.dataich.gettingstarted

import org.specs2.mutable.Specification

/**
 * Created by dataich on 5/30/15.
 */
class GettingStartedSpec extends Specification {

  import MyModule._

  "MyModule" >> {
    "EXERCISE 2.1 fib" >> {
      fib(0) mustEqual 0
      fib(1) mustEqual 1
      fib(2) mustEqual 1
      fib(3) mustEqual 2
      fib(4) mustEqual 3
      fib(5) mustEqual 5
    }

    "EXERCISE 2.2 isSorted" >> {
      def orderedInt(a: Int, b: Int): Boolean = {
        Ordering.Int.lteq(a, b)
      }
      isSorted(Array(1, 2, 3, 4, 5), orderedInt) mustEqual true
      isSorted(Array(1, 2, 3, 5, 4), orderedInt) mustEqual false
      isSorted(Array(3, 2, 1, 5, 4), orderedInt) mustEqual false


      def orderedString(a: String, b: String): Boolean = {
        Ordering.String.lteq(a, b)
      }
      isSorted(Array("a", "b", "c", "d", "e"), orderedString) mustEqual true
      isSorted(Array("a", "b", "c", "e", "d"), orderedString) mustEqual false
      isSorted(Array("c", "b", "a", "e", "d"), orderedString) mustEqual false
    }

    "EXERCISE 2.3 curry" >> {
      def sum(x: Int, y: Int): Int = {
        x + y
      }
      val increment = curry(sum)(1)
      increment(1) mustEqual 2
      increment(2) mustEqual 3
    }
  }
}
