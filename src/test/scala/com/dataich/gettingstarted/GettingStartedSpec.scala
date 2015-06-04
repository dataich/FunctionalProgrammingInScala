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
  }
}
