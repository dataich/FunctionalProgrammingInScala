package com.dataich.datastructures

import org.specs2.mutable.Specification

/**
 * Created by dataich on 6/4/15.
 */
class ListSpec extends Specification {

  import List._

  "List" >> {
    "EXERCISE 3.1" >> {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }

      x mustEqual 3
    }

    "EXERCISE 3.2 tail" >> {
      tail(List(1, 2, 3)) mustEqual List(2, 3)
      tail(List(1)) mustEqual List()
      tail(List(1)) mustEqual Nil
      tail(List(1, 2)) mustEqual List(2)
    }
  }
}