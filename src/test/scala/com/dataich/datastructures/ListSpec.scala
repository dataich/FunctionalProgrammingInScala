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

    "EXERCISE 3.3 setHead" >> {
      setHead(List(1, 2, 3), 4) mustEqual List(4, 2, 3)
      setHead(List(3), 4) mustEqual List(4)
      setHead(List(), 1) mustEqual List()
      setHead(Nil, 1) mustEqual Nil
    }

    "EXERCISE 3.4 drop" >> {
      drop(1, List(1, 2, 3)) mustEqual List(2, 3)
      drop(2, List(1, 2, 3)) mustEqual List(3)
      drop(3, List(1, 2, 3)) mustEqual List()
      drop(3, List(1, 2, 3)) mustEqual Nil
    }

    "EXERCISE 3.5 dropWhile" >> {
      dropWhile(List(1, 2, 3), { a: Int => a == 1 }) mustEqual List(2, 3)
      dropWhile(List(1, 2, 3), { a: Int => a == 2 }) mustEqual List(1, 2, 3)
      dropWhile(List(1, 2, 3), { a: Int => a <= 2 }) mustEqual List(3)
    }
  }
}
