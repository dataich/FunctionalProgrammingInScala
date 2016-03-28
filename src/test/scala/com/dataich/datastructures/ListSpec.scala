package com.dataich.datastructures

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by dataich on 6/4/15.
  */
class ListSpec extends WordSpec with Matchers {

  import List._

  "EXERCISE 3.1" should {
    "x" in {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }

      x shouldBe 3
    }
  }

  "EXERCISE 3.2" should {
    "tail" in {
      tail(List(1, 2, 3)) shouldBe List(2, 3)
      tail(List(1)) shouldBe List()
      tail(List(1)) shouldBe Nil
      tail(List(1, 2)) shouldBe List(2)
    }
  }

  "EXERCISE 3.3" should {
    "setHead" in {
      setHead(List(1, 2, 3), 4) shouldBe List(4, 2, 3)
      setHead(List(3), 4) shouldBe List(4)
      setHead(List(), 1) shouldBe List()
      setHead(Nil, 1) shouldBe Nil
    }
  }

  "EXERCISE 3.4" should {
    "drop" in {
      drop(1, List(1, 2, 3)) shouldBe List(2, 3)
      drop(2, List(1, 2, 3)) shouldBe List(3)
      drop(3, List(1, 2, 3)) shouldBe List()
      drop(3, List(1, 2, 3)) shouldBe Nil
    }
  }

  "EXERCISE 3.5" should {
    "dropWhile" in {
      dropWhile(List(1, 2, 3), { a: Int => a == 1 }) shouldBe List(2, 3)
      dropWhile(List(1, 2, 3), { a: Int => a == 2 }) shouldBe List(1, 2, 3)
      dropWhile(List(1, 2, 3), { a: Int => a <= 2 }) shouldBe List(3)
    }
  }

  "EXERCISE 3.6" should {
    "init" in {
      init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
      init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    }
  }

  "EXERCISE 3.8" should {
    "foldRight" in {
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
    }
  }

  "EXERCISE 3.9" should {
    "length" in {
      List.length(List(1, 2, 3)) shouldBe 3
      List.length(List(1, 2, 3, 4, 5)) shouldBe 5
      List.length(Nil) shouldBe 0
      List.length(List()) shouldBe 0
    }
  }

  "EXERCISE 3.10" should {
    "foldLeft" in {
      foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foldLeft(List(1, 2, 3), 1)(_ * _) shouldBe 6
    }
  }

  "EXERCISE 3.11" should {
    "sum3" in {
      sum3(List(1, 2, 3)) shouldBe 6
      sum3(List(2, 3, 0)) shouldBe 5
    }
  }

  "EXERCISE 3.11" should {
    "product3" in {
      product3(List(1, 2, 3)) shouldBe 6
      product3(List(1, 2, 0)) shouldBe 0
    }
  }

  "EXERCISE 3.11" should {
    "length2" in {
      length2(List(1, 2, 3)) shouldBe 3
      length2(List()) shouldBe 0
      length2(Nil) shouldBe 0
    }
  }

  "EXERCISE 3.12" should {
    "reverse" in {
      reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
      reverse(List(1)) shouldBe List(1)
      reverse(List()) shouldBe List()
    }
  }
}
