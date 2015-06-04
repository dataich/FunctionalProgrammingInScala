package com.dataich.datastructures

/**
 * Created by dataich on 6/4/15.
 */

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /*
   * EXERCISE 3.2
   * Listの最初の要素を削除する関数tailを実装せよ。この関数の実行時間が一定であることに注意。
   */
  def tail[A](ls: List[A]): List[A] = ls match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  /*
   * EXERCISE 3.3
   * EXERCISE 3.2と同じ考え方に基づいて、Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。
   */
  def setHead[A](ls: List[A], a: A): List[A] = ls match {
    case Cons(_, xs) => Cons(a, xs)
    case _ => Nil
  }
}