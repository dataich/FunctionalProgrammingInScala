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

  /*
   * EXERCISE 3.4
   * tailを一般化して、リストの先頭からn個の要素を削除するdropという関数に書きかえよ。
   * この関数の実行時間は削除する要素の数にのみ比例することに注意。List全体のコピーを作成する必要はない。
   */
  def drop[A](n: Int, ls: List[A]): List[A] = {
    if (n == 0) ls
    else ls match {
      case Cons(_, xs) => drop(n - 1, xs)
      case _ => ls
    }
  }

  /*
   * EXERCISE 3.5
   * 述語とマッチする場合に限り、Listからその要素までの要素を削除するdropWhileを実装せよ。
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  /*
   * EXERCISE 3.6
   * Listの末尾を除くすべての要素で構成されたListを返すinit関数を実装せよ。
   * List(1, 2, 3, 4)が与えられた場合、initはList(1, 2, 3)を返す。
   *
   * Q.この関数をtailのように一定時間で実装できないのはなぜか。
   * 
   * A.List全体を走査するため、Listの要素数に比例した時間がかかるため。
   */
  def init[A](ls: List[A]): List[A] = ls match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }
}