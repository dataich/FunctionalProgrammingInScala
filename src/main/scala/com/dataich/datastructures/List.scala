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

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
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

  /*
   * EXERCISE 3.7
   * Q.foldRightを使って実装されたproductは、0.0を検出した場合に、直ちに再帰を中止して0.0を返せるか。その理由を説明せよ。
   * 大きなリストでfoldRightを呼び出した場合の短絡の仕組みについて検討せよ。
   *
   * A.foldRightは関数fを実行する前にその引数を評価する実装になっている。
   * このため全てのリストを走査し終えないと関数fが実行されないため、評価のしようがない。
   */

  /*
   * EXERCISE 3.8
   * Q.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))のように、NilおよびCons自体をfoldRightに渡した場合はどうなるか。
   * これがfoldRightとListのデータコンストラクタとの関係について何を表していると思うか。
   *
   * A.foldRightに渡すListとfoldRightが返すListが同じ結果となる。
   * foldRightはNilが渡されるとzを返し、Consが渡されるとfを返すため、当然同じ結果となる。
   */

  /*
   * EXERCISE 3.9
   * foldRightを使ってリストの長さを計算せよ。
   */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => 1 + b)

  /*
   * EXERCISE 3.10
   * このfoldRightの実装は末尾再帰ではなく、リストが大きい場合はStackOverflowErrorになってしまう。
   * そうした状況であると仮定し、リスト再帰の総称関数foldLeftを実装せよ。
   */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /*
   * EXERCISE 3.11
   * foldLeftを使ってsum、product、およびリストの長さを計算する関数を記述せよ。
   */
  def sum3(ls: List[Int]) = foldLeft(ls, 0)(_ + _)

  def product3(ls: List[Double]) = foldLeft(ls, 1.0)(_ * _)

  def length2[A](ls: List[A]) = foldLeft(ls, 0)((b, _) => 1 + b)

}