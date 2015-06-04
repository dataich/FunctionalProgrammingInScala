package com.dataich.gettingstarted

/**
 * Created by dataich on 5/30/15.
 */
object MyModule {

  /*
   * EXERCISE 2.1
   * n番目のフィボナッチ数を取得する再帰関数を記述せよ。
   */
  def fib(n: Int) = {
    def go(n: Int, first: Int, second: Int): Int = {
      if (n <= 0) first
      else go(n - 1, second, first + second)
    }

    go(n, 0, 1)
  }

  /*
   * EXERCISE 2.2
   * 指定された比較関数に従ってArray[A]がソートされているかどうかを調べるisSortedを実装せよ。
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n + 1 > (as.length - 1)) true
      else if (ordered(as(n), as(n + 1))) go(n + 1)
      else false
    }

    go(0)
  }

}
