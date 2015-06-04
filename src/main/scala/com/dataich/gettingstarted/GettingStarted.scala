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

  /*
   * EXERCISE 2.3
   * カリー化（currying）では、引数2つの関数fが、fを部分的に適用する引数1つの関数に変換される。
   * この場合も、コンパイルできる実装は1つだけである。この実装を記述せよ。
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  /*
   * EXERCISE 2.4
   * curryによる変換を逆向きに行うuncurryを実装せよ。
   * =>は右結合であるため、A => (B => C)はA => B => Cと記述できる。
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  /*
   * EXERCISE 2.5
   * 2つの関数を合成する高階関数を実装せよ。
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
