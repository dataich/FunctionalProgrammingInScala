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
}
