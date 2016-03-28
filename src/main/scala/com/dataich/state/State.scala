package com.dataich.state

/**
  * Created by dataich on 3/28/16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = Simple(newSeed)
      val n = (newSeed >> 16).toInt
      (n, nextRng)
    }
  }

  /*
   * EXERCISE 6.1
   * RNG.nextIntを使って0 ~ Int.maxValue（0とInt.maxValueを含む）のランダムな整数を生成する関数を記述せよ。
   * なお、nextIntがInt.MinValueを返すときには、対応する自然数がない。この特異なケースにも対処する必要がある。
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, newRng) = rng.nextInt
    (if (i < 0) -(i + 1) else i, newRng)
  }

  /*
   * EXERCISE 6.2
   * 0 ~ 1（1を含まない）のDouble型の値を生成する関数を記述せよ。
   * Int.MaxValueを使って正の整数の最大値を取得できることと、x.toDoubleを使ってx: IntをDoubleに変換できることに注意。
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, newRng) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), newRng)
  }
}

