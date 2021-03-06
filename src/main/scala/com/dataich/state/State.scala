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

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
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

  /*
   * EXERCISE 6.3
   * ペア(Int, Double)、ペア(Double, Int)、および3要素のタプル(Double, Double, Double)を生成する関数を記述せよ。
   * すでに作成済みの関数を再利用できるはずだ。
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), newRng) = intDouble(rng)
    ((d, i), newRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  /*
   * EXERCISE 6.4
   * ランダムな整数のリストを生成する関数を記述せよ。
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (i, rng1) = rng.nextInt
      val (ls, rng2) = ints(count - 1)(rng1)
      (i :: ls, rng2)
    }
  }

  /*
   * EXERCISE 6.5
   * mapを使ってdoubleをもう少し要領よく実装し直せ。EXERCISE 6.2を参照。
   */
  def doubleImproved: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /*
   * EXERCISE 6.6
   * 以下のシグネチャに基いてmap2を実装せよ。
   * この関数は、raとrbの2つのアクションと、それらの結果を結合する関数fを受け取り、それらを結合する新しいアクションを返す。
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng1 => {
    val (a, rng2) = ra(rng1)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

}

