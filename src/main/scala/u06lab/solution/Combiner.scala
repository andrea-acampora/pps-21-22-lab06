package u06lab.solution

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

import GivenCombiners.given

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = combine(a)
  override def concat(a: Seq[String]): String = combine(a)
  override def max(a: List[Int]): Int = combine(a)
  def combine[A: Combiner](a: Iterable[A]): A =
    a.foldLeft(summon[Combiner[A]].unit)(summon[Combiner[A]].combine)

object GivenCombiners:
  given Combiner[Double] with
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b

  given Combiner[String] with
    override def unit: String = ""
    override def combine(a: String, b: String): String = a.concat(b)

  given Combiner[Int] with
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = if a > b then a else b

import GivenCombiners.given

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648
