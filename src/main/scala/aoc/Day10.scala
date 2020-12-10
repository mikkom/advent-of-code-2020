package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}

object Day10 extends IOApp.Simple {
  type Jolts = Int

  def calculateDistribution(joltages: List[Jolts]) = {
    val sorted = joltages.sorted
    val diffCounts = (0 :: sorted)
      .zip(sorted ++ List(sorted.max + 3))
      .map { case (from, to) =>
        to - from
      }
      .groupBy(identity)
      .view
      .mapValues(_.size)

    List(1, 3).map(diffCounts.getOrElse(_, 0)).product
  }

  def getArrangementCount(joltages: List[Jolts]) = {
    def canConnect(x: Jolts, y: Jolts) = (1 to 3).contains(y - x)

    val counts = scala.collection.mutable.HashMap[List[Jolts], Long]()

    def getCount(joltages: List[Jolts]) =
      counts.getOrElseUpdate(joltages, loop(joltages))

    def loop(joltages: List[Jolts]): Long =
      joltages match {
        case Nil =>
          0L
        case _ :: Nil =>
          1L
        case x :: y :: Nil =>
          if (canConnect(x, y))
            1L
          else
            0L
        case x :: y :: z :: Nil =>
          if (canConnect(x, z))
            2L
          else if (canConnect(x, y))
            getCount(y :: z :: Nil)
          else
            0L
        case x1 :: x2 :: x3 :: x4 :: xs =>
          if (canConnect(x1, x4))
            4 * getCount(x4 :: xs) + 2 * getCount(x3 :: xs) + getCount(x2 :: xs)
          else if (canConnect(x1, x3))
            2 * getCount(x3 :: x4 :: xs)
          else if (canConnect(x1, x2))
            getCount(x2 :: x3 :: x4 :: xs)
          else
            0L
      }

    getCount(0 :: joltages.sorted ++ List(joltages.max + 3))
  }

  def answer1(input: List[String]) =
    input.traverse(_.toIntOption).map(calculateDistribution)

  def answer2(input: List[String]) =
    input.traverse(_.toIntOption).map(getArrangementCount)

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day10.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
