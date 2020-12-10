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
            loop(y :: z :: Nil)
          else
            0L
        case x1 :: x2 :: x3 :: x4 :: xs =>
          if (canConnect(x1, x4))
            4 * loop(x4 :: xs) + 2 * loop(x3 :: xs) + loop(x2 :: xs)
          else if (canConnect(x1, x3))
            2 * loop(x3 :: x4 :: xs)
          else if (canConnect(x1, x2))
            loop(x2 :: x3 :: x4 :: xs)
          else
            0L
      }

    loop(0 :: joltages.sorted ++ List(joltages.max + 3))
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
