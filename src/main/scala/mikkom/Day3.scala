package mikkom

import cats.effect.{IO, IOApp}
import scala.io.Source

object Day3 extends IOApp.Simple {

  def countTrees(rightFactor: Int, downFactor: Int = 1)(input: List[String]) = {
    input
      .zipWithIndex
      .filter { case (_, i) =>
        i % downFactor == 0
      }
      .map { case (line, _) =>
        line
      }
      .zipWithIndex
      .map { case (line, i) =>
        line.charAt((rightFactor * i) % line.length)
      }
      .filter(_ == '#')
      .length
  }

  def answer1(input: List[String]) = countTrees(3)(input)

  def answer2(input: List[String]) = {
    val slopes = (1, 2) :: (1 to 7 by 2).map((_, 1)).toList
    slopes
      .map { case (right, down) =>
        countTrees(right, down)(input)
      }
      .product
  }

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day3.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
