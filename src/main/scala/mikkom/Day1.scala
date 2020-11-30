package mikkom

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import scala.io.Source
import cats.parse.{Parser => P, Parser1, Numbers}

object Day1 extends IOApp.Simple {

  val parseFoo: Parser1[Int] = {
    Numbers.nonNegativeIntString.map(_.toInt)
  }

  def getThere(str: String) = {
    parseFoo.parseAll(str)
  }

  def fuelNeeded(mass: Int) = mass / 3 - 2

  def fuelNeeded2(mass: Int) =
    List
      .unfold(mass)(m => fuelNeeded(m).some.filter(_ > 0).map(x => (x, x)))
      .combineAll

  def answer1(masses: List[Int]) = masses.map(fuelNeeded).combineAll

  def answer2(masses: List[Int]): Int = masses.map(fuelNeeded2).combineAll

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day1.txt").getLines().toList)
      masses = input.map(_.toInt)
      _ <- IO(println(answer1(masses)))
      _ <- IO(println(answer2(masses)))
    } yield ()
}
