package aoc

import cats.effect.{IO, IOApp}
import scala.io.Source

object Day1 extends IOApp.Simple {
  val wantedSum = 2020

  def findProductWithSum(expenses: List[Int], elemCount: Int) =
    expenses
      .combinations(elemCount)
      .filter(_.sum == wantedSum)
      .map(_.product)
      .nextOption()

  def answer1(expenses: List[Int]) = findProductWithSum(expenses, 2)

  def answer2(expenses: List[Int]) = findProductWithSum(expenses, 3)

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day1.txt").getLines().toList)
      expenses = input.map(_.toInt)
      _ <- IO(println(answer1(expenses)))
      _ <- IO(println(answer2(expenses)))
    } yield ()
}
