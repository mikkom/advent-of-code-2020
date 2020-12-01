package mikkom

import cats.effect.{IO, IOApp}
import scala.io.Source

object Day1 extends IOApp.Simple {
  val wantedSum = 2020

  def answer1(expenses: List[Int]) =
    expenses
      .foldLeft((List.empty[Int], Set.empty[Int])) { case ((acc, seen), x) =>
        if (seen.contains(wantedSum - x))
          ((x * (wantedSum - x)) :: acc, seen + x)
        else
          (acc, seen + x)
      }
      ._1
      .headOption

  def answer2(expenses: List[Int]) =
    expenses
      .foldLeft((List.empty[Int], List.empty[Int], Map.empty[Int, Int])) {
        case ((acc, seen, pairMap), x) =>
          val updatedMap = pairMap ++ seen.map(y => (x + y) -> (x * y))
          pairMap.get(wantedSum - x) match {
            case Some(productOfTwo) =>
              ((x * productOfTwo) :: acc, x :: seen, updatedMap)
            case None =>
              (acc, x :: seen, updatedMap)
          }
      }
      ._1
      .headOption

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day1.txt").getLines().toList)
      expenses = input.map(_.toInt)
      _ <- IO(println(answer1(expenses)))
      _ <- IO(println(answer2(expenses)))
    } yield ()
}
