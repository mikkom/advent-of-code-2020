package mikkom

import cats.effect.{IO, IOApp}
import scala.io.Source
import scala.annotation.tailrec

object Day1 extends IOApp.Simple {
  val wantedSum = 2020

  def answer1(expenses: List[Int]) = {
    @tailrec
    def loop(lst: List[Int], acc: Set[Int]): Int =
      lst match {
        case Nil =>
          ??? // No solution found
        case x :: xs =>
          if (acc.contains(wantedSum - x))
            x * (wantedSum - x)
          else
            loop(xs, acc + x)
      }
    loop(expenses, Set.empty[Int])
  }

  def answer2(expenses: List[Int]): Int = {
    @tailrec
    def loop(
        lst: List[Int],
        valuesSeen: List[Int],
        remainingNum: Map[Int, Int]
    ): Int =
      lst match {
        case Nil =>
          ??? // No solution found
        case x :: xs =>
          remainingNum.get(wantedSum - x) match {
            case Some(productOfTwo) =>
              x * productOfTwo
            case None =>
              val updatedMap =
                remainingNum ++ valuesSeen.map(y => (x + y) -> (x * y))
              loop(xs, x :: valuesSeen, updatedMap)

          }
      }
    loop(expenses, List.empty, Map.empty)
  }

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day1.txt").getLines().toList)
      expenses = input.map(_.toInt)
      _ <- IO(println(answer1(expenses)))
      _ <- IO(println(answer2(expenses)))
    } yield ()
}
