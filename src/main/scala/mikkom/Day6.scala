package mikkom

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}

object Day6 extends IOApp.Simple {

  def answer1(input: List[String]) = {
    val (total, _) =
      input.foldLeft((0, Set.empty[Char])) { case ((total, groupSet), form) =>
        if (form.isEmpty())
          (total + groupSet.size, Set.empty[Char])
        else
          (total, groupSet ++ form)
      }
    total
  }

  def answer2(input: List[String]) = {
    val (total, _) =
      input.foldLeft((0, none[Set[Char]])) {
        case ((total, groupSet), "") =>
          (total + groupSet.map(_.size).getOrElse(0), None)
        case ((total, None), form) =>
          (total, Some(form.toSet))
        case ((total, Some(groupSet)), form) =>
          (total, Some(groupSet.intersect(form.toSet)))
      }
    total
  }

  override def run: IO[Unit] =
    for {
      input <- IO(
        Source.fromResource("input-day6.txt").getLines().concat(List("")).toList
      )
      _ <- IO(println(answer1(input)))
      _ <- IO(println(answer2(input)))
    } yield ()
}
