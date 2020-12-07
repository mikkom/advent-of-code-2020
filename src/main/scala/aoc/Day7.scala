package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P}
import ParseUtils._

object Day7 extends IOApp.Simple {

  case class BagRule(color: String, canContain: List[(Int, String)])

  val parseBagRule = {
    val color =
      (noWhitespaceStr <* P.char(' ')) ~ noWhitespaceStr map { case (a, b) =>
        a + " " + b
      }
    val start  = color <* P.string1(" bags contain ")
    val noBags = P.string1("no other bags").as(List.empty[(Int, String)])
    val containedBag =
      (int <* P.char(' ')) ~ color <* P.string1(" bag") <* P.char('s').?

    val bagRule =
      start ~ noBags.orElse(P.repSep(containedBag, 1, P.string1(", "))) <*
        P.char('.')
    bagRule.map(BagRule.tupled)
  }

  def countContainers(color: String)(rules: List[BagRule]): Int = {
    val containedIn = rules.foldMap { case BagRule(color, canContain) =>
      canContain
        .map { case (_, containedColor) =>
          containedColor -> Set(color)
        }
        .toMap
    }

    def loop(color: String): Set[String] =
      containedIn
        .getOrElse(color, List.empty)
        .foldLeft(Set.empty[String]) { case (acc, containerColor) =>
          (acc + containerColor) ++ loop(containerColor)
        }

    loop(color).size
  }

  def countRequiredBags(color: String)(rules: List[BagRule]): Int = {
    val containedBags =
      rules
        .map { rule =>
          rule.color -> rule.canContain
        }
        .toMap

    def loop(color: String): Int =
      containedBags
        .getOrElse(color, List.empty)
        .foldLeft(0) { case (total, (count, color)) =>
          total + count + count * loop(color)
        }
    loop(color)
  }

  def answer1(input: List[String]) =
    input.traverse(parseBagRule.parseAll).map(countContainers("shiny gold"))

  def answer2(input: List[String]) =
    input.traverse(parseBagRule.parseAll).map(countRequiredBags("shiny gold"))

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day7.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
