package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P}
import ParseUtils._

object Day7 extends IOApp.Simple {

  type Color = String

  case class BagRule(color: Color, mustContain: List[(Int, Color)])

  val parseBagRule = {
    val color =
      (noWhitespaceStr <* P.char(' ')) ~ noWhitespaceStr map { case (a, b) =>
        a + " " + b
      }
    val start  = color <* P.string1(" bags contain ")
    val noBags = P.string1("no other bags").as(List.empty[(Int, Color)])
    val containedBag =
      (int <* P.char(' ')) ~ color <* P.string1(" bag") <* P.char('s').?

    val bagRule =
      start ~ noBags.orElse(P.repSep(containedBag, 1, P.string1(", "))) <*
        P.char('.')
    bagRule.map(BagRule.tupled)
  }

  def countContainers(color: Color)(rules: List[BagRule]): Int = {
    val containedIn = rules.foldMap { case BagRule(color, mustContain) =>
      mustContain
        .map { case (_, containedColor) =>
          containedColor -> Set(color)
        }
        .toMap
    }

    def loop(color: Color): Set[Color] =
      containedIn
        .getOrElse(color, List.empty)
        .foldLeft(Set.empty[Color]) { case (acc, containerColor) =>
          (acc + containerColor) ++ loop(containerColor)
        }

    loop(color).size
  }

  def countRequiredBags(color: Color)(rules: List[BagRule]): Int = {
    val containedBags =
      rules
        .map { rule =>
          rule.color -> rule.mustContain
        }
        .toMap

    def loop(color: Color): Int =
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
