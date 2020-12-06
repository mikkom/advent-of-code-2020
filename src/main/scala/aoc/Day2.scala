package aoc

import cats.effect.{IO, IOApp}
import scala.io.Source
import cats.syntax.all._
import cats.parse.{Parser => P}

object Day2 extends IOApp.Simple {

  case class Input1(requiredChar: Char, validCounts: Range, password: String)

  case class Input2(requiredChar: Char, indices: List[Int], password: String)

  def isValidPassword(input: Input1): Boolean = {
    val charCount =
      input.password.toCharArray().filter(_ == input.requiredChar).length
    input.validCounts.contains(charCount)
  }

  def isValidPassword(input: Input2): Boolean = {
    val chars = input.password.toCharArray()
    input
      .indices
      .map(idx => chars(idx - 1))
      .filter(_ == input.requiredChar)
      .length == 1
  }

  val input1Parser =
    (ParseUtils.rangeParser <* P.char(' ')) ~
      (P.anyChar <* P.string1(": ")) ~ P.anyChar.rep1.string map {
        case ((range, requiredChar), password) =>
          Input1(requiredChar, range, password)
      }

  val input2Parser =
    (P.repSep(ParseUtils.int, 0, P.char('-')) <* P.char(' ')) ~
      (P.anyChar <* P.string1(": ")) ~ P.anyChar.rep1.string map {
        case ((indices, requiredChar), password) =>
          Input2(requiredChar, indices, password)
      }

  def answer1(input: List[String]) =
    input.traverse(input1Parser.parseAll).map(_.filter(isValidPassword).length)

  def answer2(input: List[String]) =
    input.traverse(input2Parser.parseAll).map(_.filter(isValidPassword).length)

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day2.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
