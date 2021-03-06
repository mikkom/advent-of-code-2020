package aoc

import cats.parse.{Parser => P, Numbers}
import cats.syntax.all._

object ParseUtils {

  val noWhitespaceStr = P.charsWhile1(ch => !List(' ', '\n').contains(ch))

  val hexChar = P.charWhere(ch => "[0-9a-f]".r.matches(ch.toString()))

  val int = Numbers.signedIntString.flatMap(s =>
    s.toIntOption
      .fold[P[Int]](P.failWith(s"Could not parse $s as an integer"))(P.pure(_))
  )

  val long = Numbers.signedIntString.flatMap(s =>
    s.toLongOption
      .fold[P[Long]](P.failWith(s"Could not parse $s as a long"))(P.pure(_))
  )

  val intPlus = P.char('+').? *> int

  val intList = P.repSep(int, 0, P.char(','))

  val rangeParser = ParseUtils.int ~ (P.char('-') *> ParseUtils.int) map {
    case (a, b) => a to b
  }

}
