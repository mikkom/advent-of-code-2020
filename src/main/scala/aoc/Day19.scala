package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P}
import ParseUtils._

object Day19 extends IOApp.Simple {

  type RefRule = List[Int]

  sealed trait Rule
  case class Literal(ch: Char)             extends Rule
  case class Refs(refRules: List[RefRule]) extends Rule

  val parseRule = {
    val literalRule = P.anyChar.surroundedBy(P.char('"')).map(Literal)
    val refRule     = P.rep1Sep(int, 1, P.char(' ')).map(_.toList)
    val combinedRefs = P.rep1Sep(refRule, 1, P.string1(" | "))
      .map(r => Refs(r.toList))
    val ruleNumber = int <* P.string1(": ")

    ruleNumber ~ literalRule.orElse1[Rule](combinedRefs)
  }

  object Rule {

    def parse(rule: Rule, ruleSet: Map[Int, Rule])(
        input: String
    ): List[String] = rule match {
      case Literal(ch) =>
        if (input.startsWith(ch.toString())) List(input.drop(1)) else List.empty
      case Refs(refRules) => refRules.flatMap(refs =>
          refs.foldLeft(List(input)) { case (acc, ruleNum) =>
            acc.flatMap(str => parse(ruleSet.get(ruleNum).get, ruleSet)(str))
          }
        )
    }
  }

  case class Input(rules: Map[Int, Rule], messages: List[String])

  def parseInput(input: List[String]) = {
    val (rules, messages) = input.span(!_.isBlank())
    val parsedRules       = rules.traverse(parseRule.parseAll)

    parsedRules
      .map(rules => Input(rules.toMap, messages.filterNot(_.isBlank())))
  }

  def answer1(input: List[String]) = parseInput(input).map {
    case Input(rules, messages) => {
      val rule = rules.get(0).get
      messages.filter(msg => Rule.parse(rule, rules)(msg).exists(_.isEmpty()))
        .length
    }
  }

  def convertInput(str: String) = str match {
    case "8: 42"     => "8: 42 | 42 8"
    case "11: 42 31" => "11: 42 31 | 42 11 31"
    case _           => str
  }

  def answer2(input: List[String]) = parseInput(input.map(convertInput)).map {
    case Input(rules, messages) => {
      val rule = rules.get(0).get
      messages.filter(msg => Rule.parse(rule, rules)(msg).exists(_.isEmpty()))
        .length
    }
  }

  override def run: IO[Unit] = {
    for {
      input <- IO(Source.fromResource("input-day19.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
  }
}
