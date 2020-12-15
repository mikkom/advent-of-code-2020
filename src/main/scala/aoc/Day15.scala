package aoc

import cats.syntax.all._
import cats.effect.{IO, IOApp}

object Day15 extends IOApp.Simple {

  def parseInput(input: String) = input.split(',').toList
    .traverse(_.toIntOption)

  def findNthNumber(n: Int)(input: List[Int]) = {
    case class State(turn: Int, prevNum: Int, lastSeen: Map[Int, Int])

    val turns = LazyList
      .unfold(State(input.length - 1, input.last, input.zipWithIndex.toMap)) {
        case State(turn, prevNum, lastSeen) =>
          val spokenNum = lastSeen.get(prevNum).map(turn - _).getOrElse(0)
          val state =
            State(turn + 1, spokenNum, lastSeen.updated(prevNum, turn))

          Some((spokenNum, state))
      }

    turns.drop(n - input.length - 1).head
  }

  def answer1(input: String) = parseInput(input).map(findNthNumber(2020))

  def answer2(input: String) = parseInput(input).map(findNthNumber(30000000))

  override def run: IO[Unit] = {
    val input = "1,12,0,20,8,16"
    for {
      _ <- IO(println(answer1(input)))
      _ <- IO(println(answer2(input)))
    } yield ()
  }
}
