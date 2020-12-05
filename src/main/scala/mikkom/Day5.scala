package mikkom.day5

import scala.io.Source
import cats.effect.{IO, IOApp}

def parseBinary(input: String, zeroChar: Char) =
  def getBit(ch: Char) = if ch == zeroChar then 0 else 1
  input.foldLeft(0) { (acc, ch) =>
    2 * acc + getBit(ch)
  }

def parseSeatId(input: String) =
  val row = parseBinary(input.take(7), 'F')
  val col = parseBinary(input.takeRight(3), 'L')
  8 * row + col

def answer1(input: List[String]) = input.map(parseSeatId).max

def answer2(input: List[String]) =
  input
    .map(parseSeatId)
    .sorted
    .sliding(2)
    .find {
      case List(fst, snd) =>
        snd - fst == 2
      case _ =>
        false
    }
    .map(_.sum / 2)

object Day5 extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      input <- IO(Source.fromResource("input-day5.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    yield ()
