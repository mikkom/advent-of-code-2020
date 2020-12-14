package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P, Parser1}
import ParseUtils._

object Day14 extends IOApp.Simple {

  sealed trait Op
  case class SetMask(mask: String)              extends Op
  case class Assign(address: Long, value: Long) extends Op

  val parseOp: Parser1[Op] = {
    val setMask = P.string1("mask = ") *> P.rep(P.anyChar).map(_.mkString)
      .map(SetMask)

    val assign = ((P.string1("mem[") *> long) ~ (P.string1("] = ") *> long))
      .map(Assign.tupled)

    setMask.orElse1(assign)
  }

  def parseInput(input: List[String]) = input.filterNot(_.isBlank())
    .traverse(parseOp.parseAll)

  def parseBinary(str: String) = str.foldLeft(0L) { case (acc, x) =>
    val bit = x match {
      case '0' => 0
      case '1' => 1
      case _   => throw new IllegalArgumentException(s"invalid bit value $x")
    }
    2 * acc + bit
  }

  def sumOfMemoryContents(input: List[Op]): Long = {
    case class ProgramState(
        andMask: Long,
        orMask: Long,
        memory: Map[Long, Long]
    )

    val state = input.foldLeft(ProgramState(Long.MaxValue, 0L, Map.empty)) {
      case (state, SetMask(maskStr)) =>
        val andMask = maskStr.replaceAll("X", "1")
        val orMask  = maskStr.replaceAll("X", "0")
        state.copy(andMask = parseBinary(andMask), orMask = parseBinary(orMask))
      case (
            state @ ProgramState(andMask, orMask, memory),
            Assign(address, value)
          ) =>
        val maskedValue = (value & andMask) | orMask
        state.copy(memory = memory.updated(address, maskedValue))
    }
    state.memory.values.sum
  }

  def getMasks(str: String): List[(String, String)] = str
    .foldLeft(List(("", ""))) { case (acc, ch) =>
      val nexts = ch match {
        case '0' => List(("1", "0"))
        case '1' => List(("1", "1"))
        case 'X' => List(("0", "0"), ("1", "1"))
      }
      acc.flatMap { case (andMask, orMask) =>
        nexts.map { case (andSuffix, orSuffix) =>
          (andMask + andSuffix, orMask + orSuffix)
        }
      }
    }

  def sumOfMemoryContentsV2(input: List[Op]): Long = {
    case class ProgramState(masks: List[(Long, Long)], memory: Map[Long, Long])

    val state = input
      .foldLeft(ProgramState(List((Long.MaxValue, 0L)), Map.empty)) {
        case (state, SetMask(maskStr)) =>
          val masks = getMasks(maskStr).map { case (andMask, orMask) =>
            (parseBinary(andMask), parseBinary(orMask))
          }
          state.copy(masks = masks)
        case (state @ ProgramState(masks, memory), Assign(address, value)) =>
          val updatedMemory = masks
            .foldLeft(memory) { case (memory, (andMask, orMask)) =>
              val maskedAddress = (address & andMask) | orMask
              memory.updated(maskedAddress, value)
            }
          state.copy(memory = updatedMemory)
      }
    state.memory.values.sum
  }

  def answer1(input: List[String]) = parseInput(input).map(sumOfMemoryContents)

  def answer2(input: List[String]) = parseInput(input)
    .map(sumOfMemoryContentsV2)

  override def run: IO[Unit] = for {
    input <- IO(Source.fromResource("input-day14.txt").getLines().toList)
    _     <- IO(println(answer1(input)))
    _     <- IO(println(answer2(input)))
  } yield ()
}
