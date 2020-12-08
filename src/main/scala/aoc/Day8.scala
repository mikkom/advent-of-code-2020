package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P}
import ParseUtils._
import cats.parse.Parser1
import scala.annotation.tailrec

object Day8 extends IOApp.Simple {
  type LineIndex = Int

  case class ConsoleState(ip: LineIndex, acc: Int, codeModified: Boolean) {
    def jump(offset: Int) = this.copy(ip = ip + offset)
    def next              = this.jump(1)
    def inc(value: Int)   = this.copy(acc = acc + value)
    def modifyCode        = this.copy(codeModified = true)
  }

  object ConsoleState {
    val init = ConsoleState(0, 0, false)
  }

  sealed trait Op
  case class Acc(increment: Int) extends Op
  case class Jmp(offset: Int)    extends Op
  case class Nop(arg: Int)       extends Op

  val parseOp: Parser1[Op] = {
    def parse(op: String) = P.string1(op) ~ P.char(' ') *> intPlus

    P.oneOf1(
      List(parse("nop").map(Nop), parse("acc").map(Acc), parse("jmp").map(Jmp))
    )
  }

  def parseInstructions(input: List[String]) = input.traverse(parseOp.parseAll)

  def findInfiniteLoop(program: Array[Op]) = {
    @tailrec
    def loop(state: ConsoleState, seenIps: Set[LineIndex]): Option[Int] =
      if (!(0 until program.length).contains(state.ip))
        None
      else if (seenIps.contains(state.ip))
        Some(state.acc)
      else
        program(state.ip) match {
          case Nop(_) =>
            loop(state.next, seenIps + state.ip)
          case Acc(increment) =>
            loop(state.inc(increment).next, seenIps + state.ip)
          case Jmp(offset) =>
            loop(state.jump(offset), seenIps + state.ip)
        }

    loop(ConsoleState.init, Set.empty[LineIndex])
  }

  def fixCorruptBootCode(program: Array[Op]) = {
    def loop(state: ConsoleState, seenIps: Set[LineIndex]): Option[Int] =
      if (state.ip == program.length)
        Some(state.acc)
      else if (!(0 until program.length).contains(state.ip))
        None
      else if (seenIps.contains(state.ip))
        None
      else
        (program(state.ip), state.codeModified) match {
          case (Acc(increment), _) =>
            loop(state.inc(increment).next, seenIps + state.ip)
          case (Nop(_), true) =>
            loop(state.next, seenIps + state.ip)
          case (Nop(arg), false) =>
            loop(state.next, seenIps + state.ip) <+>
              loop(state.modifyCode.jump(arg), seenIps + state.ip)
          case (Jmp(offset), true) =>
            loop(state.jump(offset), seenIps + state.ip)
          case (Jmp(offset), false) =>
            loop(state.jump(offset), seenIps + state.ip) <+>
              loop(state.modifyCode.next, seenIps + state.ip)
        }

    loop(ConsoleState.init, Set.empty[LineIndex])
  }

  def answer1(input: List[String]) =
    parseInstructions(input).map(ops => findInfiniteLoop(ops.toArray))

  def answer2(input: List[String]) =
    parseInstructions(input).map(ops => fixCorruptBootCode(ops.toArray))

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day8.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
