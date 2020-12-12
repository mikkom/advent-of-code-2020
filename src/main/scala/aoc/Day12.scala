package aoc

import scala.io.Source
import cats.{Endo, Monoid, MonoidK}
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P}
import ParseUtils._

object Day12 extends IOApp.Simple {

  type Command = (Char, Int)

  val parseCommand = P.anyChar ~ int

  case class Position(east: Int, north: Int)

  object Position {
    def rotateClockwise(pos: Position) = Position(pos.north, -pos.east)

    def rotateN(n: Int) = MonoidK[Endo].algebra[Position]
      .combineN(rotateClockwise, n)

    implicit val monoid: Monoid[Position] = new Monoid[Position] {

      override def combine(x: Position, y: Position): Position =
        Position(x.east + y.east, x.north + y.north)

      override def empty: Position = Position(0, 0)
    }

    val North = Position(0, 1)
    val South = Position(0, -1)
    val East  = Position(1, 0)
    val West  = Position(-1, 0)
  }

  def move(command: Char, distance: Int) = (command match {
    case 'N' => Position.North
    case 'S' => Position.South
    case 'E' => Position.East
    case 'W' => Position.West
    case _ =>
      throw new IllegalArgumentException(s"Unknown move command $command")
  }).combineN(distance)

  def getManhattanDistance(commands: List[Command]) = {
    case class Ship(pos: Position, dir: Int)

    val startPos = Ship(Position(0, 0), 0)
    val ship = commands
      .foldLeft(startPos) { case (ship @ Ship(pos, dir), (cmd, value)) =>
        cmd match {
          case 'N' | 'S' | 'E' | 'W' => ship
              .copy(pos = pos |+| move(cmd, value))
          case 'L' | 'R' =>
            assert(value % 90 == 0)
            val sgn = if (cmd == 'R') 1 else -1
            ship.copy(dir = (360 + dir + sgn * value) % 360)
          case 'F' =>
            val moveDir = dir match {
              case 0   => Position.East
              case 90  => Position.South
              case 180 => Position.West
              case 270 => Position.North
              case _ => throw new IllegalArgumentException(
                  s"Unsupported direction $dir"
                )
            }
            ship.copy(pos = pos |+| moveDir.combineN(value))

          case _ => throw new IllegalArgumentException(s"Unknown command $cmd")
        }
      }
    Math.abs(ship.pos.east) + Math.abs(ship.pos.north)
  }

  def getManhattanDistance2(commands: List[Command]) = {
    val shipPos     = Position(0, 0)
    val waypointPos = Position(10, 1)
    val finalPos = commands.foldLeft((shipPos, waypointPos)) {
      case (
            (ship @ Position(se, sn), waypoint @ Position(we, wn)),
            (cmd, value)
          ) => cmd match {
          case 'N' | 'S' | 'E' | 'W' => (ship, waypoint |+| move(cmd, value))
          case 'L' | 'R' =>
            assert(value % 90 == 0)
            val sgn = if (cmd == 'R') 1 else -1
            val deg = (360 + sgn * value) % 360
            (ship, Position.rotateN(deg / 90)(waypoint))
          case 'F' => (
              ship.copy(east = se + value * we, north = sn + value * wn),
              waypoint
            )
          case _ => throw new IllegalArgumentException(s"Unknown command $cmd")
        }
    }._1
    Math.abs(finalPos.east) + Math.abs(finalPos.north)
  }

  def answer1(input: List[String]) = input.traverse(parseCommand.parseAll)
    .map(getManhattanDistance)

  def answer2(input: List[String]) = input.traverse(parseCommand.parseAll)
    .map(getManhattanDistance2)

  override def run: IO[Unit] = for {
    input <- IO(Source.fromResource("input-day12.txt").getLines().toList)
    _     <- IO(println(answer1(input)))
    _     <- IO(println(answer2(input)))
  } yield ()
}
