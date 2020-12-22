package aoc

import scala.io.Source
import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P}
import ParseUtils._
import Utils._

object Day22 extends IOApp.Simple {
  case class State(deck1: Vector[Int], deck2: Vector[Int])

  val parseInput = {
    def deck(player: Int) = P.string1(s"Player $player:\n") *>
      P.repSep(int, 1, P.char('\n')).map(_.toVector)

    val decks = (deck(1) <* P.string1("\n\n")) ~ deck(2)
    decks.map(State.tupled)
  }

  def playCombatRound(state: State) = state match {
    case State(c1 +: cs1, c2 +: cs2) =>
      if (c1 > c2) State(cs1 :+ c1 :+ c2, cs2) else State(cs1, cs2 :+ c2 :+ c1)
    case _ => state
  }

  def deckScore(deck: Vector[Int]) = deck.reverse.zipWithIndex
    .map { case (card, idx) => (idx + 1) * card }.sum

  def combatScore(state: State) = {
    val states = iterate(state, playCombatRound)
    states.find { case State(deck1, deck2) => deck1.isEmpty || deck2.isEmpty }
      .map { case State(deck1, deck2) => deck1 ++ deck2 }.map(deckScore)
  }

  def playRecursiveCombatRound(state: State, prevStates: Set[State]): State =
    if (prevStates.contains(state)) state.copy(deck2 = Vector.empty)
    else state match {
      case State(c1 +: cs1, c2 +: cs2) =>
        if (c1 <= cs1.size && c2 <= cs2.size) {
          val subResult = recursiveCombat(State(cs1.take(c1), cs2.take(c2)))
          if (subResult.deck2.isEmpty) State(cs1 :+ c1 :+ c2, cs2)
          else State(cs1, cs2 :+ c2 :+ c1)
        } else playCombatRound(state)
      case _ => state
    }

  def recursiveCombat(state: State): State = {
    val states = LazyList
      .unfold((state, Set.empty[State])) { case (state, prevStates) =>
        val nextState = playRecursiveCombatRound(state, prevStates)
        (state, (nextState, prevStates + state)).some
      }
    states.find { case State(deck1, deck2) => deck1.isEmpty || deck2.isEmpty }
      .get
  }

  def recursiveCombatScore(state: State) = {
    val State(deck1, deck2) = recursiveCombat(state)
    deckScore(deck1 ++ deck2)
  }

  def answer1(input: List[String]) = parseInput.parseAll(input.mkString("\n"))
    .map(combatScore)

  def answer2(input: List[String]) = parseInput.parseAll(input.mkString("\n"))
    .map(recursiveCombatScore)

  override def run: IO[Unit] = {
    for {
      input <- IO(Source.fromResource("input-day22.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
  }
}
