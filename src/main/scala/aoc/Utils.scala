package aoc

import cats.syntax.all._

object Utils {

  def iterate[A](init: A, f: A => A): LazyList[A] = LazyList
    .unfold(init)(s => (s, f(s)).some)

}
