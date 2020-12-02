package mikkom

import cats.parse.{Parser => P, Numbers}

object ParseUtils {

  val int = Numbers
    .signedIntString
    .flatMap(s =>
      s.toIntOption
        .fold[P[Int]](P.failWith(s"Could not parse $s as an integer"))(
          P.pure(_)
        )
    )

  val intList = P.repSep(int, 0, P.char(','))

  val rangeParser =
    ParseUtils.int ~
      (P.char('-') *> ParseUtils.int) map { case (a, b) =>
        a to b
      }

}
