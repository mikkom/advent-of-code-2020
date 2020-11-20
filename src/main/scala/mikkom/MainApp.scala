package mikkom

import cats.effect.{IO, IOApp}

object Example extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      _ <- IO.println("blast off!")
    } yield ()
}
