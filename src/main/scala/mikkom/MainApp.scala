package mikkom

import java.util.concurrent.TimeoutException

import cats.effect.{IO, IOApp}
import cats.effect.kernel.Deferred
import cats.syntax.all._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration._

object Example extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      _ <- IO.println("blast off!")
    } yield ()
}
