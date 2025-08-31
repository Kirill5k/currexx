package currexx.backtest

import cats.effect.{IO, IOApp}
import io.circe.{Encoder, Printer}
import io.circe.syntax.*

object JsonPrinter extends IOApp.Simple {
  def pprint[A: Encoder](obj: A): String =
    obj.asJson.printWith(Printer.spaces2)
  
  override def run: IO[Unit] =
    IO.println(pprint(TestStrategy.s1_rules))
}