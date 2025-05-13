package cinema.ui

import cats.Show
import cats.effect.IO
import cats.effect.std.Console
import cinema.ui.base.LS

import java.nio.charset.Charset

case class MockConsole(inputs: List[String]) extends Console[IO] {
  private val outputLines = scala.collection.mutable.Buffer[String]()
  private val errorLines = scala.collection.mutable.Buffer[String]()
  private var inputIndex = 0

  def getOutputLines: List[String] = outputLines.toList

  override def readLineWithCharset(charset: Charset): IO[String] = readLine

  override def readLine: IO[String] =
    if (inputIndex < inputs.length) {
      val input = inputs(inputIndex)
      inputIndex += 1
      IO.pure(input)
    } else IO.raiseError(new RuntimeException("No more inputs"))

  override def print[A](a: A)(implicit S: Show[A]): IO[Unit] =
    IO.pure(outputLines += S.show(a))

  override def println[A](a: A)(implicit S: Show[A]): IO[Unit] =
    IO.pure(outputLines += S.show(a) + LS)

  override def error[A](a: A)(implicit S: Show[A]): IO[Unit] =
    IO.pure(errorLines += S.show(a))

  override def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] =
    IO.pure(errorLines += S.show(a) + LS)
}
