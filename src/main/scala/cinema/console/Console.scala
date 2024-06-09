package cinema.console

import cats.effect.IO
import cats.effect.std.Console as CatsConsole

trait Console[F[_]] {
  def println(message: String): F[Unit]
  def readLine: F[String]
}

case class SystemConsole() extends Console[IO] {
  private val systemConsole: CatsConsole[IO] = CatsConsole.make[IO] 

  override def println(line: String): IO[Unit] = systemConsole.println(line)

  override def readLine: IO[String] = systemConsole.readLine
}

  