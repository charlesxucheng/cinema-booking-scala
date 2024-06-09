package cinema


import cats.effect.IO
import cinema.console.Console

class TestConsole() extends Console[IO] {
  private var inputs: Seq[String] = Seq.empty
  private var outputs: Seq[String] = Seq.empty

  override def println(line: String): IO[Unit] = IO {
    outputs = outputs :+ line
  }

  override def readLine: IO[String] = IO {
    val head = inputs.head
    inputs = inputs.tail
    head
  }
}
