package cinema.ui

import cats.effect.IO
import cats.effect.std.Console
import cinema.ui.base.ConsoleUI

case class CinemaConsoleUI[S <: AppState](console: Console[IO])
    extends ConsoleUI[S](console) {

  override def displayWelcomeMessage(): IO[Unit] = {
    console.println("Welcome to the Cinema Booking System!")

//    def start(): IO[ExitCode] = start(CreateMovie)
  }
}
