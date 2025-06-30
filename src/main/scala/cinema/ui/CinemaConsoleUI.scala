package cinema.ui

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cinema.ui.base.{ConsoleUI, UserInteraction}
import cinema.ui.interactions.MainMenu

case class CinemaConsoleUI[S <: AppState](console: Console[IO])
    extends ConsoleUI[AppState](console) {

  override def displayWelcomeMessage(): IO[Unit] =
    console.println("Welcome to the Cinema Booking System!")
}
