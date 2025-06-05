package cinema

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cinema.ui.interactions.MainMenu
import cinema.ui.{AppState, CinemaConsoleUI}

object MainApp extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val console = CinemaConsoleUI[AppState](Console[IO])
    console.start(MainMenu)
  }
}
