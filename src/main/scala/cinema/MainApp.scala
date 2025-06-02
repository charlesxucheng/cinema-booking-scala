package cinema

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cinema.ui.{AppState, CinemaConsoleUI, MainMenu}

object MainApp extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val console = CinemaConsoleUI[AppState](Console[IO])
    console.start(MainMenu)
  }
}
