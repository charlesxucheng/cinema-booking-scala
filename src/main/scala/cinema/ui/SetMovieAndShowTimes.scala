package cinema.ui

import cats.data.State
import cinema.Movie
import cinema.ui.base.{LS, UserInteraction}
import cinema.ui.base.UserInteraction.Result

import java.time.LocalTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.concurrent.duration.DurationInt

case object SetMovieAndShowTimes extends UserInteraction[AppState] {
  override def getPrompt: String =
    "Set movie in [Title] [Duration] [Showtime.1(hh:mm)] ... [Showtime.n] format, or press Enter to return to main menu:"

  override def handleInput(
      input: String
  ): State[AppState, UserInteraction.Result[AppState]] = State { currentState =>
    input.trim match {
      case "" => (currentState, Result("", MainMenu))
      case _ =>
        parseInput(input) match {
          case Right(movie) =>
            val updatedState = currentState.copy(movie = Some(movie))
            (
              updatedState,
              Result(
                s"Movie (${movie.title}, ${movie.duration}, show times: ${movie.showTimes.mkString(", ")}) has been set.$LS",
                MainMenu
              )
            )
          case Left(errorMessage) =>
            (
              currentState,
              Result(
                s"""Invalid input format. Please use: [Title] [DurationInMinutes] [HH:MM] [HH:MM] ...
                    |Specific error: $errorMessage
                    |""".stripMargin,
                this
              )
            )
        }
    }
  }

  private def parseInput(input: String): Either[String, Movie] = {
    val tokens = input.trim.split("\\s+")

    val durationIndex = tokens.lastIndexWhere(_.matches("\\d+"))

    if (durationIndex < 1) Left("Movie duration is missing from the input.")
    else
      try {
        val movieTitle = tokens.take(durationIndex).mkString(" ").trim
        val movieDuration = tokens(durationIndex).toInt.minutes
        val timeFormatter = DateTimeFormatter.ofPattern("HH:mm")

        val showTimes = tokens
          .drop(durationIndex + 1)
          .flatMap { timeStr =>
            try {
              Some(LocalTime.parse(timeStr, timeFormatter))
            } catch {
              case _: DateTimeParseException => None
            }
          }
          .toList

        Right(Movie.create(movieTitle, movieDuration, showTimes))
      } catch {
        case e: IllegalArgumentException => Left(e.getMessage)
      }
  }
}
