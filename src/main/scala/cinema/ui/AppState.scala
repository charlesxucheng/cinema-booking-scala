package cinema.ui

import cinema.ui.base.Empty
import cinema.{Movie, CinemaHall}

case class MovieSchedule(movie: Movie, theatre: CinemaHall)

case class AppState(movie: Option[Movie], theatre: Option[CinemaHall])

object AppState {
  def empty: AppState = new AppState(None, None)

  given Empty[AppState] with
    def empty: AppState = AppState.empty

}
