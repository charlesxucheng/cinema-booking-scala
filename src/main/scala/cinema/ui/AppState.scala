package cinema.ui

import cinema.ui.base.Empty
import cinema.{CinemaHall, Movie}

case class AppState(movie: Option[Movie], cinemaHall: Option[CinemaHall])

object AppState {
  def empty: AppState = new AppState(None, None)

  given Empty[AppState] with
    def empty: AppState = AppState.empty

}
