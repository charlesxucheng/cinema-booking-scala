package cinema.ui

import cinema.ui.base.Empty
import cinema.{Movie, MovieTheatre}

case class MovieSchedule(movie: Movie, theatre: MovieTheatre)

case class AppState(movie: Option[Movie], theatre: Option[MovieTheatre])

object AppState {
  def empty: AppState = new AppState(None, None)

  given Empty[AppState] with
    def empty: AppState = AppState.empty

}
