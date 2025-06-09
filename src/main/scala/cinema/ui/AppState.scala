package cinema.ui

import cinema.ui.base.Empty
import cinema.{CinemaHall, Movie, Screening}

case class AppState private (
    movie: Option[Movie],
    cinemaHall: Option[CinemaHall],
    screenings: Map[Int, Screening] = Map.empty
) {

  private def defaultScreenings: Map[Int, Screening] = (for {
    movie <- movie
    cinemaHall <- cinemaHall
  } yield {
    movie.showTimes.zipWithIndex
      .map { case (showTime, index) =>
        index -> Screening(movie, cinemaHall.duplicate, index)
      }
  }).getOrElse(Seq.empty).toMap

  private def setDefaultScreenings(): AppState =
    this.copy(screenings = defaultScreenings)

  def setMovie(movie: Movie): AppState =
    this.copy(movie = Some(movie)).setDefaultScreenings()

  def setCinemaHall(cinemaHall: CinemaHall): AppState =
    this.copy(cinemaHall = Some(cinemaHall)).setDefaultScreenings()

  def updateScreening(
      showtimeId: Int,
      updatedScreening: Screening
  ): AppState = {
    val updatedScreenings =
      this.screenings.updated(showtimeId, updatedScreening)
    this.copy(screenings = updatedScreenings)
  }
}

object AppState {
  def empty: AppState = new AppState(None, None)

  given Empty[AppState] with
    def empty: AppState = AppState.empty

}
