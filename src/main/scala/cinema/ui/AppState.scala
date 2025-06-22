package cinema.ui

import cinema.ui.base.Empty
import cinema.{CinemaHall, Movie, Screening, SeatingMap}

import java.time.LocalTime

case class AppState private (
    movie: Option[Movie],
    cinemaHall: Option[CinemaHall],
    screenings: Map[Int, Screening],
    selectedShowTimeId: Option[Int],
    selectedNumberOfTickets: Option[Int]
) {

  require(
    selectedShowTimeId.isEmpty || screenings.contains(selectedShowTimeId.get),
    s"Invalid showtime id $selectedShowTimeId"
  )

  def setMovie(movie: Movie): AppState =
    this.copy(movie = Some(movie)).setDefaultScreenings()

  private def setDefaultScreenings(): AppState =
    this.copy(screenings = defaultScreenings)

  private def defaultScreenings: Map[Int, Screening] = (for {
    movie <- movie
    cinemaHall <- cinemaHall
  } yield {
    movie.showTimes.zipWithIndex
      .map { case (showTime, index) =>
        index -> Screening(movie, cinemaHall.duplicate, index)
      }
  }).getOrElse(Seq.empty).toMap

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

  def setShowTimeAndNumberOfTickets(
      showtimeId: Int,
      numberOfTickets: Int
  ): AppState = this.copy(
    selectedShowTimeId = Some(showtimeId),
    selectedNumberOfTickets = Some(numberOfTickets)
  )
  
  def getCinemaHallSeatingMap: Option[SeatingMap] = cinemaHall.map(_.seatingMap)
  
  def getSeatingMapForSelectedShowTime: Option[SeatingMap] =
    selectedShowTimeId.flatMap(screenings.get).map(_.cinemaHall.seatingMap)
    
  def selectedShowTime: Option[LocalTime] =
    selectedShowTimeId.flatMap(screenings.get).map(_.showTime)
}

object AppState {
  def empty: AppState = new AppState(None, None, Map.empty, None, None)

  given Empty[AppState] with
    def empty: AppState = AppState.empty

}
