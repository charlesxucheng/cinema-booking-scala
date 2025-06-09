package cinema

import java.time.LocalTime

object Screening {
}

case class Screening (movie: Movie, cinemaHall: CinemaHall, showTimeIndex: Int) {
  require(showTimeIndex >= 0 && showTimeIndex < movie.showTimes.length)
  val showTime: LocalTime = movie.showTimes(showTimeIndex)
}
