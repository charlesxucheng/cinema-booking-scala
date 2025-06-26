package cinema

import cinema.MovieDurations.MovieDuration

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

object CinemaHall {
  val MIN_INTERMISSION_DURATION: MovieDuration = MovieDuration(30.minutes)
  val DEFAULT_NAME: String = "default"

  def apply(name: String, seatingMap: SeatingMap): CinemaHall =
    CinemaHall(name, MIN_INTERMISSION_DURATION, seatingMap)
  def apply(seatingMap: SeatingMap): CinemaHall =
    CinemaHall(DEFAULT_NAME, MIN_INTERMISSION_DURATION, seatingMap)
}

case class CinemaHall private (
    name: String,
    minimumIntermission: MovieDuration,
    seatingMap: SeatingMap
) {
  require(
    minimumIntermission.toDuration > 0.minutes,
    s"Minimum intermission must be greater than zero: $minimumIntermission"
  )

  def capacity: Int = seatingMap.capacity
}
