package cinema

import cinema.MovieDurations.MovieDuration

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

object MovieTheatre {
  val minIntermission: MovieDuration = 30.minutes
}

case class MovieTheatre(seatingPlan: SeatingMap) {
  def capacity: Int = seatingPlan.capacity
}
