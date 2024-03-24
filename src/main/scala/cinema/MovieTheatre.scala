package cinema

import cinema.MovieDurations.MovieDuration

import scala.concurrent.duration.DurationInt

object MovieTheatre {
  val minIntermission: MovieDuration = 30.minutes
}

case class MovieTheatre(seatingPlan: SeatingMap) {
  def capacity: Int = seatingPlan.capacity
}
