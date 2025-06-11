package cinema

import cinema.MovieDurations.MovieDuration
import scala.concurrent.duration.DurationInt

object TestFixtures {
  val ONE_HOUR: MovieDuration = MovieDuration(1.hours)
  val TWO_HOURS: MovieDuration = MovieDuration(2.hours)
}
