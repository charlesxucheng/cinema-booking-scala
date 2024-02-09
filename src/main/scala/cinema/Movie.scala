package cinema

import cinema.MovieDurations.MovieDuration

import java.time.LocalTime
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import concurrent.duration.DurationInt

object MovieDurations:
  opaque type MovieDuration = Duration
  object MovieDuration:
    val minDuration: Duration = 1.minutes
    val maxDuration: Duration = 23.hours

    private def isValid(duration: Duration) = duration.gteq(minDuration) && duration.lteq(maxDuration)

    def apply(d: Duration): MovieDuration = {
      require(isValid(d))
      d
    }

    extension(md: MovieDuration)
      def toDuration: Duration = md

    given durationToMovieDuration: Conversion[Duration, MovieDuration] with
      def apply(d: Duration): MovieDuration = MovieDuration(d)

object Movie:

  def apply(title: String, duration: MovieDuration, showTime: LocalTime) =
    new Movie(title, duration, Seq(showTime))


case class Movie(title: String, duration: MovieDuration, showTimes: Seq[LocalTime] ):
  require(title.trim.nonEmpty)

