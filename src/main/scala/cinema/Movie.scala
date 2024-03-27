package cinema

import cinema.MovieDurations.MovieDuration

import java.time.temporal.{ChronoUnit, TemporalAmount}
import java.time.{LocalDate, LocalDateTime, LocalTime, Duration as JDuration}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, DurationInt}

object MovieDurations {
  opaque type MovieDuration = Duration

  object MovieDuration {
    val minDuration: Duration = 1.minutes
    val maxDuration: Duration = 23.hours

    private def isValid(duration: Duration) = duration.gteq(minDuration) && duration.lteq(maxDuration)

    def apply(d: Duration): MovieDuration = {
      require(isValid(d))
      d
    }

    extension (md: MovieDuration)
      def toDuration: Duration = md
      def toJavaTemporalAmount: TemporalAmount = JDuration.ofNanos(md.toDuration.toNanos)

    given durationToMovieDuration: Conversion[Duration, MovieDuration] with
      def apply(d: Duration): MovieDuration = MovieDuration(d)
  }
}

object Movie {
  private val defaultDate: LocalDate = LocalDate.of(1977, 1, 1)

  def apply(title: String, duration: MovieDuration, showTime: LocalTime) =
    new Movie(title, duration, Seq(defaultDate.atTime(showTime)))

  def create(title: String, duration: MovieDuration, showTimes: Seq[LocalTime]): Movie = {
    new Movie(title, duration, showTimes.sorted.map(defaultDate.atTime))
  }
}

case class Movie private (title: String, duration: MovieDuration, showTimes: Seq[LocalDateTime]) {
  require(title.trim.nonEmpty && showTimesValid())

  private def showTimesValid(): Boolean =
    if (showTimes.isEmpty) false
    else if (showTimes.length == 1) true
    else compareShowTimesWithEarliestAllowed(showTimes)

  // Pair up the earliest show time allowed for the next show, and the actual show time of the next show
  // The last show time allowed should be compared with the show time of the first show next day
  // No pair's first entry should be later than the second entry.
  private def compareShowTimesWithEarliestAllowed(showTimes: Seq[LocalDateTime]): Boolean = {
    val sortedShowTimes = showTimes.sorted

    val earliestStartTimesAllowed = sortedShowTimes
      .map(_.plus(duration.toJavaTemporalAmount).plus(MovieTheatre.minIntermission.toJavaTemporalAmount))

    val pairs = earliestStartTimesAllowed
      .zip(sortedShowTimes.tail :+ sortedShowTimes.head.plus(JDuration.of(24, ChronoUnit.HOURS)))
    !pairs.exists(pair => pair._1.isAfter(pair._2))
  }
}

