package cinema

import cinema.Movie.defaultDate
import cinema.MovieDurations.MovieDuration

import java.time.temporal.{ChronoUnit, TemporalAmount}
import java.time.{LocalDate, LocalTime, Duration as JDuration}
import scala.concurrent.duration.{Duration, DurationInt}

object MovieDurations {
  opaque type MovieDuration = Duration

  object MovieDuration {
    val minDuration: MovieDuration = 1.minutes
    val maxDuration: MovieDuration = 23.hours

    def apply(d: Duration): MovieDuration = {
      require(
        isValid(d),
        s"Duration $d must be within $minDuration and $maxDuration (both inclusive)."
      )
      d
    }

    private def isValid(duration: Duration) =
      duration.gteq(minDuration.toDuration) && duration.lteq(
        maxDuration.toDuration
      )

    extension (md: MovieDuration)
      def toJavaTemporalAmount: TemporalAmount = JDuration.ofNanos(md.toNanos)
      def toDuration: Duration = md
      def -(other: Duration): MovieDuration = MovieDuration(md.minus(other))
      def +(other: Duration): MovieDuration = MovieDuration(md.plus(other))
  }
}

object Movie {
  private val defaultDate: LocalDate = LocalDate.of(1977, 1, 1)

  def apply(title: String, duration: MovieDuration, showTime: LocalTime) =
    new Movie(title, duration, Seq(showTime))

  def create(
      title: String,
      duration: MovieDuration,
      showTimes: Seq[LocalTime]
  ): Movie = {
    new Movie(title, duration, showTimes.sorted)
  }
}

case class Movie private (
    title: String,
    duration: MovieDuration,
    showTimes: Seq[LocalTime]
) {
  require(title.trim.nonEmpty, "Movie title must not be empty")
  require(
    showTimesValid(),
    s"Show times $showTimes must not be empty and there must be at least ${CinemaHall.MIN_INTERMISSION_DURATION} between two shows"
  )

  private def showTimesValid(): Boolean =
    if (showTimes.isEmpty) false
    else if (showTimes.length == 1) true
    else compareShowTimesWithEarliestAllowed(showTimes)

  // Pair up the earliest show time allowed for the next show, and the actual show time of the next show
  // The last show time allowed should be compared with the show time of the first show next day
  // No pair's first entry should be later than the second entry.
  private def compareShowTimesWithEarliestAllowed(
      showTimes: Seq[LocalTime]
  ): Boolean = {
    val sortedShowTimes = showTimes.sorted.map(defaultDate.atTime)

    val earliestStartTimesAllowed = sortedShowTimes
      .map(
        _.plus(duration.toJavaTemporalAmount).plus(
          CinemaHall.MIN_INTERMISSION_DURATION.toJavaTemporalAmount
        )
      )

    val pairs = earliestStartTimesAllowed
      .zip(
        sortedShowTimes.tail :+ sortedShowTimes.head.plus(
          JDuration.of(24, ChronoUnit.HOURS)
        )
      )

    !pairs.exists(pair => pair._1.isAfter(pair._2))
  }
}
