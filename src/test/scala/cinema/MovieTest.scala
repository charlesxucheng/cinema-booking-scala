package cinema

import java.time.LocalTime
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import concurrent.duration.DurationInt
import MovieDurations.MovieDuration.*

class MovieTest extends UnitSpec {
  "A Movie" should {
    "not allow empty titles" in {
      an [IllegalArgumentException] should be thrownBy Movie("", 1.hours, LocalTime.now())
      an [IllegalArgumentException] should be thrownBy Movie(" ", 1.hours, LocalTime.now())

    }

    s"not allow a duration that is shorter than $minDuration" in {
      an [IllegalArgumentException] should be thrownBy Movie("Aladdin", Duration.Zero, LocalTime.now())
      an [IllegalArgumentException] should be thrownBy Movie("Bad Boys", minDuration - 1.seconds, LocalTime.now())
    }

    "not allow a duration that is longer than the maximum duration allowed" in {

    }
  }

}
