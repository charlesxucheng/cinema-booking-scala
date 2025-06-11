package cinema

import cinema.MovieDurations.MovieDuration
import cinema.MovieDurations.MovieDuration.*
import cinema.TestFixtures.{ONE_HOUR, TWO_HOURS}
import org.scalatest.matchers.should.Matchers.shouldBe

import java.time.LocalTime
import scala.concurrent.duration.{Duration, DurationInt}

class MovieTest extends UnitSpec {

  "A Movie" should {
    "not allow empty titles" in {
      an[IllegalArgumentException] should be thrownBy Movie(
        "",
        ONE_HOUR,
        LocalTime.now()
      )
      an[IllegalArgumentException] should be thrownBy Movie(
        " ",
        ONE_HOUR,
        LocalTime.now()
      )
    }

    s"not allow a duration that is shorter than $minDuration" in {
      val testData = Table(
        "Duration",
        Duration.Zero,
        minDuration.toDuration - 1.seconds,
        minDuration.toDuration - 2.seconds
      )
      forAll(testData) { (duration: Duration) =>
        an[IllegalArgumentException] should be thrownBy {
          Movie(
            "Aladdin",
            MovieDuration(duration),
            LocalTime.now()
          )
        }
      }
    }

    s"not allow a duration that is longer than the $maxDuration" in {
      an[IllegalArgumentException] should be thrownBy Movie(
        "Charlie's Angles",
        maxDuration + 1.seconds,
        LocalTime.now()
      )
    }

    "allow movies with numbers in the title" in {
      Movie("Terminator 2", ONE_HOUR, LocalTime.now()) shouldBe a[
        Movie
      ]
      Movie(
        "Blade Runner 2049",
        ONE_HOUR,
        LocalTime.now()
      ) shouldBe a[Movie]
    }

    s"allow a duration that is within $minDuration and $maxDuration (inclusive)" in {
      Movie("Aladdin", minDuration, LocalTime.now()) shouldBe a[Movie]
      Movie("Bad Boys", maxDuration, LocalTime.now()) shouldBe a[Movie]
      Movie(
        "Charlie's Angels",
        TWO_HOURS,
        LocalTime.now()
      ) shouldBe a[Movie]
    }

    "allow one show time only" in {
      Movie("Aladdin", TWO_HOURS, LocalTime.of(23, 0)) shouldBe a[
        Movie
      ]
      Movie(
        "Beauty and the Beast",
        maxDuration,
        LocalTime.of(9, 30)
      ) shouldBe a[Movie]
      Movie(
        "Top Gun",
        MovieDuration(159.minutes),
        LocalTime.of(0, 0)
      ) shouldBe a[Movie]
      Movie("ABC", TWO_HOURS, LocalTime.of(9, 30)) shouldBe a[
        Movie
      ]
    }

    "not allow empty show times" in {
      an[IllegalArgumentException] should be thrownBy Movie.create(
        "Aladdin",
        ONE_HOUR,
        Seq.empty
      )
    }

    s"allow show times that have at least ${CinemaHall.MIN_INTERMISSION_DURATION} in between" in {
      val testData = Table(
        ("Duration", "ShowTimes"),
        (210.minutes, Seq(LocalTime.of(10, 0), LocalTime.of(14, 0))),
        (
          150.minutes,
          Seq(LocalTime.of(10, 35), LocalTime.of(16, 35), LocalTime.of(20, 35))
        ),
        (
          210.minutes,
          Seq(
            LocalTime.of(18, 0),
            LocalTime.of(22, 0),
            LocalTime.of(2, 0),
            LocalTime.of(6, 0)
          )
        ),
        (
          10.minutes,
          Seq(LocalTime.of(1, 0), LocalTime.of(1, 50), LocalTime.of(2, 40))
        ),
        (2.minutes, Seq(LocalTime.of(23, 55), LocalTime.of(0, 30)))
      )
      forAll(testData) { (duration: Duration, showTimes: Seq[LocalTime]) =>
        Movie.create(
          "Goodfellas",
          MovieDuration(duration),
          showTimes
        ) shouldBe a[Movie]
      }
    }

    s"not allow show times that are too close to each other (< ${CinemaHall.MIN_INTERMISSION_DURATION} )" in {
      val testData = Table(
        ("Duration", "ShowTimes"),
        (200.minutes, Seq(LocalTime.of(10, 0), LocalTime.of(12, 0))),
        (
          150.minutes,
          Seq(LocalTime.of(15, 35), LocalTime.of(16, 35), LocalTime.of(20, 35))
        ),
        (
          10.minutes,
          Seq(LocalTime.of(1, 0), LocalTime.of(1, 30), LocalTime.of(2, 0))
        ),
        (
          240.minutes,
          Seq(LocalTime.of(23, 5), LocalTime.of(2, 0), LocalTime.of(10, 10))
        )
      )
      forAll(testData) { (duration: Duration, showTimes: Seq[LocalTime]) =>
        an[IllegalArgumentException] should be thrownBy Movie.create(
          "Goodfellas",
          MovieDuration(duration),
          showTimes
        )
      }
    }
  }

}
