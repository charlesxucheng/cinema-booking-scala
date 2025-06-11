package cinema

import cinema.MovieDurations.MovieDuration
import cinema.MovieDurations.MovieDuration.*
import org.scalatest.matchers.should.Matchers.shouldBe

import java.time.LocalTime
import scala.concurrent.duration.{Duration, DurationInt}

class ScreeningTest extends UnitSpec {

  "A Screening" when {
    "is created" should {
      "have a show time that matches one of the provided movie's show times" in {
        val testData = Table(
          ("Duration", "ShowTimes", "Index"),
          (100.minutes, Seq(LocalTime.of(10, 0)), 0),
          (110.minutes, Seq(LocalTime.of(10, 0), LocalTime.of(14, 0)), 0),
          (
            150.minutes,
            Seq(
              LocalTime.of(10, 35),
              LocalTime.of(16, 35),
              LocalTime.of(20, 35)
            ),
            1
          ),
          (
            10.minutes,
            Seq(LocalTime.of(10, 0), LocalTime.of(14, 0), LocalTime.of(20, 0)),
            2
          )
        )

        forAll(testData) { (duration, showTimes, index) =>
          val movie =
            Movie.create("TestMovie", MovieDuration(duration), showTimes)
          val screening =
            Screening(movie, CinemaHall(RectangularSeatingMap(10, 10)), index)
          screening.showTime shouldBe showTimes(index)
        }
      }
    }

    "given an invalid show time index" should {
      "throw an exception" in {
        val testData = Table(
          ("Duration", "ShowTimes", "Index"),
          (100.minutes, Seq(LocalTime.of(10, 0)), 1),
          (110.minutes, Seq(LocalTime.of(10, 0), LocalTime.of(14, 0)), 3),
          (
            150.minutes,
            Seq(
              LocalTime.of(10, 35)
            ),
            -1
          ),
          (
            10.minutes,
            Seq(LocalTime.of(10, 0), LocalTime.of(14, 0), LocalTime.of(20, 0)),
            Int.MinValue
          )
        )

        forAll(testData) { (duration, showTimes, index) =>
          val movie =
            Movie.create("TestMovie", MovieDuration(duration), showTimes)
          an[IllegalArgumentException] should be thrownBy Screening(
            movie,
            CinemaHall(RectangularSeatingMap(10, 10)),
            index
          )
        }
      }
    }
  }
}
