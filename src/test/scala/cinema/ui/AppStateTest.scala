package cinema.ui

import cinema.*
import cinema.MovieDurations.MovieDuration
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers.{should, shouldBe}

import java.time.LocalTime
import scala.concurrent.duration.DurationInt

class AppStateTest extends UnitSpec {
  private val testMovie: Movie = Movie.create(
    "Avengers",
    MovieDuration(120.minutes),
    Seq(LocalTime.of(10, 0), LocalTime.of(14, 0))
  )
  private val testCinemaHall: CinemaHall = CinemaHall(
    RectangularSeatingMap(10, 10)
  )
  private val testState = AppState.empty
    .setMovie(
      testMovie
    )
    .setCinemaHall(testCinemaHall)

  "An empty AppState" should {
    "have no movie, cinema hall, screenings, selected showtime and selected number of tickets set" in {
      val emptyState = AppState.empty
      emptyState should have(
        Symbol("movie")(None),
        Symbol("cinemaHall")(None),
        Symbol("screenings")(Map.empty),
        Symbol("selectedShowTimeId")(None),
        Symbol("selectedNumberOfTickets")(None)
      )
    }
  }

  "AppState" when {
    "updated with a movie showtime Id when no screenings exist" should {
      "reject the operation with an error" in {
        an[IllegalArgumentException] should be thrownBy {
          AppState.empty.setShowTimeAndNumberOfTickets(0, 3)
        }
      }
    }

    "updated with a movie showtime Id that can be found in its screenings" should {
      "allow the operation" in {
        val appState = testState.setShowTimeAndNumberOfTickets(0, 3)
        appState.selectedShowTimeId.get shouldBe 0
        appState.selectedNumberOfTickets.get shouldBe 3
      }
    }

    "updated with a movie but no cinema hall has been defined" should {
      "not initialize the screenings" in {
        val appState = AppState.empty.setMovie(testMovie)
        appState.screenings.size shouldBe 0
      }
    }

    "updated with a cinema hall but no movie has been defined" should {
      "not initialize the screenings" in {
        val appState = AppState.empty.setCinemaHall(testCinemaHall)
        appState.screenings.size shouldBe 0
      }
    }

    "both movie and cinema hall have been defined" should {
      "initialize the screenings" in {
        val appState = testState
        appState.screenings.size shouldBe 2
        appState.screenings.map(_._2.showTime) shouldBe testMovie.showTimes
        Inspectors.forAll(appState.screenings.values) { screening =>
          screening.cinemaHall.seatingMap shouldBe testCinemaHall.seatingMap
        }
      }
    }

    "updated with a new movie" should {
      "regenerate the default screenings" in {
        val movie = Movie.create(
          "Batman",
          MovieDuration(150.minutes),
          Seq(LocalTime.of(10, 0), LocalTime.of(14, 0), LocalTime.of(19, 0))
        )
        val appState = testState.setMovie(movie)
        appState.movie.get shouldBe movie
        appState.screenings.size shouldBe 3
        appState.screenings.map(_._2.showTime) shouldBe movie.showTimes
        Inspectors.forAll(appState.screenings.values) { screening =>
          screening.cinemaHall.seatingMap shouldBe appState.getCinemaHallSeatingMap.get
        }
      }
    }

    "updated with a new cinema hall" should {
      "regenerate the default screenings" in {
        val cinemaHall = CinemaHall(RectangularSeatingMap(15, 20))
        val appState = testState.setCinemaHall(cinemaHall)
        appState.cinemaHall.get shouldBe cinemaHall
        appState.screenings.size shouldBe 2
        Inspectors.forAll(appState.screenings.values) { screening =>
          screening.cinemaHall.seatingMap shouldBe appState.getCinemaHallSeatingMap.get
        }
      }
    }

    "is updated with a new Screening" should {
      "capture the changes" in {
        val updatedSeatingMap = DefaultSeatAllocationStrategy
          .allocateSeats(testState.screenings(0).cinemaHall.seatingMap, 3)
          ._2

        val updatedScreening = testState
          .screenings(0)
          .copy(cinemaHall = CinemaHall(updatedSeatingMap))

        val appState = testState
          .setShowTimeAndNumberOfTickets(0, 3)
          .updateScreening(0, updatedScreening)

        appState.screenings(0) shouldBe updatedScreening
      }
    }
  }

}
