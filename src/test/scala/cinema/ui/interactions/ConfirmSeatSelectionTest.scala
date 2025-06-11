package cinema.ui.interactions

import cinema.MovieDurations.MovieDuration
import cinema.TestFixtures.TWO_HOURS
import cinema.ui.AppState
import cinema.{CinemaHall, Movie, RectangularSeatingMap, UnitSpec}
import org.scalatest.matchers.should.Matchers.*

import java.time.LocalTime
import scala.concurrent.duration.DurationInt

class ConfirmSeatSelectionTest extends UnitSpec {
  "ConfirmSeatSelection user interaction" should {
    "display the show time and number of tickets selected" in {
      val state = AppState.empty
        .setMovie(
          Movie("TestMovie", TWO_HOURS, LocalTime.of(14, 30))
        )
        .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))
        .setShowTimeAndNumberOfTickets(0, 4)
      val prompt = ConfirmSeatSelection.getPrompt(state)
      prompt should include (s"You chose ${state.selectedNumberOfTickets.get} tickets for the ${state.screenings(state.selectedShowTimeId.get)} showtime.")
    }
  }

}
