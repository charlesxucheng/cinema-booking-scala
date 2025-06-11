//package cinema.ui.interactions
//
//import cinema.UnitSpec
//
//class ConfirmSeatSelectionTest extends UnitSpec {
//  "ConfirmSeatSelection user interaction" should {
//    "display the show time and number of tickets selected" in {
//      val state = AppState.empty
//        .setMovie(
//          Movie("TestMovie", MovieDuration(120.minutes), LocalTime.of(14, 30))
//        )
//        .setCinemaHall(CinemaHall(RectangularSeatingMap(10, 10)))
//      val prompt = ConfirmSeatSelection.getPrompt
//      prompt shouldBe "Confirm seat selection by pressing Enter, or press Enter to return to main menu:"
//    }
//  }
//
//}
