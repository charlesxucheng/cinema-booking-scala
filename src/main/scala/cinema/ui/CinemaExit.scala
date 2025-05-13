package cinema.ui

import cinema.ui.base.Exit

case object CinemaExit extends Exit {
  override val exitMessage: String = "Thank you for using the Cinema Booking System!"
}
