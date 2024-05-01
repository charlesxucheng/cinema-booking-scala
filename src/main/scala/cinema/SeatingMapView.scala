package cinema

object SeatingMapView {
  val longHeader = "S C R E E N"
  val shortHeader = "S"
}

case class SeatingMapViewContent(header: String, separator: String, seats: Seq[String])

trait SeatingMapView[A <: SeatingMap] {
  extension (a: A) def viewAsSingleString: String
  extension (a: A) def viewAsMultiPartContent: SeatingMapViewContent
}

