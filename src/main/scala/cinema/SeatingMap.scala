package cinema

sealed trait SeatingMap:
  def capacity: Int
  def availableSeatCount: Int

case class RectangularSeatingMap(rows: Int, cols: Int) extends SeatingMap:
  override def capacity: Int = rows * cols

  override def availableSeatCount: Int = capacity
