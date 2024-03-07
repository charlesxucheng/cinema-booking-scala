package cinema

import cinema.Row.*
import org.apache.commons.lang3.Range
import SeatAllocationStrategy.*

case class AllocatedSeatBlocks(rowId: Int, seatBlocks: SeatBlocks)

object SeatAllocationStrategy {
   type AllocationResult = Seq[AllocatedSeatBlocks]
}

trait SeatAllocationStrategy {
  def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int): AllocationResult
//  def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, startingPosition: (Int, Int)): AllocationResult
}

object DefaultSeatAllocationStrategy extends SeatAllocationStrategy {
  override def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int): AllocationResult = {
    Seq(AllocatedSeatBlocks(10, SeatBlocks(Seq(Range.of(10, 11)))))
  }

//  override def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, startingPosition: (Int, Int)): AllocationResult = ???

//  private def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, allocatedSeats: Seq)

}
