package cinema

import cinema.Row.*
import cinema.SeatAllocationStrategy.*

object SeatAllocationStrategy {

  def refersToASeat(refPoint: Float): Boolean =
    refPoint - refPoint.intValue() == 0.0

  case class SingleBlockAllocationResult(
      allocatedSeatBlock: SeatBlock,
      numberOfSeatsToAllocate: Int
  )

  case class SingleRowAllocationResult(
      allocatedSeatBlocks: SeatBlocks,
      numberOfSeatsToAllocate: Int
  )

  case class AllocatedSeatBlocks(rowId: Int, seatBlocks: SeatBlocks)

  case class AllocationResult(
      allocatedSeats: Seq[AllocatedSeatBlocks],
      updatedSeatingMap: SeatingMap
  )
}

trait SeatAllocationStrategy {
  def allocateSeats(
      seatingMap: SeatingMap,
      numberOfSeatsRequested: Int
  ): AllocationResult

  def allocateSeats(
      seatingMap: SeatingMap,
      numberOfSeatsRequested: Int,
      startingRowId: Int,
      startingColId: Int
  ): AllocationResult
}
