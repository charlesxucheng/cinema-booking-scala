package cinema

import cinema.Row.*
import cinema.SeatAllocationStrategy.*
import org.apache.commons.lang3.Range

import scala.+:
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class AllocatedSeatBlocks(rowId: Int, seatBlocks: SeatBlocks)

object SeatAllocationStrategy {
  type AllocationResult = Seq[AllocatedSeatBlocks]

  case class SingleBlockAllocationResult(allocatedSeatBlock: SeatBlock, remainingSeatBlocks: SeatBlocks, numberOfSeatsToAllocate: Int)
  case class SingleRowAllocationResult(allocatedSeatBlocks: SeatBlocks, remainingSeatBlocks: SeatBlocks, numberOfSeatsToAllocate: Int)
}

trait SeatAllocationStrategy {
  def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int): AllocationResult
  //  def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, startingPosition: (Int, Int)): AllocationResult
}

object DefaultSeatAllocationStrategy extends SeatAllocationStrategy {

  private enum PositionToRefPoint {
    case LEFT, RIGHT, COVERS
  }

  import PositionToRefPoint.*

  override def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int): AllocationResult = {
    val currentRow = seatingMap.seats.last
    val result = allocateSeatsForRow(currentRow, numberOfSeatsRequested)
    Seq(AllocatedSeatBlocks(currentRow.id, result._1))
  }

  private def allocateSeatsForRow(row: Row, numberOfSeatsRequested: Int): SingleRowAllocationResult = {
    val sortedSeatBlocks = sortSeatBlocksByDistanceToRefPoint(row, row.midPoint)

    allocateSeatsFromSeatBlocksOfRow(sortedSeatBlocks, SeatBlocks.empty, SeatBlocks.empty, numberOfSeatsRequested, row.midPoint)
  }

  @tailrec
  private def allocateSeatsFromSeatBlocksOfRow(sortedSeatBlocks: Seq[(SeatBlock, PositionToRefPoint, Int)],
                                               allocatedSeatBlocks: SeatBlocks,
                                               remainingSeatBlocks: SeatBlocks,
                                               numberOfSeatsRequested: Int,
                                               refPoint: Int,
                                              ): SingleRowAllocationResult = {
    sortedSeatBlocks match {
      case Nil => SingleRowAllocationResult(allocatedSeatBlocks, remainingSeatBlocks, 0)
      case x +: xs =>
        val result = allocateSeatsFromSeatBlock(x._1, refPoint, x._2, numberOfSeatsRequested)
        allocateSeatsFromSeatBlocksOfRow(xs, allocatedSeatBlocks +: result._1, remainingSeatBlocks ++ result._2, numberOfSeatsRequested, refPoint)
    }
  }

  /**
   * Allocates seats from a seatBlock.
   *
   * @param seatBlock              The seatBlock to be allocated
   * @param refPoint               The reference point
   * @param position               The position of the seatBlock in relation to the reference point
   * @param numberOfSeatsRequested The number of seats requested
   * @return a 3-tuple with the allocated seats, the remaining seat block(s) after allocation, and the number of seats to be allocated after this operation
   */
  private def allocateSeatsFromSeatBlock(seatBlock: SeatBlock, refPoint: Int, position: PositionToRefPoint, numberOfSeatsRequested: Int): SingleBlockAllocationResult = {
    if (seatBlock.size <= numberOfSeatsRequested)
      SingleBlockAllocationResult(seatBlock, SeatBlocks.empty, 0)
    else {
      position match {
        case LEFT =>
          val allocatedBlock = Range.of(seatBlock.getMaximum - numberOfSeatsRequested + 1, seatBlock.getMaximum)
          val remainingBlock = Range.of(seatBlock.getMinimum, seatBlock.getMaximum - numberOfSeatsRequested)
          SingleBlockAllocationResult(allocatedBlock, SeatBlocks(Seq(remainingBlock)), remainingBlock.size)
        case RIGHT =>
          val allocatedBlock = Range.of(seatBlock.getMinimum, seatBlock.getMinimum + numberOfSeatsRequested - 1)
          val remainingBlock = Range.of(seatBlock.getMinimum + numberOfSeatsRequested, seatBlock.getMaximum)
          SingleBlockAllocationResult(allocatedBlock, SeatBlocks(Seq(remainingBlock)), remainingBlock.size)
        case COVERS =>
          val startPoint = refPoint - numberOfSeatsRequested / 2
          val endPoint = startPoint + numberOfSeatsRequested - 1
          val range = Range.of(startPoint, endPoint)
          // Shift the range to fit into the seat block if required
          val offset =
            if (range.getMinimum < seatBlock.getMinimum)
              seatBlock.getMinimum - range.getMinimum
            else if (range.getMaximum > seatBlock.getMaximum)
              range.getMaximum - seatBlock.getMaximum
            else 0
          val allocatedBlock = Range.of(range.getMinimum + offset, range.getMaximum + offset)
          val remainingBlocks = ListBuffer.empty[Range[Integer]]
          if (allocatedBlock.getMinimum > seatBlock.getMinimum) {
            remainingBlocks += Range.of(seatBlock.getMinimum, allocatedBlock.getMinimum - 1)
          }
          if (allocatedBlock.getMaximum < seatBlock.getMaximum) {
            remainingBlocks += Range.of(allocatedBlock.getMaximum + 1, seatBlock.getMaximum)
          }
          SingleBlockAllocationResult(allocatedBlock, SeatBlocks(remainingBlocks.toList), remainingBlocks.map(_.size).sum)
      }
    }
  }

  private def sortSeatBlocksByDistanceToRefPoint(row: Row, refPoint: Int): Seq[(SeatBlock, PositionToRefPoint, Int)] =
    assert(row.seatCount > 0)
    row.availableSeats
      .map(seatBlock => {
        val relativePosition = positionToRefPoint(seatBlock, refPoint)
        val distanceToMidPoint = distanceToRefPoint(seatBlock, relativePosition, refPoint)
        (seatBlock, relativePosition, distanceToMidPoint)
      })
      .sortBy { case (seatBlock, position, distance) => (distance, position.ordinal) }

  private def distanceToRefPoint(seatBlock: SeatBlock, relativePosition: PositionToRefPoint, refPoint: Int) = {
    relativePosition match {
      case COVERS => 0
      case LEFT => refPoint - seatBlock.getMaximum
      case RIGHT => seatBlock.getMinimum - refPoint
    }
  }

  private def positionToRefPoint(seatBlock: SeatBlock, midPoint: Int) =
    if (seatBlock.getMaximum < midPoint) LEFT
    else if (seatBlock.getMinimum > midPoint) RIGHT
    else COVERS

  //  override def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, startingPosition: (Int, Int)): AllocationResult = ???

  //  private def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, allocatedSeats: Seq)

}
