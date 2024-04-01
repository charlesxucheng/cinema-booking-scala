package cinema

import cinema.Row.*
import cinema.SeatAllocationStrategy.*
import org.apache.commons.lang3.Range

import scala.+:
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object SeatAllocationStrategy {

  case class AllocatedSeatBlocks(rowId: Int, seatBlocks: SeatBlocks)

  type AllocationResult = (Seq[AllocatedSeatBlocks], SeatingMap)
  case class SingleBlockAllocationResult(allocatedSeatBlock: SeatBlock, numberOfSeatsToAllocate: Int)
  case class SingleRowAllocationResult(allocatedSeatBlocks: SeatBlocks, numberOfSeatsToAllocate: Int)
  case class SeatingMapAllocationResult(allocatedSeatBlocks: AllocatedSeatBlocks, updatedSeatingMap: SeatingMap)
}

trait SeatAllocationStrategy {
  def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int): AllocationResult
  //  def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, startingPosition: (Int, Int)): AllocationResult
}

object DefaultSeatAllocationStrategy extends SeatAllocationStrategy {

  private enum PositionToRefPoint {
    // The ordinal values are used in sorting
    case COVERS, RIGHT, LEFT
  }

  import PositionToRefPoint.*

  override def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int): AllocationResult = {
    require(numberOfSeatsRequested > 0)
    val currentRow = seatingMap.seats.last
    val result = allocateSeatsForRow(currentRow, numberOfSeatsRequested)
    val allocatedBlocks = Seq(AllocatedSeatBlocks(currentRow.id, result.allocatedSeatBlocks))
    val updatedRows = IndexedSeq(currentRow.assignSeats(result.allocatedSeatBlocks))
    val updatedSeatingMap = seatingMap.bookSeats(updatedRows)
    (allocatedBlocks, updatedSeatingMap)
  }


  private def allocateSeatsForRow(row: Row, numberOfSeatsRequested: Int): SingleRowAllocationResult = {
    val sortedSeatBlocks = sortSeatBlocksByDistanceToRefPoint(row, row.midPoint)
    allocateSeatsFromSeatBlocksOfRow(sortedSeatBlocks, SeatBlocks.empty, numberOfSeatsRequested, row.midPoint)
  }

  @tailrec
  private def allocateSeatsFromSeatBlocksOfRow(sortedSeatBlocks: Seq[(SeatBlock, PositionToRefPoint, Int)],
                                               allocatedSeatBlocks: SeatBlocks,
                                               numberOfSeatsRequested: Int,
                                               refPoint: Int,
                                              ): SingleRowAllocationResult = {
    sortedSeatBlocks match {
      case Nil => SingleRowAllocationResult(allocatedSeatBlocks, 0)
      case x +: xs =>
        val result = allocateSeatsFromSeatBlock(x._1, refPoint, x._2, numberOfSeatsRequested)
        if (result.numberOfSeatsToAllocate == 0)
          SingleRowAllocationResult(allocatedSeatBlocks +: result.allocatedSeatBlock, 0)
        else
          allocateSeatsFromSeatBlocksOfRow(xs, allocatedSeatBlocks +: result._1, result._2, refPoint)
    }
  }

  /**
   * Allocates seats from a seatBlock.
   *
   * @param seatBlock              The seatBlock to be allocated
   * @param refPoint               The reference point
   * @param position               The position of the seatBlock in relation to the reference point
   * @param numberOfSeatsRequested The number of seats requested
   * @return Allocation result consisting of the allocated seat block and the number of remaining seats to be allocated
   */
  private def allocateSeatsFromSeatBlock(seatBlock: SeatBlock, refPoint: Int, position: PositionToRefPoint, numberOfSeatsRequested: Int): SingleBlockAllocationResult = {
    if (seatBlock.size <= numberOfSeatsRequested)
      SingleBlockAllocationResult(seatBlock, numberOfSeatsRequested - seatBlock.size)
    else {
      val allocatedBlock = position match {
        case LEFT =>
          Range.of(seatBlock.getMaximum - numberOfSeatsRequested + 1, seatBlock.getMaximum)
        case RIGHT =>
          Range.of(seatBlock.getMinimum, seatBlock.getMinimum + numberOfSeatsRequested - 1)
        case COVERS =>
          val startPoint = refPoint - numberOfSeatsRequested / 2
          val endPoint = startPoint + numberOfSeatsRequested - 1
          val blockCenteredAtRefPoint = Range.of(startPoint, endPoint)
          val offset =
            if (blockCenteredAtRefPoint.getMinimum < seatBlock.getMinimum)
              seatBlock.getMinimum - blockCenteredAtRefPoint.getMinimum
            else if (blockCenteredAtRefPoint.getMaximum > seatBlock.getMaximum)
              blockCenteredAtRefPoint.getMaximum - seatBlock.getMaximum
            else 0
          Range.of(blockCenteredAtRefPoint.getMinimum + offset, blockCenteredAtRefPoint.getMaximum + offset)
      }
      SingleBlockAllocationResult(allocatedBlock, 0)
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
