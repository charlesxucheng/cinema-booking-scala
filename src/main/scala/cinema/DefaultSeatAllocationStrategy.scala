package cinema

import cinema.Row.{SeatBlock, SeatBlocks}
import cinema.SeatAllocationStrategy.*

import scala.+:
import scala.annotation.tailrec
import scala.collection.immutable.Range
object DefaultSeatAllocationStrategy extends SeatAllocationStrategy {

  private def validateNumberOfSeatsRequested(
      seatingMap: SeatingMap,
      numberOfSeatsRequested: Int
  ): Unit = {
    require(
      numberOfSeatsRequested > 0,
      s"Number of seats requested ($numberOfSeatsRequested) must be greater than 0."
    )
    require(
      numberOfSeatsRequested <= seatingMap.availableSeatCount,
      s"Number of requested seats ($numberOfSeatsRequested) cannot be greater than the available seat count (${seatingMap.availableSeatCount})."
    )
  }

  override def allocateSeats(
      seatingMap: SeatingMap,
      numberOfSeatsRequested: Int
  ): AllocationResult = {
    validateNumberOfSeatsRequested(seatingMap, numberOfSeatsRequested)

    val rowsToProcess = seatingMap.availableRows
    val result = allocateSeatsRec(
      rowsToProcess,
      numberOfSeatsRequested,
      Seq.empty,
      IndexedSeq.empty
    )
    val allocatedBlocks = result._1
    val updatedSeatingMap = seatingMap.holdSeatsForBooking(result._2)

    AllocationResult(allocatedBlocks, updatedSeatingMap)
  }

  override def allocateSeats(
      seatingMap: SeatingMap,
      numberOfSeatsRequested: Int,
      startingRowId: Int,
      startingColId: Int
  ): AllocationResult = {
    validateNumberOfSeatsRequested(seatingMap, numberOfSeatsRequested)

    // Allocate seats in the first row based on startingPosition
    val seatsAllocatedInTheStartingRow = allocateSeatsFromStartingPosition(
      seatingMap,
      numberOfSeatsRequested,
      startingRowId,
      startingColId
    )

    // Allocate the rest as per usual
    val rowsToProcess = seatingMap.availableRows.filter(_.id > startingRowId)
    val firstRowAllocationResult = allocateSeatsRec(
      rowsToProcess,
      numberOfSeatsRequested - seatsAllocatedInTheStartingRow._1.seatBlocks.seatCount,
      Seq.empty,
      IndexedSeq.empty
    )

    // Put the two allocation results together
    val combinedAllocatedSeatBlocks =
      seatsAllocatedInTheStartingRow._1 +: firstRowAllocationResult._1
    val updatedRows =
      firstRowAllocationResult._2 :+ seatsAllocatedInTheStartingRow._2

    val updatedSeatingMap = seatingMap.holdSeatsForBooking(updatedRows)

    AllocationResult(combinedAllocatedSeatBlocks, updatedSeatingMap)
  }

  private def allocateSeatsFromStartingPosition(
      seatingMap: SeatingMap,
      numberOfSeatsRequested: Int,
      startingRowId: Int,
      startingColId: Int
  ): (AllocatedSeatBlocks, Row) = {
    try {
      val startingRow = seatingMap.row(startingRowId)
      val numberOfSeatsToAllocate =
        Math.min(
          numberOfSeatsRequested,
          startingRow.seatCount - (startingColId - 1)
        )

      val seatsToAllocate =
        Range.inclusive(
          startingColId,
          startingColId + numberOfSeatsToAllocate - 1
        )

      val allocatedSeatBlocks =
        AllocatedSeatBlocks(
          startingRowId,
          SeatBlocks.of(
            Seq((startingColId, startingColId + numberOfSeatsToAllocate - 1))
          )
        )

      (
        allocatedSeatBlocks,
        startingRow.holdSeatsForBooking(allocatedSeatBlocks.seatBlocks)
      )
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalArgumentException(
          s"Cannot allocate $numberOfSeatsRequested seats from starting position as there are already booked seats to the right of the position. Please try again."
        )
    }
  }

  import PositionToRefPoint.*

  @tailrec
  private def allocateSeatsRec(
      rows: Seq[Row],
      numberOfSeatsRequested: Int,
      allocatedSeatBlocksAcc: Seq[AllocatedSeatBlocks],
      updatedRowsAcc: IndexedSeq[Row]
  ): (Seq[AllocatedSeatBlocks], IndexedSeq[Row]) =
    rows match {
      case Seq() =>
        require(
          numberOfSeatsRequested == 0,
          "Not enough seats available for booking."
        )
        (allocatedSeatBlocksAcc, updatedRowsAcc)
      case r +: rs =>
        if (numberOfSeatsRequested == 0)
          (allocatedSeatBlocksAcc, updatedRowsAcc)
        else {
          val result = allocateSeatsForRow(r, numberOfSeatsRequested)
          val allocatedBlocks = Seq(
            AllocatedSeatBlocks(r.id, result.allocatedSeatBlocks)
          )
          val updatedRow = r.holdSeatsForBooking(result.allocatedSeatBlocks)
          allocateSeatsRec(
            rs,
            result.numberOfSeatsToAllocate,
            allocatedSeatBlocksAcc ++ allocatedBlocks,
            updatedRowsAcc :+ updatedRow
          )
        }
    }

  private def allocateSeatsForRow(
      row: Row,
      numberOfSeatsRequested: Int
  ): SingleRowAllocationResult = {
    val sortedSeatBlocks = sortSeatBlocksByDistanceToRefPoint(row, row.midPoint)
    allocateSeatsFromSeatBlocksOfRow(
      sortedSeatBlocks,
      SeatBlocks.empty,
      numberOfSeatsRequested,
      row.midPoint
    )
  }

  @tailrec
  private def allocateSeatsFromSeatBlocksOfRow(
      sortedSeatBlocks: Seq[(SeatBlock, PositionToRefPoint, Float)],
      allocatedSeatBlocks: SeatBlocks,
      numberOfSeatsRequested: Int,
      refPoint: Float
  ): SingleRowAllocationResult = {
    sortedSeatBlocks match {
      case Nil =>
        SingleRowAllocationResult(allocatedSeatBlocks, numberOfSeatsRequested)
      case x +: xs =>
        val result = allocateSeatsFromSeatBlock(
          x._1,
          refPoint,
          x._2,
          numberOfSeatsRequested
        )
        if (result.numberOfSeatsToAllocate == 0)
          SingleRowAllocationResult(
            allocatedSeatBlocks :+ result.allocatedSeatBlock,
            0
          )
        else
          allocateSeatsFromSeatBlocksOfRow(
            xs,
            allocatedSeatBlocks :+ result._1,
            result._2,
            refPoint
          )
    }
  }

  private def deriveEndPoint(refPoint: Float, numberOfSeatsRequested: Int) =
    if (refersToASeat(refPoint))
      refPoint.intValue + numberOfSeatsRequested / 2
    else
      math
        .ceil(refPoint)
        .intValue + numberOfSeatsRequested / 2 - (1 - numberOfSeatsRequested % 2)

  /** Allocates seats from a seatBlock.
    *
    * @param seatBlock
    *   The seatBlock to be used for allocation
    * @param refPoint
    *   The reference point
    * @param position
    *   The position of the seatBlock in relation to the reference point
    * @param numberOfSeatsRequested
    *   The number of seats requested
    * @return
    *   Allocation result consisting of the seat block allocated and the number
    *   of seats yet to be allocated
    */
  private def allocateSeatsFromSeatBlock(
      seatBlock: SeatBlock,
      refPoint: Float,
      position: PositionToRefPoint,
      numberOfSeatsRequested: Int
  ): SingleBlockAllocationResult = {
    if (seatBlock.size <= numberOfSeatsRequested)
      SingleBlockAllocationResult(
        seatBlock,
        numberOfSeatsRequested - seatBlock.size
      )
    else {
      val allocatedBlock = position match {
        case LEFT =>
          Range.inclusive(
            seatBlock.end - numberOfSeatsRequested + 1,
            seatBlock.end
          )
        case RIGHT =>
          Range.inclusive(
            seatBlock.start,
            seatBlock.start + numberOfSeatsRequested - 1
          )
        case COVERS =>
          val endPoint = deriveEndPoint(refPoint, numberOfSeatsRequested)
          val startPoint = endPoint - numberOfSeatsRequested + 1
          val blockCenteredAtRefPoint = Range.inclusive(startPoint, endPoint)

          val offset =
            if (blockCenteredAtRefPoint.start < seatBlock.start)
              seatBlock.start - blockCenteredAtRefPoint.start
            else if (blockCenteredAtRefPoint.end > seatBlock.end)
              seatBlock.end - blockCenteredAtRefPoint.end
            else 0

          Range.inclusive(
            blockCenteredAtRefPoint.start + offset,
            blockCenteredAtRefPoint.end + offset
          )
      }
      SingleBlockAllocationResult(allocatedBlock, 0)
    }
  }

  private def sortSeatBlocksByDistanceToRefPoint(
      row: Row,
      refPoint: Float
  ): Seq[(SeatBlock, PositionToRefPoint, Float)] =
    assert(row.seatCount > 0)
    row.availableSeats
      .map(seatBlock => {
        val relativePosition = positionToRefPoint(seatBlock, refPoint)
        val distanceToMidPoint =
          distanceToRefPoint(seatBlock, relativePosition, refPoint)
        (seatBlock, relativePosition, distanceToMidPoint)
      })
      .sortBy { case (seatBlock, position, distance) =>
        (distance, position.ordinal)
      }

  private def distanceToRefPoint(
      seatBlock: SeatBlock,
      relativePosition: PositionToRefPoint,
      refPoint: Float
  ) = {
    relativePosition match {
      case COVERS => 0
      case LEFT   => refPoint - seatBlock.end
      case RIGHT  => seatBlock.start - refPoint
    }
  }

  private def positionToRefPoint(seatBlock: SeatBlock, refPoint: Float) =
    if (seatBlock.end < refPoint) LEFT
    else if (seatBlock.start > refPoint) RIGHT
    else COVERS

  private enum PositionToRefPoint {
    // The ordinal values are used in sorting
    case COVERS, RIGHT, LEFT
  }

  //  private def allocateSeats(seatingMap: SeatingMap, numberOfSeatsRequested: Int, allocatedSeats: Seq)

}
