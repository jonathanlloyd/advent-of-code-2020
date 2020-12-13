import scala.io.Source

case class SeatingRange(
    rowLowerBound: Int,
    rowUpperBound: Int,
    colLowerBound: Int,
    colUpperBound: Int,
) {
    def width(): Int = colUpperBound - colLowerBound
    def height(): Int = rowUpperBound - rowLowerBound
}

object Part1 extends App {
    val input = Source.fromInputStream(System.in).getLines().toList
    val seatIds = input.map(calculateSeatNumber).map(seatId)
    println(seatIds.max)

    def calculateSeatNumber(boardingPass: String): (Int, Int) = {
        def calc(boardingPass: String, seating: SeatingRange = SeatingRange(0, 128, 0, 8)): SeatingRange = {
            if (boardingPass.length == 0)
                seating
            else {
                val nextSeating = boardingPass(0) match {
                    case 'F' => seating.copy(rowUpperBound = seating.rowUpperBound - seating.height / 2)
                    case 'B' => seating.copy(rowLowerBound = seating.rowLowerBound + seating.height / 2)
                    case 'L' => seating.copy(colUpperBound = seating.colUpperBound - seating.width / 2)
                    case 'R' => seating.copy(colLowerBound = seating.colLowerBound + seating.width / 2)
                }
                val nextBoardingPass = boardingPass.slice(1, boardingPass.length)
                calc(nextBoardingPass, nextSeating)
            }
        }

        val finalSeatingRange = calc(boardingPass)

        (finalSeatingRange.colLowerBound, finalSeatingRange.rowLowerBound)
    }

    def seatId(seatNumber: (Int, Int)): Int = seatNumber match {
        case (col, row) => row * 8 + col
    }
}