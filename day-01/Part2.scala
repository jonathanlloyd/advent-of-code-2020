import scala.io.Source
import scala.collection.immutable.HashMap

object Part2 extends App {
    val TargetNumber = 2020

    val input = Source.fromInputStream(System.in)
    val values = valuesThatSumTo(
        input.getLines().map(s => s.toInt).toList,
        TargetNumber,
    )

    if (values.length != 3)
        println("Oops! Should only have 3 values")
    else
        println(values(0) * values(1) * values(2))

    def valuesThatSumTo(values: List[Int], target: Int): List[Int] = {
        val valueSet = values.toSet

        // Find all the elements that have the required remaining value in the hashmap
        values
            .flatMap(v1 => values.map(v2 => (v1, v2)))
            .filter({
                case (v1, v2) => {
                    val v3 = target - v1 - v2
                    valueSet.contains(v3)
                }
            })
            .map(t => t._1)
            .distinct
    }
}