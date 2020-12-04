import scala.io.Source
import scala.collection.immutable.HashMap

object Part1 extends App {
    val TargetNumber = 2020

    val input = Source.fromInputStream(System.in)
    val values = valuesThatSumTo(
        input.getLines().map(s => s.toInt).toList,
        TargetNumber,
    )
    if (values.length != 2)
        println("Oops! Should only have 2 values")
    else
        println(values(0) * values(1))

    def valuesThatSumTo(values: List[Int], target: Int): List[Int] = {
        val valueSet = values.toSet

        // Find all the elements that have target - elem in the hashmap
        values
            .filter(elem => {
                val delta = target - elem
                valueSet.contains(delta)
            })
    }
}