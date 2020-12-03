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
        // Create a hashmap from value -> index 
        val index = values
            .zipWithIndex
            .foldLeft(HashMap[Int, Int]())({
                case (curr, (value, count)) => curr + (value -> count)
            })

        // Find all the elements that have target - elem in the hashmap
        // (where the element in the map is not itself)
        values
            .zipWithIndex
            .filter({
                case (elem, elemIndex) => {
                    val delta = target - elem
                    index.contains(delta) && index(delta) != elemIndex
                }
            })
            .map(t => t._1)
    }
}