import scala.io.Source
import scala.collection.immutable.HashMap


object Part1 extends App {
    val slope = (3, 1)

    val input = Source.fromInputStream(System.in).getLines().toList
    val map = parseMap(input)
    val numTrees = countTrees(map, slope)
    println(numTrees)

    def parseMap(input: List[String]): List[List[Boolean]] = {
        input.map(line => {
            line.map(line => line match {
                case '#' => true
                case _ => false
            }).toList
        })
    }

    def countTrees(map: List[List[Boolean]], slope: (Int, Int)): Int = {
        val width = map(0).length
        val height = map.length

        var x = 0
        var y = 0
        var numTrees = 0

        do {
            if (map(y)(x)) {
                numTrees += 1
            }
            x = (x + slope._1) % width
            y = y + slope._2
        } while (y < height) 

        return numTrees
    }
}