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

        def count(x: Int, y: Int, numTrees: Int): Int = {
            val nextNumTrees = if (map(y)(x)) numTrees + 1 else numTrees
            val nextX = (x + slope._1) % width
            val nextY = y + slope._2

            if (nextY < height)
                count(nextX, nextY, nextNumTrees)
            else
                nextNumTrees
        }

        count(0, 0, 0)
    }
}