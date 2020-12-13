import scala.io.Source
import scala.collection.immutable.HashMap


object Part1 extends App {
    val fields = Set(
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid",
        "cid",
    )
    val optionalFields = Set(
        "cid"
    )

    val input = Source.fromInputStream(System.in).getLines().toList
    val passports = parsePassportList(input)
    val validPassports = passports.filter(isValidPassport(fields, optionalFields))
    println(validPassports.length)

    def parsePassportList(logs: List[String]): List[HashMap[String, String]] = {
        def parseLogLine(line: String): Map[String, String] = {
            if (line == "")
                HashMap()
            else
                line.split(" ")
                    .map(s => s.split(":") match {
                        case Array(k, v) => (k, v)
                        case _ => throw new RuntimeException("Unexpected k/v format")
                    })
                    .toMap
        }

        logs.map(parseLogLine).foldLeft(
            List(HashMap[String, String]())
        )((mergedLogs, m) => {
            if (m.isEmpty)
                HashMap[String, String]() :: mergedLogs
            else
                mergedLogs.head ++ m :: mergedLogs.drop(1)
        })
    }

    def isValidPassport(fields: Set[String], optional: Set[String]) = (passport: HashMap[String, String]) => {
        val passportFields = passport.keySet
        val missingFields = fields.filter(f => !passportFields.contains(f))
        missingFields.forall(f => optional.contains(f))
    }
}