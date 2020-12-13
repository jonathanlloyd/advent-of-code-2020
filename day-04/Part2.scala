import scala.io.Source
import scala.collection.immutable.HashMap
import scala.util.Success
import scala.util.Try
import scala.util.Failure


object Part2 extends App {
    val hairColorPattern = "#[0-9a-f]{6}".r
    val eyeColors = Set(
        "amb",
        "blu",
        "brn",
        "gry",
        "grn",
        "hzl",
        "oth",
    )
    val passportIdPattern = "[0-9]{9}".r

    val fields = HashMap[String, (String) => Boolean](
        "byr" -> (v => {
            val year = Try(v.toInt)
            year match {
                case Success(v) => v >= 1920 && v <= 2002
                case _ => false
            }
        }),
        "iyr" -> (v => {
            val year = Try(v.toInt)
            year match {
                case Success(v) => v >= 2010 && v <= 2020
                case _ => false
            }
        }),
        "eyr" -> (v => {
            val year = Try(v.toInt)
            year match {
                case Success(v) => v >= 2020 && v <= 2030
                case _ => false
            }       
        }),
        "hgt" -> (v => {
            if (v.length < 3)
                false
            else {
                val value = Try(v.slice(0, v.length - 2).toInt)
                value match {
                    case Success(value) => {
                        val unitStr = v.slice(v.length - 2, v.length)
                        unitStr match {
                            case "cm" => value >= 150 && value <= 193
                            case "in" => value >= 59 && value <= 76
                            case _ => false
                        }
                    }
                    case Failure(exception) => false
                }
            }
        }),
        "hcl" -> (v => hairColorPattern.pattern.matcher(v).matches),
        "ecl" -> (v => eyeColors.contains(v)),
        "pid" -> (v => passportIdPattern.pattern.matcher(v).matches),
        "cid" -> (v => true),
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

    def isValidPassport(fields: HashMap[String, (String) => Boolean], optional: Set[String]) = (passport: HashMap[String, String]) => {
        val passportFields = passport.keySet
        fields.keySet.forall(f => {
            (
                passportFields.contains(f) && fields(f)(passport(f))
            ) || (
                optional.contains(f)
            )
        })
    }
}