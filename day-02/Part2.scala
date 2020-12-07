import scala.io.Source
import scala.collection.immutable.HashMap
import scala.util.Try
import scala.util.Failure
import scala.util.Success

case class ParseError(val message: String)

case class PasswordRule(ruleLetter: Char, i1: Int, i2: Int) {
    def matches(password: String): Boolean = {
        val i1Found = Try(password(i1 - 1) == ruleLetter)
        val i2Found = Try(password(i2 - 1) == ruleLetter)
        val exactlyOneFound = for {
            i1Found <- i1Found
            i2Found <- i2Found
        } yield !(!i1Found && !i2Found) && !(i1Found && i2Found)

        exactlyOneFound match {
            case Failure(_) => false
            case Success(value) => value
        }
    }
}

object PasswordRule {
    def fromString(s: String): Either[ParseError, PasswordRule] = {
        for {
            parts <- s.split(" ") match {
                case Array(a, b) => Right((a, b))
                case _ => Left(ParseError("Could not break password rule on space"))
            }
            ruleLetter <- Right(parts._2.toCharArray().head)
            rangeParts <- parts._1.split("-") match {
                case Array(a, b) => Right((a, b))
                case _ => Left(ParseError("Range must be separated with '-'"))
            }
            i1 <- {
                Try(rangeParts._1.toInt) match {
                    case Failure(exception) => return Left(ParseError("lower range bound must be an int"))
                    case Success(value) => Right(value)
                }
            }
            i2 <- {
                Try(rangeParts._2.toInt) match {
                    case Failure(exception) => return Left(ParseError("upper range bound must be an int"))
                    case Success(value) => Right(value)
                }
            }
        } yield PasswordRule(ruleLetter, i1, i2)
    }
}

object Part2 extends App {
    val input = Source.fromInputStream(System.in).getLines().toList
    val passwordDb = parsePasswordDb(input)

    passwordDb match {
        case Left(error) => println(error)
        case Right(passwordDb) => {
            val validPasswords = passwordDb.filter({
                case(rule, password) => rule.matches(password)
            })
            val numValidPasswords = validPasswords.length

            println(numValidPasswords)
        }
    }

    def parsePasswordDb(dbLines: List[String]): Either[ParseError, List[(PasswordRule, String)]] = {
        val parsedRecords = dbLines.map(line => parsePasswordDbRecord(line))
        parsedRecords.partitionMap(identity) match {
            case(Nil, rights) => Right(rights)
            case(lefts, _) => Left(lefts.head)
        }
    }
    def parsePasswordDbRecord(record: String): Either[ParseError, (PasswordRule, String)] = {
        for {
            parts <- record.split(":") match {
                case Array(a, b) => Right((a, b))
                case _ => Left(ParseError("Could not split password record on ':'"))
            }
            passwordRule <- PasswordRule.fromString(parts._1)
            password <- Right(parts._2.trim)
        } yield (passwordRule, password)
    }
}