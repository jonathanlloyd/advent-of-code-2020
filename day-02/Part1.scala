import scala.io.Source
import scala.collection.immutable.HashMap
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class PasswordRule(val ruleLetter: Char, val minCount: Int, val maxCount: Int) {
    def matches(password: String): Boolean = {
        val letterCounts = password.foldLeft(
            HashMap[Char, Int]().withDefaultValue(0)
        )(
            (counts, char) => counts + (char -> (counts(char) + 1))
        )
        letterCounts(ruleLetter) >= minCount && letterCounts(ruleLetter) <= maxCount
    }
}

class ParseError(val message: String)

object PasswordRule {
    def fromString(s: String): Either[ParseError, PasswordRule] = {
        val parts = if(s.split(" ").length == 2)
            Right(s.split(" "))
        else
            Left(new ParseError("Could not break password rule on space"))

        val rangeStr = parts.map(parts => parts(0))
        val ruleLetter = parts.map(parts => parts(1).toCharArray.head)

        val rangeParts = rangeStr.flatMap(rangeStr => {
            if (rangeStr.split("-").length == 2)
                Right(rangeStr.split("-"))
            else
                Left(new ParseError("Range should be separated by '-'"))
        })

        val minCount = rangeParts.flatMap(rangeParts => {
            Try(rangeParts(0).toInt) match {
                case Failure(exception) => return Left(new ParseError("lower range bound must be an int"))
                case Success(value) => Right(value)
            }
        })
        val maxCount = rangeParts.flatMap(rangeParts => {
            Try(rangeParts(1).toInt) match {
                case Failure(exception) => return Left(new ParseError("uppser range bound must be an int"))
                case Success(value) => Right(value)
            }
        })

        for {
            ruleLetter <- ruleLetter
            minCount <- minCount
            maxCount <- maxCount
        } yield new PasswordRule(ruleLetter, minCount, maxCount)
    }
}

object Part1 extends App {
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
        val parts = if (record.split(":").length == 2)
            Right(record.split(":"))
        else
            Left(new ParseError("Could not split password record on ':'"))

        val passwordRule = parts.flatMap(parts => PasswordRule.fromString(parts(0)))
        val password = parts.map(parts => parts(1).trim)

        for {
            passwordRule <- passwordRule
            password <- password
        } yield (passwordRule, password)
    }
}