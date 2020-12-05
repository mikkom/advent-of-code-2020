package mikkom

import scala.io.Source
import cats.effect.{IO, IOApp}
import cats.parse.{Parser => P, Numbers}
import cats.data.NonEmptyList
import ParseUtils._

object Day4 extends IOApp.Simple {

  sealed trait PassportItem
  case class Height(value: String)      extends PassportItem
  case class BirthYear(value: Int)      extends PassportItem
  case class IssueYear(value: Int)      extends PassportItem
  case class ExpirationYear(value: Int) extends PassportItem
  case class HairColor(value: String)   extends PassportItem
  case class EyeColor(value: String)    extends PassportItem
  case class PassportId(value: String)  extends PassportItem
  case class CountryId(value: String)   extends PassportItem

  val parseHeight         = P.string1("hgt:") *> noWhitespaceStr.map(Height)
  val parseBirthYear      = P.string1("byr:") *> int.map(BirthYear)
  val parseIssueYear      = P.string1("iyr:") *> int.map(IssueYear)
  val parseExpirationYear = P.string1("eyr:") *> int.map(ExpirationYear)
  val parseHairColor      = P.string1("hcl:") *> noWhitespaceStr.map(HairColor)
  val parseEyeColor       = P.string1("ecl:") *> noWhitespaceStr.map(EyeColor)
  val parsePassportId     = P.string1("pid:") *> noWhitespaceStr.map(PassportId)
  val parseCountryId      = P.string1("cid:") *> noWhitespaceStr.map(CountryId)

  val parsePassportItem = P.oneOf1[PassportItem](
    List(
      parseHeight,
      parseBirthYear,
      parseIssueYear,
      parseExpirationYear,
      parseHairColor,
      parseEyeColor,
      parsePassportId,
      parseCountryId
    )
  )

  val parsePassport = P.rep1Sep(parsePassportItem, 1, P.charIn(' ', '\n'))

  val parseInput = P.repSep(
    parsePassport,
    0,
    P.rep(P.char(' ')) ~ P.string("\n\n") ~ P.rep(P.char(' '))
  )

  def hasRequiredFields(passport: NonEmptyList[PassportItem]): Boolean = {
    passport.length == 8 ||
    (passport.length == 7 &&
      passport.forall {
        case CountryId(_) =>
          false
        case _ =>
          true
      })
  }

  val validateHeight =
    (int ~ (P.string1("cm").as("cm") orElse1 P.string1("in").as("in")))
      .flatMap {
        case (cm, "cm") if (150 to 193).contains(cm) =>
          P.pure(s"$cm cm")
        case (in, "in") if (59 to 76).contains(in) =>
          P.pure(s"$in inches")
        case _ =>
          P.fail
      }

  val validateHairColor = (P.string1("#") *> P.rep1(hexChar, 6) <* P.end)
    .flatMap(hs =>
      if (hs.length == 6)
        P.pure(hs)
      else
        P.fail
    )

  val validateEyeColor = P.oneOf1(
    List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").map(P.string1)
  )

  val validatePassportId = (Numbers.digits1 <* P.end).flatMap(digits =>
    if (digits.length == 9)
      P.pure(digits)
    else
      P.fail
  )

  def validate[A](parser: P[A], value: String) = parser.parseAll(value).isRight

  def isValidItem(item: PassportItem) =
    item match {
      case BirthYear(value) =>
        (1920 to 2002).contains(value)
      case IssueYear(value) =>
        (2010 to 2020).contains(value)
      case ExpirationYear(value) =>
        (2020 to 2030).contains(value)
      case Height(value) =>
        validate(validateHeight, value)
      case HairColor(value) =>
        validate(validateHairColor, value)
      case EyeColor(value) =>
        validate(validateEyeColor, value)
      case PassportId(value) =>
        validate(validatePassportId, value)
      case CountryId(_) =>
        true
    }

  def hasValidData(passport: NonEmptyList[PassportItem]) =
    hasRequiredFields(passport) && passport.forall(isValidItem)

  def answer1(input: List[String]) =
    parseInput
      .parseAll(input.mkString("\n").trim())
      .map(_.filter(hasRequiredFields).length)

  def answer2(input: List[String]) =
    parseInput
      .parseAll(input.mkString("\n").trim())
      .map(_.filter(hasValidData).length)

  override def run: IO[Unit] =
    for {
      input <- IO(Source.fromResource("input-day4.txt").getLines().toList)
      _     <- IO(println(answer1(input)))
      _     <- IO(println(answer2(input)))
    } yield ()
}
