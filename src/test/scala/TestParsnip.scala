package parsnip

import Parser._
import scalaz._; import Scalaz._

import org.specs2.mutable._
import org.specs2.matcher._

class ParsnipSpec extends Specification with ResultMatchers {

  /** success matcher for a Validation */
  def beSuccessful[E, A]: Matcher[Validation[E, A]] = (v: Validation[E, A]) => (v.fold(_ => false, _ => true), v + " successful", v + " is not successfull")

  /** failure matcher for a Validation */
  def beAFailure[E, A]: Matcher[Validation[E, A]] = (v: Validation[E, A]) => (v.fold(_ => true, _ => false), v + " is a failure", v + " is not a failure")

  /** success matcher for a Validation with a specific value */
  def succeedWith[E, A](a: => A) = validationWith[E, A](Success(a))

  /** failure matcher for a Validation with a specific value */
  def failWith[E, A](e: => E) = validationWith[E, A](Failure(e))

  private def validationWith[E, A](f: => Validation[E, A]): Matcher[Validation[E, A]] = (v: Validation[E, A]) => {
    val expected = f
    (expected == v, v + " is a " + expected, v + " is not aa " + expected)
  }

  "digit parser" should {
    "parse a digit" in {

      digit("11".toStream) should beSuccessful
      digit.parse("1".toStream) should succeedWith('1')
    }
    "parse only digits" in {
      digit("a11".toStream) should beAFailure
      digit.parse("a".toStream) should beAFailure
      digit.parse("11".toStream) should beAFailure
    }
  }

  "alpha parser" should {
    "parse a letter" in {

      alpha("a11".toStream) should beSuccessful
      alpha.parse("a".toStream) should succeedWith('a')
    }
    "parse only letters" in {
      alpha("11".toStream) should beAFailure
      alpha.parse("aa".toStream) should beAFailure
      alpha.parse("1".toStream) should beAFailure
    }
  }

  "alnum parser" should {
    "parse a letter or number" in {
      alnum("a11".toStream) should beSuccessful
      alnum("11".toStream) should beSuccessful
      alnum.parse("a".toStream) should succeedWith('a')
      alnum.parse("1".toStream) should succeedWith('1')
    }
    "parse only letters and numbers" in {
      alnum("^11".toStream) should beAFailure
      alnum.parse("aa".toStream) should beAFailure
    }
  }

  "whitespace parser" should {
    "parse whitespace" in {
      whitespace(" asdf".toStream) should beSuccessful
      whitespace("\t".toStream) should beSuccessful
      whitespace("\r".toStream) should beSuccessful
      whitespace.parse("\n".toStream) should beSuccessful
      whitespace(" \t\r\n".toStream) should beSuccessful
    }
    "parse only whitespace" in {
      whitespace("x".toStream) should beAFailure
    }
  }

  "str parser" should {
    "parse strings" in {
      str("asdf")("asdf".toStream) should beSuccessful
    }
    "fail on an empty stream" in {
      str("asdf")("".toStream) should beAFailure
    }
  }

  "optional parser" should {
    "succeed when input is present" in {
      (str("asdf")?).parse("asdf".toStream) should succeedWith("asdf".some)
    }
    "succeed when input is not present" in {
      (str("asdf")?).parse("".toStream) should succeedWith(None)
    }
  }

  "either parser" should {
    val p = (digit | str("asdf"))
    "succeed on the left" in {
      p.parse("1".toStream) should succeedWith('1'.left[String])
    }
    "succeed on the right" in {
      p.parse("asdf".toStream) should succeedWith("asdf".right[Char])
    }
    "fail if neither found" in {
      p.parse("qwer".toStream) should beAFailure
    }
  }

  "either either either parser" should {
    val p = (str("qwer") ||| str("asdf"))
    "succeed on the left" in {
      p.parse("qwer".toStream) should succeedWith("qwer")
    }
    "succeed on the right" in {
      p.parse("asdf".toStream) should succeedWith("asdf")
    }
    "fail if neither found" in {
      p.parse("".toStream) should beAFailure
    }
  }

  "star parser" should {
    val p = (str("qwer")*)
    "match zero items" in {
      p.parse("".toStream) should succeedWith(List[String]())
    }
    "match one item" in {
      p.parse("qwer".toStream) should succeedWith(List("qwer"))
    }
    "match more items" in {
      p.parse("qwerqwerqwerqwer".toStream) should succeedWith(List("qwer", "qwer", "qwer", "qwer"))
    }
  }

  "plus parser" should {
    val p = (str("qwer")+)
    "fail with zero items" in {
      p.parse("".toStream) should beAFailure
    }
    "match one item" in {
      p.parse("qwer".toStream) should succeedWith(NonEmptyList("qwer"))
    }
    "match more items" in {
      p.parse("qwerqwerqwerqwer".toStream) should succeedWith(NonEmptyList("qwer", "qwer", "qwer", "qwer"))
    }
  }

  "ignore right parser" should {
    val p = (str("asdf") <~ str("qwer"))
    "fail unless both are found" in {
      p.parse("asdf".toStream) should beAFailure
      p.parse("qwer".toStream) should beAFailure
    }
    "succeed with left when both are present" in {
      p.parse("asdfqwer".toStream) should succeedWith("asdf")
    }
  }

  "ignore left parser" should {
    val p = (str("asdf") ~> str("qwer"))
    "fail unless both are found" in {
      p.parse("asdf".toStream) should beAFailure
      p.parse("qwer".toStream) should beAFailure
    }
    "succeed with left when both are present" in {
      p.parse("asdfqwer".toStream) should succeedWith("qwer")
    }
  }

  "times parser" should {
    val p = (str("asdf").times(3))
    "fail if too few are found" in {
      p.parse("asdfasdf".toStream) should beAFailure
      p.parse("asdf".toStream) should beAFailure
    }
    "succeed if enough present" in {
      p.parse("asdfasdfasdf".toStream) should succeedWith(Seq("asdf", "asdf", "asdf"))
    }
    "fail if too many present" in {
      p.parse("asdfasdfasdfasdf".toStream) should beAFailure
    }
  }

  "parsers" should {
    "be a Functor" in {
      val d : Parser[Char] = digit
      val ds : Parser[String] = d.map(_.toString)
      ds.parse("1".toStream) should succeedWith("1")
    }

    "be Applicative" in {
      val s = str("asdf")
      val d = digit
      case class Fluffy(a: String, b: Char)
      val p: Parser[Fluffy] = ^(s, d)(Fluffy)
      p.parse("asdf1".toStream) should succeedWith(Fluffy("asdf", '1'))
    }

    "be a Monad" in {
      case class Huffy(a: Char, b: Char, c: String)
      val p : Parser[Huffy] = for (
        a <- digit;
        b <- digit;
        s <- str("asdf")
      ) yield (Huffy(a, b, s))
        
      p.parse("11asdf".toStream) should succeedWith(Huffy('1', '1', "asdf"))
    }

    "be Pointed" in {
      val p : Parser[Double] = 1.2.point[Parser]
      p.parse(Stream.Empty) should succeedWith(1.2)
    }
  }
}
