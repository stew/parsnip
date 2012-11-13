package parsnip

import Parser._

import scalaz._; import Scalaz._

import org.specs2.mutable._
import org.specs2.matcher._


class ParsnipSpec extends Specification with ResultMatchers {

  /** success matcher for a \/ */
  def beSuccessful[E, A]: Matcher[\/[E, A]] = (v: \/[E, A]) => (v.fold(_ => false, _ => true), v + " successful", v + " is not successfull")

  /** failure matcher for a \/ */
  def beAFailure[E, A]: Matcher[\/[E, A]] = (v: \/[E, A]) => (v.fold(_ => true, _ => false), v + " is a failure", v + " is not a failure")

  /** success matcher for a Validation with a specific value */
  def succeedWith[E, A](a: => A) = validationWith[E, A](a.right)

  /** failure matcher for a Validation with a specific value */
  def failWith[E, A](e: => E) = validationWith[E, A](e.left)

  private def validationWith[E, A](f: => \/[E, A]): Matcher[\/[E, A]] = (v: \/[E, A]) => {
    val expected = f
    (expected == v, v + " is a " + expected, v + " is not aa " + expected)
  }

  "digit parser" should {
    "parse a digit" in {

      digit("11") should beSuccessful
      digit.parse("1") should succeedWith('1')
    }

    "parse only digits" in {
      digit("a11") should beAFailure
      digit.parse("a") should beAFailure
      digit.parse("11") should beAFailure
    }
  }

  "alpha parser" should {
    "parse a letter" in {

      alpha("a11") should beSuccessful
      alpha.parse("a") should succeedWith('a')
    }
    "parse only letters" in {
      alpha("11") should beAFailure
      alpha.parse("aa") should beAFailure
      alpha.parse("1") should beAFailure
    }
  }

  "alnum parser" should {
    "parse a letter or number" in {
      alnum("a11") should beSuccessful
      alnum("11") should beSuccessful
      alnum.parse("a") should succeedWith('a')
      alnum.parse("1") should succeedWith('1')
    }
    "parse only letters and numbers" in {
      alnum("^11") should beAFailure
      alnum.parse("aa") should beAFailure
    }
  }

  "whitespace parser" should {
    "parse whitespace" in {
      whitespace(" asdf") should beSuccessful
      whitespace("\t") should beSuccessful
      whitespace("\r") should beSuccessful
      whitespace.parse("\n") should beSuccessful
      whitespace(" \t\r\n") should beSuccessful
    }
    "parse only whitespace" in {
      whitespace("x") should beAFailure
    }
  }

  "str parser" should {
    "parse strings" in {
      str("asdf")("asdf") should beSuccessful
    }
    "fail on an empty stream" in {
      str("asdf")("") should beAFailure
    }
  }

  "optional parser" should {
    "succeed when input is present" in {
      (str("asdf")?).parse("asdf") should succeedWith("asdf".some)
    }
    "succeed when input is not present" in {
      (str("asdf")?).parse("") should succeedWith(None)
    }
  }

  "either parser" should {
    val p = (digit | str("asdf"))
    "succeed on the left" in {
      p.parse("1") should succeedWith('1'.left[String])
    }
    "succeed on the right" in {
      p.parse("asdf") should succeedWith("asdf".right[Char])
    }
    "fail if neither found" in {
      p.parse("qwer") should beAFailure
    }
  }

  "either either either parser" should {
    val p = (str("qwer") ||| str("asdf"))
    "succeed on the left" in {
      p.parse("qwer") should succeedWith("qwer")
    }
    "succeed on the right" in {
      p.parse("asdf") should succeedWith("asdf")
    }
    "fail if neither found" in {
      p.parse("") should beAFailure
    }
  }

  "star parser" should {
    val p = (str("qwer")*)
    "match zero items" in {
      p.parse("") should succeedWith(List[String]())
    }
    "match one item" in {
      p.parse("qwer") should succeedWith(List("qwer"))
    }
    "match more items" in {
      p.parse("qwerqwerqwerqwer") should succeedWith(List("qwer", "qwer", "qwer", "qwer"))
    }
  }

  "plus parser" should {
    val p = (str("qwer")+)
    "fail with zero items" in {
      p.parse("") should beAFailure
    }
    "match one item" in {
      p.parse("qwer") should succeedWith(NonEmptyList("qwer"))
    }
    "match more items" in {
      p.parse("qwerqwerqwerqwer") should succeedWith(NonEmptyList("qwer", "qwer", "qwer", "qwer"))
    }
  }

  "ignore right parser" should {
    val p = (str("asdf") <~ str("qwer"))
    "fail unless both are found" in {
      p.parse("asdf") should beAFailure
      p.parse("qwer") should beAFailure
    }
    "succeed with left when both are present" in {
      p.parse("asdfqwer") should succeedWith("asdf")
    }
  }

  "ignore left parser" should {
    val p = (str("asdf") ~> str("qwer"))
    "fail unless both are found" in {
      p.parse("asdf") should beAFailure
      p.parse("qwer") should beAFailure
    }
    "succeed with left when both are present" in {
      p.parse("asdfqwer") should succeedWith("qwer")
    }
  }

  "times parser" should {
    val p = (str("asdf").times(3))
    "fail if too few are found" in {
      p.parse("asdfasdf") should beAFailure
      p.parse("asdf") should beAFailure
    }
    "succeed if enough present" in {
      p.parse("asdfasdfasdf") should succeedWith(Seq("asdf", "asdf", "asdf"))
    }
    "fail if too many present" in {
      p.parse("asdfasdfasdfasdf") should beAFailure
    }
  }

  "repsep" should { 
    val p = repsep[String,Char]("asdf",',')
    "parse an empty list" in {
      p.parse(Stream.empty) should succeedWith(List[String]())
    }
    "parse a single rep" in {
      p.parse("asdf") should succeedWith(List("asdf"))
    }
    "parse many reps" in {
      p.parse("asdf,asdf,asdf,asdf") should succeedWith(List("asdf","asdf","asdf","asdf"))
    }
  }

  "parsers" should {
    "be Pointed" in {
      val p : Parser[Double] = 1.2.point[Parser]
      p.parse(Stream.Empty) should succeedWith(1.2)
    }
    
    "be a Semigroup" in {
      (str("a") |+| str("b")).parse("ab") should succeedWith("ab")
    }

    "be a Functor" in {
      val d : Parser[Char] = digit
      val ds : Parser[String] = d.map { c: Char => c.toString }
      d.parse("1") should succeedWith('1')
      ds.parse("1") should succeedWith("1")
    }

    "have an Apply instance" in {
      val s = str("asdf")
      val d = digit
      case class Fluffy(a: String, b: Char)
      val p: Parser[Fluffy] = ^(s, d)(Fluffy)
      p.parse("asdf1") should succeedWith(Fluffy("asdf", '1'))
    }

    "have an Applicative instance" in {
      val s = str("asdf")
      val d = digit
      case class Fluffy(a: String, b: Char)
      val p: Parser[Fluffy] = (s |@| d)(Fluffy)
      p.parse("asdf1") should succeedWith(Fluffy("asdf", '1'))
    }

    "have a Monad instance" in {
      case class Huffy(a: Char, b: Char, c: String)
      val p : Parser[Huffy] = for (
        a <- digit;
        b <- digit;
        s <- str("asdf")
      ) yield (Huffy(a, b, s))
        
      p.parse("11asdf") should succeedWith(Huffy('1', '1', "asdf"))
    }

  }
}
