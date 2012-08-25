import scalaz._; import Scalaz._

import scala.collection.LinearSeq

package object parsnip {
  type Input = LinearSeq[Char]
  type ParseResult[A] = Validation[ParsnipError, (Input, A)]
}

package parsnip {

  sealed trait ParsnipError

  case class UnexpectedChar(char: Char) extends ParsnipError
  case class UnexpectedString(str: String) extends ParsnipError
  case class UnconsumedInput(str: String) extends ParsnipError
  case class UnexpectedEnd() extends ParsnipError

  trait Parser[A] extends (Input => ParseResult[A]) { self =>
    /**
     * create a parser which succeeds with Some[A] if this parser succeeds or
     * succeeds with None if this parser Fails
     */
    def ? = Parser[Option[A]] { input =>
      self(input) match {
        case Failure(e) => Success((input, None))
        case Success((i, s)) => Success((i, Some(s)))
      }
    }

    /**
     * given a Parser[B], return either a Success[Right[A]], a Success[Left[B]] or a Failure
     */
    def |[B](pb: Parser[B]) = Parser[A \/ B] { input =>
      self(input) match {
        case Success((i, s)) => Success((i, s.left[B]))
        case Failure(e) => pb(input).map(r => (r._1, r._2.right))
      }
    }

    /**
     * given a Parser[B] return a Parser[C] where C is a supertype of both A and B
     */
    def |||[B,C](pb: Parser[B])(implicit ev1: <:<[B,C], ev2: <:<[A,C]) = Parser[C] { input =>
      self(input) match {
        case Success((i, s)) => Success((i, s))
        case Failure(e) => pb(input).map(r => (r._1, r._2))
      }
    }

    /**
     * return a Parser[List[A]] which uses this parser to match zero or more times
     */
    def * : Parser[List[A]] = Parser[List[A]] { input =>
      self(input) match {
        case Failure(_) => Success((input, List()))
        case Success((i, a)) =>
          *(i) map (s => (s._1, a +: s._2))
      }
    }

    /**
     * return a Parser[NonEmptyList[A]] which uses this parser to match one or more times
     */
    def + : Parser[NonEmptyList[A]] = ^(self , self.*)(NonEmptyList.nel)

    /**
     * given a Parser[B] create a parser which succeeds only of both parsers suceed, but
     * discard the output of the given parser
     */
    def <~[B](pb: Parser[B]): Parser[A] = ^(self, pb){(a,_) => a}

    /**
     * given a Parser[B] create a parser which succeeds only of both parsers suceed, but
     * discard the output of this parser, returning on the output of the given parser
     */
    def ~>[B](pb: Parser[B]): Parser[B] = ^(self, pb){(_,b) => b}

    /**
     * create a Parser[Seq[A]] which uses this parser exactly n times
     */
    def times(n: Int): Parser[Seq[A]] = 
      if(n == 0) Parser.value(Seq())
     else ^(self, times(n-1)){(a,b) => a +: b}

    /**
     * parse the given input, and return Success only if parsing is successful and all
     * of the input is consumed
     */
    def parse(input: Input) : Validation[ParsnipError, A] = self(input) match {
        case Success((i, s)) if(i.isEmpty) => s.success
        case Success((i, s)) => UnconsumedInput(i.toString).failure
        case Failure(f) => Failure(f)
    }
  }

  object Parser {
    def apply[A](f: Input => ParseResult[A]): Parser[A] = new Parser[A] {
      def apply(input: Input): ParseResult[A] = f(input)
    }

    def charAccept(accept: (Char => Boolean)) = Parser { input =>
      if (input.isEmpty)
        UnexpectedEnd().failure
      else if (!input.isEmpty && accept(input.head))
        (input.tail, input.head).success
     else
        UnexpectedChar(input.head).failure
    }

    def value[A](a: A) : Parser[A] = Parser[A]{ input => (input,a).success }

    val digit = charAccept(_.isDigit)
    val alpha = charAccept(_.isLetter)
    val alnum = charAccept(_.isLetterOrDigit)
    val whitespace = charAccept(_.isWhitespace)

    def str(a: String): Parser[String] = Parser { input =>
      if(input.startsWith(a)) {
        (input.drop(a.length), a).success
      } else {
        UnexpectedString(input.take(5).toString).failure
      }
    }

    implicit val parserInstance: Monad[Parser] with Applicative[Parser] = new Monad[Parser] with Applicative[Parser] with Apply[Parser] {
      def point[A](a: => A) : Parser[A] = Parser.value(a)

      override def map[A, B](fa: Parser[A])(f: A => B) = Parser[B] { x =>
        fa(x) match {
          case Success((i, a)) => Success((i, f(a)))
          case Failure(f) => f.failure
        }
      }
      def bind[A, B](fa: Parser[A])(f: A => Parser[B]) = Parser[B] { x =>
        fa(x) match {
          case Success((i, a)) => f(a)(i)
          case Failure(f) => f.failure
        }
      }
    }
  }
}
