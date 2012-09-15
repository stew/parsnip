import scalaz._; import Scalaz._

import scala.collection.LinearSeq

package object parsnip {
  type ParsnipResult[+A] = ParsnipError \/ A
  type ParseResult[+A] = ParsnipResult[(Input, A)]
}

package parsnip {
  case class Input(input: LinearSeq[Char],
                   line: Int,
                   col: Int) {
    def head: Char = input.head
    def tail: Input = 
      if (input.head == '\n')
        Input(input.tail, line+1, 0)
      else
        Input(input.tail, line, col+1)
    def isEmpty = input.isEmpty
    def startsWith(a: String) = input.startsWith(a)
    def drop(n: Int): Input = if(n == 0 ) this else this.tail.drop(n-1)
  }

  object Input {
    implicit def fromString(a: String) : Input = Input(a.toStream, 0, 0)
    implicit def fromString(a: LinearSeq[Char]) : Input = Input(a, 0, 0)
  }


  trait ParsnipError

  case class UnexpectedChar(input: Input) extends ParsnipError
  case class UnexpectedString(input: Input) extends ParsnipError
  case class UnconsumedInput(str: String) extends ParsnipError
  case class UnexpectedEnd() extends ParsnipError
  case class UnexpectedError(str: String) extends ParsnipError

  trait Parser[+A] extends (Input => ParseResult[A]) { self =>
    /**
     * create a parser which succeeds with Some[A] if this parser succeeds or
     * succeeds with None if this parser Fails
     */
    def ? = Parser[Option[A]] { input =>
      self(input) fold(
        { e => (input, None).right},
        { case(i,s) => (i, Some(s)).right })
    }

    /**
     * given a Parser[B], return either a Success[Right[A]], a Success[Left[B]] or a Failure
     */
    def |[B](pb: Parser[B]) = Parser[A \/ B] { input =>
      self(input) fold (
        { e => pb(input).map(r => (r._1, r._2.right)) },
        { case (i, s)=> (i, s.left[B]).right})
    }

    /**
     * given a Parser[B] return a Parser[C] where C is a supertype of both A and B
     */
    def |||[B, C](pb: Parser[B])(implicit ev1: <:<[B, C], ev2: <:<[A, C]) = Parser[C] { input =>
      self(input) fold (
        { e => pb(input).map(r => (r._1, r._2)) },
        { case (i, s) => (i, s:C).right })
    }

    /**
     * return a Parser[List[A]] which uses this parser to match zero or more times
     */
    def * : Parser[List[A]] = Parser[List[A]] { input =>
      self(input) fold (
        { _ => (input, List()).right },
        { case (i, a) => *(i) map (s => (s._1, a +: s._2)) })
    }


    /**
     * return a Parser[NonEmptyList[A]] which uses this parser to match one or more times
     */
    def + : Parser[NonEmptyList[A]] = ^(self, self.*)(NonEmptyList.nel)

    /**
     * given a Parser[B] create a parser which succeeds only of both parsers suceed, but
     * discard the output of the given parser
     */
    def <~[B](pb: Parser[B]): Parser[A] = ^(self, pb) { (a, _) => a }

    /**
     * given a Parser[B] create a parser which succeeds only of both parsers suceed, but
     * discard the output of this parser, returning on the output of the given parser
     */
    def ~>[B](pb: Parser[B]): Parser[B] = ^(self, pb) { (_, b) => b }

    /**
     * create a Parser[Seq[A]] which uses this parser exactly n times
     */
    def times(n: Int): Parser[Seq[A]] =
      if (n == 0) Parser.value(Seq())
      else ^(self, times(n - 1)) { (a, b) => a +: b }

    def flatMap[B](f: A=>Parser[B]) = Parser[B] { x =>
        self(x) fold (
          {f => f.left},
          { case (i, a) => f(a)(i) })
    }

    def ++[AA](next: Parser[AA])(implicit append: Semigroup[AA], ev: <:<[A,AA]) : Parser[AA] = 
      ^(self, next){ (a,b) => append.append(a,b)}

    /**
     * parse the given input, and return Success only if parsing is successful and all
     * of the input is consumed
     */
    def parse(input: Input): ParsnipError \/ A = 
      self(input) fold (
        {f => f.left},
        {case (i,s) => if(i.isEmpty) s.right else UnconsumedInput(i.toString).left })
   }

  object Parser {
    def apply[A](f: Input => ParsnipError \/ (Input,A)): Parser[A] = new Parser[A] {
      def apply(input: Input): ParsnipError \/ (Input,A) = f(input)
    }

    def fail[A](e: ParsnipError) = Parser[A] { _ => e.left }

    def charAccept(accept: (Char => Boolean)) = Parser { input =>
      if (input.isEmpty)
        UnexpectedEnd().left
      else if (!input.isEmpty && accept(input.head))
        (input.tail, input.head).right
      else
        UnexpectedChar(input).left
    }

    def accept[A](accept: PartialFunction[Char, A]) = Parser { input =>
      if (input.isEmpty)
        UnexpectedEnd().left
      else if (accept.isDefinedAt(input.head))
        (input.tail, accept.apply(input.head)).right
      else
        UnexpectedChar(input).left
    }

    def char(c: Char) = Parser { input =>
      if (input.isEmpty)
        UnexpectedEnd().left
      else if(c == input.head) (input.tail, input.head).right
      else UnexpectedChar(input).left
    }

    def isHex(c: Char) =
      c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    def value[A](a: A): Parser[A] = Parser[A] { input => (input, a).right }

    val digit = charAccept(_.isDigit)
    val alpha = charAccept(_.isLetter)
    val alnum = charAccept(_.isLetterOrDigit)
    val hexDigit = charAccept(isHex(_))
    val whitespace = charAccept(_.isWhitespace)

    def str(a: String): Parser[String] = Parser { input =>
      if (input.startsWith(a)) {
        (input.drop(a.length), a).right[ParsnipError]
      } else {
        UnexpectedString(input).left
      }
    }

    // todo: free monad?
    def seprep[A,B](rep: Parser[A], sep: Parser[B]) : Parser[List[A]] = (sep ~> rep).*
    def repsep[A,B](rep: Parser[A], sep: Parser[B]) : Parser[List[A]] = 
      ^(rep, seprep(rep,sep))(_ :: _) ||| value(List[A]())

    implicit def strParser(s: String): Parser[String] = str(s)
    implicit def charParser(c: Char): Parser[Char] = char(c)


      implicit def parserMonoid[A: Monoid] : Monoid[Parser[A]] = new Monoid[Parser[A]] {
        def append(a: Parser[A], aa: => Parser[A]) = a ++ aa
        def zero: Parser[A] = (Monoid[A].zero).point[Parser]
      }

     implicit val parserInstance: Monad[Parser] = new Monad[Parser] {
       override def point[A](a: => A): Parser[A] = Parser.value(a)
       override def bind[A, B](fa: Parser[A])(f: A => Parser[B]) = fa flatMap f
     }
  }
}
