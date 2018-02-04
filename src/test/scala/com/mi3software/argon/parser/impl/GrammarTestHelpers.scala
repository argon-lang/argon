package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scalaz._
import Scalaz._

trait GrammarTestHelpers {

  protected type TGrammar = Grammar[Int, WithSource[String], Int]

  protected implicit val errorFactory: Grammar.ErrorFactory[Int, String, WithSource[String]] = new Grammar.ErrorFactory[Int, String, WithSource[String]] {
    override def createError(error: GrammarError[Int, String]): WithSource[String] =
      WithSource(s"$error", error.location)

    override def createAmbiguityError(location: SourceLocation): WithSource[String] =
      WithSource("Ambiguity", location)

    override def errorEndLocationOrder: Order[WithSource[String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  protected def numberToken(n: Int): TGrammar = Grammar.matcher(n.toString, m => Some(m).filter(_ === n))

  protected val pos = FilePosition(0, 0)

  protected def ws[T](t: T) = WithSource(t, SourceLocation.empty)

}
