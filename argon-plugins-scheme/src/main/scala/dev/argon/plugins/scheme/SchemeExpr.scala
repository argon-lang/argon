package dev.argon.plugins.scheme

import scala.math.BigInt
import dev.argon.plugins.scheme.SchemeExpr.Number.NegativeNaN
import zio.*
import zio.stream.*

private[scheme] enum SchemeExpr {
  case Boolean(value: scala.Boolean)
  case Char(c: Int)
  case Symbol(name: java.lang.String)
  case String(value: java.lang.String)
  case Num(n: SchemeExpr.Number)
  case List(values: Seq[SchemeExpr], markedValue: Option[SchemeExpr])
  case Abbreviation(abbrev: SchemeExpr.AbbreviationType, datum: SchemeExpr)
  case Vector(values: Seq[SchemeExpr])
  case ByteVector(values: Array[Byte])

  def writeTo(write: java.lang.String => UIO[Unit]): UIO[Unit] =
    this match {
      case Boolean(value) =>
        write(if value then "#t" else "#f")

      case Char(c) =>
        for
          _ <- write("#\\")
          _ <- c match {
            case '\u0007' => write("alarm")
            case '\b' => write("backspace")
            case '\u007F' => write("delete")
            case '\u001B' => write("esc")
            case '\n' => write("linefeed")
            case '\u000C' => write("page")
            case '\r' => write("return")
            case ' ' => write("space")
            case '\t' => write("tab")
            case '\u000B' => write("vtab")
            case _ => write(Character.toString(c))
          }
        yield ()

      case Symbol(name) =>
        import SchemeExpr.{isInitialChar, isSubsequentChar}

        def isPeculiarIdentifier(name: java.lang.String): scala.Boolean =
          name == "+" || name == "-" || name == "..." || name.startsWith("->")

          
        def writeInitial(c: Int): UIO[Unit] =
          if isInitialChar(c) then
            write(Character.toString(c))
          else
            write("\\x%X;".formatted(c))


        def writeSubsequent(c: Int): UIO[Unit] =
          if isSubsequentChar(c) then
            write(Character.toString(c))
          else
            write("\\x%X;".formatted(c))

        if isPeculiarIdentifier(name) then
          eachCodePoint(name)(writeSubsequent)
        else
          val first = name.codePointAt(0)
          var nextIndex = Character.charCount(first)
          writeInitial(first) *>
            eachCodePoint(name, startIndex = nextIndex)(writeSubsequent)
        end if


      case String(value) =>
        for
          _ <- write("\"")
          _ <- eachCodePoint(value) {
            case '\u0007' => write("\\a")
            case '\b' => write("\\b")
            case '\t' => write("\\t")
            case '\n' => write("\\n")
            case '\u000B' => write("\\v")
            case '\u000C' => write("\\f")
            case '\r' => write("\\r")
            case '"' => write("\\\"")
            case '\\' => write("\\\\")
            case c if Character.isISOControl(c) =>
              write("\\x")
              write(Integer.toHexString(c))
              write(";")

            case c =>
              write(Character.toString(c))
          }
          _ <- write("\"")
        yield ()

      case Num(n) =>
        import SchemeExpr.Number

        def isNeg(n: Number): scala.Boolean =
          n match {
            case Number.Complex(real, _) => isNeg(real)
            case Number.Rational(num, _) => isNeg(num)
            case Number.ExactReal(dec) => dec < 0
            case Number.Flonum(f) => f < 0
            case Number.NegativeNaN() => true
            case Number.Integer(n) => n < 0
          }

        def writeNum(n: Number): UIO[Unit] =
          n match {
            case Number.Complex(real, imag) =>
              writeNum(real) *> write("+").whenDiscard(!isNeg(imag)) *> writeNum(imag)

            case Number.Rational(num, den) =>
              writeNum(num) *> write("/") *> writeNum(den)

            case Number.ExactReal(dec) =>
              write(dec.toString()) *> write("e0")

            case Number.Flonum(f) if f.isNaN =>
              write("+nan.0")

            case Number.Flonum(f) =>
              write(f.toString)

            case Number.NegativeNaN() =>
              write("-nan.0")

            case Number.Integer(i) =>
              write(i.toString)
          }

        writeNum(n)

      case List(values, markedValue) =>
        write("(") *>
          ZIO.foreachDiscard(values.view.zipWithIndex) { (v, i) =>
            write(" ").whenDiscard(i > 0) *> v.writeTo(write)
          } *>
          write(")")

      case Abbreviation(abbrev, datum) =>
        import SchemeExpr.AbbreviationType

        write(abbrev match {
          case AbbreviationType.Quote => "'"
          case AbbreviationType.QuasiQuote => "`"
          case AbbreviationType.Unquote => ","
          case AbbreviationType.UnquoteSplicing => ",@"
          case AbbreviationType.Syntax => "#'"
          case AbbreviationType.QuasiSyntax => "#`"
          case AbbreviationType.Unsyntax => "#,"
          case AbbreviationType.UnsyntaxSplicing => "#,@"
        }) *>
          datum.writeTo(write)

      case Vector(values) =>
        write("#(") *>
          ZIO.foreachDiscard(values.view.zipWithIndex) { (v, i) =>
            write(" ").whenDiscard(i > 0) *> v.writeTo(write)
          } *>
          write(")")

      case ByteVector(values) =>
        write("#vu8(") *>
          ZIO.foreachDiscard(values.view.zipWithIndex) { (b, i) =>
            write(" ").whenDiscard(i > 0) *> write(java.lang.Byte.toUnsignedInt(b).toString)
          } *>
          write(")")
    }
    
  private def eachCodePoint(s: java.lang.String, startIndex: Int = 0)(f: Int => UIO[Unit]): UIO[Unit] =
    def iter(i: Int): UIO[Unit] =
      if i < s.length() then
        val c = s.codePointAt(i)
        f(c) *> iter(i + Character.charCount(c))
      else
        ZIO.unit
      end if

    iter(startIndex)
  end eachCodePoint

}

object SchemeExpr {
  sealed trait Number
  object Number {
    sealed trait Real extends Number

    final case class Complex(real: Real, imag: Real) extends Number
    final case class Rational(numerator: Integer, denominator: Integer) extends Real
    final case class ExactReal(value: BigDecimal) extends Real
    final case class Flonum(value: Double) extends Real
    final case class NegativeNaN() extends Real
    final case class Integer(value: BigInt) extends Real
  }

  enum AbbreviationType derives CanEqual {
    case Quote, QuasiQuote, Unquote, UnquoteSplicing
    case Syntax, QuasiSyntax, Unsyntax, UnsyntaxSplicing
  }


  def call(name: java.lang.String, args: SchemeExpr*): SchemeExpr =
    SchemeExpr.List(SchemeExpr.Symbol(name) +: args, None)


  private def isWhitespace(c: Int): scala.Boolean =
    c == '\t' || // <character tabulation>
      c == '\n' || // <linefeed>
      c == '\r' || // <carriage return>
      c == 0xB || // <line tabulation>
      c == '\f' || // <form feed>
      c == ' ' || // <space>
      c == 0x85 || // <next line>
      c == 0x2028 || // <line separator>
      c == 0x2029 // <paragraph separator>

  private def isNonCRLineEnding(c: Int): scala.Boolean =
    c == '\n' || // <linefeed>
      c == 0x85 // <next line>

  private def isDelimiter(c: Int): scala.Boolean =
    isWhitespace(c) ||
      c == '(' ||
      c == ']' ||
      c == '"' ||
      c == ';' ||
      c == '#'

  private val initialCategories = Set[Int](
      Character.UPPERCASE_LETTER, // Lu
      Character.LOWERCASE_LETTER, // Ll
      Character.TITLECASE_LETTER, // Lt
      Character.MODIFIER_LETTER,  // Lm
      Character.OTHER_LETTER,     // Lo
      Character.NON_SPACING_MARK, // Mn
      Character.LETTER_NUMBER,    // Nl
      Character.OTHER_NUMBER,     // No
      Character.DASH_PUNCTUATION, // Pd
      Character.CONNECTOR_PUNCTUATION, // Pc
      Character.OTHER_PUNCTUATION, // Po
      Character.CURRENCY_SYMBOL,  // Sc
      Character.MATH_SYMBOL,      // Sm
      Character.MODIFIER_SYMBOL,  // Sk
      Character.OTHER_SYMBOL,     // So
      Character.PRIVATE_USE,      // Co
  )

  private val subsequentCategories = Set[Int](
      Character.COMBINING_SPACING_MARK, // Mc
      Character.ENCLOSING_MARK,   // Me
      Character.DECIMAL_DIGIT_NUMBER, // Nd
  )

  def isInitialChar(c: Int): scala.Boolean =
    if java.lang.Integer.compareUnsigned(c, 127) > 0 then
      initialCategories.contains(Character.getType(c))
    else
      (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        "!$%&*/:<=>?~_^".contains(c.toChar)

  def isSubsequentChar(c: Int): scala.Boolean =
    isInitialChar(c) ||
      (c >= '0' && c <= '9') ||
      (java.lang.Integer.compareUnsigned(c, 255) <= 0 && ".+-@".contains(c.toChar)) ||
      subsequentCategories.contains(Character.getType(c))
}
