package dev.argon.parser

import dev.argon.util.{WithLocation, WithSource}
import zio.stream.{ZChannel, ZStream}
import zio.{Chunk, Scope, ZIO}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object ArgonLexerTests extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("Lexer")(
      suite("Keywords")(
        test("Let Keyword") {
          for
            tokens <- lex("let")
          yield assertTrue(tokens == Seq(Token.KW_LET))
        },
        test("Let Prefix Letter") {
          for
            tokens <- lex("lets")
          yield assertTrue(tokens == Seq(Token.Identifier("lets")))
        },
        test("Let Prefix !") {
          for
            tokens <- lex("let!")
          yield assertTrue(tokens == Seq(Token.Identifier("let!")))
        },
        test("Let Keyword, Identifier") {
          for
            tokens <- lex("let s")
          yield assertTrue(tokens == Seq(Token.KW_LET, Token.Identifier("s")))
        },
        test("Box Keyword") {
          for
            tokens <- lex("box")
          yield assertTrue(tokens == Seq(Token.KW_BOX))
        },
        test("Boxed Keyword") {
          for
            tokens <- lex("boxed")
          yield assertTrue(tokens == Seq(Token.KW_BOXED))
        },
      ),
      suite("Operators")(
        test("Plus") {
          for
            tokens <- lex("+")
          yield assertTrue(tokens == Seq(Token.OP_PLUS))
        },
        test("Minus") {
          for
            tokens <- lex("-")
          yield assertTrue(tokens == Seq(Token.OP_MINUS))
        },
        test("Concat") {
          for
            tokens <- lex("++")
          yield assertTrue(tokens == Seq(Token.OP_CONCAT))
        },
        test("Double Plus") {
          for
            tokens <- lex("+ +")
          yield assertTrue(tokens == Seq(Token.OP_PLUS, Token.OP_PLUS))
        },
      ),
      suite("Integers")(
        test("Simple Decimal Integer") {
          for
            tokens <- lex("123")
          yield assertTrue(tokens == Seq(Token.IntToken(123)))
        },
        test("Decimal Integer with leading zero") {
          for
            tokens <- lex("0123")
          yield assertTrue(tokens == Seq(Token.IntToken(123)))
        },
        test("Negative Decimal Integer") {
          for
            tokens <- lex("-123")
          yield assertTrue(tokens == Seq(Token.OP_MINUS, Token.IntToken(123)))
        },
        test("Invalid Decimal Integer") {
          for
            tokens <- lex("123A").either
          yield assertTrue(tokens.swap.toOption.get.isInstanceOf[SyntaxError.InvalidIntegerToken])
        },
        test("Binary Integer Lowercase") {
          for
            tokens <- lex("0b101010")
          yield assertTrue(tokens == Seq(Token.IntToken(0b101010)))
        },
        test("Binary Integer Uppercase B") {
          for
            tokens <- lex("0B101010")
          yield assertTrue(tokens == Seq(Token.IntToken(0b101010)))
        },
        test("Negative Binary Integer") {
          for
            tokens <- lex("-0b101010")
          yield assertTrue(tokens == Seq(Token.OP_MINUS, Token.IntToken(0b101010)))
        },
        test("Invalid Binary Integer") {
          for
            tokens <- lex("0b101010A").either
          yield assertTrue(tokens.swap.toOption.get.isInstanceOf[SyntaxError.InvalidIntegerToken])
        },
        test("Octal Integer") {
          for
            tokens <- lex("0o1234567")
          yield assertTrue(tokens == Seq(Token.IntToken(BigInt("1234567", 8))))
        },
        test("Negative Octal Integer") {
          for
            tokens <- lex("-0o1234567")
          yield assertTrue(tokens == Seq(Token.OP_MINUS, Token.IntToken(BigInt("1234567", 8))))
        },
        test("Invalid Octal Integer") {
          for
            tokens <- lex("0o1234567A").either
          yield assertTrue(tokens.swap.toOption.get.isInstanceOf[SyntaxError.InvalidIntegerToken])
        },
        test("Hex Integer Lowercase") {
          for
            tokens <- lex("0x123456789abcdef")
          yield assertTrue(tokens == Seq(Token.IntToken(0x123456789abcdefL)))
        },
        test("Hex Integer Uppercase X") {
          for
            tokens <- lex("0X123456789abcdef")
          yield assertTrue(tokens == Seq(Token.IntToken(0x123456789abcdefL)))
        },
        test("Hex Integer Uppercase Digits") {
          for
            tokens <- lex("0x123456789ABCDEF")
          yield assertTrue(tokens == Seq(Token.IntToken(0x123456789ABCDEFL)))
        },
        test("Hex Integer Uppercase X and Digits") {
          for
            tokens <- lex("0X123456789ABCDEF")
          yield assertTrue(tokens == Seq(Token.IntToken(0x123456789ABCDEFL)))
        },
        test("Negative Hex Integer") {
          for
            tokens <- lex("-0x123456789abcdef")
          yield assertTrue(tokens == Seq(Token.OP_MINUS, Token.IntToken(0x123456789abcdefL)))
        },
        test("Invalid Hex Integer") {
          for
            tokens <- lex("0x123456789abcdefG").either
          yield assertTrue(tokens.swap.toOption.get.isInstanceOf[SyntaxError.InvalidIntegerToken])
        },
      ),
      suite("Identifiers")(
        test("Simple Identifier") {
          for
            tokens <- lex("s")
          yield assertTrue(tokens == Seq(Token.Identifier("s")))
        },
        test("Identifier with digits") {
          for
            tokens <- lex("s1")
          yield assertTrue(tokens == Seq(Token.Identifier("s1")))
        },
        test("Identifier with underscore in middle") {
          for
            tokens <- lex("some_name")
          yield assertTrue(tokens == Seq(Token.Identifier("some_name")))
        },
        test("Identifier starting with underscore (not lone underscore)") {
          for
            tokens <- lex("_value1")
          yield assertTrue(tokens == Seq(Token.Identifier("_value1")))
        },
        test("Lone underscore is keyword _") {
          for
            tokens <- lex("_")
          yield assertTrue(tokens == Seq(Token.KW_UNDERSCORE))
        },
        test("Identifier ending with question mark") {
          for
            tokens <- lex("isEmpty?")
          yield assertTrue(tokens == Seq(Token.Identifier("isEmpty?")))
        },
        test("Identifier ending with exclamation mark") {
          for
            tokens <- lex("bang!")
          yield assertTrue(tokens == Seq(Token.Identifier("bang!")))
        },
        test("Only one trailing terminator; following ! is operator") {
          for
            tokens <- lex("a?!")
          yield assertTrue(tokens == Seq(Token.Identifier("a?"), Token.OP_LOGICAL_NOT))
        },
        test("Identifier in pipes") {
          for
            tokens <- lex("|a|")
          yield assertTrue(tokens == Seq(Token.SYM_PIPE, Token.Identifier("a"), Token.SYM_PIPE))
        }
      ),
      suite("Strings")(
        test("String Start") {
          for
            tokens <- lex("\"")
          yield assertTrue(tokens == Seq(Token.StringStart))
        },
        test("Simple String") {
          for
            tokens <- lexString("abc\"")
          yield assertTrue(tokens == Seq(Token.StringText("abc"), Token.StringEnd))
        },
        test("String with escaped characters") {
          for
            tokens <- lexString("abc\\ndef\"")
          yield assertTrue(tokens == Seq(Token.StringText("abc"), Token.StringText("\n"), Token.StringText("def"), Token.StringEnd))
        },
        test("String interpolation with variable") {
          for
            tokens <- lexString("a #{")
          yield assertTrue(tokens == Seq(Token.StringText("a "), Token.StringInterpolationStart))
        },
      ),
      suite("New Lines")(
        test("Simple New Line") {
          for
            tokens <- lex("\n")
          yield assertTrue(tokens == Seq(Token.NewLine))
        },
      ),
    )

  private def lex(s: String): ZIO[Any, Any, Seq[Token]] =
    ZIO.scoped(
      for
        textReader <- StreamTextReader.make(ZStream(s))
        lexer <- ArgonLexer.make(None)(textReader)
        tokens <- ZStream.repeatZIO(lexer.nextToken(LexerMode.Normal))
          .collectWhile {
            case WithLocation(token, _) => token
          }
          .runCollect
      yield tokens
    )

  private def lexString(s: String): ZIO[Any, Any, Seq[Token]] =
    ZIO.scoped(
      for
        textReader <- StreamTextReader.make(ZStream(s))
        lexer <- ArgonLexer.make(None)(textReader)
        tokens <- ZStream.repeatZIO(lexer.nextToken(LexerMode.StringText))
          .collectWhile {
            case WithLocation(token, _) => token
          }
          .runCollect
      yield tokens
    )

}
