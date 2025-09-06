package dev.argon.parser

import dev.argon.parser.*
import dev.argon.util.{*, given}

import scala.language.postfixOps
import zio.{Chunk, UIO, ZIO}
import dev.argon.parser
import dev.argon.parser.TokenLexer.State

private[parser] object ArgonLexer {

  private final class MutLexer[R, E >: SyntaxError](fileName: Option[String])(reader: TextReader[R, E]) extends Lexer[R, E] {

    private var accText = Cord.empty
    private var tokenStartPos = FilePosition(1, 1)
    private var lineNumber = 1
    private var linePosition = 1
    private var currentText: String = ""
    private var currentTextOffset = 0
    private var nextText: Chunk[String] = Chunk.empty
    private var hasEof = false

    private def currentPosition: FilePosition = FilePosition(lineNumber, linePosition)
    private def currentTokenLocation: SourceLocation = Location(fileName, tokenStartPos, currentPosition)

    override def nextToken(mode: LexerMode): ZIO[R, E, WithSource[Token] | FilePosition] =
      mode match {
        case LexerMode.Normal => NormalTokenProcessor().nextToken
        case LexerMode.SkipNewLines => SkipNewLinesTokenProcessor().nextToken
        case LexerMode.StringText => StringTokenProcessor().nextToken
      }
      
      
      
    private abstract class TokenProcessor[State] {
      protected def lexInitialState: State
      protected def lexStep(state: State, c: Int): State
      protected def lexIsReject(state: State): Boolean
      protected def lexIsAccept(state: State): Boolean
      protected def processToken(state: State): WithSource[Token] | Null

      private final case class BoxedState(val state: State)
      
      def nextToken: ZIO[R, E, WithSource[Token] | FilePosition] =
        ZIO.suspendSucceed {
          
          def iter(state: State): ZIO[R, E, WithSource[Token] | FilePosition] =
            if hasEof then
              ZIO.succeed(currentPosition)
            else if currentTextOffset >= currentText.length then
              nextText match {
                case head +: tail =>
                  currentText = head
                  currentTextOffset = 0
                  nextText = tail
                  iter(state)

                case _ =>
                  reader.read.flatMap(newText => ZIO.suspendSucceed {
                    if newText.isEmpty then
                        try
                          val res = endOfFile(state)
                          ZIO.succeed(res)
                        catch
                          case SyntaxError.SyntaxErrorException(e) => ZIO.fail(e)
                        end try
                    else
                      nextText = newText
                      iter(state)
                    end if
                  })
              }
            else
              val tokenRes =
                try
                  processText(state)
                catch
                  case SyntaxError.SyntaxErrorException(e) => return ZIO.fail(e)
                end try

              tokenRes match {
                case token: WithSource[Token] => ZIO.succeed(token)
                case BoxedState(state) => iter(state)
              }
            end if

          def processText(state0: State): WithSource[Token] | BoxedState =
            var i = currentTextOffset
            val tokenStart = currentTextOffset
            var state = state0

            while i < currentText.length do
              val c = currentText.codePointAt(i)

              val nextState = lexStep(state, c)

              if lexIsReject(nextState) then
                accText = Cord.concat(accText, StringSlice(currentText, tokenStart, i))

                if !lexIsAccept(state) then
                  throw SyntaxError.SyntaxErrorException(SyntaxError.LexerError(currentTokenLocation, Some(c), accText.toString))

                val token = processToken(state)

                currentTextOffset = i
                accText = Cord.empty

                if token == null then
                  return BoxedState(lexInitialState)
                else
                  return token
              else
                state = nextState
                i += Character.charCount(c)
                if c == '\n' then
                  lineNumber += 1
                  linePosition = 1
                else
                  linePosition += 1
                end if
              end if
            end while

            accText = Cord.concat(accText, StringSlice(currentText, tokenStart, i))
            currentTextOffset = i

            BoxedState(state)
          end processText

          def endOfFile(state: State): WithSource[Token] | FilePosition =
            hasEof = true
            if accText.length == 0 then
              currentPosition
            else if lexIsAccept(state) then
              val token = processToken(state)
              if token == null then
                currentPosition
              else
                token
            else
              throw SyntaxError.SyntaxErrorException(SyntaxError.LexerError(currentTokenLocation, None, accText.toString))
            end if
          end endOfFile

          iter(lexInitialState)
        }
    }
    
    private class NormalTokenProcessor() extends TokenProcessor[TokenLexer.State] {
      override protected def lexInitialState: TokenLexer.State = TokenLexer.initialState
      override protected def lexStep(state: TokenLexer.State, c: Int): TokenLexer.State =
        TokenLexer.step(state, c)
      override protected def lexIsReject(state: TokenLexer.State): Boolean =
        TokenLexer.isReject(state)
      override protected def lexIsAccept(state: TokenLexer.State): Boolean =
        TokenLexer.isAccept(state)
      override protected def processToken(state: TokenLexer.State): WithSource[Token] | Null =
        val token: Token | Null = TokenLexer.acceptance(state) match {
          case TokenType.Token(t) => t
          case TokenType.Whitespace => null

          case TokenType.StringStart => Token.StringStart

          case TokenType.BinInteger =>
            val value = BigInt(accText.subSequence(2, accText.length).toString, 2)
            Token.IntToken(value)

          case TokenType.OctInteger =>
            val value = BigInt(accText.subSequence(2, accText.length).toString, 8)
            Token.IntToken(value)

          case TokenType.DecInteger =>
            val value = BigInt(accText.toString)
            Token.IntToken(value)

          case TokenType.HexInteger =>
            val value = BigInt(accText.subSequence(2, accText.length).toString, 16)
            Token.IntToken(value)

          case TokenType.InvalidInteger =>
            throw SyntaxError.SyntaxErrorException(SyntaxError.InvalidIntegerToken(currentTokenLocation, accText.toString))

          case TokenType.Identifier =>
            Token.Identifier(accText.toString)
        }

        val pos = currentPosition

        val tokenWithLoc: WithSource[Token] | Null =
          if token == null then
            null
          else
            WithLocation(token, Location(fileName, tokenStartPos, pos))
        
        tokenStartPos = pos
        
        tokenWithLoc
      end processToken
    }

    private final class SkipNewLinesTokenProcessor extends NormalTokenProcessor {
      override protected def processToken(state: State): WithSource[Token] | Null =
        super.processToken(state) match {
          case WithLocation(Token.NewLine, _) => null
          case token => token
        }
    }

    private final class StringTokenProcessor extends TokenProcessor[StringLexer.State] {
      override protected def lexInitialState: StringLexer.State = StringLexer.initialState
      override protected def lexStep(state: StringLexer.State, c: Int): StringLexer.State =
        StringLexer.step(state, c)
      override protected def lexIsReject(state: StringLexer.State): Boolean =
        StringLexer.isReject(state)
      override protected def lexIsAccept(state: StringLexer.State): Boolean =
        StringLexer.isAccept(state)
        
      override protected def processToken(state: StringLexer.State): WithSource[Token] | Null =
        val pos = currentPosition

        def stringToken(s: String): WithSource[Token] =
          WithLocation(Token.StringText(s), Location(fileName, tokenStartPos, pos))

        val token = StringLexer.acceptance(state) match {
          case StringTokenType.LiteralText =>
            stringToken(accText.toString)

          case StringTokenType.EscapedBackspace =>
            stringToken("\b")

          case StringTokenType.EscapedFormFeed =>
            stringToken("\f")

          case StringTokenType.EscapedNewLine =>
            stringToken("\n")

          case StringTokenType.EscapedCarriageReturn =>
            stringToken("\r")

          case StringTokenType.EscapedTab =>
            stringToken("\t")

          case StringTokenType.EscapedVerticalTab =>
            stringToken("\u000B")

          case StringTokenType.EscapedBackslash =>
            stringToken("\\")

          case StringTokenType.EscapedSingleQuote =>
            stringToken("'")

          case StringTokenType.EscapedDoubleQuote =>
            stringToken("\"")

          case StringTokenType.EscapedHash =>
            stringToken("#")

          case StringTokenType.EscapedOpenBracket =>
            stringToken("[")

          case StringTokenType.EscapedCloseBracket =>
            stringToken("]")

          case StringTokenType.EscapedOpenCurly =>
            stringToken("{")

          case StringTokenType.EscapedCloseCurly =>
            stringToken("}")

          case StringTokenType.EscapedUnicodeCodePoint =>
            val codePoint = Integer.parseUnsignedInt(accText.subSequence(3, accText.length - 1).toString, 16)
            stringToken(Character.toString(codePoint))

          case StringTokenType.InterpolationStart =>
            WithLocation(Token.StringInterpolationStart, Location(fileName, tokenStartPos, pos))

          case StringTokenType.EndOfString =>
            WithLocation(Token.StringEnd, Location(fileName, tokenStartPos, pos))
        }

        tokenStartPos = pos

        token
      end processToken
        
    }
    
  }

  def make[R, E >: SyntaxError](fileName: Option[String])(reader: TextReader[R, E]): UIO[Lexer[R, E]] =
    ZIO.succeed(new MutLexer(fileName)(reader))
}
