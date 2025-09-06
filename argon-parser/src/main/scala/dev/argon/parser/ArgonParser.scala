package dev.argon.parser

import dev.argon.ast.ModuleDeclaration
import dev.argon.util.{*, given}
import zio.{UIO, ZEnvironment, ZIO}

import scala.reflect.TypeTest

private[parser] class ArgonParser[R1, E1 >: SyntaxError](
  lexer: Lexer[R1, E1],
  private var currentSourceLocation: SourceLocation
) extends ArgonParserImpl {
  override type R = R1
  override type E = E1

  private var currentToken: WithSource[Token] | Null = null

  override protected def currentLocation: UIO[SourceLocation] = ZIO.succeed(currentSourceLocation)

  override protected def peek: F[Token] =
    ParseEffect(
      ZIO.suspendSucceed {
        val token = currentToken
        if token != null then
          ZIO.succeed(token)
        else
          ZIO.serviceWithZIO[LexerMode](lexer.nextToken)
            .flatMap {
              case token: WithSource[Token] =>
                ZIO.succeed {
                  currentToken = token
                  token
                }

              case pos: FilePosition =>
                ZIO.succeed {
                  val token = WithLocation(Token.EndOfFile, Location(currentSourceLocation.fileName, pos, pos))
                  currentToken = token
                  token
                }
            }
        end if
      }
    )



  override protected def terminal[T <: Token : TypeCategorySet](using TypeTest[Token, T])(using ruleName: sourcecode.Name): F[T] =
    ParseEffect(
      peek.run.flatMap {
        case WithLocation(token: T, loc) =>
          ZIO.succeed {
            currentToken = null
            WithLocation(token, loc)
          }

        case token =>
          ZIO.fail(SyntaxError.ParserError(ruleName.value, token, summon[TypeCategorySet[T]].categories))
      }
    )

  override protected def error[T <: Token : TypeCategorySet](using ruleName: sourcecode.Name): F[Nothing] =
    ParseEffect(
      peek.run
        .flatMap { token =>
          ZIO.fail(SyntaxError.ParserError(
            ruleName.value,
            token,
            summon[TypeCategorySet[T]].categories
          ))
        }
    )

  def parse: ZIO[R, E, ModuleDeclaration] =
    Start_0.run
      .map(_.value)
      .provideSomeEnvironment[R](_ ++ ZEnvironment(LexerMode.Normal))
}

object ArgonParser {
  def make[R, E >: SyntaxError](fileName: Option[String])(lexer: Lexer[R, E]): UIO[ArgonParser[R, E]] =
    ZIO.succeed {
      val pos = FilePosition(1, 1)
      new ArgonParser[R, E](lexer, Location(fileName, pos, pos))
    }
}

