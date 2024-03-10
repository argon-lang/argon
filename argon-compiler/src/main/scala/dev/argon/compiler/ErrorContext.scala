package dev.argon.compiler

import dev.argon.ast.{IdentifierExpr, Modifier}
import dev.argon.util.{FilePosition, Location}
import zio.*

trait ErrorContext {
  self: Context =>

  import self.TRExprContext.Expr as TRExpr

  enum CompilerError {
    case DuplicateModuleDefinition(modulePath: ModulePath)
    
    case TypeError(
      loc: Location[FilePosition],
      expected: TRExpr,
      actual: TRExpr,
    )

    case InvalidTypeError(
      loc: Location[FilePosition],
      expr: TRExpr,
      exprType: TRExpr,
    )

    case InvalidModifier(
      loc: Location[FilePosition],
      modifier: Modifier,
    )

    case DuplicateModifier(
      loc: Location[FilePosition],
      modifier: Modifier,
    )

    case UnknownIdentifier(
      loc: Location[FilePosition],
      id: IdentifierExpr,
    )
    
    case InvalidAssignmentTarget(loc: Location[FilePosition])

    case AmbiguousOverload(loc: Location[FilePosition], attempted: Seq[self.Scopes.Overloadable])
    case InvalidOverload(loc: Location[FilePosition], attempted: Seq[self.Scopes.Overloadable])
  }

  trait ErrorLog {
    def logError(error: => CompilerError): UIO[Unit]
  }

  object ErrorLog {
    def logError(error: => CompilerError): URIO[ErrorLog, Unit] =
      ZIO.serviceWithZIO[ErrorLog](_.logError(error))
  }
  
  
}
