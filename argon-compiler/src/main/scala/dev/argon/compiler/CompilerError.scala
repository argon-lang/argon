package dev.argon.compiler

import dev.argon.ast.{IdentifierExpr, Modifier}
import dev.argon.util.{FilePosition, Location}
import zio.*
import ErrorExprContext.Expr

enum CompilerError {
  case DuplicateModuleDefinition(modulePath: ModulePath)

  case TypeError(
    loc: Location[FilePosition],
    expected: Expr,
    actual: Expr,
  )

  case InvalidTypeError(
    loc: Location[FilePosition],
    expr: Expr,
    exprType: Expr,
  )

  case InvalidModifier(
    loc: Location[FilePosition],
    modifiers: Set[Modifier],
    expectedModifiers: NonEmptyChunk[Set[Modifier]],
  )

  case DuplicateModifier(
    loc: Location[FilePosition],
    modifier: Modifier,
  )

  case UnknownIdentifier(
    loc: Location[FilePosition],
    id: IdentifierExpr,
  )
  
  case PurityError(
    loc: Location[FilePosition],
  )

  case ErasedExpressionNotAllowed(
    loc: Location[FilePosition],
  )

  case TokenExpressionRequired(
    loc: Location[FilePosition],
  )
  
  case ErasedMustBePure(
    loc: Location[FilePosition],
  )

  case TokenMustBePure(
    loc: Location[FilePosition],
  )
  
  case CouldNotInfer(
    loc: Location[FilePosition],
  )

  case CanNotMutate(
    loc: Location[FilePosition],
  )
  
  case ImplicitNotFound(
    loc: Location[FilePosition],
  )
  
  case FunctionTypeRequired(
    loc: Location[FilePosition],
    t: Expr,
  )

  case TupleTypeRequired(
    loc: Location[FilePosition],
    t: Expr,
  )

  case TupleSizeMismatch(
    loc: Location[FilePosition],
    t: Expr,
    actualSize: Int,
  )
  
  case TypeParameterIsConcrete(
    loc: Location[FilePosition],
  )

  case UnknownModule(tube: TubeName, modulePath: ModulePath, loc: Location[FilePosition])

  case InvalidAssignmentTarget(loc: Location[FilePosition])

  case AmbiguousOverload(loc: Location[FilePosition], attempted: Seq[AttemptedOverload])
  case InvalidOverload(loc: Location[FilePosition], attempted: Seq[AttemptedOverloadWithErrors])
  
  case UnknownExtern(loc: Location[FilePosition], name: String)
}


