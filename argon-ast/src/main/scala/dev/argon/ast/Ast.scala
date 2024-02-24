package dev.argon.ast

import cats.data.NonEmptyList
import dev.argon.util.WithSource

sealed trait Stmt
sealed trait RecordBodyStmt

final case class FunctionDeclarationStmt(
  modifiers: Vector[WithSource[Modifier]],
  purity: Boolean,
  name: WithSource[Option[IdentifierExpr]],
  parameters: Vector[WithSource[FunctionParameterList]],
  returnType: WithSource[ReturnTypeSpecifier],
  body: WithSource[Expr],
) extends Stmt

final case class VariableDeclarationStmt(
  modifiers: Vector[WithSource[Modifier]],
  isMutable: Boolean,
  name: Option[IdentifierExpr],
  varType: Option[WithSource[Expr]],
  value: WithSource[Expr],
) extends Stmt

final case class RecordDeclarationStmt(
  modifiers: Vector[WithSource[Modifier]],
  name: WithSource[Option[IdentifierExpr]],
  parameters: Vector[WithSource[FunctionParameterList]],
  body: Vector[WithSource[RecordBodyStmt]],
) extends Stmt

final case class MethodDeclarationStmt(
  modifiers: Vector[WithSource[Modifier]],
  purity: Boolean,
  instanceName: WithSource[Option[IdentifierExpr]],
  instanceType: Option[WithSource[Expr]],
  name: WithSource[Option[IdentifierExpr]],
  parameters: Vector[WithSource[FunctionParameterList]],
  returnType: WithSource[ReturnTypeSpecifier],
  body: Option[WithSource[Expr]],
) extends RecordBodyStmt


final case class FunctionParameter
(paramType: WithSource[Expr], name: IdentifierExpr)

enum FunctionParameterListType {
  case NormalList
  case InferrableList
  case QuoteList
  case RequiresList
}

final case class FunctionParameterList
(
  listType: FunctionParameterListType,
  isErased: Boolean,
  parameters: Vector[WithSource[FunctionParameter]],
  hasTrailingComma: Boolean,
)

final case class ReturnTypeSpecifier
(
  returnType: WithSource[Expr],
  ensuresClauses: Seq[WithSource[Expr]],
)

enum Modifier {
  case Public, Protected, Private, Internal
  case Erased, Proof, Inline
}

enum ImportStmt extends Stmt {
  case Absolute(path: ImportPathSegment)
  case Relative(upCount: Int, path: ImportPathSegment)
  case Tube(tubeName: NonEmptyList[String], path: ImportPathSegment)
  case Member(memberPath: ImportPathSegment)
}

sealed trait ImportPathSegment derives CanEqual

object ImportPathSegment {
  sealed trait End extends ImportPathSegment derives CanEqual

  final case class Cons(id: String, subPath: ImportPathSegment) extends ImportPathSegment
  final case class Many(segments: Seq[ImportPathSegment]) extends ImportPathSegment
  final case class Renaming(importing: IdentifierExpr, viewedName: Option[IdentifierExpr]) extends End
  final case class Imported(id: IdentifierExpr) extends End
  case object Wildcard extends End
}

final case class ExportStmt(fromImport: ImportStmt) extends Stmt


final case class AssertStmt(t: WithSource[Expr]) extends Stmt


sealed trait Expr extends Stmt

object Expr:
  final case class As(value: WithSource[Expr], valueType: WithSource[Expr]) extends Expr
  final case class Assert(t: WithSource[Expr]) extends Expr
  final case class BinaryOperation(a: WithSource[Expr], op: BinaryOperator, b: WithSource[Expr]) extends Expr
  final case class Block(body: WithSource[Seq[WithSource[Stmt]]]) extends Expr
  final case class BoolLiteral(b: Boolean) extends Expr
  final case class Builtin(name: String) extends Expr
  final case class Dot(o: WithSource[Expr], member: WithSource[IdentifierExpr]) extends Expr
  final case class Extern(name: String) extends Expr
  final case class FunctionLiteral(parameterName: Option[IdentifierExpr], body: WithSource[Expr]) extends Expr
  final case class FunctionCall(func: WithSource[Expr], listType: FunctionParameterListType, arg: WithSource[Expr]) extends Expr
  case object FunctionResultValue extends Expr
  final case class FunctionType(a: WithSource[Expr], r: WithSource[Expr]) extends Expr
  final case class IfElse(condition: WithSource[Expr], whenTrue: WithSource[Seq[WithSource[Stmt]]], whenFalse: WithSource[Seq[WithSource[Stmt]]]) extends Expr
  final case class IntLiteral(i: BigInt) extends Expr
  final case class StringLiteral(parts: NonEmptyList[StringFragment]) extends Expr
  final case class Summon(t: WithSource[Expr]) extends Expr
  final case class Tuple(items: Seq[WithSource[Expr]]) extends Expr
  final case class Type(n: Option[WithSource[Expr]]) extends Expr
  final case class BigType(n: BigInt) extends Expr
  final case class UnaryOperation(op: UnaryOperator, a: WithSource[Expr]) extends Expr
end Expr

enum IdentifierExpr extends Expr {
  case Named(s: String)
  case Op(op: Operator)
  case Extension(inner: IdentifierExpr)
  case Inverse(inner: IdentifierExpr)
  case Update(inner: IdentifierExpr)
}

enum StringFragment {
  case Text(s: String)
  case Interpolate(format: Option[WithSource[String]], value: WithSource[Expr])
}

sealed trait Operator:
  val symbol: String
end Operator

enum BinaryOperator(val symbol: String) extends Operator:
  case Assign extends BinaryOperator("+")
  case Plus extends BinaryOperator("+")
  case Minus extends BinaryOperator("-")
  case Mul extends BinaryOperator("×")
  case Div extends BinaryOperator("÷")
  case Equal extends BinaryOperator("=")
  case NotEqual extends BinaryOperator("!=")
  case LessThan extends BinaryOperator("<")
  case LessThanEq extends BinaryOperator("≤")
  case GreaterThan extends BinaryOperator(">")
  case GreaterThanEq extends BinaryOperator("≥")
  case BitOr extends BinaryOperator("|||")
  case BitXOr extends BinaryOperator("^^^")
  case BitAnd extends BinaryOperator("&&&")
  case LogicalOr extends BinaryOperator("||")
  case LogicalAnd extends BinaryOperator("&&")
  case ShiftLeft extends BinaryOperator("<<")
  case ShiftRight extends BinaryOperator(">>")
  case Concat extends BinaryOperator("++")
  case PropEqual extends BinaryOperator("==")
  case PropDisjunction extends BinaryOperator("\\/")
  case PropConjunction extends BinaryOperator("/\\")
end BinaryOperator

enum UnaryOperator(val symbol: String) extends Operator:
  case Plus extends UnaryOperator("+")
  case Minus extends UnaryOperator("-")
  case BitNot extends UnaryOperator("~~~")
  case LogicalNot extends UnaryOperator("!")
end UnaryOperator




final case class TubeSpec(
  modules: Seq[ModulePatternMapping],
)


final case class ModulePatternMapping(
  module: Seq[ModulePatternSegment],
  fileNameTemplate: Expr.StringLiteral,
)


enum ModulePatternSegment {
  case Named(name: String)
  case Star(boundName: IdentifierExpr)
  case DoubleStar(boundName: IdentifierExpr)
}

