package dev.argon.ast

import cats.data.NonEmptySeq
import dev.argon.util.{WithSource, FilePosition, Location}

sealed trait Stmt
sealed trait RecordBodyStmt
sealed trait EnumBodyStmt

final case class FunctionDeclarationStmt(
  modifiers: Seq[WithSource[Modifier]],
  purity: Boolean,
  name: WithSource[Option[IdentifierExpr]],
  parameters: Seq[WithSource[FunctionParameterList]],
  returnType: WithSource[ReturnTypeSpecifier],
  body: FunctionBody,
) extends Stmt with RecordBodyStmt with EnumBodyStmt

final case class VariableDeclarationStmt(
  modifiers: Seq[WithSource[Modifier]],
  isMutable: Boolean,
  name: Option[IdentifierExpr],
  varType: Option[WithSource[Expr]],
  value: WithSource[Expr],
) extends Stmt

final case class RecordDeclarationStmt(
  modifiers: Seq[WithSource[Modifier]],
  name: WithSource[IdentifierExpr],
  parameters: Seq[WithSource[FunctionParameterList]],
  returnType: Option[WithSource[Expr]],
  body: Seq[WithSource[RecordBodyStmt]],
) extends Stmt

final case class RecordField(
  isMutable: Boolean,
  name: WithSource[IdentifierExpr],
  fieldType: WithSource[Expr],
) extends RecordBodyStmt

final case class EnumDeclarationStmt(
  modifiers: Seq[WithSource[Modifier]],
  name: WithSource[IdentifierExpr],
  parameters: Seq[WithSource[FunctionParameterList]],
  returnType: Option[WithSource[Expr]],
  body: Seq[WithSource[EnumBodyStmt]],
) extends Stmt

enum EnumVariant extends EnumBodyStmt {
  case Constructor(
    modifiers: Seq[WithSource[Modifier]],
    name: WithSource[IdentifierExpr],
    parameters: Seq[WithSource[FunctionParameterList]],
    returnType: Option[WithSource[Expr]],
  )
  case Record(record: RecordDeclarationStmt)
}

final case class MethodDeclarationStmt(
  modifiers: Seq[WithSource[Modifier]],
  purity: Boolean,
  instanceName: WithSource[Option[IdentifierExpr]],
  instanceType: Option[WithSource[Expr]],
  name: WithSource[Option[IdentifierExpr]],
  parameters: Seq[WithSource[FunctionParameterList]],
  returnType: WithSource[ReturnTypeSpecifier],
  body: Option[FunctionBody],
) extends Stmt with RecordBodyStmt with EnumBodyStmt

final case class FunctionParameter
(paramType: WithSource[Expr], name: IdentifierExpr)

enum FunctionParameterListType derives CanEqual {
  case NormalList
  case InferrableList
  case QuoteList
  case RequiresList
}

final case class FunctionParameterList
(
  listType: FunctionParameterListType,
  isErased: Boolean,
  parameters: Seq[WithSource[FunctionParameter]],
  hasTrailingComma: Boolean,
)

final case class ReturnTypeSpecifier
(
  returnType: WithSource[Expr],
  ensuresClauses: Seq[WithSource[Expr]],
)

enum Modifier derives CanEqual {
  case Public, Protected, Private, Internal
  case Erased, Witness, Inline
}

enum ImportStmt extends Stmt {
  case Absolute(path: ImportPathSegment)
  case Relative(upCount: Int, path: ImportPathSegment)
  case Tube(tubeName: NonEmptySeq[String], path: ImportPathSegment)
  case Member(memberPath: ImportPathSegment)
}

sealed trait ImportPathSegment derives CanEqual

object ImportPathSegment {
  sealed trait End extends ImportPathSegment derives CanEqual

  final case class Cons(id: String, subPath: ImportPathSegment) extends ImportPathSegment
  final case class Many(segments: Seq[ImportPathSegment]) extends ImportPathSegment
  final case class Renaming(importing: WithSource[IdentifierExpr], viewedName: WithSource[Option[IdentifierExpr]]) extends End
  final case class Imported(id: WithSource[IdentifierExpr]) extends End
  final case class Wildcard(location: Location[FilePosition]) extends End
}

final case class ExportStmt(fromImport: WithSource[ImportStmt]) extends Stmt


final case class AssertStmt(t: WithSource[Expr]) extends Stmt


sealed trait Expr extends Stmt

object Expr:
  final case class As(value: WithSource[Expr], valueType: WithSource[Expr]) extends Expr
  final case class Assert(t: WithSource[Expr]) extends Expr
  final case class BinaryOperation(a: WithSource[Expr], op: BinaryOperator, b: WithSource[Expr]) extends Expr
  final case class Block(body: WithSource[Seq[WithSource[Stmt]]], finallyBody: Option[WithSource[Seq[WithSource[Stmt]]]]) extends Expr
  final case class BoolLiteral(b: Boolean) extends Expr
  final case class Builtin(name: String) extends Expr
  final case class Dot(o: WithSource[Expr], member: WithSource[IdentifierExpr]) extends Expr
  final case class FunctionLiteral(parameterName: Option[IdentifierExpr], body: WithSource[Expr]) extends Expr
  final case class FunctionCall(func: WithSource[Expr], listType: FunctionParameterListType, arg: WithSource[Expr]) extends Expr
  case object FunctionResultValue extends Expr
  final case class FunctionType(a: WithSource[Expr], r: WithSource[Expr]) extends Expr
  final case class IfElse(condition: WithSource[Expr], whenTrue: WithSource[Seq[WithSource[Stmt]]], whenFalse: WithSource[Seq[WithSource[Stmt]]]) extends Expr
  final case class IntLiteral(i: BigInt) extends Expr
  final case class Is(value: WithSource[Expr], pattern: WithSource[Pattern]) extends Expr
  final case class Match(value: WithSource[Expr], cases: Seq[WithSource[MatchCase]]) extends Expr
  final case class RecordLiteral(recordExpr: WithSource[Expr], fields: WithSource[Seq[WithSource[RecordFieldLiteral]]]) extends Expr
  final case class StringLiteral(parts: Seq[StringFragment]) extends Expr
  final case class Summon(t: WithSource[Expr]) extends Expr
  final case class Tuple(items: Seq[WithSource[Expr]]) extends Expr
  case object Type extends Expr
  final case class BigType(n: BigInt) extends Expr
  final case class UnaryOperation(op: UnaryOperator, a: WithSource[Expr]) extends Expr
  final case class BoxedType(t: WithSource[Expr]) extends Expr
  final case class Box(value: WithSource[Expr]) extends Expr
  final case class Unbox(value: WithSource[Expr]) extends Expr
end Expr

enum IdentifierExpr extends Expr derives CanEqual {
  case Named(s: String)
  case Op(op: Operator.ValidIdentifier)
  case Extension(inner: IdentifierExpr)
  case Inverse(inner: IdentifierExpr)
  case Update(inner: IdentifierExpr)
}

enum FunctionBody derives CanEqual {
  case ExprBody(expr: WithSource[Expr])
  case ExternBody(name: WithSource[String])
}

final case class RecordFieldLiteral(
  name: WithSource[IdentifierExpr],
  value: WithSource[Expr],
)

enum StringFragment {
  case Text(s: String)
  case Interpolate(value: WithSource[Expr])
}

sealed trait Operator derives CanEqual:
  val symbol: String
end Operator

object Operator:
  sealed trait ValidIdentifier
end Operator


enum BinaryOperator(val symbol: String) extends Operator derives CanEqual:
  case Assign extends BinaryOperator(":=")
  case Plus extends BinaryOperator("+") with Operator.ValidIdentifier
  case Minus extends BinaryOperator("-") with Operator.ValidIdentifier
  case Mul extends BinaryOperator("×") with Operator.ValidIdentifier
  case Div extends BinaryOperator("÷") with Operator.ValidIdentifier
  case Equal extends BinaryOperator("=") with Operator.ValidIdentifier
  case NotEqual extends BinaryOperator("≠") with Operator.ValidIdentifier
  case LessThan extends BinaryOperator("<") with Operator.ValidIdentifier
  case LessThanEq extends BinaryOperator("≤") with Operator.ValidIdentifier
  case GreaterThan extends BinaryOperator(">") with Operator.ValidIdentifier
  case GreaterThanEq extends BinaryOperator("≥") with Operator.ValidIdentifier
  case BitOr extends BinaryOperator("|||") with Operator.ValidIdentifier
  case BitXOr extends BinaryOperator("^^^") with Operator.ValidIdentifier
  case BitAnd extends BinaryOperator("&&&") with Operator.ValidIdentifier
  case LogicalOr extends BinaryOperator("||")
  case LogicalAnd extends BinaryOperator("&&")
  case ShiftLeft extends BinaryOperator("<<") with Operator.ValidIdentifier
  case ShiftRight extends BinaryOperator(">>") with Operator.ValidIdentifier
  case Concat extends BinaryOperator("++") with Operator.ValidIdentifier
  case PropEqual extends BinaryOperator("==")
  case PropDisjunction extends BinaryOperator("\\/")
  case PropConjunction extends BinaryOperator("/\\")
end BinaryOperator

enum UnaryOperator(val symbol: String) extends Operator derives CanEqual:
  case Plus extends UnaryOperator("+") with Operator.ValidIdentifier
  case Minus extends UnaryOperator("-") with Operator.ValidIdentifier
  case BitNot extends UnaryOperator("~~~") with Operator.ValidIdentifier
  case LogicalNot extends UnaryOperator("!") with Operator.ValidIdentifier
end UnaryOperator

final case class MatchCase(
  pattern: WithSource[Pattern],
  body: WithSource[Expr],
)

enum Pattern derives CanEqual {
  case Discard
  case Tuple(elements: Seq[WithSource[Pattern]])
  case Binding(isMutable: Boolean, name: WithSource[IdentifierExpr], pattern: WithSource[Pattern])
  case Constructor(path: WithSource[PatternPath], args: Seq[WithSource[Pattern]])
  case Record(path: WithSource[PatternPath], args: Seq[WithSource[Pattern]], recordFieldPatterns: Seq[WithSource[RecordFieldPattern]])
}

enum PatternPath {
  case Member(base: WithSource[PatternPath], member: WithSource[IdentifierExpr])
  case Base(name: WithSource[IdentifierExpr])
}

final case class RecordFieldPattern(
  fieldName: WithSource[IdentifierExpr],
  pattern: WithSource[Pattern],
)




final case class TubeSpec(
  modules: Seq[ModulePatternMapping],
)


final case class ModulePatternMapping(
  module: Seq[WithSource[ModulePatternSegment]],
  fileNameTemplate: Expr.StringLiteral,
)


enum ModulePatternSegment {
  case Named(name: String)
  case Star(boundName: IdentifierExpr)
  case DoubleStar(boundName: IdentifierExpr)
}

final case class ModuleDeclaration(
  modulePath: Seq[String],
  stmts: Seq[WithSource[Stmt]],
)

