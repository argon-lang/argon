package dev.argon.parser

import dev.argon.util.*
import scala.collection.immutable.Seq

sealed trait Stmt {

  @SuppressWarnings(Array("scalafix:Disable.toString"))
  def dumpInfo: String = toString

}

final case class TraitDeclarationStmt
  (
    baseType: Option[WithSource[Expr]],
    name: WithSource[Option[IdentifierExpr]],
    parameters: Vector[WithSource[FunctionParameterList]],
    body: Vector[WithSource[Stmt]],
    instanceBody: Vector[WithSource[Stmt]],
    modifiers: Vector[WithSource[TraitModifier]],
  ) extends Stmt

final case class ClassDeclarationStmt
  (
    baseType: Option[WithSource[Expr]],
    name: WithSource[Option[IdentifierExpr]],
    parameters: Vector[WithSource[FunctionParameterList]],
    body: Vector[WithSource[Stmt]],
    instanceBody: Vector[WithSource[Stmt]],
    modifiers: Vector[WithSource[ClassModifier]],
  ) extends Stmt

final case class FunctionDeclarationStmt
  (
    name: WithSource[Option[IdentifierExpr]],
    parameters: Vector[WithSource[FunctionParameterList]],
    returnType: WithSource[ReturnTypeSpecifier],
    body: WithSource[Expr],
    modifiers: Vector[WithSource[FunctionModifier]],
    purity: Boolean,
  ) extends Stmt

final case class MethodDeclarationStmt
  (
    instanceName: Option[IdentifierExpr],
    name: WithSource[Option[IdentifierExpr]],
    parameters: Vector[WithSource[FunctionParameterList]],
    returnType: WithSource[ReturnTypeSpecifier],
    body: Option[WithSource[Expr]],
    modifiers: Vector[WithSource[MethodModifier]],
    purity: Boolean,
  ) extends Stmt

final case class ClassConstructorDeclarationStmt
  (
    newLocation: SourceLocation,
    parameters: Vector[WithSource[FunctionParameterList]],
    body: WithSource[Vector[WithSource[Stmt]]],
    modifiers: Vector[WithSource[ClassConstructorModifier]],
    purity: Boolean,
  ) extends Stmt

final case class VariableDeclarationStmt
  (
    modifiers: Seq[WithSource[LocalVariableModifier]],
    isMutable: Boolean,
    varType: Option[WithSource[Expr]],
    name: Option[IdentifierExpr],
    value: WithSource[Expr],
  ) extends Stmt

final case class InitializeStmt
  (
    name: Option[IdentifierExpr],
    value: Option[WithSource[Expr]],
  ) extends Stmt

final case class FieldDeclarationStmt
  (
    isMutable: Boolean,
    name: IdentifierExpr,
    fieldType: WithSource[Expr],
  ) extends Stmt

final case class FieldInitializationStmt
  (
    name: IdentifierExpr,
    value: WithSource[Expr],
  ) extends Stmt

sealed trait Expr extends Stmt

final case class BuiltinExpr(id: String) extends Expr

final case class AsExpr(value: WithSource[Expr], valueType: WithSource[Expr]) extends Expr

final case class BinaryOperatorExpr(op: WithSource[BinaryOperator], left: WithSource[Expr], right: WithSource[Expr])
    extends Expr

final case class BlockExpr
  (
    body: WithSource[Vector[WithSource[Stmt]]],
    rescueClauses: Vector[MatchExprCase],
    elseBody: Option[WithSource[Vector[WithSource[Stmt]]]],
    ensureBody: Option[WithSource[Vector[WithSource[Stmt]]]],
  ) extends Expr

final case class BoolValueExpr(value: Boolean) extends Expr
final case class ClassConstructorExpr(classExpr: WithSource[Expr]) extends Expr
final case class DotExpr(left: WithSource[Expr], right: WithSource[IdentifierExpr]) extends Expr
final case class ExternExpr(specifier: String) extends Expr

final case class FunctionCallExpr(func: WithSource[Expr], listType: FunctionParameterListType, arg: WithSource[Expr])
    extends Expr

enum IdentifierExpr extends Expr derives CanEqual {
  case Named(name: String)
  case OperatorIdentifier(op: Operator)
  case Extension(inner: IdentifierExpr)
  case Inverse(inner: IdentifierExpr)
  case Update(inner: IdentifierExpr)
  case FunctionResultValue
}

final case class IfElseExpr
  (
    condition: WithSource[Expr],
    ifBody: WithSource[Vector[WithSource[Stmt]]],
    elseBody: WithSource[Vector[WithSource[Stmt]]],
  ) extends Expr

final case class IntValueExpr(sign: Int, base: BigInt, digits: Vector[BigInt]) extends Expr {
  def value: BigInt = sign * digits.foldLeft(0: BigInt) { (acc, digit) => acc * base + digit }
}

object IntValueExpr {
  def apply(token: Token.IntToken): IntValueExpr = IntValueExpr(token.sign, token.base, token.digits)
}

final case class LambdaTypeExpr(argType: WithSource[Expr], resultType: WithSource[Expr]) extends Expr
final case class LambdaExpr(name: Option[IdentifierExpr], body: WithSource[Expr]) extends Expr
final case class MatchExpr(value: WithSource[Expr], cases: Seq[WithSource[MatchExprCase]]) extends Expr
final case class RaiseExpr(exception: WithSource[Expr]) extends Expr
final case class StringValueExpr(value: Token.StringToken) extends Expr
final case class TupleExpr(values: Vector[WithSource[Expr]]) extends Expr
final case class TypeExpr(level: Option[WithSource[Expr]]) extends Expr
final case class MetaTypeExpr(level: BigInt) extends Expr
final case class TypeOfExpr(ofExpr: WithSource[Expr]) extends Expr
final case class UnaryOperatorExpr(op: WithSource[UnaryOperator], inner: WithSource[Expr]) extends Expr

final case class AssertExpr(assertionType: WithSource[Expr]) extends Expr
final case class SummonExpr(summonedType: WithSource[Expr]) extends Expr

sealed trait Pattern
final case class DeconstructPattern(constructor: WithSource[Expr], args: Vector[WithSource[Pattern]]) extends Pattern
final case class TuplePattern(values: Vector[WithSource[Pattern]]) extends Pattern
final case class BindingPattern(name: Option[IdentifierExpr]) extends Pattern
final case class TypeTestPattern(name: Option[IdentifierExpr], patternType: WithSource[Expr]) extends Pattern

final case class FunctionParameter
  (paramType: WithSource[Expr], name: IdentifierExpr)

final case class ReturnTypeSpecifier
(
  returnType: WithSource[Expr],
  ensuresClauses: Seq[WithSource[Expr]],
)

final case class FunctionParameterList
(
  listType: FunctionParameterListType,
  isErased: Boolean,
  parameters: Vector[WithSource[FunctionParameter]],
  hasTrailingComma: Boolean,
)

sealed trait Operator derives CanEqual, TypeNameLookup {
  def symbol: String
}

sealed trait BinaryOperator extends Operator derives CanEqual {
  def symbol: String
}

object BinaryOperator {

  case object Assign extends BinaryOperator {
    override def symbol: String = ":="
  }

  case object Plus extends BinaryOperator with UnaryOperator {
    override def symbol: String = "+"
  }

  case object Minus extends BinaryOperator with UnaryOperator {
    override def symbol: String = "-"
  }

  case object Mul extends BinaryOperator {
    override def symbol: String = "×"
  }

  case object Div extends BinaryOperator {
    override def symbol: String = "÷"
  }

  case object Equal extends BinaryOperator {
    override def symbol: String = "="
  }

  case object NotEqual extends BinaryOperator {
    override def symbol: String = "≠"
  }

  case object LessThan extends BinaryOperator {
    override def symbol: String = "<"
  }

  case object LessThanEq extends BinaryOperator {
    override def symbol: String = "≤"
  }

  case object GreaterThan extends BinaryOperator {
    override def symbol: String = ">"
  }

  case object GreaterThanEq extends BinaryOperator {
    override def symbol: String = "≥"
  }

  case object BitOr extends BinaryOperator {
    override def symbol: String = "|||"
  }

  case object BitXOr extends BinaryOperator {
    override def symbol: String = "^^^"
  }

  case object BitAnd extends BinaryOperator {
    override def symbol: String = "&&&"
  }

  case object BoolOr extends BinaryOperator {
    override def symbol: String = "||"
  }

  case object BoolAnd extends BinaryOperator {
    override def symbol: String = "&&"
  }

  case object ShiftLeft extends BinaryOperator {
    override def symbol: String = "<<<"
  }

  case object ShiftRight extends BinaryOperator {
    override def symbol: String = ">>>"
  }

  case object Union extends BinaryOperator {
    override def symbol: String = "|"
  }

  case object Intersection extends BinaryOperator {
    override def symbol: String = "&"
  }

  case object Concat extends BinaryOperator {
    override def symbol: String = "++"
  }

  case object SubType extends BinaryOperator {
    override def symbol: String = "<:"
  }

  case object PropEqual extends BinaryOperator {
    override def symbol: String = "=="
  }

  case object PropDisjunction extends BinaryOperator {
    override def symbol: String = "\\/"
  }

  case object PropConjunction extends BinaryOperator {
    override def symbol: String = "/\\"
  }
}

sealed trait UnaryOperator extends Operator derives CanEqual {
  def symbol: String
}

object UnaryOperator {

  case object BitNot extends UnaryOperator {
    override def symbol: String = "~~~"
  }

  case object BoolNot extends UnaryOperator {
    override def symbol: String = "!"
  }

  val Plus: BinaryOperator.Plus.type = BinaryOperator.Plus
  val Minus: BinaryOperator.Minus.type = BinaryOperator.Minus
}

sealed trait FunctionParameterListType derives CanEqual

object FunctionParameterListType {
  case object NormalList extends FunctionParameterListType derives CanEqual
  case object InferrableList extends FunctionParameterListType derives CanEqual
  case object InferrableList2 extends FunctionParameterListType derives CanEqual
  case object RequiresList extends FunctionParameterListType derives CanEqual
}

final case class MatchExprCase(pattern: WithSource[Pattern], body: WithSource[Vector[WithSource[Stmt]]])

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
