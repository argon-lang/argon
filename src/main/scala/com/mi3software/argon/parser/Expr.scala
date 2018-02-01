package com.mi3software.argon.parser

import com.mi3software.argon.util.WithSource

import scala.collection.immutable.Seq

sealed trait Stmt
final case class TraitDeclarationStmt(
                                       baseType: WithSource[Expr],
                                       name: Option[String],
                                       parameters: Seq[FunctionParameterList],
                                       body: Seq[WithSource[Stmt]],
                                       instanceBody: Seq[WithSource[Stmt]],
                                       modifiers: Vector[WithSource[Modifier]]
                                     ) extends Stmt
final case class ConstructorDeclarationStmt(
                                             name: Option[String],
                                             parameters: Seq[FunctionParameterList],
                                             returnType: WithSource[Expr],
                                             body: Seq[WithSource[Stmt]],
                                             modifiers: Vector[WithSource[Modifier]]
                                           ) extends Stmt
final case class ClassDeclarationStmt(
                                       baseType: WithSource[Expr],
                                       name: Option[String],
                                       parameters: Seq[FunctionParameterList],
                                       body: Seq[WithSource[Stmt]],
                                       instanceBody: Seq[WithSource[Stmt]],
                                       modifiers: Vector[WithSource[Modifier]]
                                     ) extends Stmt
final case class FunctionDeclarationStmt(
                                          name: Option[String],
                                          parameters: Seq[FunctionParameterList],
                                          returnType: WithSource[Expr],
                                          body: WithSource[Seq[WithSource[Stmt]]],
                                          modifiers: Vector[WithSource[Modifier]]
                                        ) extends Stmt
final case class MethodDeclarationStmt(
                                        instanceName: Option[String],
                                        name: Option[String],
                                        parameters: Seq[FunctionParameterList],
                                        returnType: WithSource[Expr],
                                        body: WithSource[Seq[WithSource[Stmt]]],
                                        modifiers: Vector[WithSource[Modifier]]
                                      ) extends Stmt
final case class ClassConstructorDeclarationStmt(
                                                  parameters: Seq[FunctionParameterList],
                                                  body: WithSource[Seq[WithSource[Stmt]]],
                                                  modifiers: Vector[WithSource[Modifier]]
                                                ) extends Stmt
final case class VariableDeclarationStmt(
                                          isMutable: Boolean,
                                          varType: Option[WithSource[Expr]],
                                          name: Option[String],
                                          value: WithSource[Expr]
                                        ) extends Stmt
final case class InitializeStmt(
                                 name: Option[String],
                                 value: Option[WithSource[Expr]]
                               ) extends Stmt
final case class FieldDeclarationStmt(
                                       isMutable: Boolean,
                                       name: Option[String],
                                       fieldType: WithSource[Expr]
                                     ) extends Stmt
final case class FieldInitializationStmt(
                                       name: String,
                                       value: WithSource[Expr]
                                     ) extends Stmt

sealed trait Expr extends Stmt
final case class AsExpr(value: WithSource[Expr], valueType: WithSource[Expr]) extends Expr
final case class BinaryOperatorExpr(op: BinaryOperator, left: WithSource[Expr], right: WithSource[Expr]) extends Expr
final case class BoolValueExpr(value: Boolean) extends Expr
final case class ClassConstructorExpr(classExpr: WithSource[Expr]) extends Expr
final case class DotExpr(left: WithSource[Expr], right: String) extends Expr
final case class FunctionCallExpr(func: WithSource[Expr], arg: WithSource[Expr]) extends Expr
final case class IdentifierExpr(name: String) extends Expr
final case class IfExpr(condition: WithSource[Expr], body: WithSource[Vector[WithSource[Stmt]]]) extends Expr
final case class IfElseExpr(condition: WithSource[Expr], ifBody: WithSource[Vector[WithSource[Stmt]]], elseBody: WithSource[Vector[WithSource[Stmt]]]) extends Expr
final case class IntValueExpr(value: BigInt) extends Expr
final case class LambdaTypeExpr(argType: WithSource[Expr], resultType: WithSource[Expr]) extends Expr
final case class LambdaExpr(name: Option[String], body: WithSource[Expr]) extends Expr
final case class MatchExpr(value: WithSource[Expr], cases: Seq[WithSource[MatchExprCase]]) extends Expr
final case class StringValueExpr(value: String) extends Expr
final case class TupleExpr(values: Vector[WithSource[Expr]]) extends Expr
final case class TypeExpr(instanceType: Option[WithSource[Expr]], subtypeOf: Option[WithSource[Expr]], supertypeOf: Option[WithSource[Expr]]) extends Expr
final case class TypeOfExpr(ofExpr: WithSource[Expr]) extends Expr
final case class UnaryOperatorExpr(op: UnaryOperator, inner: WithSource[Expr]) extends Expr


sealed trait Pattern
final case class DeconstructPattern(constructor: WithSource[Expr], args: Seq[WithSource[Pattern]]) extends Pattern
final case class TuplePattern(values: Seq[WithSource[Pattern]]) extends Pattern
case object DiscardPattern extends Pattern
final case class BindingPattern(name: String) extends Pattern
final case class TypeTestPattern(name: Option[String], patternType: WithSource[Expr]) extends Pattern


final case class FunctionParameter(paramType: Option[WithSource[Expr]], subTypeOf: Option[WithSource[Expr]], name: String)
final case class FunctionParameterList(listType: FunctionParameterListType, parameters: Vector[FunctionParameter])

sealed trait BinaryOperator
object BinaryOperator {
  case object Assign extends BinaryOperator
  case object Add extends BinaryOperator
  case object Sub extends BinaryOperator
  case object Mul extends BinaryOperator
  case object Div extends BinaryOperator
  case object Equal extends BinaryOperator
  case object NotEqual extends BinaryOperator
  case object LessThan extends BinaryOperator
  case object LessThanEq extends BinaryOperator
  case object GreaterThan extends BinaryOperator
  case object GreaterThanEq extends BinaryOperator
  case object BitOr extends BinaryOperator
  case object BitXOr extends BinaryOperator
  case object BitAnd extends BinaryOperator
  case object BoolOr extends BinaryOperator
  case object BoolAnd extends BinaryOperator
  case object ShiftLeft extends BinaryOperator
  case object ShiftRight extends BinaryOperator
}

sealed trait UnaryOperator
object UnaryOperator {
  case object BitNot extends UnaryOperator
  case object BoolNot extends UnaryOperator
  case object UnaryPlus extends UnaryOperator
  case object UnaryMinus extends UnaryOperator
}

sealed trait FunctionParameterListType
object FunctionParameterListType {
  case object NormalList extends FunctionParameterListType
  case object InferrableList extends FunctionParameterListType
}

final case class MatchExprCase(pattern: WithSource[Pattern], body: WithSource[Vector[WithSource[Stmt]]])
