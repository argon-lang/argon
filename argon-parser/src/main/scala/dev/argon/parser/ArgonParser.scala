package dev.argon.parser

import Token.*
import dev.argon.ast
import dev.argon.ast.*
import dev.argon.util.{*, given}
import scala.reflect.TypeTest
import dev.argon.grammar.{Grammar, GrammarError, TokenMatcher}
import Grammar.Operators.{*, given}
import Grammar.{GrammarFactory, UnionGrammar}
import zio.{Chunk, NonEmptyChunk}
import zio.stream.ZChannel
import Function.const
import cats.data.NonEmptySeq

object ArgonParser {

  private[parser] object Rule {

    enum ParenAllowedState derives CanEqual {
      case ParenAllowed, ParenDisallowed
    }
    export ParenAllowedState.*


    sealed trait ArgonRuleName[T]

    object ArgonRuleName {
      given [T, U] => CanEqual[ArgonRuleName[T], ArgonRuleName[U]] = CanEqual.canEqualAny
    }

    case object IdentifierOptional extends ArgonRuleName[Option[IdentifierExpr]]
    case object OperatorIdentifier extends ArgonRuleName[IdentifierExpr]
    final case class IdentifierParen(parenAllowed: ParenAllowedState) extends ArgonRuleName[IdentifierExpr]
    val Identifier: IdentifierParen = IdentifierParen(ParenAllowed)
    case object BinaryOperatorName extends ArgonRuleName[BinaryOperatorToken[BinaryOperator & Operator.ValidIdentifier]]
    case object UnaryOperatorName extends ArgonRuleName[UnaryOperatorToken[UnaryOperator & Operator.ValidIdentifier]]
    case object MethodName extends ArgonRuleName[Option[IdentifierExpr]]
    case object NewLines extends ArgonRuleName[Unit]
    case object StatementSeparator extends ArgonRuleName[Unit]
    case object ImportStatement extends ArgonRuleName[ImportStmt]
    case object ExportStatement extends ArgonRuleName[ExportStmt]

    // If
    case object IfExpr extends ArgonRuleName[Expr]
    case object IfExprStart extends ArgonRuleName[(WithSource[Expr], WithSource[Vector[WithSource[Stmt]]])]
    case object IfExprPart extends ArgonRuleName[Expr]

    // Common Expressions
    final case class PrimaryExpr(parenAllowed: ParenAllowedState) extends ArgonRuleName[Expr]
    final case class PostfixExpr(parenAllowed: ParenAllowedState) extends ArgonRuleName[Expr]
    case object CurryCallExpr extends ArgonRuleName[Expr]
    case object ParenArgList extends ArgonRuleName[(FunctionParameterListType, Expr)]
    case object MemberAccess extends ArgonRuleName[WithSource[Expr] => Expr]
    case object UnaryExpr extends ArgonRuleName[Expr]
    case object TypeExpr extends ArgonRuleName[Expr]
    case object MultiplicativeExpr extends ArgonRuleName[Expr]
    case object AdditiveExpr extends ArgonRuleName[Expr]
    case object ShiftExpr extends ArgonRuleName[Expr]
    case object AndExpr extends ArgonRuleName[Expr]
    case object XorExpr extends ArgonRuleName[Expr]
    case object OrExpr extends ArgonRuleName[Expr]
    case object LambdaTypeExpr extends ArgonRuleName[Expr]
    case object RelationalExpr extends ArgonRuleName[Expr]
    case object EqualityExpr extends ArgonRuleName[Expr]
    case object PropConjunctionExpr extends ArgonRuleName[Expr]
    case object PropDisjunctionExpr extends ArgonRuleName[Expr]
    case object PropEqualityExpr extends ArgonRuleName[Expr]
    case object AsExpr extends ArgonRuleName[Expr]
    case object LambdaExpr extends ArgonRuleName[Expr]
    case object PatternType extends ArgonRuleName[Expr]
    case object Type extends ArgonRuleName[Expr]
    case object TupleExpr extends ArgonRuleName[Expr]
    case object AssignExpr extends ArgonRuleName[Expr]
    case object AssertExpr extends ArgonRuleName[Expr]
    case object Expression extends ArgonRuleName[Expr]
    case object ExpressionStmt extends ArgonRuleName[Expr]

    // Variable Declaration
    case object VariableMutSpec extends ArgonRuleName[Boolean]
    case object VariableDeclaration extends ArgonRuleName[Stmt]

    case object Modifiers extends ArgonRuleName[Vector[WithSource[Modifier]]]

    // Functions and Methods
    case object MethodParameter extends ArgonRuleName[FunctionParameter]
    case object MethodParameterList extends ArgonRuleName[(Vector[WithSource[FunctionParameter]], Boolean)]
    case object MethodParameters extends ArgonRuleName[Vector[WithSource[FunctionParameterList]]]
    case object MethodReturnType extends ArgonRuleName[ReturnTypeSpecifier]
    case object MethodBody extends ArgonRuleName[Expr]
    case object BlockBody extends ArgonRuleName[Expr.Block]
    case object MethodPurity extends ArgonRuleName[Boolean]
    case object FunctionDefinitionStmt extends ArgonRuleName[Stmt]
    case object RecordDeclarationStmt extends ArgonRuleName[ast.RecordDeclarationStmt]
    case object RecordBody extends ArgonRuleName[Vector[WithSource[RecordBodyStmt]]]
    case object RecordField extends ArgonRuleName[ast.RecordField]
    case object MethodDefinitionStmt extends ArgonRuleName[MethodDeclarationStmt]

    // Types

    case object Statement extends ArgonRuleName[Stmt]
    case object StatementList extends ArgonRuleName[Vector[WithSource[Stmt]]]
    case object ImportPathAbsolute extends ArgonRuleName[ImportStmt]
    case object ImportPathRelative extends ArgonRuleName[ImportStmt]
    case object ImportPathTube extends ArgonRuleName[ImportStmt]
    case object ImportPathMember extends ArgonRuleName[ImportStmt]
    case object ImportPathTubeName extends ArgonRuleName[NonEmptySeq[String]]

    // TubeSpec
    case object ModulePatternMappingStmt extends ArgonRuleName[ModulePatternMapping]
    case object ModulePatternExpr extends ArgonRuleName[Seq[WithSource[ModulePatternSegment]]]
    case object ModulePatternSegmentExpr extends ArgonRuleName[ModulePatternSegment]

    final case class ImportPathSegmentRule(separator: ImportPathSegmentSeparator)
        extends ArgonRuleName[ImportPathSegment]

    enum ImportPathSegmentSeparator derives CanEqual {
      case Slash, Dot
    }

    case object PaddedStatement extends ArgonRuleName[WithSource[Stmt]]

  }

  private[parser] final class ArgonGrammarFactory(override val fileName: Option[String]) extends GrammarFactory[Token, TokenCategory, FilePosition, Rule.ArgonRuleName] {

    def second[T](pair: (?, T)): T = pair._2

    private def matchTokenFactory[TToken <: Token](factory: TokenFactory[TToken])(using tt: TypeTest[Token, TToken])
      : TGrammar[TToken] = Grammar.matcher(factory.category, TokenMatcher.Subtype[Token, TToken](tt))

    private def matchToken[TToken <: Token](token: TToken)
      (using tt: TypeTest[Token, TToken])
      : TGrammar[TToken] = Grammar.matcher(token.category, TokenMatcher.Subtype[Token, TToken](tt))

    private val tokenUnderscore: TGrammar[Unit] = matchToken(KW_UNDERSCORE).discard

    private val tokenIdentifier: TGrammar[String] = matchTokenFactory(Identifier) --> { _.name }


    def ruleModifier[TToken <: Token & ModifierToken]
    (token: TToken)
    (using TypeTest[Token, TToken])
    : TGrammar[Modifier] =
      matchToken(token) --> const(token.modifier)

    private def createLeftRec[A](left: TGrammar[A])(rightRepeat: TGrammar[WithSource[A] => A]): TGrammar[A] =
      left.observeLocation ++ rightRepeat.observeLocation.* --> { case (a, fs) =>
        fs.foldLeft(a) { case (a, WithLocation(f, loc)) =>
          WithLocation(f(a), Location.merge(a.location, loc))
        }.value
      }

    protected override def createGrammar[T](name: Rule.ArgonRuleName[T]): TGrammar[T] =
      name match {
        case Rule.IdentifierOptional =>
          tokenUnderscore --> const(Option.empty[IdentifierExpr]) |
            rule(Rule.Identifier) --> Some.apply

        case Rule.OperatorIdentifier =>
          matchToken(KW_UNARY).discard ++! matchToken(KW_OPERATOR).discard ++ rule(Rule.UnaryOperatorName) --> { op => IdentifierExpr.Op(op.unOperator) } |
            matchToken(KW_OPERATOR).discard ++! rule(Rule.BinaryOperatorName) --> { op => IdentifierExpr.Op(op.binOperator) }

        case Rule.IdentifierParen(Rule.ParenDisallowed) =>
          tokenIdentifier --> IdentifierExpr.Named.apply |
            matchToken(KW_EXTENSION).discard ++! rule(Rule.Identifier) --> IdentifierExpr.Extension.apply |
            matchToken(KW_INVERSE).discard ++! rule(Rule.Identifier) --> IdentifierExpr.Inverse.apply |
            matchToken(KW_UPDATE).discard ++! rule(Rule.Identifier) --> IdentifierExpr.Update.apply

        case Rule.IdentifierParen(Rule.ParenAllowed) =>
          rule(Rule.IdentifierParen(Rule.ParenDisallowed)) |
            rule(Rule.OperatorIdentifier)

        case Rule.UnaryOperatorName =>
          matchToken(OP_LOGICAL_NOT) |
            matchToken(OP_PLUS) |
            matchToken(OP_MINUS) |
            matchToken(OP_BITNOT)

        case Rule.BinaryOperatorName =>
          matchToken(OP_EQUALS) |
            matchToken(OP_NOTEQUALS) |
            matchToken(OP_LESSTHANEQ) |
            matchToken(OP_GREATERTHANEQ) |
            matchToken(OP_SHIFTLEFT) |
            matchToken(OP_SHIFTRIGHT) |
            matchToken(OP_PLUS) |
            matchToken(OP_MINUS) |
            matchToken(OP_STAR) |
            matchToken(OP_MUL) |
            matchToken(OP_SLASH) |
            matchToken(OP_DIV) |
            matchToken(OP_BITAND) |
            matchToken(OP_BITOR) |
            matchToken(OP_BITXOR) |
            matchToken(OP_LESSTHAN) |
            matchToken(OP_GREATERTHAN) |
            matchToken(OP_CONCAT)

        case Rule.MethodName =>
          tokenUnderscore --> const(None) |
            rule(Rule.Identifier) ++ matchToken(OP_ASSIGN).? --> {
              case (name, None) => Some(name)
              case (name, Some(_)) => Some(IdentifierExpr.Update(name))
            }

        case Rule.NewLines => matchToken(NewLine).*.discard
        case Rule.StatementSeparator => matchToken(NewLine).discard | matchToken(Semicolon).discard

        case Rule.ImportStatement =>
          matchToken(KW_IMPORT).discard ++! rule(Rule.NewLines).discard ++ (
            rule(Rule.ImportPathTube) |
              rule(Rule.ImportPathAbsolute) |
              rule(Rule.ImportPathRelative) |
              rule(Rule.ImportPathMember)
            )

        case Rule.ExportStatement =>
          matchToken(KW_EXPORT).discard ++! (rule(Rule.NewLines).discard ++ (
            rule(Rule.ImportPathTube) |
              rule(Rule.ImportPathAbsolute) |
              rule(Rule.ImportPathRelative) |
              rule(Rule.ImportPathMember)
            ).observeLocation) --> ExportStmt.apply

        case Rule.IfExpr => matchToken(KW_IF).discard ++! rule(Rule.IfExprPart)
        case Rule.IfExprStart =>
          rule(Rule.Expression).observeLocation ++ matchToken(KW_THEN).discard ++ rule(Rule.StatementList).observeLocation

        case Rule.IfExprPart =>
          rule(Rule.IfExprStart) ++ matchToken(KW_END).observeLocation --> {
            case (condition, body, WithLocation(_, endLocation)) =>
              Expr.IfElse(condition, body, WithLocation(Vector.empty, endLocation))
          } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSE).discard ++! rule(Rule.StatementList).observeLocation ++ matchToken(
              KW_END
            ).discard --> { case (condition, body, elseBody) => Expr.IfElse(condition, body, elseBody) } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSIF).discard ++! rule(Rule.IfExprPart).observeLocation --> {
              case (condition, body, elseExpr) =>
                Expr.IfElse(condition, body, WithLocation(Vector(elseExpr), elseExpr.location))
            }

        case Rule.PrimaryExpr(Rule.ParenDisallowed) =>
          rule(Rule.IdentifierParen(Rule.ParenDisallowed)) |
            matchTokenFactory(StringToken) --> { str => Expr.StringLiteral(str.parts) } |
            matchTokenFactory(IntToken) --> { i => Expr.IntLiteral(i.value) } |
            matchToken(KW_TRUE) --> const(Expr.BoolLiteral(true)) |
            matchToken(KW_FALSE) --> const(Expr.BoolLiteral(false)) |
            rule(Rule.IfExpr) |
            matchToken(KW_EXTERN).discard ++! matchTokenFactory(Identifier) --> {
              case Identifier(id) => Expr.Extern(id)
            } |
            matchToken(KW_BEGIN).discard ++! rule(Rule.BlockBody) ++ matchToken(KW_END).discard |
            matchToken(OP_FUNCTION_RESULT_VALUE) --> const(Expr.FunctionResultValue) |
            matchToken(KW_ARGON_BUILTIN) ++! matchTokenFactory(Identifier) --> {
              case (_, Identifier(id)) => Expr.Builtin(id)
            }

        case Rule.PrimaryExpr(Rule.ParenAllowed) =>
          matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> const(Expr.Tuple(Seq.empty)) |
            rule(Rule.OperatorIdentifier) |
            matchToken(OP_OPENPAREN) ++ rule(Rule.Expression) ++ matchToken(OP_CLOSEPAREN) --> {
              case (_, expr, _) => expr
            } | rule(Rule.PrimaryExpr(Rule.ParenDisallowed))

        case Rule.PostfixExpr(Rule.ParenAllowed) =>
          createLeftRec(
            rule(Rule.PrimaryExpr(Rule.ParenAllowed))
          )(
            postfixExprCommon |
              rule(Rule.ParenArgList).observeLocation --> {
                case WithLocation((listType, argList), location) =>
                  (funcExpr: WithSource[Expr]) => Expr.FunctionCall(funcExpr, listType, WithLocation(argList, location))
              }
          )

        case Rule.PostfixExpr(Rule.ParenDisallowed) =>
          createLeftRec(rule(Rule.PrimaryExpr(Rule.ParenDisallowed)))(postfixExprCommon)

        case Rule.ParenArgList =>
          matchToken(OP_OPENPAREN) ++ matchToken(KW_REQUIRES).? ++ rule(Rule.Expression).? ++ matchToken(
            OP_CLOSEPAREN
          ) --> {
            case (_, Some(_), Some(argList), _) => (FunctionParameterListType.RequiresList, argList)
            case (_, Some(_), None, _) => (FunctionParameterListType.RequiresList, Expr.Tuple(Seq.empty))
            case (_, None, Some(argList), _) => (FunctionParameterListType.NormalList, argList)
            case (_, None, None, _) => (FunctionParameterListType.NormalList, Expr.Tuple(Seq.empty))
          }

        case Rule.MemberAccess =>
          rule(Rule.Identifier).observeLocation --> { id => (baseExpr: WithSource[Expr]) => Expr.Dot(baseExpr, id) }

        case Rule.CurryCallExpr =>
          createLeftRec(
            rule(Rule.PostfixExpr(Rule.ParenAllowed))
          )(
            rule(Rule.PostfixExpr(Rule.ParenDisallowed)).observeLocation --> {
              argExpr => (funcExpr: WithSource[Expr]) =>
                Expr.FunctionCall(funcExpr, FunctionParameterListType.NormalList, argExpr)
            } |
              rule(Rule.ParenArgList).observeLocation --> {
                case WithLocation((listType, argExpr), location) =>
                  (funcExpr: WithSource[Expr]) => Expr.FunctionCall(funcExpr, listType, WithLocation(argExpr, location))
              }
          )

        case Rule.UnaryExpr =>
          def matchUnaryOp[TToken <: Token & UnaryOperatorToken[?]](token: TToken)
            (using TypeTest[Token, TToken])
            : TGrammar[Expr] =
            matchToken(token) ++! rule(Rule.UnaryExpr).observeLocation --> { case (opToken, inner) =>
              Expr.UnaryOperation(opToken.unOperator, inner)
            }

          matchUnaryOp(OP_BITNOT) |
            matchUnaryOp(OP_LOGICAL_NOT) |
            matchUnaryOp(OP_PLUS) |
            matchUnaryOp(OP_MINUS) |
            rule(Rule.TypeExpr) |
            rule(Rule.CurryCallExpr)

        case Rule.TypeExpr =>
          (
            matchToken(KW_TYPE).discard ++! (
              matchToken(OP_OPENBRACKET).discard ++ rule(Rule.Expression).observeLocation ++ matchToken(OP_CLOSEBRACKET).discard
            ).? --> {
              level => Expr.Type(level)
            }
          ) | (
            matchToken(KW_BIGTYPE).discard ++! matchToken(OP_OPENBRACKET).discard ++ matchTokenFactory(IntToken) ++ matchToken(
              OP_CLOSEBRACKET
            ).discard --> {
              level => Expr.BigType(level.value)
            }
          )

        case Rule.MultiplicativeExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_MUL),
            ruleBinaryOperator(OP_STAR),
            ruleBinaryOperator(OP_DIV),
            ruleBinaryOperator(OP_SLASH),
          )(rule(Rule.UnaryExpr))

        case Rule.AdditiveExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_PLUS),
            ruleBinaryOperator(OP_MINUS),
            ruleBinaryOperator(OP_CONCAT),
          )(rule(Rule.MultiplicativeExpr))

        case Rule.ShiftExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_SHIFTLEFT),
            ruleBinaryOperator(OP_SHIFTRIGHT),
          )(rule(Rule.AdditiveExpr))

        case Rule.AndExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_BITAND)
          )(rule(Rule.ShiftExpr))

        case Rule.XorExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_BITXOR)
          )(rule(Rule.AndExpr))

        case Rule.OrExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_BITOR)
          )(rule(Rule.XorExpr))

        case Rule.LambdaTypeExpr =>
          rule(Rule.OrExpr).observeLocation ++ (matchToken(OP_LAMBDA_TYPE).discard ++! rule(
            Rule.LambdaTypeExpr
          ).observeLocation).? --> {
            case (WithLocation(left, _), None) => left
            case (left, Some(right)) => Expr.FunctionType(left, right)
          }

        case Rule.RelationalExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_LESSTHAN),
            ruleBinaryOperator(OP_LESSTHANEQ),
            ruleBinaryOperator(OP_GREATERTHAN),
            ruleBinaryOperator(OP_GREATERTHANEQ),
          )(rule(Rule.LambdaTypeExpr))

        case Rule.EqualityExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_EQUALS),
            ruleBinaryOperator(OP_NOTEQUALS),
          )(rule(Rule.RelationalExpr))

        case Rule.PropConjunctionExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_PROP_CONJUNCTION),
          )(rule(Rule.EqualityExpr))

        case Rule.PropDisjunctionExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_PROP_DISJUNCTION),
          )(rule(Rule.PropConjunctionExpr))

        case Rule.PropEqualityExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_PROP_EQUAL),
          )(rule(Rule.PropDisjunctionExpr))

        case Rule.AsExpr =>
          val nextRule = rule(Rule.PropEqualityExpr)

          nextRule.observeLocation ++ (matchToken(KW_AS).discard ++! nextRule.observeLocation).? --> {
            case (WithLocation(left, _), None) => left
            case (left, Some(right)) => Expr.As(left, right)
          }

        case Rule.LambdaExpr =>
          rule(Rule.IdentifierOptional) ++ matchToken(OP_LAMBDA) ++! rule(Rule.LambdaExpr).observeLocation --> {
            case (id, _, body) => Expr.FunctionLiteral(id, body)
          } |
            matchToken(KW_SUMMON).discard ++! rule(Rule.AsExpr).observeLocation --> {
              summonedType =>
                Expr.Summon(summonedType)
            } |
            rule(Rule.AsExpr)

        case Rule.PatternType =>
          rule(Rule.LambdaTypeExpr)

        case Rule.Type =>
          rule(Rule.PatternType)

        case Rule.TupleExpr =>
          val nextRule = rule(Rule.LambdaExpr)
          nextRule.observeLocation ++ (matchToken(OP_COMMA).discard ++! nextRule.observeLocation).* --> {
            case (WithLocation(expr, _), Chunk()) => expr
            case (head, tail) => Expr.Tuple((head +: tail).toVector)
          }

        case Rule.AssignExpr =>
          val nextRule = rule(Rule.TupleExpr)
          nextRule.observeLocation ++ (matchToken(OP_ASSIGN).discard ++! nextRule.observeLocation).? --> {
            case (WithLocation(left, _), None) => left
            case (left, Some(right)) =>
              Expr.BinaryOperation(left, BinaryOperator.Assign, right)
          }

        case Rule.AssertExpr =>
          matchToken(KW_ASSERT) ++! rule(Rule.TupleExpr).observeLocation --> {
            case (_, assertType) =>
              Expr.Assert(assertType)
          }

        case Rule.Expression =>
          rule(Rule.AssertExpr) | rule(Rule.AssignExpr)

        case Rule.ExpressionStmt =>
          rule(Rule.Expression)

        case Rule.VariableMutSpec =>
          matchToken(KW_VAL) --> const(false) |
            matchToken(KW_VAR) --> const(true)

        case Rule.VariableDeclaration =>
          rule(Rule.Modifiers) ++
            rule(Rule.VariableMutSpec) ++!
            rule(Rule.IdentifierOptional) ++
            (matchToken(OP_COLON).discard ++ rule(Rule.Type).observeLocation).? ++
            matchToken(OP_EQUALS).discard ++
            rule(Rule.Expression).observeLocation --> {
            case (modifiers, isMutable, id, typeAnnotation, value) =>
              VariableDeclarationStmt(modifiers, isMutable, id, typeAnnotation, value)
            }

        case Rule.Modifiers =>

          val anyModifier =
            ruleModifier(KW_PUBLIC) |
              ruleModifier(KW_PROTECTED) |
              ruleModifier(KW_PRIVATE) |
              ruleModifier(KW_INTERNAL) |
              ruleModifier(KW_PROOF) |
              ruleModifier(KW_ERASED) |
              ruleModifier(KW_INLINE)

          anyModifier.observeLocation.* --> {
            _.toVector
          }

        case Rule.MethodParameter =>
          rule(Rule.Identifier) ++!
          rule(Rule.NewLines) ++
          matchToken(OP_COLON).discard ++
          rule(Rule.NewLines) ++
          rule(Rule.Type).observeLocation --> {
              case (name, paramType) =>
                FunctionParameter(paramType, name)
            }

        case Rule.MethodParameterList =>
          (
            rule(Rule.MethodParameter).observeLocation ++
              rule(Rule.NewLines) ++
              (matchToken(OP_COMMA).discard ++ rule(Rule.MethodParameter).observeLocation ++ rule(Rule.NewLines).discard).* ++
              matchToken(OP_COMMA).? --> {
                case (firstParam, restParams, trailingComma) =>
                  (firstParam +: restParams, trailingComma.isDefined)
              }
          ).? --> { _.map { case (params, hasTrailingComma) => (params.toVector, hasTrailingComma) }.getOrElse((Vector.empty, false)) }

        case Rule.MethodParameters =>
          (
            (
              matchToken(OP_OPENPAREN).discard ++!
                rule(Rule.NewLines) ++
                matchToken(KW_REQUIRES).? ++
                rule(Rule.NewLines) ++
                matchToken(KW_ERASED).? ++
                rule(Rule.NewLines) ++
                rule(Rule.MethodParameterList) ++
                rule(Rule.NewLines) ++
                matchToken(OP_CLOSEPAREN).discard
                --> {
                case (requiresToken, erasedToken, (params, hasTrailingComma)) =>
                  val listType =
                    if requiresToken.isDefined then FunctionParameterListType.RequiresList
                    else FunctionParameterListType.NormalList
                  FunctionParameterList(
                    listType,
                    isErased = erasedToken.isDefined,
                    params,
                    hasTrailingComma = hasTrailingComma,
                  )
              }
            ) |
              (
                matchToken(OP_OPENBRACKET).discard ++!
                  rule(Rule.NewLines) ++
                  matchToken(KW_ERASED).? ++
                  rule(Rule.NewLines) ++
                  rule(Rule.MethodParameterList) ++
                  rule(Rule.NewLines) ++
                  matchToken(OP_CLOSEBRACKET).discard --> {
                  case (erasedToken, (params, hasTrailingComma)) =>
                    FunctionParameterList(
                      FunctionParameterListType.InferrableList,
                      isErased = erasedToken.isDefined,
                      params,
                      hasTrailingComma = hasTrailingComma,
                    )
                }
              )
          ).observeLocation.* --> { _.toVector }

        case Rule.MethodReturnType =>
          rule(Rule.Type).observeLocation ++
            (
              rule(Rule.NewLines) ++
                matchToken(KW_ENSURES).discard ++!
                rule(Rule.NewLines) ++
                rule(Rule.Type).observeLocation
            ).* --> {
            case (returnType, ensuresClauses) =>
              ReturnTypeSpecifier(returnType, ensuresClauses)
          }

        case Rule.MethodBody =>
          matchToken(KW_DO).discard ++! rule(Rule.BlockBody) ++ matchToken(KW_END).discard |
            matchToken(OP_EQUALS).discard ++! rule(Rule.NewLines) ++ rule(Rule.Expression)

        case Rule.BlockBody =>
          rule(Rule.StatementList).observeLocation ++
            (
              matchToken(KW_FINALLY).discard ++ rule(Rule.StatementList).observeLocation
            ).? --> {
            case (body, finallyBody) =>
              Expr.Block(body, finallyBody)
          }

        case Rule.MethodPurity =>
          matchToken(KW_DEF) --> const(true) |
            matchToken(KW_PROC) --> const(false)

        case Rule.FunctionDefinitionStmt =>
          rule(Rule.Modifiers) ++
            rule(Rule.MethodPurity) ++
            rule(Rule.IdentifierOptional).observeLocation ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodParameters) ++!
            matchToken(OP_COLON).discard ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodReturnType).observeLocation ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodBody).observeLocation --> {
              case (modifiers, purity, name, params, returnType, body) =>
                FunctionDeclarationStmt(modifiers, purity, name, params, returnType, body)
            }

        case Rule.RecordDeclarationStmt =>
          rule(Rule.Modifiers) ++
            matchToken(KW_RECORD).discard ++
            rule(Rule.Identifier).observeLocation ++
            rule(Rule.MethodParameters) ++
            (
              matchToken(OP_COLON).discard ++
                rule(Rule.NewLines) ++
                rule(Rule.Type).observeLocation
            ).? ++
            rule(Rule.StatementSeparator).discard ++
            rule(Rule.RecordBody) ++
            matchToken(KW_END).discard --> {
              case (modifiers, name, params, returnType, body) =>
                RecordDeclarationStmt(modifiers, name, params, returnType, body)
            }

        case Rule.RecordBody =>
          rule(Rule.StatementSeparator).*.discard ++ (
            (
              rule(Rule.RecordField) |
                rule(Rule.MethodDefinitionStmt)
            ).observeLocation ++ rule(
            Rule.StatementSeparator
          ).*.discard).* --> {
            stmts => stmts.toVector
          }

        case Rule.RecordField =>
          rule(Rule.VariableMutSpec).? ++
            rule(Rule.Identifier).observeLocation ++
            rule(Rule.NewLines) ++
            matchToken(OP_COLON).discard ++
            rule(Rule.Type).observeLocation --> {
              case (isMutable, name, t) =>
                RecordField(isMutable.getOrElse(false), name, t)
            }

        case Rule.MethodDefinitionStmt =>
          rule(Rule.Modifiers) ++
            rule(Rule.MethodPurity) ++
            rule(Rule.IdentifierOptional).observeLocation ++
            rule(Rule.NewLines) ++
            matchToken(OP_DOT).discard ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodName).observeLocation ++!
            rule(Rule.NewLines) ++
            rule(Rule.MethodParameters) ++
            matchToken(OP_COLON).discard ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodReturnType).observeLocation ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodBody).observeLocation.? --> {
              case (modifiers, purity, instanceName, name, params, returnType, body) =>
                MethodDeclarationStmt(modifiers, purity, instanceName, None, name, params, returnType, body)
            }

        case Rule.Statement =>
          rule(Rule.VariableDeclaration) |
            rule(Rule.FunctionDefinitionStmt) |
            rule(Rule.RecordDeclarationStmt) |
            rule(Rule.ExpressionStmt) |
            rule(Rule.ImportStatement) |
            rule(Rule.ExportStatement)

        case Rule.StatementList =>
          rule(Rule.StatementSeparator).*.discard ++ (rule(Rule.Statement).observeLocation ++ rule(
            Rule.StatementSeparator
          ).*.discard).* --> {
            stmts => stmts.toVector
          }

        case Rule.ImportPathAbsolute =>
          matchToken(OP_SLASH).discard ++ rule(Rule.NewLines) ++! rule(
            Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)
          ) --> ImportStmt.Absolute.apply

        case Rule.ImportPathRelative =>
          val currentLevel =
            matchToken(OP_DOT).discard ++ matchToken(OP_SLASH).discard ++! rule(Rule.NewLines) ++ rule(
              Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)
            ) --> {
              path => ImportStmt.Relative(0, path)
            }

          val fromParent =
            (matchToken(OP_DOTDOT).discard ++ matchToken(OP_SLASH).discard ++! rule(Rule.NewLines)).* ++ rule(Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)) --> {
              case (parentDirs, path) => ImportStmt.Relative(parentDirs.size, path)
            }

          currentLevel | fromParent

        case Rule.ImportPathTube =>
          rule(Rule.ImportPathTubeName) ++ matchToken(OP_SLASH).discard ++! rule(Rule.NewLines) ++ rule(
            Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)
          ) --> {
            case (packageName, path) => ImportStmt.Tube(packageName, path)
          }

        case Rule.ImportPathMember =>
          rule(Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Dot)) --> ImportStmt.Member.apply

        case Rule.ImportPathTubeName =>
          tokenIdentifier ++ (
            rule(Rule.NewLines) ++ matchToken(OP_DOT).discard ++! rule(Rule.NewLines) ++ tokenIdentifier
          ).* --> {
            case (h, t) => NonEmptySeq(h, t.toList)
          }

        case Rule.ImportPathSegmentRule(sep) =>
          val sepGrammar =
            sep match {
              case Rule.ImportPathSegmentSeparator.Slash => matchToken(OP_SLASH)
              case Rule.ImportPathSegmentSeparator.Dot => matchToken(OP_DOT)
            }

          val cons =
            tokenIdentifier ++ rule(Rule.NewLines) ++ sepGrammar.discard ++ rule(Rule.NewLines) ++! rule(
              Rule.ImportPathSegmentRule(sep)
            ) --> {
              case (id, subPath) => ImportPathSegment.Cons(id, subPath)
            }

          val many =
            matchToken(OP_OPENCURLY).discard ++ rule(Rule.NewLines) ++!
              (
                rule(Rule.ImportPathSegmentRule(sep)) ++
                  (
                    rule(Rule.NewLines) ++ rule(Rule.ImportPathSegmentRule(sep)) ++ rule(Rule.NewLines) ++ matchToken(
                      OP_COMMA
                    ).discard ++ rule(Rule.NewLines)
                  ).* ++
                  matchToken(OP_COMMA).?
              ).? --> {
                case Some((headSeg, tailSegs, _)) => ImportPathSegment.Many(headSeg +: tailSegs)
                case None => ImportPathSegment.Many(Seq.empty)
              }

          val renaming =
            rule(Rule.Identifier).observeLocation ++ rule(Rule.NewLines) ++ matchToken(OP_LAMBDA).discard ++ rule(Rule.NewLines) ++ rule(
              Rule.IdentifierOptional
            ).observeLocation --> {
              case (importing, viewedName) => ImportPathSegment.Renaming(importing, viewedName)
            }

          val imported = rule(Rule.Identifier).observeLocation --> ImportPathSegment.Imported.apply
          val wildcard = matchToken(OP_STAR).observeLocation --> { case WithLocation(_, loc) => ImportPathSegment.Wildcard(loc) }

          cons | many | renaming | imported | wildcard

        case Rule.PaddedStatement =>
          rule(Rule.StatementSeparator).*.discard ++ rule(Rule.Statement).observeLocation ++ rule(Rule.StatementSeparator).*.discard

        // TubeSpec
        case Rule.ModulePatternMappingStmt =>
          rule(Rule.NewLines) ++ rule(Rule.ModulePatternExpr) ++! rule(Rule.NewLines) ++ matchToken(OP_LAMBDA).discard ++ matchTokenFactory(StringToken) ++ rule(Rule.NewLines) --> {
            case (module, fileNameTemplate) => ModulePatternMapping(module, Expr.StringLiteral(fileNameTemplate.parts))
          }

        case Rule.ModulePatternExpr =>
          matchToken(OP_SLASH) --> const(Seq.empty) |
            rule(Rule.ModulePatternSegmentExpr).observeLocation ++ (rule(Rule.NewLines) ++ matchToken(OP_SLASH).discard ++ rule(Rule.NewLines) ++ rule(Rule.ModulePatternSegmentExpr).observeLocation).* --> {
              case (head, tail) => head +: tail
            }

        case Rule.ModulePatternSegmentExpr =>
          tokenIdentifier --> ModulePatternSegment.Named.apply |
            matchToken(OP_STAR).discard ++ rule(Rule.NewLines) ++ matchToken(KW_AS).discard ++ rule(Rule.NewLines) ++ rule(Rule.Identifier) --> { id => ModulePatternSegment.Star(id) } |
            matchToken(OP_STARSTAR).discard ++ rule(Rule.NewLines) ++ matchToken(KW_AS).discard ++ rule(Rule.NewLines) ++ rule(Rule.Identifier) --> { id => ModulePatternSegment.DoubleStar(id) }
      }

    // Expressions
    private def postfixExprCommon: TGrammar[WithSource[Expr] => Expr] =
      postfixExprMemberAccess | postfixExprRecordLit

    private def postfixExprMemberAccess: TGrammar[WithSource[Expr] => Expr] =
      (matchToken(OP_DOT).discard ++! rule(Rule.MemberAccess)) --> {
        memberAccessFunc => (baseExpr: WithSource[Expr]) => memberAccessFunc(baseExpr)
      }

    private def postfixExprRecordLit: TGrammar[WithSource[Expr] => Expr] =
      (
        matchToken(OP_OPENCURLY).discard ++!
          rule(Rule.StatementSeparator).*.discard ++
          (
            rule(Rule.Identifier).observeLocation ++!
              rule(Rule.NewLines) ++
              matchToken(OP_COLON).discard ++
              rule(Rule.NewLines) ++
              rule(Rule.Type).observeLocation ++
              rule(Rule.StatementSeparator).*.discard --> {
              case (name, t) => RecordFieldLiteral(name, t)
            }
          ).observeLocation.*.observeLocation ++
          matchToken(OP_CLOSECURLY).discard
      ) --> {
        fields => (baseExpr: WithSource[Expr]) =>
          Expr.RecordLiteral(baseExpr, fields)
      }
      

    private def createLeftAssociativeOperatorRule
      (firstOpGrammar: TGrammar[BinaryOperator], opGrammars: TGrammar[BinaryOperator]*)(nextGrammar: TGrammar[Expr])
      : TGrammar[Expr] = {
      val opGrammarsNec = NonEmptyChunk(firstOpGrammar, opGrammars*)

      val rightGrammars =
        opGrammarsNec.map { opGrammar =>
          Lazy {
            (opGrammar ++! nextGrammar.observeLocation) --> { case (op, right) =>
              (left: WithSource[Expr]) => Expr.BinaryOperation(left, op, right)
            }
          }
        }

      nextGrammar.observeLocation ++ UnionGrammar.fromList(rightGrammars).observeLocation.* --> {
        case (left, rightSeq) =>
          rightSeq.foldLeft(left) { case (l, WithLocation(f, rightLoc)) =>
            WithLocation(f(l), Location(fileName, l.location.start, rightLoc.end))
          }.value
      }
    }

    private def ruleBinaryOperator[TToken <: Token & BinaryOperatorToken[?]]
      (token: TToken)(using TypeTest[Token, TToken])
      : TGrammar[BinaryOperator] = matchToken(token) --> const(token.binOperator)

  }

  def parse(fileName: Option[String]): ZChannel[Any, Nothing, Chunk[WithSource[Token]], FilePosition, SyntaxError, Chunk[WithSource[Stmt]], FilePosition] =
    Grammar.parseAll(ArgonGrammarFactory(fileName))(Rule.PaddedStatement).mapError(SyntaxError.ParserError(fileName, _))

  def parseTubeSpec(fileName: Option[String]): ZChannel[Any, Nothing, Chunk[WithSource[Token]], FilePosition, SyntaxError, Chunk[ModulePatternMapping], FilePosition] =
    Grammar.parseAll(ArgonGrammarFactory(fileName))(Rule.ModulePatternMappingStmt).mapError(SyntaxError.ParserError(fileName, _))

}
