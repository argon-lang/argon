package dev.argon.parser.impl

import dev.argon.parser.Token.*
import dev.argon.parser.*
import dev.argon.util.{*, given}
import scala.reflect.TypeTest
import scala.language.postfixOps
import dev.argon.grammar.{Grammar, GrammarError, TokenMatcher}
import Grammar.Operators.*
import Grammar.{GrammarFactory, UnionGrammar}
import zio.{Chunk, NonEmptyChunk}
import zio.stream.ZChannel
import Function.const

object ArgonParser {

  private[parser] object Rule {

    sealed trait ParenAllowedState derives CanEqual
    case object ParenAllowed extends ParenAllowedState

    case object ParenDisallowed extends ParenAllowedState

    sealed trait ArgonRuleName[T]

    object ArgonRuleName {
      given [T, U]: CanEqual[ArgonRuleName[T], ArgonRuleName[U]] = CanEqual.canEqualAny
    }

    case object IdentifierOptional extends ArgonRuleName[Option[IdentifierExpr]]
    case object OperatorIdentifier extends ArgonRuleName[IdentifierExpr]
    final case class IdentifierParen(parenAllowed: ParenAllowedState) extends ArgonRuleName[IdentifierExpr]
    val Identifier: IdentifierParen = IdentifierParen(ParenAllowed)
    case object OperatorName extends ArgonRuleName[OperatorToken]
    case object MethodName extends ArgonRuleName[Option[IdentifierExpr]]
    case object NewLines extends ArgonRuleName[Unit]
    case object StatementSeparator extends ArgonRuleName[Unit]
    case object ImportStatement extends ArgonRuleName[ImportStmt]

    // If
    case object IfExpr extends ArgonRuleName[Expr]
    case object IfExprStart extends ArgonRuleName[(WithSource[Expr], WithSource[Vector[WithSource[Stmt]]])]
    case object IfExprPart extends ArgonRuleName[Expr]

    // Pattern Matching
    case object ParenPattern extends ArgonRuleName[Pattern]
    case object VariablePattern extends ArgonRuleName[Pattern]
    case object DiscardPattern extends ArgonRuleName[Pattern]
    case object ConstructorExprPattern extends ArgonRuleName[Expr]
    case object ConstructorExprPatternIdPath extends ArgonRuleName[Expr]
    case object ContainedPattern extends ArgonRuleName[Pattern]
    case object PatternSeq extends ArgonRuleName[Vector[WithSource[Pattern]]]
    case object DeconstructPattern extends ArgonRuleName[Pattern]
    case object PatternSpec extends ArgonRuleName[Pattern]
    case object MatchCase extends ArgonRuleName[MatchExprCase]
    case object MatchExpr extends ArgonRuleName[Expr]

    // Common Expressions
    final case class PrimaryExpr(parenAllowed: ParenAllowedState) extends ArgonRuleName[Expr]
    final case class PostfixExpr(parenAllowed: ParenAllowedState) extends ArgonRuleName[Expr]
    case object CurryCallExpr extends ArgonRuleName[Expr]
    case object ParenArgList extends ArgonRuleName[(FunctionParameterListType, Expr)]
    case object MemberAccess extends ArgonRuleName[WithSource[Expr] => Expr]
    case object UnaryExpr extends ArgonRuleName[Expr]
    case object TypeExpr extends ArgonRuleName[Expr]
    case object IntersectionExpr extends ArgonRuleName[Expr]
    case object UnionExpr extends ArgonRuleName[Expr]
    case object MultiplicativeExpr extends ArgonRuleName[Expr]
    case object AdditiveExpr extends ArgonRuleName[Expr]
    case object ShiftExpr extends ArgonRuleName[Expr]
    case object AndExpr extends ArgonRuleName[Expr]
    case object XorExpr extends ArgonRuleName[Expr]
    case object OrExpr extends ArgonRuleName[Expr]
    case object LambdaTypeExpr extends ArgonRuleName[Expr]
    case object RelationalExpr extends ArgonRuleName[Expr]
    case object EqualityExpr extends ArgonRuleName[Expr]
    case object AsExpr extends ArgonRuleName[Expr]
    case object LambdaExpr extends ArgonRuleName[Expr]
    case object PatternType extends ArgonRuleName[Expr]
    case object Type extends ArgonRuleName[Expr]
    case object TupleExpr extends ArgonRuleName[Expr]
    case object AssignExpr extends ArgonRuleName[Expr]
    case object Expression extends ArgonRuleName[Expr]
    case object ExpressionStmt extends ArgonRuleName[Expr]

    // Variable Declaration
    case object VariableMutSpec extends ArgonRuleName[Boolean]
    case object VariableDeclaration extends ArgonRuleName[Stmt]

    // Fields
    case object FieldDeclarationStmt extends ArgonRuleName[Stmt]
    case object FieldInitializationStmt extends ArgonRuleName[Stmt]
    case object InitializeStmt extends ArgonRuleName[Stmt]

    case object Modifiers extends ArgonRuleName[Vector[WithSource[Modifier]]]

    // Functions and Methods
    case object MethodParameter extends ArgonRuleName[FunctionParameter]
    case object MethodParameterList extends ArgonRuleName[Vector[WithSource[FunctionParameter]]]
    case object MethodParameters extends ArgonRuleName[Vector[WithSource[FunctionParameterList]]]
    case object MethodBody extends ArgonRuleName[Expr]
    case object BlockBody extends ArgonRuleName[BlockExpr]
    case object MethodPurity extends ArgonRuleName[Boolean]
    case object FunctionDefinitionStmt extends ArgonRuleName[Stmt]
    case object MethodDefinitionStmt extends ArgonRuleName[Stmt]
    case object ClassConstructorDefinitionStmt extends ArgonRuleName[Stmt]

    // Types
    case object StaticInstanceBody extends ArgonRuleName[(Vector[WithSource[Stmt]], Vector[WithSource[Stmt]])]
    case object BaseTypeSpecifier extends ArgonRuleName[Option[WithSource[Expr]]]
    case object TraitDeclarationStmt extends ArgonRuleName[Stmt]
    case object DataConstructorDeclarationStmt extends ArgonRuleName[Stmt]
    case object ClassDeclarationStmt extends ArgonRuleName[Stmt]

    case object Statement extends ArgonRuleName[Stmt]
    case object StatementList extends ArgonRuleName[Vector[WithSource[Stmt]]]
    case object ImportPathAbsolute extends ArgonRuleName[ImportStmt]
    case object ImportPathRelative extends ArgonRuleName[ImportStmt]
    case object ImportPathTube extends ArgonRuleName[ImportStmt]
    case object ImportPathMember extends ArgonRuleName[ImportStmt]
    case object ImportPathTubeName extends ArgonRuleName[NonEmptyList[String]]

    final case class ImportPathSegmentRule(separator: ImportPathSegmentSeparator)
        extends ArgonRuleName[ImportPathSegment]

    enum ImportPathSegmentSeparator derives CanEqual {
      case Slash, Dot
    }

    case object PaddedStatement extends ArgonRuleName[Stmt]

  }

  private[parser] object ArgonGrammarFactory extends GrammarFactory[Token, SyntaxError, Rule.ArgonRuleName] {

    private implicit val errorFactory: Grammar.ErrorFactory[Token, TokenCategory, SyntaxError] =
      new Grammar.ErrorFactory[Token, TokenCategory, SyntaxError] {
        override def createError(error: GrammarError[Token, TokenCategory]): SyntaxError = SyntaxError.ParserError(error)

        override def errorEndLocationOrder: Ordering[SyntaxError] =
          (a, b) => implicitly[Ordering[FilePosition]].compare(a.location.end, b.location.end)

      }

    def second[T](pair: (?, T)): T = pair._2

    private def matchTokenFactory[TToken <: Token](factory: TokenFactory[TToken])(using tt: TypeTest[Token, TToken])
      : TGrammar[TToken] = Grammar.matcher(factory.category, TokenMatcher.Subtype[Token, TToken](tt))

    private def matchToken[TToken <: TokenWithCategory[? <: TokenCategory] with Token](token: TToken)
      (using tt: TypeTest[Token, TToken])
      : TGrammar[TToken] = Grammar.matcher(token.category, TokenMatcher.Subtype[Token, TToken](tt))

    private val tokenUnderscore: TGrammar[Unit] = matchToken(KW_UNDERSCORE).discard

    private val tokenIdentifier: TGrammar[String] = matchTokenFactory(Identifier) --> { _.name }

    private def createLeftRec[A](left: TGrammar[A])(rightRepeat: TGrammar[WithSource[A] => A]): TGrammar[A] =
      left.observeSource ++ (rightRepeat.observeSource*) --> { case (a, fs) =>
        fs.foldLeft(a) { case (a, WithSource(f, loc)) =>
          WithSource(f(a), SourceLocation.merge(a.location, loc))
        }.value
      }

    protected override def createGrammar[T](name: Rule.ArgonRuleName[T]): TGrammar[T] =
      name match {
        case Rule.IdentifierOptional =>
          tokenUnderscore --> const(None: Option[IdentifierExpr]) |
            rule(Rule.Identifier) --> Some.apply

        case Rule.OperatorIdentifier =>
          matchToken(OP_OPENPAREN) ++ rule(Rule.OperatorName) ++ matchToken(OP_CLOSEPAREN) --> { case (_, op, _) =>
            IdentifierExpr.OperatorIdentifier(op.operator)
          }

        case Rule.IdentifierParen(Rule.ParenDisallowed) =>
          tokenIdentifier --> IdentifierExpr.Named.apply |
            matchToken(KW_EXTENSION) ++! rule(Rule.Identifier) --> { case (_, inner) =>
              IdentifierExpr.Extension(inner)
            } |
            matchToken(KW_INVERSE) ++! rule(Rule.Identifier) --> { case (_, inner) => IdentifierExpr.Inverse(inner) } |
            matchToken(KW_UPDATE) ++! rule(Rule.Identifier) --> { case (_, inner) => IdentifierExpr.Update(inner) }

        case Rule.IdentifierParen(Rule.ParenAllowed) =>
          rule(Rule.IdentifierParen(Rule.ParenDisallowed)) |
            rule(Rule.OperatorIdentifier)

        case Rule.OperatorName =>
          matchToken(OP_EQUALS) |
            matchToken(OP_NOTEQUALS) |
            matchToken(OP_LESSTHANEQ) |
            matchToken(OP_GREATERTHANEQ) |
            matchToken(OP_SHIFTLEFT) |
            matchToken(OP_SHIFTRIGHT) |
            matchToken(OP_BOOLNOT) |
            matchToken(OP_ADD) |
            matchToken(OP_SUB) |
            matchToken(OP_MUL) |
            matchTokenFactory(DivisionOperator) |
            matchToken(OP_BITAND) |
            matchToken(OP_BITOR) |
            matchToken(OP_BITXOR) |
            matchToken(OP_BITNOT) |
            matchToken(OP_LESSTHAN) |
            matchToken(OP_GREATERTHAN) |
            matchToken(OP_UNION) |
            matchToken(OP_INTERSECTION)

        case Rule.MethodName =>
          tokenUnderscore --> const(None) |
            rule(Rule.Identifier) ++ matchToken(OP_ASSIGN).? --> {
              case (name, None) => Some(name)
              case (name, Some(_)) => Some(IdentifierExpr.Update(name))
            }

        case Rule.NewLines => (matchToken(NewLine) *).discard
        case Rule.StatementSeparator => matchToken(NewLine).discard | matchToken(Semicolon).discard

        case Rule.ImportStatement =>
          matchToken(KW_IMPORT) ++! (rule(Rule.NewLines) ++ (
            rule(Rule.ImportPathAbsolute) |
              rule(Rule.ImportPathRelative) |
              rule(Rule.ImportPathTube) |
              rule(Rule.ImportPathMember)
          )) --> {
            case (_, (_, ns)) => ns
          }

        case Rule.IfExpr => matchToken(KW_IF) ++! rule(Rule.IfExprPart) --> second
        case Rule.IfExprStart =>
          rule(Rule.Expression).observeSource ++ matchToken(KW_THEN) ++ rule(Rule.StatementList).observeSource --> {
            case (condition, _, body) => (condition, body)
          }

        case Rule.IfExprPart =>
          rule(Rule.IfExprStart) ++ matchToken(KW_END).observeSource --> {
            case (condition, body, WithSource(_, endLocation)) =>
              IfElseExpr(condition, body, WithSource(Vector.empty, endLocation)): Expr
          } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSE) ++! rule(Rule.StatementList).observeSource ++ matchToken(
              KW_END
            ) --> { case (condition, body, _, elseBody, _) => IfElseExpr(condition, body, elseBody) } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSIF) ++! rule(Rule.IfExprPart).observeSource --> {
              case (condition, body, _, elseExpr) =>
                IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location))
            }

        case Rule.ParenPattern =>
          matchToken(OP_OPENPAREN) ++! rule(Rule.PatternSpec) ++ matchToken(OP_CLOSEPAREN) --> {
            case (_, pattern, _) => pattern
          }

        case Rule.VariablePattern =>
          matchToken(KW_VAL) ++! rule(Rule.IdentifierOptional) ++ ((matchToken(OP_COLON) ++ rule(
            Rule.PatternType
          ).observeSource) ?) --> {
            case (_, idOpt, Some((_, varType))) => TypeTestPattern(idOpt, varType)
            case (_, idOpt, None) => BindingPattern(idOpt)
          }

        case Rule.DiscardPattern =>
          matchToken(KW_UNDERSCORE) --> const(BindingPattern(None))

        case Rule.ConstructorExprPattern =>
          rule(Rule.ConstructorExprPatternIdPath) |
            matchToken(OP_OPENCURLY) ++ rule(Rule.Expression) ++ matchToken(OP_CLOSECURLY) --> { case (_, expr, _) =>
              expr
            }

        case Rule.ConstructorExprPatternIdPath =>
          createLeftRec(
            rule(Rule.Identifier)
          )(
            (matchToken(OP_DOT) ++ rule(Rule.Identifier)) --> {
              case (_, id) => (baseExpr: WithSource[Expr]) => DotExpr(baseExpr, id)
            }
          )

        case Rule.ContainedPattern =>
          rule(Rule.ConstructorExprPattern).observeSource --> { expr => DeconstructPattern(expr, Vector()): Pattern } |
            rule(Rule.DiscardPattern) |
            rule(Rule.ParenPattern)

        case Rule.PatternSeq =>
          (rule(Rule.ContainedPattern).observeSource *) --> { _.toVector }

        case Rule.DeconstructPattern =>
          rule(Rule.ConstructorExprPattern).observeSource ++ rule(
            Rule.PatternSeq
          ) --> (DeconstructPattern.apply _).tupled

        case Rule.PatternSpec =>
          rule(Rule.ParenPattern) |
            rule(Rule.VariablePattern) |
            rule(Rule.DeconstructPattern) |
            rule(Rule.DiscardPattern)

        case Rule.MatchCase =>
          rule(Rule.NewLines) ++ matchToken(KW_CASE) ++! (rule(Rule.NewLines) ++ rule(
            Rule.PatternSpec
          ).observeSource ++ matchToken(OP_EQUALS) ++ rule(Rule.StatementList).observeSource) --> {
            case (_, _, (_, pattern, _, body)) => MatchExprCase(pattern, body)
          }

        case Rule.MatchExpr =>
          matchToken(KW_MATCH) ++! (rule(Rule.Expression).observeSource ++ (rule(
            Rule.MatchCase
          ).observeSource*) ++ matchToken(KW_END)) --> {
            case (_, (cmpValue, cases, _)) =>
              MatchExpr(cmpValue, cases)
          }

        case Rule.PrimaryExpr(Rule.ParenDisallowed) =>
          rule(Rule.IdentifierParen(Rule.ParenDisallowed)) |
            matchTokenFactory(StringToken) --> { str => StringValueExpr(str) } |
            matchTokenFactory(IntToken) --> IntValueExpr.apply |
            matchToken(KW_TRUE) --> const(BoolValueExpr(true)) |
            matchToken(KW_FALSE) --> const(BoolValueExpr(false)) |
            rule(Rule.IfExpr) |
            rule(Rule.MatchExpr) |
            (matchToken(KW_EXTERN) ++! matchTokenFactory(Identifier)) --> {
              case (_, Identifier(id)) => ExternExpr(id)
            } |
            (matchToken(KW_BEGIN) ++! (rule(Rule.BlockBody) ++ matchToken(KW_END))) --> {
              case (_, (block, _)) => block
            }

        case Rule.PrimaryExpr(Rule.ParenAllowed) =>
          matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> const(UnitLiteral) |
            rule(Rule.OperatorIdentifier) |
            matchToken(OP_OPENPAREN) ++ rule(Rule.Expression) ++ matchToken(OP_CLOSEPAREN) --> {
              case (_, expr, _) => expr
            } | rule(Rule.PrimaryExpr(Rule.ParenDisallowed))

        case Rule.PostfixExpr(Rule.ParenAllowed) =>
          createLeftRec(
            rule(Rule.PrimaryExpr(Rule.ParenAllowed))
          )(
            postfixExprMemberAccess |
              rule(Rule.ParenArgList).observeSource --> {
                case WithSource((listType, argList), location) =>
                  (funcExpr: WithSource[Expr]) => FunctionCallExpr(funcExpr, listType, WithSource(argList, location))
              }
          )

        case Rule.PostfixExpr(Rule.ParenDisallowed) =>
          createLeftRec(rule(Rule.PrimaryExpr(Rule.ParenDisallowed)))(postfixExprMemberAccess)

        case Rule.ParenArgList =>
          matchToken(OP_OPENPAREN) ++ (matchToken(KW_REQUIRES) ?) ++ (rule(Rule.Expression) ?) ++ matchToken(
            OP_CLOSEPAREN
          ) --> {
            case (_, Some(_), Some(argList), _) => (FunctionParameterListType.RequiresList, argList)
            case (_, Some(_), None, _) => (FunctionParameterListType.RequiresList, UnitLiteral)
            case (_, None, Some(argList), _) => (FunctionParameterListType.NormalList, argList)
            case (_, None, None, _) => (FunctionParameterListType.NormalList, UnitLiteral)
          }

        case Rule.MemberAccess =>
          rule(Rule.Identifier) --> { id => (baseExpr: WithSource[Expr]) => DotExpr(baseExpr, id) } |
            matchToken(KW_NEW) --> const(ClassConstructorExpr.apply _) |
            matchToken(KW_TYPE) --> const(TypeOfExpr.apply _)

        case Rule.CurryCallExpr =>
          createLeftRec(
            rule(Rule.PostfixExpr(Rule.ParenAllowed))
          )(
            rule(Rule.PostfixExpr(Rule.ParenDisallowed)).observeSource --> {
              argExpr => (funcExpr: WithSource[Expr]) =>
                FunctionCallExpr(funcExpr, FunctionParameterListType.NormalList, argExpr)
            } |
              rule(Rule.ParenArgList).observeSource --> {
                case WithSource((listType, argExpr), location) =>
                  (funcExpr: WithSource[Expr]) => FunctionCallExpr(funcExpr, listType, WithSource(argExpr, location))
              }
          )

        case Rule.UnaryExpr =>
          def matchUnaryOp[TToken <: TokenWithCategory[? <: TokenCategory] with UnaryOperatorToken](token: TToken)
            (using TypeTest[Token, TToken])
            : TGrammar[Expr] =
            matchToken(token).observeSource ++! rule(Rule.UnaryExpr).observeSource --> { case (opToken, inner) =>
              UnaryOperatorExpr(opToken.map(_.operator), inner)
            }

          matchUnaryOp(OP_BITNOT) |
            matchUnaryOp(OP_BOOLNOT) |
            matchUnaryOp(OP_ADD) |
            matchUnaryOp(OP_SUB) |
            rule(Rule.TypeExpr) |
            rule(Rule.CurryCallExpr)

        case Rule.TypeExpr =>
          (
            matchToken(KW_TYPE) ++! ((matchToken(OP_OPENBRACKET) ++ rule(Rule.Expression).observeSource ++ matchToken(
              OP_CLOSEBRACKET
            ) --> { case (_, level, _) => level }) ?) --> {
              case (_, level) => TypeExpr(level)
            }
          ) | (
            matchToken(KW_METATYPE) ++! (matchToken(OP_OPENBRACKET) ++ matchTokenFactory(IntToken) ++ matchToken(
              OP_CLOSEBRACKET
            )) --> {
              case (_, (_, level, _)) => MetaTypeExpr(IntValueExpr(level).value)
            }
          )

        case Rule.IntersectionExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_INTERSECTION)
          )(rule(Rule.UnaryExpr))

        case Rule.UnionExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_UNION)
          )(rule(Rule.IntersectionExpr))

        case Rule.MultiplicativeExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_MUL),
            ruleBinaryOperator(OP_DIV),
          )(rule(Rule.UnionExpr))

        case Rule.AdditiveExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_ADD),
            ruleBinaryOperator(OP_SUB),
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
          rule(Rule.OrExpr).observeSource ++ ((matchToken(OP_LAMBDA_TYPE) ++! rule(
            Rule.LambdaTypeExpr
          ).observeSource) ?) --> {
            case (WithSource(left, _), None) => left
            case (left, Some((_, right))) => LambdaTypeExpr(left, right)
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

        case Rule.AsExpr =>
          val nextRule = rule(Rule.EqualityExpr)

          nextRule.observeSource ++ ((matchToken(KW_AS) ++! nextRule.observeSource) ?) --> {
            case (WithSource(left, _), None) => left
            case (left, Some((_, right))) => AsExpr(left, right)
          }

        case Rule.LambdaExpr =>
          rule(Rule.IdentifierOptional) ++ matchToken(OP_LAMBDA) ++! rule(Rule.LambdaExpr).observeSource --> {
            case (id, _, body) => LambdaExpr(id, body)
          } |
            rule(Rule.AsExpr)

        case Rule.PatternType =>
          rule(Rule.LambdaTypeExpr)

        case Rule.Type =>
          rule(Rule.PatternType)

        case Rule.TupleExpr =>
          val nextRule = rule(Rule.LambdaExpr)
          nextRule.observeSource ++ ((matchToken(OP_COMMA) ++! nextRule.observeSource --> second)*) --> {
            case (WithSource(expr, _), Chunk()) => expr
            case (head, tail) => TupleExpr(NonEmptyList.cons(head, tail.toList))
          }

        case Rule.AssignExpr =>
          val nextRule = rule(Rule.TupleExpr)
          nextRule.observeSource ++ ((matchToken(OP_ASSIGN).observeSource ++! nextRule.observeSource) ?) --> {
            case (WithSource(left, _), None) => left
            case (left, Some((WithSource(_, opLocation), right))) =>
              BinaryOperatorExpr(WithSource(BinaryOperator.Assign, opLocation), left, right)
          }

        case Rule.Expression =>
          rule(Rule.AssignExpr)

        case Rule.ExpressionStmt =>
          rule(Rule.Expression)

        case Rule.VariableMutSpec =>
          matchToken(KW_VAL) --> const(false) |
            matchToken(KW_VAR) --> const(true)

        case Rule.VariableDeclaration =>
          rule(Rule.VariableMutSpec) ++! (
            rule(Rule.IdentifierOptional) ++
              ((matchToken(OP_COLON) ++ rule(Rule.Type).observeSource --> second) ?) ++
              matchToken(OP_EQUALS) ++
              rule(Rule.Expression).observeSource
          ) --> { case (isMutable, (id, typeAnnotation, _, value)) =>
            VariableDeclarationStmt(isMutable, typeAnnotation, id, value)
          }

        case Rule.FieldDeclarationStmt =>
          matchToken(KW_FIELD) ++
            (rule(Rule.VariableMutSpec) ?) ++
            rule(Rule.Identifier) ++
            matchToken(OP_COLON) ++!
            rule(Rule.Type).observeSource --> { case (_, isMutable, id, _, typeAnnotation) =>
              FieldDeclarationStmt(isMutable.getOrElse(false), id, typeAnnotation)
            }

        case Rule.FieldInitializationStmt =>
          matchToken(KW_FIELD) ++
            rule(Rule.Identifier) ++
            matchToken(OP_EQUALS) ++!
            rule(Rule.Expression).observeSource --> { case (_, id, _, value) =>
              FieldInitializationStmt(id, value)
            }

        case Rule.InitializeStmt =>
          matchToken(KW_INITIALIZE) ++
            rule(Rule.IdentifierOptional) ++
            (
              (matchToken(OP_EQUALS) ++ rule(Rule.Expression).observeSource --> second) ?
            ) --> { case (_, id, value) =>
              InitializeStmt(id, value)
            }

        case Rule.Modifiers =>
          def ruleModifier[TToken <: TokenWithCategory[? <: TokenCategory] with ModifierToken](token: TToken)
            (using TypeTest[Token, TToken])
            : TGrammar[Modifier] = matchToken(token) --> const(token.modifier)

          val anyModifier =
            ruleModifier(KW_PUBLIC) |
              ruleModifier(KW_PROTECTED) |
              ruleModifier(KW_PRIVATE) |
              ruleModifier(KW_INTERNAL) |
              ruleModifier(KW_VIRTUAL) |
              ruleModifier(KW_ABSTRACT) |
              ruleModifier(KW_OVERRIDE) |
              ruleModifier(KW_SEALED) |
              ruleModifier(KW_OPEN)

          (anyModifier.observeSource *) --> { _.toVector }

        case Rule.MethodParameter =>
          rule(Rule.Identifier) ++
            rule(Rule.NewLines) ++
            ((matchToken(OP_COLON) ++! (rule(Rule.NewLines) ++ rule(Rule.Type).observeSource) --> { case (_, (_, t)) =>
              t
            }) ?) ++
            ((matchToken(OP_SUBTYPE) ++! (rule(Rule.NewLines) ++ rule(Rule.Type).observeSource) --> {
              case (_, (_, t)) => t
            }) ?) --> {
              case (name, _, paramType, subTypeOf) =>
                FunctionParameter(paramType, subTypeOf, name)
            }

        case Rule.MethodParameterList =>
          ((
            rule(Rule.MethodParameter).observeSource ++
              rule(Rule.NewLines) ++
              ((matchToken(OP_COMMA) ++ rule(Rule.MethodParameter).observeSource ++ rule(Rule.NewLines) --> {
                case (_, param, _) => param
              })*) ++
              (matchToken(OP_COMMA) ?) --> {
                case (firstParam, _, restParams, _) =>
                  firstParam +: restParams
              }
          ) ?) --> { _.map(_.toVector).getOrElse(Vector.empty) }

        case Rule.MethodParameters =>
          ((
            (
              matchToken(OP_OPENPAREN) ++! (rule(Rule.NewLines) ++ matchToken(KW_REQUIRES).? ++ rule(
                Rule.NewLines
              ) ++ matchToken(KW_ERASED).? ++ rule(Rule.NewLines) ++ rule(Rule.MethodParameterList) ++ rule(
                Rule.NewLines
              ) ++ matchToken(OP_CLOSEPAREN)) --> {
                case (_, (_, requiresToken, _, erasedToken, _, params, _, _)) =>
                  val listType =
                    if requiresToken.isDefined then FunctionParameterListType.RequiresList
                    else FunctionParameterListType.NormalList
                  FunctionParameterList(listType, isErased = erasedToken.isDefined, params)
              }
            ) |
              (
                matchToken(OP_OPENBRACKET) ++! (rule(Rule.NewLines) ++ matchToken(KW_ERASED).? ++ rule(
                  Rule.NewLines
                ) ++ rule(Rule.MethodParameterList) ++ rule(Rule.NewLines) ++ matchToken(OP_CLOSEBRACKET)) --> {
                  case (_, (_, erasedToken, _, params, _, _)) =>
                    FunctionParameterList(
                      FunctionParameterListType.InferrableList,
                      isErased = erasedToken.isDefined,
                      params,
                    )
                }
              )
          ).observeSource *) --> { _.toVector }

        case Rule.MethodBody =>
          matchToken(KW_DO) ++! (rule(Rule.BlockBody) ++ matchToken(KW_END)) --> { case (_, (body, _)) => body: T } |
            matchToken(OP_EQUALS) ++! (rule(Rule.NewLines) ++ rule(Rule.Expression)) --> { case (_, (_, expr)) => expr }

        case Rule.BlockBody =>
          rule(Rule.StatementList).observeSource ++
            (matchToken(KW_RESCUE) ++! (rule(Rule.NewLines) ++ rule(Rule.PatternSpec).observeSource ++ rule(
              Rule.StatementList
            ).observeSource)).* ++
            (matchToken(KW_ELSE) ++! rule(Rule.StatementList).observeSource).? ++
            (matchToken(KW_ENSURE) ++! rule(Rule.StatementList).observeSource).? --> {
              case (body, rescueCases, elseBody, ensureBody) =>
                BlockExpr(
                  body,
                  rescueCases.map {
                    case (_, (_, pattern, rescueBody)) =>
                      MatchExprCase(pattern, rescueBody)
                  }.toVector,
                  elseBody.map { case (_, stmts) => stmts },
                  ensureBody.map { case (_, stmts) => stmts },
                )
            }

        case Rule.MethodPurity =>
          matchToken(KW_DEF) --> const(true) |
            matchToken(KW_PROC) --> const(false)

        case Rule.FunctionDefinitionStmt =>
          rule(Rule.Modifiers) ++
            rule(Rule.MethodPurity) ++
            rule(Rule.IdentifierOptional) ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodParameters) ++! (
              matchToken(OP_COLON) ++
                rule(Rule.NewLines) ++
                rule(Rule.Type).observeSource ++
                rule(Rule.NewLines) ++
                rule(Rule.MethodBody).observeSource
            ) --> {
              case (modifiers, purity, name, _, params, (_, _, returnType, _, body)) =>
                FunctionDeclarationStmt(name, params, returnType, body, modifiers, purity)
            }

        case Rule.MethodDefinitionStmt =>
          rule(Rule.Modifiers) ++
            rule(Rule.MethodPurity) ++
            rule(Rule.IdentifierOptional) ++
            rule(Rule.NewLines) ++
            matchToken(OP_DOT) ++
            rule(Rule.NewLines) ++
            rule(Rule.MethodName) ++! (
              rule(Rule.NewLines) ++
                rule(Rule.MethodParameters) ++
                matchToken(OP_COLON) ++
                rule(Rule.NewLines) ++
                rule(Rule.Type).observeSource ++
                rule(Rule.NewLines) ++
                (rule(Rule.MethodBody).observeSource ?)
            ) --> {
              case (modifiers, purity, instanceName, _, _, _, name, (_, params, _, _, returnType, _, body)) =>
                MethodDeclarationStmt(instanceName, name, params, returnType, body, modifiers, purity)
            }

        case Rule.ClassConstructorDefinitionStmt =>
          rule(Rule.Modifiers) ++
            rule(Rule.MethodPurity).? ++
            matchToken(KW_NEW) ++! (
              rule(Rule.MethodParameters) ++
                rule(Rule.StatementSeparator) ++
                rule(Rule.StatementList).observeSource ++
                matchToken(KW_END)
            ) --> {
              case (modifiers, purity, _, (params, _, body, _)) =>
                ClassConstructorDeclarationStmt(params, body, modifiers, purity.getOrElse(true))
            }

        case Rule.StaticInstanceBody =>
          rule(Rule.NewLines) ++ matchToken(KW_STATIC) ++! (rule(Rule.StatementList) ++ ((matchToken(
            KW_INSTANCE
          ) ++! rule(Rule.StatementList) --> { case (_, instanceBody) => instanceBody }) ?)) --> {
            case (_, _, (staticBody, instanceBody)) =>
              (staticBody, instanceBody.getOrElse(Vector.empty))
          } |
            ((rule(Rule.NewLines) ++ matchToken(KW_INSTANCE)) ?) ++ rule(Rule.StatementList) --> {
              case (_, instanceBody) =>
                (Vector.empty, instanceBody)
            }

        case Rule.BaseTypeSpecifier =>
          matchToken(KW_UNDERSCORE) --> { _ => Option.empty[WithSource[Expr]] } |
            rule(Rule.Type).observeSource --> Some.apply

        case Rule.TraitDeclarationStmt =>
          rule(Rule.Modifiers) ++
            matchToken(KW_TRAIT) ++! (
              rule(Rule.IdentifierOptional) ++
                rule(Rule.NewLines) ++
                rule(Rule.MethodParameters) ++
                matchToken(OP_SUBTYPE) ++
                rule(Rule.BaseTypeSpecifier) ++
                rule(Rule.StatementSeparator) ++
                rule(Rule.StaticInstanceBody) ++
                matchToken(KW_END)
            ) --> {
              case (modifiers, _, (name, _, parameters, _, baseType, _, (staticBody, instanceBody), _)) =>
                TraitDeclarationStmt(baseType, name, parameters, staticBody, instanceBody, modifiers)
            }

        case Rule.DataConstructorDeclarationStmt =>
          rule(Rule.Modifiers) ++
            matchToken(KW_CONSTRUCTOR) ++! (
              rule(Rule.IdentifierOptional).observeSource ++
                rule(Rule.NewLines) ++
                rule(Rule.MethodParameters) ++
                rule(Rule.NewLines) ++
                matchToken(OP_COLON) ++
                rule(Rule.NewLines) ++
                rule(Rule.Type).observeSource ++
                rule(Rule.StatementSeparator) ++
                rule(Rule.StatementList).observeSource ++
                matchToken(KW_END)
            ) --> {
              case (modifiers, _, (name, _, params, _, _, _, returnType, _, body, _)) =>
                DataConstructorDeclarationStmt(name, params, returnType, body, modifiers)
            }

        case Rule.ClassDeclarationStmt =>
          rule(Rule.Modifiers) ++
            matchToken(KW_CLASS) ++! (
              rule(Rule.IdentifierOptional).observeSource ++
                rule(Rule.MethodParameters) ++
                matchToken(OP_SUBTYPE) ++
                rule(Rule.BaseTypeSpecifier) ++
                rule(Rule.StatementSeparator) ++
                rule(Rule.StaticInstanceBody) ++
                matchToken(KW_END)
            ) --> {
              case (modifiers, _, (name, params, _, baseType, _, (staticBody, instanceBody), _)) =>
                ClassDeclarationStmt(baseType, name, params, staticBody, instanceBody, modifiers)
            }

        case Rule.Statement =>
          rule(Rule.VariableDeclaration) |
            rule(Rule.FieldDeclarationStmt) |
            rule(Rule.FieldInitializationStmt) |
            rule(Rule.InitializeStmt) |
            rule(Rule.MethodDefinitionStmt) |
            rule(Rule.FunctionDefinitionStmt) |
            rule(Rule.ClassConstructorDefinitionStmt) |
            rule(Rule.TraitDeclarationStmt) |
            rule(Rule.DataConstructorDeclarationStmt) |
            rule(Rule.ClassDeclarationStmt) |
            rule(Rule.ExpressionStmt)

        case Rule.StatementList =>
          (rule(Rule.StatementSeparator) *) ++ (((rule(Rule.Statement).observeSource ++ (rule(
            Rule.StatementSeparator
          )*)) --> { case (stmt, _) => stmt })*) --> {
            case (_, stmts) => stmts.toVector
          }

        case Rule.ImportPathAbsolute =>
          matchToken(OP_SLASH) ++ rule(Rule.NewLines) ++! rule(
            Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)
          ) --> {
            case (_, _, path) => ImportStmt.Absolute(path)
          }

        case Rule.ImportPathRelative =>
          val currentLevel =
            matchToken(OP_DOT) ++ matchToken(OP_SLASH) ++ rule(Rule.NewLines) ++! rule(
              Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)
            ) --> {
              case (_, _, _, path) => ImportStmt.Relative(0, path)
            }

          val fromParent =
            (matchToken(OP_DOTDOT) ++ matchToken(OP_SLASH) ++ rule(Rule.NewLines)).+~ ++! rule(
              Rule.ImportPathTubeName
            ) ++ rule(Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)) --> {
              case (parentDirs, packageName, path) => ImportStmt.Relative(parentDirs.size, path)
            }

          currentLevel | fromParent

        case Rule.ImportPathTube =>
          rule(Rule.ImportPathTubeName) ++ matchToken(OP_SLASH) ++ rule(Rule.NewLines) ++! rule(
            Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Slash)
          ) --> {
            case (packageName, _, _, path) => ImportStmt.Tube(packageName, path)
          }

        case Rule.ImportPathMember =>
          rule(Rule.ImportPathSegmentRule(Rule.ImportPathSegmentSeparator.Dot)) --> ImportStmt.Member.apply

        case Rule.ImportPathTubeName =>
          tokenIdentifier ++ (
            rule(Rule.NewLines) ++ matchToken(OP_DOT) ++ rule(Rule.NewLines) ++ tokenIdentifier --> {
              case (_, _, _, id) => id
            }
          ).* --> {
            case (h, t) => NonEmptyList.cons(h, t.toList)
          }

        case Rule.ImportPathSegmentRule(sep) =>
          val sepGrammar =
            sep match {
              case Rule.ImportPathSegmentSeparator.Slash => matchToken(OP_SLASH)
              case Rule.ImportPathSegmentSeparator.Dot => matchToken(OP_DOT)
            }

          val cons =
            tokenIdentifier ++ rule(Rule.NewLines) ++ sepGrammar ++ rule(Rule.NewLines) ++! rule(
              Rule.ImportPathSegmentRule(sep)
            ) --> {
              case (id, _, _, _, subPath) => ImportPathSegment.Cons(id, subPath)
            }

          val many =
            matchToken(OP_OPENCURLY) ++ rule(Rule.NewLines) ++! (
              (
                rule(Rule.ImportPathSegmentRule(sep)) ++
                  (
                    rule(Rule.NewLines) ++ rule(Rule.ImportPathSegmentRule(sep)) ++ rule(Rule.NewLines) ++ matchToken(
                      OP_COMMA
                    ) ++ rule(Rule.NewLines) --> {
                      case (_, seg, _, _, _) => seg
                    }
                  ).* ++
                  matchToken(OP_COMMA).?
              ).? --> {
                case Some((headSeg, tailSegs, _)) => headSeg +: tailSegs
                case None => Seq.empty
              }
            ) --> {
              case (_, _, segs) => ImportPathSegment.Many(segs)
            }

          val renaming =
            rule(Rule.Identifier) ++ rule(Rule.NewLines) ++ matchToken(OP_LAMBDA) ++ rule(Rule.NewLines) ++ rule(
              Rule.IdentifierOptional
            ) --> {
              case (importing, _, _, _, viewedName) => ImportPathSegment.Renaming(importing, viewedName)
            }

          val imported = rule(Rule.Identifier) --> ImportPathSegment.Imported.apply
          val wildcard = matchToken(OP_STAR) --> const(ImportPathSegment.Wildcard)

          cons | many | renaming | imported | wildcard

        case Rule.PaddedStatement =>
          (rule(Rule.StatementSeparator) *) ++ rule(Rule.Statement) ++ (rule(Rule.StatementSeparator)*) --> {
            case (_, stmt, _) => stmt
          }
      }

    // Expressions
    private def postfixExprMemberAccess: TGrammar[WithSource[Expr] => Expr] =
      (matchToken(OP_DOT) ++! rule(Rule.MemberAccess)) --> {
        case (_, memberAccessFunc) => (baseExpr: WithSource[Expr]) => memberAccessFunc(baseExpr)
      }

    private def createLeftAssociativeOperatorRule
      (firstOpGrammar: TGrammar[BinaryOperator], opGrammars: TGrammar[BinaryOperator]*)(nextGrammar: TGrammar[Expr])
      : TGrammar[Expr] = {
      val opGrammarsNec = NonEmptyChunk(firstOpGrammar, opGrammars*)

      val rightGrammars =
        opGrammarsNec.map { opGrammar =>
          Lazy {
            (opGrammar.observeSource ++! nextGrammar.observeSource) --> { case (op, right) =>
              (left: WithSource[Expr]) => BinaryOperatorExpr(op, left, right)
            }
          }
        }

      nextGrammar.observeSource ++ (UnionGrammar.fromList(rightGrammars).observeSource*) --> {
        case (left, rightSeq) =>
          rightSeq.foldLeft(left) { case (l, WithSource(f, rightLoc)) =>
            WithSource(f(l), SourceLocation(l.location.start, rightLoc.end))
          }.value
      }
    }

    private def ruleBinaryOperator[TToken <: TokenWithCategory[? <: TokenCategory] with BinaryOperatorToken]
      (token: TToken)(using TypeTest[Token, TToken])
      : TGrammar[BinaryOperator] = matchToken(token) --> const(token.operator)

  }

  private[impl] def grammarFactory: GrammarFactory[Token, SyntaxError, Rule.ArgonRuleName] = ArgonGrammarFactory

  def parse[E]: ZChannel[Any, E, Chunk[WithSource[Token]], FilePosition, E | SyntaxError, Chunk[Stmt], FilePosition] =
    Grammar.parseAll(ArgonGrammarFactory)(Rule.PaddedStatement)

}
