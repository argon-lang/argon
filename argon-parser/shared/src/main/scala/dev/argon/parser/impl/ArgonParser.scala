package dev.argon.parser.impl

import dev.argon.parser.Token._
import dev.argon.parser._
import dev.argon.util._

import scala.reflect.ClassTag
import scala.language.postfixOps
import cats._
import cats.data._
import cats.implicits._
import dev.argon.grammar.{Grammar, GrammarError, TokenMatcher}
import Grammar.Operators._
import Grammar.{GrammarFactory, UnionGrammar}
import dev.argon.stream.StreamTransformation
import dev.argon.stream._

import Function.const

object ArgonParser {

  private[parser] object Rule {

    sealed trait ParenAllowedState
    case object ParenAllowed extends ParenAllowedState
    case object ParenDisallowed extends ParenAllowedState
    

    sealed trait ArgonRuleName extends Grammar.RuleLabel
    sealed trait ArgonRuleNameTyped[T] extends ArgonRuleName {
      override type RuleType = T
    }

    case object Identifier extends ArgonRuleNameTyped[Option[String]]
    case object NewLines extends ArgonRuleNameTyped[Unit]
    case object StatementSeparator extends ArgonRuleNameTyped[Unit]
    case object ImportNamespace extends ArgonRuleNameTyped[TopLevelStatement]

    // If
    case object IfExpr extends ArgonRuleNameTyped[Expr]
    case object IfExprStart extends ArgonRuleNameTyped[(WithSource[Expr], WithSource[Vector[WithSource[Stmt]]])]
    case object IfExprPart extends ArgonRuleNameTyped[Expr]

    // Pattern Matching
    case object ParenPattern extends ArgonRuleNameTyped[Pattern]
    case object VariablePattern extends ArgonRuleNameTyped[Pattern]
    case object DiscardPattern extends ArgonRuleNameTyped[Pattern]
    case object ConstructorExprPattern extends ArgonRuleNameTyped[Expr]
    case object ConstructorExprPatternIdPath extends ArgonRuleNameTyped[Expr]
    case object ContainedPattern extends ArgonRuleNameTyped[Pattern]
    case object PatternSeq extends ArgonRuleNameTyped[Vector[WithSource[Pattern]]]
    case object DeconstructPattern extends ArgonRuleNameTyped[Pattern]
    case object PatternSpec extends ArgonRuleNameTyped[Pattern]
    case object MatchCase extends ArgonRuleNameTyped[MatchExprCase]
    case object MatchExpr extends ArgonRuleNameTyped[Expr]

    // Common Expressions
    final case class PrimaryExpr(parenAllowed: ParenAllowedState) extends ArgonRuleNameTyped[Expr]
    final case class PostfixExpr(parenAllowed: ParenAllowedState) extends ArgonRuleNameTyped[Expr]
    case object CurryCallExpr extends ArgonRuleNameTyped[Expr]
    case object ParenArgList extends ArgonRuleNameTyped[Expr]
    case object MemberAccess extends ArgonRuleNameTyped[WithSource[Expr] => Expr]
    case object UnaryExpr extends ArgonRuleNameTyped[Expr]
    case object ConstrainedTypeExpr extends ArgonRuleNameTyped[Expr]
    case object IntersectionExpr extends ArgonRuleNameTyped[Expr]
    case object UnionExpr extends ArgonRuleNameTyped[Expr]
    case object MultiplicativeExpr extends ArgonRuleNameTyped[Expr]
    case object AdditiveExpr extends ArgonRuleNameTyped[Expr]
    case object ShiftExpr extends ArgonRuleNameTyped[Expr]
    case object AndExpr extends ArgonRuleNameTyped[Expr]
    case object XorExpr extends ArgonRuleNameTyped[Expr]
    case object OrExpr extends ArgonRuleNameTyped[Expr]
    case object LambdaTypeExpr extends ArgonRuleNameTyped[Expr]
    case object RelationalExpr extends ArgonRuleNameTyped[Expr]
    case object EqualityExpr extends ArgonRuleNameTyped[Expr]
    case object AsExpr extends ArgonRuleNameTyped[Expr]
    case object LambdaExpr extends ArgonRuleNameTyped[Expr]
    case object PatternType extends ArgonRuleNameTyped[Expr]
    case object Type extends ArgonRuleNameTyped[Expr]
    case object TupleExpr extends ArgonRuleNameTyped[Expr]
    case object AssignExpr extends ArgonRuleNameTyped[Expr]
    case object Expression extends ArgonRuleNameTyped[Expr]
    case object ExpressionStmt extends ArgonRuleNameTyped[Expr]

    // Variable Declaration
    case object VariableMutSpec extends ArgonRuleNameTyped[Boolean]
    case object VariableDeclaration extends ArgonRuleNameTyped[Stmt]

    // Fields
    case object FieldDeclarationStmt extends ArgonRuleNameTyped[Stmt]
    case object FieldInitializationStmt extends ArgonRuleNameTyped[Stmt]
    case object InitializeStmt extends ArgonRuleNameTyped[Stmt]


    case object Modifiers extends ArgonRuleNameTyped[Vector[WithSource[Modifier]]]

    // Functions and Methods
    case object MethodParameter extends ArgonRuleNameTyped[FunctionParameter]
    case object MethodParameterList extends ArgonRuleNameTyped[Vector[WithSource[FunctionParameter]]]
    case object MethodParameters extends ArgonRuleNameTyped[Vector[WithSource[FunctionParameterList]]]
    case object MethodBody extends ArgonRuleNameTyped[Vector[WithSource[Stmt]]]
    case object MethodPurity extends ArgonRuleNameTyped[Boolean]
    case object FunctionDefinitionStmt extends ArgonRuleNameTyped[Stmt]
    case object MethodDefinitionStmt extends ArgonRuleNameTyped[Stmt]
    case object ClassConstructorDefinitionStmt extends ArgonRuleNameTyped[Stmt]

    // Types
    case object StaticInstanceBody extends ArgonRuleNameTyped[(Vector[WithSource[Stmt]], Vector[WithSource[Stmt]])]
    case object BaseTypeSpecifier extends ArgonRuleNameTyped[Option[WithSource[Expr]]]
    case object TraitDeclarationStmt extends ArgonRuleNameTyped[Stmt]
    case object DataConstructorDeclarationStmt extends ArgonRuleNameTyped[Stmt]
    case object ClassDeclarationStmt extends ArgonRuleNameTyped[Stmt]


    case object Statement extends ArgonRuleNameTyped[Stmt]
    case object StatementList extends ArgonRuleNameTyped[Vector[WithSource[Stmt]]]
    case object NamespacePathRule extends ArgonRuleNameTyped[NamespacePath]
    case object NamespaceDeclaration extends ArgonRuleNameTyped[TopLevelStatement]

    case object TopLevelStatementRule extends ArgonRuleNameTyped[TopLevelStatement]
    case object PaddedTopLevelStatement extends ArgonRuleNameTyped[TopLevelStatement]


  }

  private[ArgonParser] object ArgonGrammarFactory extends GrammarFactory[Token, SyntaxError, Rule.ArgonRuleName] {

    private implicit val errorFactory = new Grammar.ErrorFactory[Token, TokenCategory, SyntaxError] {
      override def createError(error: GrammarError[Token, TokenCategory]): SyntaxError =
        SyntaxError.ParserError(error)

      override def createAmbiguityError(location: SourceLocation): SyntaxError =
        SyntaxError.AmbiguousParse(location)

      override def errorEndLocationOrder: Order[SyntaxError] =
        Order.from((a, b) => implicitly[Order[FilePosition]].compare(a.location.end, b.location.end))
    }

    def second[T](pair: (_, T)): T = pair._2

    private def matchTokenFactory[TTokenCategory <: TokenCategory, TToken <: Token : ClassTag](factory: TokenWithCategory[TTokenCategory] with TokenFactory[TToken]): TGrammar[TToken] =
      Grammar.matcher(factory.category, TokenMatcher.Subtype[Token, TToken](implicitly[ClassTag[TToken]]))

    private def matchToken[TToken <: TokenWithCategory[_ <: TokenCategory] with Token : ClassTag](token: TToken): TGrammar[TToken] =
      Grammar.matcher(token.category, TokenMatcher.Subtype[Token, TToken](implicitly[ClassTag[TToken]]))

    private val tokenUnderscore: TGrammar[Unit] =
      matchToken(KW_UNDERSCORE).discard

    private val tokenIdentifier: TGrammar[String] =
      matchTokenFactory(Identifier) --> { _.name }

    private def createLeftRec[A](left: TGrammar[A])(rightRepeat: TGrammar[WithSource[A] => A]): TGrammar[A] =
      left.observeSource ++ (rightRepeat.observeSource*) --> { case (a, fs) =>
        fs.foldLeft(a) { case (a, WithSource(f, loc)) =>
          WithSource(f(a), SourceLocation.merge(a.location, loc))
        }.value
      }

    protected override def createGrammar[T](name: Rule.ArgonRuleName { type RuleType = T }): TGrammar[T] =
      name match {
        case Rule.Identifier =>
          tokenUnderscore --> const(None : Option[String]) |
            tokenIdentifier --> Some.apply

        case Rule.NewLines => (matchToken(NewLine)*).discard
        case Rule.StatementSeparator => matchToken(NewLine).discard | matchToken(Semicolon).discard

        case Rule.ImportNamespace =>
          matchToken(KW_IMPORT) ++! (rule(Rule.NamespacePathRule) ++ matchToken(OP_DOT) ++ matchToken(KW_UNDERSCORE)) --> {
            case (_, (ns, _, _)) => TopLevelStatement.Import(ns)
          }



        case Rule.IfExpr => matchToken(KW_IF) ++! rule(Rule.IfExprPart) --> second
        case Rule.IfExprStart =>
          rule(Rule.Expression).observeSource ++ matchToken(KW_THEN) ++ rule(Rule.StatementList).observeSource --> { case (condition, _, body) => (condition, body) }

        case Rule.IfExprPart =>
          rule(Rule.IfExprStart) ++ matchToken(KW_END) --> { case (condition, body, _) => IfExpr(condition, body) : Expr } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSE) ++! rule(Rule.StatementList).observeSource ++ matchToken(KW_END) -->
              { case (condition, body, _, elseBody, _) => IfElseExpr(condition, body, elseBody) } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSIF) ++! rule(Rule.IfExprPart).observeSource -->
              { case (condition, body, _, elseExpr) => IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location)) }



        case Rule.ParenPattern =>
          matchToken(OP_OPENPAREN) ++! rule(Rule.PatternSpec) ++ matchToken(OP_CLOSEPAREN) --> {
            case (_, pattern, _) => pattern
          }

        case Rule.VariablePattern =>
          matchToken(KW_VAL) ++! rule(Rule.Identifier) ++ ((matchToken(OP_COLON) ++ rule(Rule.PatternType).observeSource)?) --> {
            case (_, idOpt, Some((_, varType))) => TypeTestPattern(idOpt, varType)
            case (_, Some(id), None) => BindingPattern(id)
            case (_, None, None) => DiscardPattern
          }

        case Rule.DiscardPattern =>
          matchToken(KW_UNDERSCORE) --> const(DiscardPattern)

        case Rule.ConstructorExprPattern =>
          rule(Rule.ConstructorExprPatternIdPath) |
            matchToken(OP_OPENCURLY) ++ rule(Rule.Expression) ++ matchToken(OP_CLOSECURLY) --> { case (_, expr, _) => expr }

        case Rule.ConstructorExprPatternIdPath =>
          createLeftRec(
            tokenIdentifier --> { id => IdentifierExpr(id) : Expr }
          )(
            (matchToken(OP_DOT) ++ tokenIdentifier) --> {
              case (_, id) => (baseExpr: WithSource[Expr]) => DotExpr(baseExpr, id)
            }
          )

        case Rule.ContainedPattern =>
          rule(Rule.ConstructorExprPattern).observeSource --> { expr => DeconstructPattern(expr, Vector()) : Pattern } |
            rule(Rule.DiscardPattern) |
            rule(Rule.ParenPattern)

        case Rule.PatternSeq =>
          (rule(Rule.ContainedPattern).observeSource*) --> { _.toVector }

        case Rule.DeconstructPattern =>
          rule(Rule.ConstructorExprPattern).observeSource ++ rule(Rule.PatternSeq) --> (DeconstructPattern.apply _).tupled

        case Rule.PatternSpec =>
          rule(Rule.ParenPattern) |
            rule(Rule.VariablePattern) |
            rule(Rule.DeconstructPattern) |
            rule(Rule.DiscardPattern)

        case Rule.MatchCase =>
          rule(Rule.NewLines) ++ matchToken(KW_CASE) ++! (rule(Rule.NewLines) ++ rule(Rule.PatternSpec).observeSource ++ matchToken(OP_EQUALS) ++ rule(Rule.StatementList).observeSource) --> {
            case (_, _, (_, pattern, _, body)) => MatchExprCase(pattern, body)
          }

        case Rule.MatchExpr =>
          matchToken(KW_MATCH) ++! (rule(Rule.Expression).observeSource ++ (rule(Rule.MatchCase).observeSource*) ++ matchToken(KW_END)) --> {
            case (_, (cmpValue, cases, _)) =>
              MatchExpr(cmpValue, cases)
          }

        case Rule.PrimaryExpr(Rule.ParenDisallowed) =>
          matchTokenFactory(Identifier) --> { case Identifier(id) => IdentifierExpr(id) : Expr } |
            matchTokenFactory(StringToken) --> {
              case StringToken(NonEmptyList(StringToken.StringPart(str), Nil)) => StringValueExpr(str)
              case StringToken(NonEmptyList(StringToken.StringPart(str), _ :: _)) => ???
            } |
            matchTokenFactory(IntToken) --> { case IntToken(sign, base, digits) => IntValueExpr(sign, base, digits) } |
            matchToken(KW_TRUE) --> const(BoolValueExpr(true)) |
            matchToken(KW_FALSE) --> const(BoolValueExpr(false)) |
            rule(Rule.IfExpr) |
            rule(Rule.MatchExpr) |
            (matchToken(KW_EXTERN) ++! matchTokenFactory(Identifier)) --> {
              case (_, Identifier(id)) => ExternExpr(id)
            }

        case Rule.PrimaryExpr(Rule.ParenAllowed) =>
          matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> const(UnitLiteral) |
            matchToken(OP_OPENPAREN) ++ rule(Rule.Expression) ++ matchToken(OP_CLOSEPAREN) --> {
              case (_, expr, _) => expr
            } | rule(Rule.PrimaryExpr(Rule.ParenDisallowed))

        case Rule.PostfixExpr(Rule.ParenAllowed) =>
          createLeftRec(
            rule(Rule.PrimaryExpr(Rule.ParenAllowed))
          )(
            postfixExprMemberAccess |
              rule(Rule.ParenArgList).observeSource --> { argList => (funcExpr: WithSource[Expr]) => FunctionCallExpr(funcExpr, argList) }
          )

        case Rule.PostfixExpr(Rule.ParenDisallowed) =>
          createLeftRec(rule(Rule.PrimaryExpr(Rule.ParenDisallowed)))(postfixExprMemberAccess)

        case Rule.ParenArgList =>
          matchToken(OP_OPENPAREN) ++ (rule(Rule.Expression)?) ++ matchToken(OP_CLOSEPAREN) --> {
            case (_, Some(argList), _) => argList
            case (_, None, _) => UnitLiteral
          }

        case Rule.MemberAccess =>
          matchTokenFactory(Identifier) --> { case Identifier(id) => baseExpr: WithSource[Expr] => DotExpr(baseExpr, id) } |
            matchToken(KW_NEW) --> const(ClassConstructorExpr.apply _) |
            matchToken(KW_TYPE) --> const(TypeOfExpr.apply _)

        case Rule.CurryCallExpr =>
          createLeftRec(
            rule(Rule.PostfixExpr(Rule.ParenAllowed))
          )(
            rule(Rule.PostfixExpr(Rule.ParenDisallowed)).observeSource --> {
              argExpr => (funcExpr: WithSource[Expr]) => FunctionCallExpr(funcExpr, argExpr)
            } |
              rule(Rule.ParenArgList).observeSource --> {
                argExpr => (funcExpr: WithSource[Expr]) => FunctionCallExpr(funcExpr, argExpr)
              }
          )

        case Rule.UnaryExpr =>
          def matchUnaryOp[TToken <: TokenWithCategory[_ <: TokenCategory] with UnaryOperatorToken : ClassTag](token: TToken): TGrammar[Expr] =
            matchToken(token) ++! rule(Rule.UnaryExpr).observeSource --> { case (_, inner) => UnaryOperatorExpr(token.unaryOperator, inner) }

          matchUnaryOp(OP_BITNOT) |
            matchUnaryOp(OP_BOOLNOT) |
            matchUnaryOp(OP_ADD) |
            matchUnaryOp(OP_SUB) |
            rule(Rule.ConstrainedTypeExpr) |
            rule(Rule.CurryCallExpr)

        case Rule.ConstrainedTypeExpr =>
          matchToken(KW_TYPE) ++!
            ((matchToken(OP_OPENBRACKET) ++ rule(Rule.Expression).observeSource ++ matchToken(OP_CLOSEBRACKET) --> { case (_, level, _) => level })?) ++
            ((matchToken(OP_SUBTYPE) ++ rule(Rule.UnaryExpr).observeSource --> second)?) ++
            ((matchToken(OP_SUPERTYPE) ++ rule(Rule.UnaryExpr).observeSource --> second)?) ++
            ((matchToken(OP_COLON) ++ rule(Rule.UnaryExpr).observeSource --> second)?) --> {
              case (_, level, subtypeOf, supertypeOf, instanceType) =>
                TypeExpr(level, instanceType, subtypeOf, supertypeOf)
            }

        case Rule.IntersectionExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_INTERSECTION),
          )(rule(Rule.UnaryExpr))

        case Rule.UnionExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_UNION),
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
          )(rule(Rule.MultiplicativeExpr))

        case Rule.ShiftExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_SHIFTLEFT),
            ruleBinaryOperator(OP_SHIFTRIGHT),
          )(rule(Rule.AdditiveExpr))

        case Rule.AndExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_BITAND),
          )(rule(Rule.ShiftExpr))

        case Rule.XorExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_BITXOR),
          )(rule(Rule.AndExpr))

        case Rule.OrExpr =>
          createLeftAssociativeOperatorRule(
            ruleBinaryOperator(OP_BITOR),
          )(rule(Rule.XorExpr))

        case Rule.LambdaTypeExpr =>
          rule(Rule.OrExpr).observeSource ++ ((matchToken(OP_LAMBDA_TYPE) ++! rule(Rule.LambdaTypeExpr).observeSource)?) --> {
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

          nextRule.observeSource ++ ((matchToken(KW_AS) ++! nextRule.observeSource)?) --> {
            case (WithSource(left, _), None) => left
            case (left, Some((_, right))) => AsExpr(left, right)
          }

        case Rule.LambdaExpr =>
          rule(Rule.Identifier) ++ matchToken(OP_LAMBDA) ++! rule(Rule.LambdaExpr).observeSource -->
            { case (id, _, body) => LambdaExpr(id, body) } |
            rule(Rule.AsExpr)

        case Rule.PatternType =>
          rule(Rule.LambdaTypeExpr)

        case Rule.Type =>
          rule(Rule.PatternType)

        case Rule.TupleExpr =>
          val nextRule = rule(Rule.LambdaExpr)
          nextRule.observeSource ++ ((matchToken(OP_COMMA) ++! nextRule.observeSource --> second)*) --> {
            case (WithSource(expr, _), Vector()) => expr
            case (head, tail) => TupleExpr(NonEmptyList(head, tail.toList))
          }

        case Rule.AssignExpr =>
          val nextRule = rule(Rule.TupleExpr)
          nextRule.observeSource ++ ((matchToken(OP_ASSIGN) ++! nextRule.observeSource)?) --> {
            case (WithSource(left, _), None) => left
            case (left, Some((_, right))) => BinaryOperatorExpr(BinaryOperator.Assign, left, right)
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
            rule(Rule.Identifier) ++
              ((matchToken(OP_COLON) ++ rule(Rule.Type).observeSource --> second)?) ++
              matchToken(OP_EQUALS) ++
              rule(Rule.Expression).observeSource
            ) --> { case (isMutable, (id, typeAnnotation, _, value)) =>
              VariableDeclarationStmt(isMutable, typeAnnotation, id, value)
            }

        case Rule.FieldDeclarationStmt =>
          matchToken(KW_FIELD) ++
            (rule(Rule.VariableMutSpec)?) ++
            rule(Rule.Identifier) ++
            matchToken(OP_COLON) ++!
            rule(Rule.Type).observeSource --> { case (_, isMutable, id, _, typeAnnotation) =>
              FieldDeclarationStmt(isMutable.getOrElse(false), id, typeAnnotation)
            }

        case Rule.FieldInitializationStmt =>
          matchToken(KW_FIELD) ++
            tokenIdentifier ++
            matchToken(OP_EQUALS) ++!
            rule(Rule.Expression).observeSource --> { case (_, id, _, value) =>
              FieldInitializationStmt(id, value)
            }

        case Rule.InitializeStmt =>
          matchToken(KW_INITIALIZE) ++
            rule(Rule.Identifier) ++
            (
              (matchToken(OP_EQUALS) ++ rule(Rule.Expression).observeSource --> second)?
            ) --> { case (_, id, value) =>
              InitializeStmt(id, value)
            }

        case Rule.Modifiers =>
          def ruleModifier[TToken <: TokenWithCategory[_ <: TokenCategory] with ModifierToken : ClassTag](token: TToken): TGrammar[Modifier] =
            matchToken(token) --> const(token.modifier)

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

          (anyModifier.observeSource*) --> { _.toVector }

        case Rule.MethodParameter =>
          tokenIdentifier ++
            rule(Rule.NewLines) ++
            ((matchToken(OP_COLON) ++! (rule(Rule.NewLines) ++ rule(Rule.Type).observeSource) --> { case (_, (_, t)) => t })?) ++
            ((matchToken(OP_SUBTYPE) ++! (rule(Rule.NewLines) ++ rule(Rule.Type).observeSource) --> { case (_, (_, t)) => t })?) --> {
              case (name, _, paramType, subTypeOf) =>
                FunctionParameter(paramType, subTypeOf, name)
            }

        case Rule.MethodParameterList =>
          ((
            rule(Rule.MethodParameter).observeSource ++
              rule(Rule.NewLines) ++
              ((matchToken(OP_COMMA) ++ rule(Rule.MethodParameter).observeSource ++ rule(Rule.NewLines) --> { case (_, param, _) => param })*) ++
              (matchToken(OP_COMMA)?) --> {
                case (firstParam, _, restParams, _) =>
                  firstParam +: restParams
              }
          )?) --> { _.getOrElse(Vector.empty) }

        case Rule.MethodParameters =>
          ((
            (
              matchToken(OP_OPENPAREN) ++! (rule(Rule.NewLines) ++ rule(Rule.MethodParameterList) ++ rule(Rule.NewLines) ++ matchToken(OP_CLOSEPAREN)) --> {
                case (_, (_, params, _, _)) =>
                  FunctionParameterList(FunctionParameterListType.NormalList, params)
              }
            ) |
              (
                matchToken(OP_OPENBRACKET) ++! (rule(Rule.NewLines) ++ rule(Rule.MethodParameterList) ++ rule(Rule.NewLines) ++ matchToken(OP_CLOSEBRACKET)) --> {
                  case (_, (_, params, _, _)) =>
                    FunctionParameterList(FunctionParameterListType.InferrableList, params)
                }
              )
          ).observeSource*) --> { _.toVector }

        case Rule.MethodBody =>
          matchToken(KW_DO) ++! (rule(Rule.StatementList) ++ matchToken(KW_END)) --> { case (_, (body, _)) => body } |
            matchToken(OP_EQUALS) ++! (rule(Rule.NewLines) ++ rule(Rule.Expression).observeSource) --> { case (_, (_, expr)) => Vector(expr) }

        case Rule.MethodPurity =>
          matchToken(KW_DEF) --> const(true) |
            matchToken(KW_PROC) --> const(false)

        case Rule.FunctionDefinitionStmt =>
          rule(Rule.Modifiers) ++
            rule(Rule.MethodPurity) ++
            rule(Rule.Identifier) ++
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
            rule(Rule.Identifier) ++
            rule(Rule.NewLines) ++
            matchToken(OP_DOT) ++
            rule(Rule.NewLines) ++
            rule(Rule.Identifier) ++! (
              rule(Rule.NewLines) ++
                rule(Rule.MethodParameters) ++
                matchToken(OP_COLON) ++
                rule(Rule.NewLines) ++
                rule(Rule.Type).observeSource ++
                rule(Rule.NewLines) ++
                (rule(Rule.MethodBody).observeSource?)
            ) --> {
            case (modifiers, purity, instanceName, _, _, _, name, (_, params, _, _, returnType, _, body)) =>
                MethodDeclarationStmt(instanceName, name, params, returnType, body, modifiers, purity)
            }

        case Rule.ClassConstructorDefinitionStmt =>
          rule(Rule.Modifiers) ++
            matchToken(KW_NEW) ++! (
              rule(Rule.MethodParameters) ++
                rule(Rule.StatementSeparator) ++
                rule(Rule.StatementList).observeSource ++
                matchToken(KW_END)
            ) --> {
              case (modifiers, _, (params, _, body, _)) =>
                ClassConstructorDeclarationStmt(params, body, modifiers)
            }

        case Rule.StaticInstanceBody =>
          rule(Rule.NewLines) ++ matchToken(KW_STATIC) ++! (rule(Rule.StatementList) ++ ((matchToken(KW_INSTANCE) ++! rule(Rule.StatementList) --> { case (_, instanceBody) => instanceBody })?)) --> {
            case (_, _, (staticBody, instanceBody)) =>
              (staticBody, instanceBody.getOrElse(Vector.empty))
          } |
            ((rule(Rule.NewLines) ++ matchToken(KW_INSTANCE))?) ++ rule(Rule.StatementList) --> {
              case (_, instanceBody) =>
                (Vector.empty, instanceBody)
            }

        case Rule.BaseTypeSpecifier =>
          matchToken(KW_UNDERSCORE) --> { _ => Option.empty[WithSource[Expr]] } |
            rule(Rule.Type).observeSource --> Some.apply

        case Rule.TraitDeclarationStmt =>
          rule(Rule.Modifiers) ++
            matchToken(KW_TRAIT) ++! (
              rule(Rule.Identifier) ++
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
              rule(Rule.Identifier).observeSource ++
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
              rule(Rule.Identifier).observeSource ++
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
          (rule(Rule.StatementSeparator)*) ++ (((rule(Rule.Statement).observeSource ++ (rule(Rule.StatementSeparator)*)) --> { case (stmt, _) => stmt })*) --> {
            case (_, stmts) => stmts
          }

        case Rule.NamespacePathRule =>
          tokenIdentifier ++ ((matchToken(OP_DOT) ++ tokenIdentifier --> second)*) --> {
            case (head, tail) => NamespacePath(head +: tail)
          }

        case Rule.NamespaceDeclaration =>
          matchToken(KW_NAMESPACE) ++! rule(Rule.NamespacePathRule) --> { case (_, ns) => TopLevelStatement.Namespace(ns) }

        case Rule.TopLevelStatementRule =>
          rule(Rule.NamespaceDeclaration) |
            rule(Rule.ImportNamespace) |
            rule(Rule.Statement).observeSource --> TopLevelStatement.Statement

        case Rule.PaddedTopLevelStatement =>
          (rule(Rule.StatementSeparator)*) ++ rule(Rule.TopLevelStatementRule) ++ (rule(Rule.StatementSeparator)*) --> {
            case (_, stmt, _) => stmt
          }
      }


    // Expressions
    private def postfixExprMemberAccess: TGrammar[WithSource[Expr] => Expr] =
      (matchToken(OP_DOT) ++! rule(Rule.MemberAccess)) --> {
        case (_, memberAccessFunc) => (baseExpr: WithSource[Expr]) => memberAccessFunc(baseExpr)
      }


    private def createLeftAssociativeOperatorRule(firstOpGrammar: TGrammar[BinaryOperator], opGrammars: TGrammar[BinaryOperator]*)(nextGrammar: TGrammar[Expr]): TGrammar[Expr] = {
      val opGrammarsNev = NonEmptyVector(firstOpGrammar, opGrammars.toVector)

      val rightGrammars = opGrammarsNev.map { opGrammar =>
        Lazy { (opGrammar ++! nextGrammar.observeSource) --> { case (op, right) => left: WithSource[Expr] => BinaryOperatorExpr(op, left, right) } }
      }

      nextGrammar.observeSource ++ (UnionGrammar.fromList(rightGrammars).observeSource*) --> {
        case (left, rightSeq) =>
          rightSeq.foldLeft(left) { case (l, WithSource(f, rightLoc)) =>
            WithSource(f(l), SourceLocation(l.location.start, rightLoc.end))
          }.value
      }
    }

    private def ruleBinaryOperator[TToken <: TokenWithCategory[_ <: TokenCategory] with BinaryOperatorToken : ClassTag](token: TToken): TGrammar[BinaryOperator] =
      matchToken(token) --> const(token.binaryOperator)

  }

  private[impl] def grammarFactory: GrammarFactory[Token, SyntaxError, Rule.ArgonRuleName] = ArgonGrammarFactory

  def parse[F[-_, +_, +_]](implicit monadError: MonadError[F[Any, NonEmptyVector[SyntaxError], ?], NonEmptyVector[SyntaxError]]): StreamTransformation[F, Any, NonEmptyVector[SyntaxError], WithSource[Token], FilePosition, TopLevelStatement, Unit] =
    Grammar.parseAll[F, Token, SyntaxError, Rule.ArgonRuleName, TopLevelStatement](ArgonGrammarFactory)(Rule.PaddedTopLevelStatement)
      .mapResult(const(()))

}