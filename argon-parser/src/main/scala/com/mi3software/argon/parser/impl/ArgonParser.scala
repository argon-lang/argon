package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.Token._
import com.mi3software.argon.parser._
import com.mi3software.argon.util._

import scala.reflect.ClassTag
import scala.language.postfixOps
import scalaz._
import Scalaz._
import com.mi3software.argon.grammar.{Grammar, GrammarError, TokenMatcher}
import Grammar.Operators._
import Grammar.{GrammarFactory, UnionGrammar}
import fs2._

import Function.const

object ArgonParser {

  private[parser] object Rule {


    sealed trait ArgonRuleName extends Grammar.RuleLabel
    sealed trait ArgonRuleNameTyped[T] extends ArgonRuleName {
      override type RuleType = T
    }

    final case object Identifier extends ArgonRuleNameTyped[Option[String]]
    final case object NewLines extends ArgonRuleNameTyped[Unit]
    final case object StatementSeparator extends ArgonRuleNameTyped[Unit]
    final case object ImportNamespace extends ArgonRuleNameTyped[TopLevelStatement]

    // If
    final case object IfExpr extends ArgonRuleNameTyped[Expr]
    final case object IfExprStart extends ArgonRuleNameTyped[(WithSource[Expr], WithSource[Vector[WithSource[Stmt]]])]
    final case object IfExprPart extends ArgonRuleNameTyped[Expr]

    // Pattern Matching
    final case object ParenPattern extends ArgonRuleNameTyped[Pattern]
    final case object VariablePattern extends ArgonRuleNameTyped[Pattern]
    final case object DiscardPattern extends ArgonRuleNameTyped[Pattern]
    final case object ConstructorExprPattern extends ArgonRuleNameTyped[Expr]
    final case object ContainedPattern extends ArgonRuleNameTyped[Pattern]
    final case object PatternSeq extends ArgonRuleNameTyped[Vector[WithSource[Pattern]]]
    final case object DeconstructPattern extends ArgonRuleNameTyped[Pattern]
    final case object Pattern extends ArgonRuleNameTyped[Pattern]
    final case object MatchCase extends ArgonRuleNameTyped[MatchExprCase]
    final case object MatchExpr extends ArgonRuleNameTyped[Expr]


    final case object TopLevelStatement extends ArgonRuleNameTyped[WithSource[TopLevelStatement]]


  }

  private[ArgonParser] object ArgonGrammarFactory extends GrammarFactory[Token, SyntaxError, Rule.ArgonRuleName] {

    private implicit val errorFactory = new Grammar.ErrorFactory[Token, TokenCategory, SyntaxError] {
      override def createError(error: GrammarError[Token, TokenCategory]): SyntaxError =
        SyntaxError.ParserError(error)

      override def createAmbiguityError(location: SourceLocation): SyntaxError =
        SyntaxError.AmbiguousParse(location)

      override def errorEndLocationOrder: Order[SyntaxError] =
        (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
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

    override def apply[T](name: Rule.ArgonRuleName { type RuleType = T }): TGrammar[T] =
      name match {
        case Rule.Identifier =>
          tokenUnderscore --> const(None : Option[String]) |
            tokenIdentifier --> Some.apply

        case Rule.NewLines => (matchToken(NewLine)*).discard
        case Rule.StatementSeparator => matchToken(NewLine).discard | matchToken(Semicolon).discard

        case Rule.ImportNamespace =>
          matchToken(KW_IMPORT) ++! (ruleNamespacePath ++ matchToken(OP_DOT) ++ matchToken(KW_UNDERSCORE)) --> {
            case (_, (ns, _, _)) => TopLevelStatement.Import(ns)
          }



        case Rule.IfExpr => matchToken(KW_IF) ++! rule(Rule.IfExprPart) --> second
        case Rule.IfExprStart =>
          ruleExpression.observeSource ++ matchToken(KW_THEN) ++ ruleStatementList.observeSource --> { case (condition, _, body) => (condition, body) }

        case Rule.IfExprPart =>
          rule(Rule.IfExprStart) ++ matchToken(KW_END) --> { case (condition, body, _) => IfExpr(condition, body) : Expr } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSE) ++! ruleStatementList.observeSource ++ matchToken(KW_END) -->
              { case (condition, body, _, elseBody, _) => IfElseExpr(condition, body, elseBody) } |
            rule(Rule.IfExprStart) ++ matchToken(KW_ELSIF) ++! rule(Rule.IfExprPart).observeSource -->
              { case (condition, body, _, elseExpr) => IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location)) }



        case Rule.ParenPattern =>
          matchToken(OP_OPENPAREN) ++ rule(Rule.Pattern) ++ matchToken(OP_CLOSEPAREN) --> {
            case (_, pattern, _) => pattern
          }

        case Rule.VariablePattern =>
          matchToken(KW_VAL) ++ rule(Rule.Identifier) ++ ((matchToken(OP_COLON) ++ ruleExpressionType.observeSource)?) --> {
            case (_, idOpt, Some((_, varType))) => TypeTestPattern(idOpt, varType)
            case (_, Some(id), None) => BindingPattern(id)
            case (_, None, None) => DiscardPattern
          }

        case Rule.DiscardPattern =>
          matchToken(KW_UNDERSCORE) --> const(DiscardPattern)

        case Rule.ConstructorExprPattern =>
          {
            lazy val idPath: TGrammar[Expr] =
              idPath -- (matchToken(OP_DOT) ++ tokenIdentifier) -\> {
                case (baseExpr, (_, id)) => DotExpr(baseExpr, id)
              } |
                tokenIdentifier --> { id => IdentifierExpr(id) : Expr }

            idPath
          } |
            matchToken(OP_OPENCURLY) ++ ruleExpression ++ matchToken(OP_CLOSECURLY) --> { case (_, expr, _) => expr }

        case Rule.ContainedPattern =>
          rule(Rule.ConstructorExprPattern).observeSource --> { expr => DeconstructPattern(expr, Vector()) : Pattern } |
            rule(Rule.DiscardPattern) |
            rule(Rule.ParenPattern)

        case Rule.PatternSeq =>
          (rule(Rule.ContainedPattern).observeSource*) --> { _.toVector }

        case Rule.DeconstructPattern =>
          rule(Rule.ConstructorExprPattern).observeSource ++ rule(Rule.PatternSeq) --> (DeconstructPattern.apply _).tupled

        case Rule.Pattern =>
          rule(Rule.ParenPattern) |
            rule(Rule.VariablePattern) |
            rule(Rule.DeconstructPattern) |
            rule(Rule.DiscardPattern)

        case Rule.MatchCase =>
          rule(Rule.NewLines) ++ matchToken(KW_CASE) ++! (rule(Rule.NewLines) ++ rule(Rule.Pattern).observeSource ++ matchToken(OP_EQUALS) ++ ruleStatementList.observeSource) --> {
            case (_, _, (_, pattern, _, body)) => MatchExprCase(pattern, body)
          }

        case Rule.MatchExpr =>
          matchToken(KW_MATCH) ++! (ruleExpression.observeSource ++ (rule(Rule.MatchCase).observeSource*) ++ matchToken(KW_END)) --> {
            case (_, (cmpValue, cases, _)) =>
              MatchExpr(cmpValue, cases)
          }


        case Rule.TopLevelStatement =>
          (rule(Rule.StatementSeparator)*) ++ ruleTopLevelStatement.observeSource ++ (rule(Rule.StatementSeparator)*) --> {
            case (_, stmt, _) => stmt
          }
      }



    // Expressions

    private lazy val ruleExpressionOther: TGrammar[Expr] =
      matchTokenFactory(Identifier) --> { case Identifier(id) => IdentifierExpr(id) : Expr } |
        matchTokenFactory(StringToken) --> {
          case StringToken(NonEmptyList(StringToken.StringPart(str), INil())) => StringValueExpr(str)
          case StringToken(NonEmptyList(StringToken.StringPart(str), ICons(_, _))) => ???
        } |
        matchTokenFactory(IntToken) --> { case IntToken(sign, base, digits) => IntValueExpr(sign, base, digits) } |
        matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> const(TupleExpr(Vector())) |
        matchToken(OP_OPENPAREN) ++ ruleExpression ++ matchToken(OP_CLOSEPAREN) --> {
          case (_, expr, _) => expr
        } |
        matchToken(KW_TRUE) --> const(BoolValueExpr(true)) |
        matchToken(KW_FALSE) --> const(BoolValueExpr(false)) |
        rule(Rule.IfExpr) |
        rule(Rule.MatchExpr)


    private trait ParenCallHandlerBase {
      def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T]
    }

    private object ParenCallHandler extends ParenCallHandlerBase {

      private lazy val ruleArgList: TGrammar[Expr] =
        matchToken(OP_OPENPAREN) ++ (ruleExpression?) ++ matchToken(OP_CLOSEPAREN) --> {
          case (_, Some(argList), _) => argList
          case (_, None, _) => TupleExpr(Vector.empty)
        }

      override def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T] = {
        lazy val rule: TGrammar[T] =
          rule -- ruleArgList.observeSource -\> f | nextRule

        rule
      }
    }

    private object SkipParenCallHandler extends ParenCallHandlerBase {
      override def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T] = nextRule
    }


    private val ruleMemberAccess: TGrammar[WithSource[Expr] => Expr] =
      matchTokenFactory(Identifier) --> { case Identifier(id) => baseExpr: WithSource[Expr] => DotExpr(baseExpr, id) } |
        matchToken(KW_NEW) --> const(ClassConstructorExpr.apply _) |
        matchToken(KW_TYPE) --> const(TypeOfExpr.apply _)

    private def ruleExpressionDot(parenCallHandler: ParenCallHandlerBase)(nextRule: TGrammar[Expr]): TGrammar[Expr] = {
      lazy val rule: TGrammar[Expr] =
        rule -- (matchToken(OP_DOT) ++! ruleMemberAccess) -\> {
          case (baseExpr, (_, memberAccessFunc)) => memberAccessFunc(baseExpr)
        } | nextRule

      rule
    }

    private def createLeftAssociativeOperatorRule(firstOpGrammar: TGrammar[BinaryOperator], opGrammars: TGrammar[BinaryOperator]*)(nextGrammar: TGrammar[Expr]): TGrammar[Expr] = {
      val opGrammarsNel = NonEmptyList.nel(firstOpGrammar, IList(opGrammars: _*))

      lazy val grammar: TGrammar[Expr] = {
        val rightGrammars = opGrammarsNel.map { opGrammar =>
          Lazy { (opGrammar ++! grammar.observeSource) --> { case (op, right) => left: WithSource[Expr] => BinaryOperatorExpr(op, left, right) } }
        }

        nextGrammar.observeSource ++ (UnionGrammar.fromList(rightGrammars)?) --> {
          case (WithSource(left, _), None) => left
          case (left, Some(rightFunc)) => rightFunc(left)
        }
      }

      grammar
    }

    private def ruleBinaryOperator[TToken <: TokenWithCategory[_ <: TokenCategory] with BinaryOperatorToken : ClassTag](token: TToken): TGrammar[BinaryOperator] =
      matchToken(token) --> const(token.binaryOperator)


    private final class RuleChainer[T, TChain](val nextRule: TGrammar[T], skip: TChain) {

      def chain[U](label: String)(ruleFunc: TGrammar[T] => TGrammar[U]): RuleChainer[U, (TGrammar[T], TChain)] =
        chainPrev(label)((rule, _) => ruleFunc(rule))

      def chainPrev[U](label: String)(ruleFunc: (TGrammar[T], TChain) => TGrammar[U]): RuleChainer[U, (TGrammar[T], TChain)] =
        new RuleChainer[U, (TGrammar[T], TChain)](ruleFunc(nextRule, skip), (nextRule, skip))

    }

    private def chainRules[T](bottomRule: TGrammar[T]): RuleChainer[T, Unit] = new RuleChainer[T, Unit](bottomRule, ())

    private val chainUpToComparison =
      chainRules(ruleExpressionOther)
        .chain("function_call")(ParenCallHandler(FunctionCallExpr.apply))
        .chain("dot_expr_paren")(ruleExpressionDot(ParenCallHandler))
        .chain("unary_operators")(nextExpr => {
          lazy val rule: TGrammar[Expr] = {
            def matchPrefixOp[TToken <: TokenWithCategory[_ <: TokenCategory] with UnaryOperatorToken : ClassTag](token: TToken): TGrammar[Expr] =
              matchToken(token) ++ rule.observeSource --> { case (_, inner) => UnaryOperatorExpr(token.unaryOperator, inner) }

            nextExpr |
              matchPrefixOp(OP_BITNOT) |
              matchPrefixOp(OP_BOOLNOT) |
              matchPrefixOp(OP_ADD) |
              matchPrefixOp(OP_SUB)
          }

          rule
        })
        .chainPrev("dot_expr_noparen") { case (nextExpr, (_, (skippedCallExpr, _))) => {
          lazy val rule: TGrammar[Expr] =
            rule -- ruleExpressionDot(SkipParenCallHandler)(skippedCallExpr).observeSource -\> FunctionCallExpr | nextExpr

          rule
        } }
        .chain("muldiv_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_MUL),
          ruleBinaryOperator(OP_DIV),
        ))
        .chain("addsub_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_ADD),
          ruleBinaryOperator(OP_SUB),
        ))
        .chain("bitshift_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_SHIFTLEFT),
          ruleBinaryOperator(OP_SHIFTRIGHT),
        ))
        .chain("bitand_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_BITAND),
        ))
        .chain("bitxor_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_BITXOR),
        ))
        .chain("bitor_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_BITOR),
        ))
        .chain("type_expr")(nextRule =>
          nextRule |
            matchToken(KW_TYPE) ++
              ((matchToken(OP_SUBTYPE) ++ nextRule.observeSource --> second)?) ++
              ((matchToken(OP_SUPERTYPE) ++ nextRule.observeSource --> second)?) ++
              ((matchToken(OP_COLON) ++ nextRule.observeSource --> second)?) --> {
              case (_, subtypeOf, supertypeOf, instanceType) =>
                TypeExpr(instanceType, subtypeOf, supertypeOf)
            }
        )

    private val ruleExpressionSkipCompare: TGrammar[Expr] = chainUpToComparison.nextRule


    private val ruleCommonExpr: TGrammar[Expr] =
      chainUpToComparison
        .chain("inequality_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_LESSTHAN),
          ruleBinaryOperator(OP_LESSTHANEQ),
          ruleBinaryOperator(OP_GREATERTHAN),
          ruleBinaryOperator(OP_GREATERTHANEQ),
        ))
        .chain("equality_expr")(createLeftAssociativeOperatorRule(
          ruleBinaryOperator(OP_EQUALS),
          ruleBinaryOperator(OP_NOTEQUALS),
        ))
        .chain("as_expr")(nextRule =>
          nextRule.observeSource ++ ((matchToken(KW_AS) ++! nextRule.observeSource)?) --> {
            case (WithSource(left, _), None) => left
            case (left, Some((_, right))) => AsExpr(left, right)
          }
        )
        .nextRule

    private def ruleExpressionAssignment(nextRule: => TGrammar[Expr]): TGrammar[Expr] =
      nextRule.observeSource ++ ((matchToken(OP_ASSIGN) ++! nextRule.observeSource)?) --> {
        case (WithSource(left, _), None) => left
        case (left, Some((_, right))) => BinaryOperatorExpr(BinaryOperator.Assign, left, right)
      }

    private def ruleExpressionLambda(nextRule: => TGrammar[Expr]): TGrammar[Expr] = {
      lazy val grammar: TGrammar[Expr] =
        rule(Rule.Identifier) ++ matchToken(OP_LAMBDA) ++! (grammar | nextRule).observeSource -->
          { case (id, _, body) => LambdaExpr(id, body) }

      grammar
    }

    private def ruleExpressionLambdaType(nextRule: => TGrammar[Expr]): TGrammar[Expr] = {
      lazy val rule: TGrammar[Expr] =
        nextRule.observeSource ++ ((matchToken(OP_LAMBDA_TYPE) ++! rule.observeSource)?) --> {
          case (WithSource(left, _), None) => left
          case (left, Some((_, right))) => LambdaTypeExpr(left, right)
        }

      rule
    }

    private def ruleExpressionTuple(nextRule: TGrammar[Expr]): TGrammar[Expr] =
      nextRule.observeSource ++ ((matchToken(OP_COMMA) ++! nextRule.observeSource --> second)*) --> {
        case (WithSource(expr, _), Vector()) => expr
        case (head, tail) => TupleExpr(head +: tail)
      }

    private lazy val ruleExpression: TGrammar[Expr] =
      ruleExpressionAssignment(ruleExpressionTuple(
        ruleExpressionLambda(ruleExpressionAssignment(ruleCommonExpr)) | ruleExpressionLambdaType(ruleCommonExpr)
      ))


    private lazy val ruleExpressionStatement: TGrammar[Stmt] = ruleExpression --> identity

    private lazy val ruleExpressionType: TGrammar[Expr] =
      ruleExpressionLambda(ruleExpressionSkipCompare) |
        ruleExpressionLambdaType(ruleExpressionSkipCompare)

    // Variable Declaration
    private val ruleVariableMutSpec: TGrammar[Boolean] =
      matchToken(KW_VAL) --> const(false) | matchToken(KW_VAR) --> const(true)

    private val ruleVariableDeclaration: TGrammar[Stmt] =
      ruleVariableMutSpec ++! (
        rule(Rule.Identifier) ++
          ((matchToken(OP_COLON) ++ ruleExpressionType.observeSource --> second)?) ++
          matchToken(OP_EQUALS) ++
          ruleExpression.observeSource
        ) --> { case (isMutable, (id, typeAnnotation, _, value)) =>
        VariableDeclarationStmt(isMutable, typeAnnotation, id, value)
      }

    // Field
    private val ruleFieldDeclaration: TGrammar[Stmt] =
      matchToken(KW_FIELD) ++
        (ruleVariableMutSpec?) ++
        rule(Rule.Identifier) ++
        matchToken(OP_COLON) ++!
        ruleExpressionType.observeSource --> { case (_, isMutable, id, _, typeAnnotation) =>
        FieldDeclarationStmt(isMutable.getOrElse(false), id, typeAnnotation)
      }

    private val ruleFieldInitialization: TGrammar[Stmt] =
      matchToken(KW_FIELD) ++
        tokenIdentifier ++
        matchToken(OP_EQUALS) ++!
        ruleExpression.observeSource --> { case (_, id, _, value) =>
        FieldInitializationStmt(id, value)
      }

    private val ruleInitializeStatement: TGrammar[Stmt] =
      matchToken(KW_INITIALIZE) ++
        rule(Rule.Identifier) ++
        (
          (matchToken(OP_EQUALS) ++ ruleExpression.observeSource --> second)?
          ) --> { case (_, id, value) =>
        InitializeStmt(id, value)
      }

    private val ruleModifiers: TGrammar[Vector[WithSource[Modifier]]] = {
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
    }

    // Function and Method Definition
    private val ruleMethodParameter: TGrammar[FunctionParameter] =
      tokenIdentifier ++
        rule(Rule.NewLines) ++
        ((matchToken(OP_COLON) ++ rule(Rule.NewLines) ++ ruleExpressionType.observeSource --> { case (_, _, t) => t })?) ++
        ((matchToken(OP_SUBTYPE) ++ rule(Rule.NewLines) ++ ruleExpressionType.observeSource --> { case (_, _, t) => t })?) --> {
        case (name, _, paramType, subTypeOf) =>
          FunctionParameter(paramType, subTypeOf, name)
      }

    private val ruleMethodParameterList: TGrammar[Vector[WithSource[FunctionParameter]]] =
      ((
        ruleMethodParameter.observeSource ++
          rule(Rule.NewLines) ++
          ((matchToken(OP_COMMA) ++ ruleMethodParameter.observeSource ++ rule(Rule.NewLines) --> { case (_, param, _) => param })*) ++
          (matchToken(OP_COMMA)?) --> {
          case (firstParam, _, restParams, _) =>
            firstParam +: restParams.toVector
        }
        )?) --> { _.getOrElse(Vector.empty) }

    private lazy val ruleMethodParameters: TGrammar[Vector[FunctionParameterList]] =
      ((
        (
          matchToken(OP_OPENPAREN) ++ rule(Rule.NewLines) ++ ruleMethodParameterList ++ rule(Rule.NewLines) ++ matchToken(OP_CLOSEPAREN) --> {
            case (_, _, params, _, _) =>
              FunctionParameterList(FunctionParameterListType.NormalList, params)
          }
          ) |
          (
            matchToken(OP_OPENBRACKET) ++ rule(Rule.NewLines) ++ ruleMethodParameterList ++ rule(Rule.NewLines) ++ matchToken(OP_CLOSEBRACKET) --> {
              case (_, _, params, _, _) =>
                FunctionParameterList(FunctionParameterListType.InferrableList, params)
            }
            )
        )*) --> { _.toVector }

    private lazy val ruleMethodBody: TGrammar[Vector[WithSource[Stmt]]] =
      matchToken(KW_DO) ++! (ruleStatementList ++ matchToken(KW_END)) --> { case (_, (body, _)) => body } |
        matchToken(OP_EQUALS) ++! (rule(Rule.NewLines) ++ ruleExpression.observeSource) --> { case (_, (_, expr)) => Vector(expr) }

    private val ruleMethodPurity: TGrammar[Boolean] =
      matchToken(KW_DEF) --> const(true) |
        matchToken(KW_PROC) --> const(false)

    private lazy val ruleFunctionDefinition: TGrammar[Stmt] =
      ruleModifiers ++
        ruleMethodPurity ++
        rule(Rule.Identifier) ++
        rule(Rule.NewLines) ++
        ruleMethodParameters ++! (
        matchToken(OP_COLON) ++
          rule(Rule.NewLines) ++
          ruleExpressionType.observeSource ++
          rule(Rule.NewLines) ++
          ruleMethodBody.observeSource
        ) --> {
        case (modifiers, purity, name, _, params, (_, _, returnType, _, body)) =>
          FunctionDeclarationStmt(name, params, returnType, body, modifiers, purity)
      }

    private lazy val ruleMethodDefinition: TGrammar[Stmt] =
      ruleModifiers ++
        ruleMethodPurity ++
        rule(Rule.Identifier) ++
        rule(Rule.NewLines) ++
        matchToken(OP_DOT) ++
        rule(Rule.NewLines) ++
        rule(Rule.Identifier) ++! (
        rule(Rule.NewLines) ++
          ruleMethodParameters ++
          matchToken(OP_COLON) ++
          rule(Rule.NewLines) ++
          ruleExpressionType.observeSource ++
          rule(Rule.NewLines) ++
          (ruleMethodBody.observeSource?)
        ) --> {
        case (modifiers, purity, instanceName, _, _, _, name, (_, params, _, _, returnType, _, body)) =>
          MethodDeclarationStmt(instanceName, name, params, returnType, body, modifiers, purity)
      }

    private lazy val ruleClassConstructorDefinition: TGrammar[Stmt] =
      ruleModifiers ++
        matchToken(KW_NEW) ++! (
        ruleMethodParameters ++
          rule(Rule.StatementSeparator) ++
          ruleStatementList.observeSource ++
          matchToken(KW_END)
        ) --> {
        case (modifiers, _, (params, _, body, _)) =>
          ClassConstructorDeclarationStmt(params, body, modifiers)
      }

    // Types
    private val ruleStaticInstanceBody: TGrammar[(Vector[WithSource[Stmt]], Vector[WithSource[Stmt]])] =
      rule(Rule.NewLines) ++ matchToken(KW_STATIC) ++! (ruleStatementList ++ ((matchToken(KW_INSTANCE) ++! ruleStatementList --> { case (_, instanceBody) => instanceBody })?)) --> {
        case (_, _, (staticBody, instanceBody)) =>
          (staticBody, instanceBody.getOrElse(Vector.empty))
      } |
        ((rule(Rule.NewLines) ++ matchToken(KW_INSTANCE))?) ++ ruleStatementList --> {
          case (_, instanceBody) =>
            (Vector.empty, instanceBody)
        }

    private lazy val ruleTraitDefinition: TGrammar[Stmt] =
      ruleModifiers ++
        matchToken(KW_TRAIT) ++! (
        rule(Rule.Identifier) ++
          ruleMethodParameters ++
          matchToken(OP_SUBTYPE) ++
          ruleExpressionType.observeSource ++
          rule(Rule.StatementSeparator) ++
          ruleStaticInstanceBody ++
          matchToken(KW_END)
        ) --> {
        case (modifiers, _, (name, parameters, _, baseType, _, (staticBody, instanceBody), _)) =>
          TraitDeclarationStmt(baseType, name, parameters, staticBody, instanceBody, modifiers)
      }

    private lazy val ruleDataConstructorDefinition: TGrammar[Stmt] =
      ruleModifiers ++
        matchToken(KW_CONSTRUCTOR) ++! (
        rule(Rule.Identifier) ++
          rule(Rule.NewLines) ++
          ruleMethodParameters ++
          rule(Rule.NewLines) ++
          matchToken(OP_COLON) ++
          rule(Rule.NewLines) ++
          ruleExpressionType.observeSource ++
          rule(Rule.StatementSeparator) ++
          ruleStatementList ++
          matchToken(KW_END)
        ) --> {
        case (modifiers, _, (name, _, params, _, _, _, returnType, _, body, _)) =>
          DataConstructorDeclarationStmt(name, params, returnType, body, modifiers)
      }

    private lazy val ruleClassDefinition: TGrammar[Stmt] =
      ruleModifiers ++
        matchToken(KW_CLASS) ++! (
        rule(Rule.Identifier) ++
          ruleMethodParameters ++
          matchToken(OP_SUBTYPE) ++
          ruleExpressionType.observeSource ++
          rule(Rule.StatementSeparator) ++
          ruleStaticInstanceBody ++
          matchToken(KW_END)
        ) --> {
        case (modifiers, _, (name, params, _, baseType, _, (staticBody, instanceBody), _)) =>
          ClassDeclarationStmt(baseType, name, params, staticBody, instanceBody, modifiers)
      }

    private lazy val ruleStatement: TGrammar[Stmt] =
      ruleVariableDeclaration |
        ruleFieldDeclaration |
        ruleFieldInitialization |
        ruleInitializeStatement |
        ruleMethodDefinition |
        ruleFunctionDefinition |
        ruleClassConstructorDefinition |
        ruleTraitDefinition |
        ruleDataConstructorDefinition |
        ruleClassDefinition |
        ruleExpressionStatement


    private lazy val ruleStatementList: TGrammar[Vector[WithSource[Stmt]]] =
      (rule(Rule.StatementSeparator)*) ++ (((ruleStatement.observeSource ++ (rule(Rule.StatementSeparator)*)) --> { case (stmt, _) => stmt })*) --> {
        case (_, stmts) => stmts
      }

    private lazy val ruleNamespacePath: TGrammar[NamespacePath] =
      tokenIdentifier ++ ((matchToken(OP_DOT) ++ tokenIdentifier --> second)*) --> {
        case (head, tail) => NamespacePath(head +: tail)
      }

    private lazy val ruleNamespaceDeclaration: TGrammar[TopLevelStatement] =
      matchToken(KW_NAMESPACE) ++! ruleNamespacePath --> { case (_, ns) => TopLevelStatement.Namespace(ns) }

    private lazy val ruleTopLevelStatement: TGrammar[TopLevelStatement] =
      ruleNamespaceDeclaration |
        rule(Rule.ImportNamespace) |
        ruleStatement.observeSource --> TopLevelStatement.Statement

    ruleTopLevelStatement

  }

  private[impl] def grammarFactory: GrammarFactory[Token, SyntaxError, Rule.ArgonRuleName] = ArgonGrammarFactory

  def parse[F[_]: Monad]: Pipe[EitherT[F, NonEmptyList[SyntaxError], ?], WithSource[Token], TopLevelStatement] =
    _
      .through(Grammar.parseAll[F, Token, SyntaxError, Rule.ArgonRuleName, WithSource[TopLevelStatement]](ArgonGrammarFactory)(Rule.TopLevelStatement)(FilePosition(1, 1)))
      .map { _.value }

}