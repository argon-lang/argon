package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.Token._
import com.mi3software.argon.parser._
import com.mi3software.argon.util.{FilePosition, NamespacePath, WithSource}

import scala.reflect.ClassTag
import scala.language.postfixOps
import scalaz.{ICons, INil, NonEmptyList, Order}
import Grammar.Operators._

import Function.const

object Parser {

  type TGrammar[T] = Grammar[Token, SyntaxError, T]

  private implicit val errorFactory = new Grammar.ErrorFactory[Token, TokenCategory, SyntaxError] {
    override def createError(error: GrammarError[Token, TokenCategory]): SyntaxError =
      SyntaxError.ParserError(error)

    override def errorEndLocationOrder: Order[SyntaxError] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  def second[T](pair: (_, T)): T = pair._2

  private def partialMatcher[T](category: TokenCategory)(f: PartialFunction[Token, T]): TGrammar[T] =
    Grammar.partialMatcher(category)(f)

  private def matchTokenFactory[TTokenCategory <: TokenCategory, TToken <: Token : ClassTag](factory: TokenWithCategory[TTokenCategory] with TokenFactory[TToken]): TGrammar[TToken] =
    partialMatcher(factory.category) { case t: TToken => t }

  private def matchToken[TToken <: TokenWithCategory[_ <: TokenCategory] with Token : ClassTag](token: TToken): TGrammar[TToken] =
    partialMatcher(token.category) { case t: TToken => t }

  private val tokenUnderscore: TGrammar[Unit] =
    matchToken(KW_UNDERSCORE).discard

  private val tokenIdentifier: TGrammar[String] =
    matchTokenFactory(Identifier) --> { _.name }



  private val ruleIdentifier: TGrammar[Option[String]] =
    tokenUnderscore --> const(None : Option[String]) |
      tokenIdentifier --> Some.apply

  private val skipNewLines: TGrammar[Unit] =
    (matchToken(NewLine)*).discard

  private val ruleStatementSeparator: TGrammar[Unit] =
    matchToken(NewLine).discard | matchToken(Semicolon).discard

  private trait RecRuleFunc[T] {
    def apply(self: => T): T
  }

  private def recRule[T](f: RecRuleFunc[T]): T = {
    lazy val value: T = f(value)
    value
  }


  // Expressions
  private val ruleIfExpr: TGrammar[Expr] = {

    type RuleFunc = (WithSource[Expr], WithSource[Vector[WithSource[Stmt]]]) => Expr

    lazy val ifRulePart: TGrammar[Expr] =
      (
        ruleExpression.observeSource ++ matchToken(KW_THEN) ++ ruleStatementList.observeSource ++ (
          matchToken(KW_END) -->[RuleFunc] { _ => (condition, body) => IfExpr(condition, body) } |
            (matchToken(KW_ELSE) ++ ruleStatementList.observeSource ++ matchToken(KW_END)) -->[RuleFunc] {
              case (_, elseBody, _) => (condition, body) => IfElseExpr(condition, body, elseBody)
            } |
            (matchToken(KW_ELSIF) ++ ifRulePart.observeSource) -->[RuleFunc] {
              case (_, elseExpr) => (condition, body) => IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location))
            }
        )
      ) --> { case (condition, _, body, ruleFunc) => ruleFunc(condition, body) }

    matchToken(KW_IF) ++ ifRulePart --> second
  }

  private val ruleParenPattern: TGrammar[Pattern] =
    matchToken(OP_OPENPAREN) ++ rulePattern ++ matchToken(OP_CLOSEPAREN) --> {
      case (_, pattern, _) => pattern
    }

  private val ruleVariablePattern: TGrammar[Pattern] =
    matchToken(KW_VAL) ++ ruleIdentifier ++ ((matchToken(OP_COLON) ++ ruleExpression.observeSource)?) --> {
      case (_, idOpt, Some((_, varType))) => TypeTestPattern(idOpt, varType)
      case (_, Some(id), None) => BindingPattern(id)
      case (_, None, None) => DiscardPattern
    }

  private val ruleDiscardPattern: TGrammar[Pattern] =
    matchToken(KW_UNDERSCORE) --> const(DiscardPattern)

  private val rulePatternConstructorExpr: TGrammar[Expr] =
    {
      lazy val idPath: TGrammar[Expr] =
        tokenIdentifier --> { id => IdentifierExpr(id) : Expr } |
          idPath.observeSource ++ matchToken(OP_DOT) ++ tokenIdentifier --> {
            case (baseExpr, _, id) => DotExpr(baseExpr, id)
          }

      idPath
    } |
    matchToken(OP_OPENCURLY) ++ ruleExpression ++ matchToken(OP_CLOSECURLY) --> { case (_, expr, _) => expr }

  private val rulePatternSeq: TGrammar[Vector[WithSource[Pattern]]] =
    ((
      rulePatternConstructorExpr.observeSource --> { expr => DeconstructPattern(expr, Vector()) : Pattern } |
        ruleDiscardPattern |
        ruleParenPattern
    ).observeSource*) --> { _.toVector }

  private val ruleDeconstructPattern: TGrammar[Pattern] =
    rulePatternConstructorExpr.observeSource ++ rulePatternSeq --> (DeconstructPattern.apply _).tupled

  private lazy val rulePattern: TGrammar[Pattern] =
    ruleParenPattern |
      ruleVariablePattern |
      ruleDeconstructPattern |
      ruleDiscardPattern

  private val ruleExpressionMatch: TGrammar[Expr] = {
    val matchCaseRule: TGrammar[MatchExprCase] =
      rulePattern.observeSource ++ matchToken(OP_EQUALS) ++ ruleStatementList.observeSource --> {
        case (pattern, _, body) => MatchExprCase(pattern, body)
      }


    matchToken(KW_MATCH) ++ ruleExpression.observeSource ++ (matchCaseRule.observeSource*) ++ matchToken(KW_END) --> {
      case (_, cmpValue, cases, _) =>
        MatchExpr(cmpValue, cases.toVector)
    }
  }

  private val ruleExpressionOther: TGrammar[Expr] =
    matchTokenFactory(Identifier) --> { case Identifier(id) => IdentifierExpr(id) : Expr } |
      matchTokenFactory(StringToken) --> {
        case StringToken(NonEmptyList(StringToken.StringPart(str), INil())) => StringValueExpr(str)
        case StringToken(NonEmptyList(StringToken.StringPart(str), ICons(_, _))) => ???
      } |
      matchTokenFactory(IntToken) --> { case IntToken(i) => IntValueExpr(i) } |
      matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> const(TupleExpr(Vector())) |
      matchToken(OP_OPENPAREN) ++ ruleExpression ++ matchToken(OP_CLOSEPAREN) --> {
        case (_, expr, _) => expr
      } |
      matchToken(KW_TRUE) --> const(BoolValueExpr(true)) |
      matchToken(KW_FALSE) --> const(BoolValueExpr(false)) |
      ruleIfExpr |
      ruleExpressionMatch


  private trait ParenCallHandlerBase {
    def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T]
  }

  private object ParenCallHandler extends ParenCallHandlerBase {

    private val ruleArgList: TGrammar[Expr] =
      matchToken(OP_OPENPAREN) ++ (ruleExpression?) ++ matchToken(OP_CLOSEPAREN) --> {
        case (_, Some(argList), _) => argList
        case (_, None, _) => TupleExpr(Vector.empty)
      }

    override def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T] = {
      lazy val rule: TGrammar[T] =
        nextRule | rule.observeSource ++ ruleArgList.observeSource --> f.tupled

      rule
    }
  }

  private object SkipParenCallHandler extends ParenCallHandlerBase {
    override def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T] = nextRule
  }


  private val ruleMemberAccess: TGrammar[WithSource[Expr] => Expr] =
    matchTokenFactory(Identifier) --> [WithSource[Expr] => Expr] { case Identifier(id) => baseExpr => DotExpr(baseExpr, id) } |
      matchToken(KW_NEW) --> const(ClassConstructorExpr.apply) |
      matchToken(KW_TYPE) --> const(TypeOfExpr.apply)

  private def ruleExpressionDot(parenCallHandler: ParenCallHandlerBase)(nextRule: TGrammar[Expr]): TGrammar[Expr] =
    recRule[TGrammar[Expr]](rule =>
      nextRule | rule.observeSource ++ matchToken(OP_DOT) ++ ruleMemberAccess --> {
        case (baseExpr, _, memberAccessFunc) => memberAccessFunc(baseExpr)
      }
    )

  private def createLeftAssociativeOperatorRule(opGrammars: TGrammar[BinaryOperator]*)(nextGrammar: TGrammar[Expr]): TGrammar[Expr] =
    recRule[TGrammar[Expr]] { grammar =>
      opGrammars.foldLeft(nextGrammar) { case (accum, opGrammar) =>
        accum | nextGrammar.observeSource ++ opGrammar ++ grammar.observeSource --> { case (left, op, right) => BinaryOperatorExpr(op, left, right) }
      }
    }

  private def ruleBinaryOperator[TToken <: TokenWithCategory[_ <: TokenCategory] with BinaryOperatorToken : ClassTag](token: TToken): TGrammar[BinaryOperator] =
    matchToken(token) --> const(token.binaryOperator)


  private final class RuleChainer[TRule, TChain](val nextRule: TRule, skip: TChain) {

    def chain[U](ruleFunc: TRule => U): RuleChainer[U, (TRule, TChain)] =
      chainPrev((rule, _) => ruleFunc(rule))

    def chainPrev[U](ruleFunc: (TRule, TChain) => U): RuleChainer[U, (TRule, TChain)] =
      new RuleChainer[U, (TRule, TChain)](ruleFunc(nextRule, skip), (nextRule, skip))

  }

  private def chainRules[T](bottomRule: T): RuleChainer[T, Unit] = new RuleChainer[T, Unit](bottomRule, ())

  private val chainUpToComparison =
    chainRules(ruleExpressionOther)
      .chain(ParenCallHandler(FunctionCallExpr.apply))
      .chain(ruleExpressionDot(ParenCallHandler))
      .chain(nextExpr => recRule[TGrammar[Expr]](rule => {
        def matchPrefixOp[TToken <: TokenWithCategory[_ <: TokenCategory] with UnaryOperatorToken : ClassTag](token: TToken): TGrammar[Expr] =
          matchToken(token) ++ rule.observeSource --> { case (_, inner) => UnaryOperatorExpr(token.unaryOperator, inner) }

        nextExpr |
          matchPrefixOp(OP_BITNOT) |
          matchPrefixOp(OP_BOOLNOT) |
          matchPrefixOp(OP_ADD) |
          matchPrefixOp(OP_SUB)
      }))
      .chainPrev { case (nextExpr, (_, (skippedCallExpr, _))) => recRule[TGrammar[Expr]] { rule =>
        nextExpr | rule.observeSource ++ ruleExpressionDot(SkipParenCallHandler)(skippedCallExpr).observeSource --> FunctionCallExpr.tupled
      }
      }
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_MUL),
        ruleBinaryOperator(OP_DIV),
      ))
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_ADD),
        ruleBinaryOperator(OP_SUB),
      ))
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_SHIFTLEFT),
        ruleBinaryOperator(OP_SHIFTRIGHT),
      ))
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_BITAND),
      ))
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_BITXOR),
      ))
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_BITOR),
      ))
      .chain(nextRule =>
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
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_LESSTHAN),
        ruleBinaryOperator(OP_LESSTHANEQ),
        ruleBinaryOperator(OP_GREATERTHAN),
        ruleBinaryOperator(OP_GREATERTHANEQ),
      ))
      .chain(createLeftAssociativeOperatorRule(
        ruleBinaryOperator(OP_EQUALS),
        ruleBinaryOperator(OP_NOTEQUALS),
      ))
      .chain(nextRule =>
        nextRule | nextRule.observeSource ++ matchToken(KW_AS) ++ nextRule.observeSource --> {
          case (left, _, right) => AsExpr(left, right)
        }
      )
      .nextRule

  private def ruleExpressionAssignment(leftRule: => TGrammar[Expr], rightRule: => TGrammar[Expr]): TGrammar[Expr] =
    leftRule.observeSource ++ matchToken(OP_ASSIGN) ++ rightRule.observeSource --> {
      case (left, _, right) => BinaryOperatorExpr(BinaryOperator.Assign, left, right)
    }


  private def ruleExpressionLambdas(nextRule: => TGrammar[Expr]): TGrammar[Expr] =
    recRule[TGrammar[Expr]](rule =>
      nextRule |
        ruleIdentifier ++ matchToken(OP_LAMBDA) ++ rule.observeSource --> {
          case (id, _, body) => LambdaExpr(id, body)
        } |
        nextRule.observeSource ++ matchToken(OP_LAMBDA_TYPE) ++ rule.observeSource --> {
          case (left, _, right) => LambdaTypeExpr(left, right)
        }
    )

  private def ruleExpressionTuple(nextRule: TGrammar[Expr]): TGrammar[Expr] =
    nextRule.observeSource ++ ((matchToken(OP_COMMA) ++ nextRule.observeSource --> second)*) --> {
      case (WithSource(expr, _), INil()) => expr
      case (head, tail) => TupleExpr(head +: tail.toVector)
    }


  private lazy val ruleExpression_lambdaRule: TGrammar[Expr] = ruleExpressionLambdas(ruleExpression_assignRule | ruleCommonExpr)
  private lazy val ruleExpression_assignRule: TGrammar[Expr] = ruleExpressionAssignment(ruleCommonExpr, ruleExpression_lambdaRule)

  private lazy val ruleExpression: TGrammar[Expr] =
    ruleExpressionTuple(ruleExpression_lambdaRule | ruleExpression_assignRule)


  private lazy val ruleExpressionStatement: TGrammar[Stmt] = ruleExpression --> identity

  private lazy val ruleExpressionType: TGrammar[Expr] =
    ruleExpressionLambdas(ruleExpressionSkipCompare)

  // Variable Declaration
  private val ruleVariableMutSpec: TGrammar[Boolean] =
    matchToken(KW_VAL) --> const(false) | matchToken(KW_VAR) --> const(true)

  private val ruleVariableDeclaration: TGrammar[Stmt] =
    ruleVariableMutSpec ++
      ruleIdentifier ++
      ((matchToken(OP_COLON) ++ ruleExpressionType.observeSource --> second)?) ++
      matchToken(OP_EQUALS) ++
      ruleExpression.observeSource --> { case (isMutable, id, typeAnnotation, _, value) =>
      VariableDeclarationStmt(isMutable, typeAnnotation, id, value)
    }

  // Field
  private val ruleFieldDeclaration: TGrammar[Stmt] =
    matchToken(KW_FIELD) ++
      (ruleVariableMutSpec?) ++
      ruleIdentifier ++
      matchToken(OP_COLON) ++
      ruleExpressionType.observeSource --> { case (_, isMutable, id, _, typeAnnotation) =>
      FieldDeclarationStmt(isMutable.getOrElse(false), id, typeAnnotation)
    }

  private val ruleFieldInitialization: TGrammar[Stmt] =
    matchToken(KW_FIELD) ++
      tokenIdentifier ++
      matchToken(OP_EQUALS) ++
      ruleExpression.observeSource --> { case (_, id, _, value) =>
      FieldInitializationStmt(id, value)
    }

  private val ruleInitializeStatement: TGrammar[Stmt] =
    matchToken(KW_INITIALIZE) ++
      ruleIdentifier ++
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
      skipNewLines ++
      ((matchToken(OP_COLON) ++ skipNewLines ++ ruleExpressionType.observeSource --> { case (_, _, t) => t })?) ++
      ((matchToken(OP_SUBTYPE) ++ skipNewLines ++ ruleExpressionType.observeSource --> { case (_, _, t) => t })?) --> {
      case (name, _, paramType, subTypeOf) =>
        FunctionParameter(paramType, subTypeOf, name)
    }

  private val ruleMethodParameterList: TGrammar[Vector[FunctionParameter]] =
    ((
      ruleMethodParameter ++
        skipNewLines ++
        ((matchToken(OP_COMMA) ++ ruleMethodParameter ++ skipNewLines --> { case (_, param, _) => param })*) ++
        (matchToken(OP_COMMA)?) --> {
        case (firstParam, _, restParams, _) =>
          firstParam +: restParams.toVector
      }
    )?) --> { _.getOrElse(Vector.empty) }

  private val ruleMethodParameters: TGrammar[Vector[FunctionParameterList]] =
    ((
      (
        matchToken(OP_OPENPAREN) ++ skipNewLines ++ ruleMethodParameterList ++ skipNewLines ++ matchToken(OP_CLOSEPAREN) --> {
          case (_, _, params, _, _) =>
            FunctionParameterList(FunctionParameterListType.NormalList, params)
        }
      ) |
        (
          matchToken(OP_OPENBRACKET) ++ skipNewLines ++ ruleMethodParameterList ++ skipNewLines ++ matchToken(OP_CLOSEBRACKET) --> {
            case (_, _, params, _, _) =>
              FunctionParameterList(FunctionParameterListType.InferrableList, params)
          }
        )
    )*) --> { _.toVector }

  private val ruleMethodBody: TGrammar[Vector[WithSource[Stmt]]] =
    matchToken(KW_DO) ++ ruleStatementList ++ matchToken(KW_END) --> { case (_, body, _) => body } |
      matchToken(OP_EQUALS) ++ ruleExpression.observeSource --> { case (_, expr) => Vector(expr) }

  private val ruleMethodPurity: TGrammar[Boolean] =
    matchToken(KW_DEF) --> const(true) |
      matchToken(KW_PROC) --> const(false)

  private val ruleFunctionDefinition: TGrammar[Stmt] =
    ruleModifiers ++
      ruleMethodPurity ++
      ruleIdentifier ++
      skipNewLines ++
      ruleMethodParameters ++
      matchToken(OP_COLON) ++
      skipNewLines ++
      ruleExpressionType.observeSource ++
      skipNewLines ++
      ruleMethodBody.observeSource --> {
      case (modifiers, purity, name, _, params, _, _, returnType, _, body) =>
        FunctionDeclarationStmt(name, params, returnType, body, modifiers, purity)
    }

  private val ruleMethodDefinition: TGrammar[Stmt] =
    ruleModifiers ++
      ruleMethodPurity ++
      ruleIdentifier ++
      skipNewLines ++
      matchToken(OP_DOT) ++
      skipNewLines ++
      ruleIdentifier ++
      skipNewLines ++
      ruleMethodParameters ++
      matchToken(OP_COLON) ++
      skipNewLines ++
      ruleExpressionType.observeSource ++
      skipNewLines ++
      ruleMethodBody.observeSource --> {
      case (modifiers, purity, instanceName, _, _, _, name, _, params, _, _, returnType, _, body) =>
        MethodDeclarationStmt(instanceName, name, params, returnType, body, modifiers, purity)
    }

  private val ruleClassConstructorDefinition: TGrammar[Stmt] =
    ruleModifiers ++
      matchToken(KW_NEW) ++
      ruleMethodParameters ++
      ruleStatementSeparator ++
      ruleStatementList.observeSource ++
      matchToken(KW_END) --> {
      case (modifiers, _, params, _, body, _) =>
        ClassConstructorDeclarationStmt(params, body, modifiers)
    }

  // Types
  private val ruleStaticInstanceBody: TGrammar[(Vector[WithSource[Stmt]], Vector[WithSource[Stmt]])] =
    skipNewLines ++ matchToken(KW_STATIC) ++ ruleStatementList ++ ((matchToken(KW_INSTANCE) ++ ruleStatementList --> { case (_, instanceBody) => instanceBody })?) --> {
      case (_, _, staticBody, instanceBody) =>
        (staticBody, instanceBody.getOrElse(Vector.empty))
    } |
      ((skipNewLines ++ matchToken(KW_INSTANCE))?) ++ ruleStatementList --> {
        case (_, instanceBody) =>
          (Vector.empty, instanceBody)
      }

  private val ruleTraitDefinition: TGrammar[Stmt] =
    ruleModifiers ++
      matchToken(KW_TRAIT) ++
      ruleIdentifier ++
      ruleMethodParameters ++
      matchToken(OP_SUBTYPE) ++
      ruleExpressionType.observeSource ++
      ruleStatementSeparator ++
      ruleStaticInstanceBody ++
      matchToken(KW_END) --> {
      case (modifiers, _, name, parameters, _, baseType, _, (staticBody, instanceBody), _) =>
        TraitDeclarationStmt(baseType, name, parameters, staticBody, instanceBody, modifiers)
    }

  private val ruleDataConstructorDefinition: TGrammar[Stmt] =
    ruleModifiers ++
      matchToken(KW_CONSTRUCTOR) ++
      ruleIdentifier ++
      skipNewLines ++
      ruleMethodParameters ++
      skipNewLines ++
      matchToken(OP_COLON) ++
      skipNewLines ++
      ruleExpressionType.observeSource ++
      ruleStatementSeparator ++
      ruleStatementList ++
      matchToken(KW_END) --> {
      case (modifiers, _, name, _, params, _, _, _, returnType, _, body, _) =>
        DataConstructorDeclarationStmt(name, params, returnType, body, modifiers)
    }

  private val ruleClassDefinition: TGrammar[Stmt] =
    ruleModifiers ++
      matchToken(KW_CLASS) ++
      ruleIdentifier ++
      ruleMethodParameters ++
      matchToken(OP_SUBTYPE) ++
      ruleExpressionType.observeSource ++
      ruleStatementSeparator ++
      ruleStaticInstanceBody ++
      matchToken(KW_END) --> {
      case (modifiers, _, name, params, _, baseType, _, (staticBody, instanceBody), _) =>
        ClassDeclarationStmt(baseType, name, params, staticBody, instanceBody, modifiers)
    }

  private val ruleStatement: TGrammar[Stmt] =
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
    (ruleStatementSeparator*) ++ (((ruleStatement.observeSource ++ (ruleStatementSeparator*)) --> { case (stmt, _) => stmt })*) --> {
      case (_, stmts) => stmts.toVector
    }

  private val ruleNamespacePath: TGrammar[NamespacePath] =
    tokenIdentifier ++ ((matchToken(OP_DOT) ++ tokenIdentifier --> second)*) --> {
      case (head, tail) => NamespacePath(head +: tail.toVector)
    }

  private val ruleNamespaceDeclaration: TGrammar[TopLevelStatement] =
    matchToken(KW_NAMESPACE) ++ ruleNamespacePath --> { case (_, ns) => TopLevelStatement.Namespace(ns) }

  private val ruleImportNamespace: TGrammar[TopLevelStatement] =
    matchToken(KW_IMPORT) ++ ruleNamespacePath ++ matchToken(OP_DOT) ++ matchToken(KW_UNDERSCORE) --> {
      case (_, ns, _, _) => TopLevelStatement.Import(ns)
    }

  private val ruleTopLevelStatement: TGrammar[TopLevelStatement] =
    ruleNamespaceDeclaration |
      ruleImportNamespace |
      ruleStatement.observeSource --> TopLevelStatement.Statement

}
