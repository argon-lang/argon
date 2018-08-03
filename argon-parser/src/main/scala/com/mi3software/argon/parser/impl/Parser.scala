package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.Token._
import com.mi3software.argon.parser._
import com.mi3software.argon.util._

import scala.reflect.ClassTag
import scala.language.postfixOps
import scalaz._
import Scalaz._
import Grammar.Operators._
import Grammar.{UnionGrammar, label => labelRule}

import Function.const

final class Parser {

  type TGrammar[T] = Grammar[Token, SyntaxError, String, T]

  private implicit val errorFactory = new Grammar.ErrorFactory[Token, TokenCategory, SyntaxError] {
    override def createError(error: GrammarError[Token, TokenCategory]): SyntaxError =
      SyntaxError.ParserError(error)

    override def createAmbiguityError(location: SourceLocation): SyntaxError =
      SyntaxError.AmbiguousParse(location)

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

  // Expressions
  private val ruleIfExpr: TGrammar[Expr] = labelRule("if_expr") {

    type BodyList = WithSource[Vector[WithSource[Stmt]]]

    val ifToken = matchToken(KW_IF)
    val elseIfToken = matchToken(KW_ELSIF)

    lazy val ifRulePart: TGrammar[Any] => TGrammar[Expr] = Memo.immutableHashMapMemo(prefix =>
      (
        prefix ++! (ruleExpression.observeSource ++ matchToken(KW_THEN) ++ ruleStatementList.observeSource ++ (
          matchToken(KW_END) --> { _ => (condition: WithSource[Expr], body: BodyList) => IfExpr(condition, body) } |
            (matchToken(KW_ELSE) ++ ruleStatementList.observeSource ++ matchToken(KW_END)) --> {
              case (_, elseBody, _) => (condition: WithSource[Expr], body: BodyList) => IfElseExpr(condition, body, elseBody)
            } |
            ifRulePart(elseIfToken).observeSource --> {
              elseExpr => (condition: WithSource[Expr], body: BodyList) => IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location))
            }
          )
        )) --> { case (_, (condition, _, body, ruleFunc)) => ruleFunc(condition, body) }
    )

    ifRulePart(ifToken)
  }

  private val ruleParenPattern: TGrammar[Pattern] =
    matchToken(OP_OPENPAREN) ++ rulePattern ++ matchToken(OP_CLOSEPAREN) --> {
      case (_, pattern, _) => pattern
    }

  private val ruleVariablePattern: TGrammar[Pattern] =
    matchToken(KW_VAL) ++ ruleIdentifier ++ ((matchToken(OP_COLON) ++ ruleExpressionType.observeSource)?) --> {
      case (_, idOpt, Some((_, varType))) => TypeTestPattern(idOpt, varType)
      case (_, Some(id), None) => BindingPattern(id)
      case (_, None, None) => DiscardPattern
    }

  private val ruleDiscardPattern: TGrammar[Pattern] =
    matchToken(KW_UNDERSCORE) --> const(DiscardPattern)

  private val rulePatternConstructorExpr: TGrammar[Expr] =
    {
      lazy val idPath: TGrammar[Expr] =
        idPath -- (matchToken(OP_DOT) ++ tokenIdentifier) -\> {
          case (baseExpr, (_, id)) => DotExpr(baseExpr, id)
        } |
          tokenIdentifier --> { id => IdentifierExpr(id) : Expr }

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

  private lazy val rulePattern: TGrammar[Pattern] = labelRule("pattern") {
    ruleParenPattern |
      ruleVariablePattern |
      ruleDeconstructPattern |
      ruleDiscardPattern
  }

  private val ruleExpressionMatch: TGrammar[Expr] = labelRule("match_expr") {
    val matchCaseRule: TGrammar[MatchExprCase] =
      skipNewLines ++ matchToken(KW_CASE) ++! (skipNewLines ++ rulePattern.observeSource ++ matchToken(OP_EQUALS) ++ ruleStatementList.observeSource) --> {
        case (_, _, (_, pattern, _, body)) => MatchExprCase(pattern, body)
      }


    matchToken(KW_MATCH) ++! (ruleExpression.observeSource ++ (matchCaseRule.observeSource*) ++ matchToken(KW_END)) --> {
      case (_, (cmpValue, cases, _)) =>
        MatchExpr(cmpValue, cases)
    }
  }

  private val ruleExpressionOther: TGrammar[Expr] = labelRule("expression_other")(
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
      ruleIfExpr |
      ruleExpressionMatch
  )


  private trait ParenCallHandlerBase {
    def apply[T](f: (WithSource[T], WithSource[Expr]) => T)(nextRule: TGrammar[T]): TGrammar[T]
  }

  private object ParenCallHandler extends ParenCallHandlerBase {

    private val ruleArgList: TGrammar[Expr] = labelRule("argument_list")(
      matchToken(OP_OPENPAREN) ++ (ruleExpression?) ++ matchToken(OP_CLOSEPAREN) --> {
        case (_, Some(argList), _) => argList
        case (_, None, _) => TupleExpr(Vector.empty)
      }
    )

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
      new RuleChainer[U, (TGrammar[T], TChain)](labelRule(label + "_level")(ruleFunc(nextRule, skip)), (nextRule, skip))

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
    lazy val rule: TGrammar[Expr] =
      ruleIdentifier ++ matchToken(OP_LAMBDA) ++! (rule | nextRule).observeSource -->
        { case (id, _, body) => LambdaExpr(id, body) }

    rule
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

  private lazy val ruleExpression: TGrammar[Expr] = labelRule("expression")(
    ruleExpressionAssignment(ruleExpressionTuple(
      ruleExpressionLambda(ruleExpressionAssignment(ruleCommonExpr)) | ruleExpressionLambdaType(ruleCommonExpr)
    ))
  )


  private lazy val ruleExpressionStatement: TGrammar[Stmt] = ruleExpression --> identity

  private lazy val ruleExpressionType: TGrammar[Expr] = labelRule("expression_type")(
    ruleExpressionLambda(ruleExpressionSkipCompare) |
      ruleExpressionLambdaType(ruleExpressionSkipCompare)
  )

  // Variable Declaration
  private val ruleVariableMutSpec: TGrammar[Boolean] =
    matchToken(KW_VAL) --> const(false) | matchToken(KW_VAR) --> const(true)

  private val ruleVariableDeclaration: TGrammar[Stmt] =
    ruleVariableMutSpec ++! (
      ruleIdentifier ++
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
      ruleIdentifier ++
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

  private val ruleMethodParameterList: TGrammar[Vector[WithSource[FunctionParameter]]] =
    ((
      ruleMethodParameter.observeSource ++
        skipNewLines ++
        ((matchToken(OP_COMMA) ++ ruleMethodParameter.observeSource ++ skipNewLines --> { case (_, param, _) => param })*) ++
        (matchToken(OP_COMMA)?) --> {
        case (firstParam, _, restParams, _) =>
          firstParam +: restParams.toVector
      }
    )?) --> { _.getOrElse(Vector.empty) }

  private val ruleMethodParameters: TGrammar[Vector[FunctionParameterList]] = labelRule("method_parameters")(
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
  )

  private val ruleMethodBody: TGrammar[Vector[WithSource[Stmt]]] = labelRule("method_body")(
    matchToken(KW_DO) ++! (ruleStatementList ++ matchToken(KW_END)) --> { case (_, (body, _)) => body } |
      matchToken(OP_EQUALS) ++! (skipNewLines ++ ruleExpression.observeSource) --> { case (_, (_, expr)) => Vector(expr) }
  )

  private val ruleMethodPurity: TGrammar[Boolean] =
    matchToken(KW_DEF) --> const(true) |
      matchToken(KW_PROC) --> const(false)

  private val ruleFunctionDefinition: TGrammar[Stmt] = labelRule("function_definition")(
    ruleModifiers ++
      ruleMethodPurity ++
      ruleIdentifier ++
      skipNewLines ++
      ruleMethodParameters ++! (
        matchToken(OP_COLON) ++
          skipNewLines ++
          ruleExpressionType.observeSource ++
          skipNewLines ++
          ruleMethodBody.observeSource
      ) --> {
        case (modifiers, purity, name, _, params, (_, _, returnType, _, body)) =>
          FunctionDeclarationStmt(name, params, returnType, body, modifiers, purity)
      }
  )

  private val ruleMethodDefinition: TGrammar[Stmt] = labelRule("method_definition")(
    ruleModifiers ++
      ruleMethodPurity ++
      ruleIdentifier ++
      skipNewLines ++
      matchToken(OP_DOT) ++
      skipNewLines ++
      ruleIdentifier ++! (
        skipNewLines ++
          ruleMethodParameters ++
          matchToken(OP_COLON) ++
          skipNewLines ++
          ruleExpressionType.observeSource ++
          skipNewLines ++
          (ruleMethodBody.observeSource?)
      ) --> {
        case (modifiers, purity, instanceName, _, _, _, name, (_, params, _, _, returnType, _, body)) =>
          MethodDeclarationStmt(instanceName, name, params, returnType, body, modifiers, purity)
      }
  )

  private val ruleClassConstructorDefinition: TGrammar[Stmt] = labelRule("class_ctor_definition")(
    ruleModifiers ++
      matchToken(KW_NEW) ++! (
        ruleMethodParameters ++
          ruleStatementSeparator ++
          ruleStatementList.observeSource ++
          matchToken(KW_END)
      ) --> {
        case (modifiers, _, (params, _, body, _)) =>
          ClassConstructorDeclarationStmt(params, body, modifiers)
      }
  )

  // Types
  private val ruleStaticInstanceBody: TGrammar[(Vector[WithSource[Stmt]], Vector[WithSource[Stmt]])] =
    skipNewLines ++ matchToken(KW_STATIC) ++! (ruleStatementList ++ ((matchToken(KW_INSTANCE) ++! ruleStatementList --> { case (_, instanceBody) => instanceBody })?)) --> {
      case (_, _, (staticBody, instanceBody)) =>
        (staticBody, instanceBody.getOrElse(Vector.empty))
    } |
      ((skipNewLines ++ matchToken(KW_INSTANCE))?) ++ ruleStatementList --> {
        case (_, instanceBody) =>
          (Vector.empty, instanceBody)
      }

  private val ruleTraitDefinition: TGrammar[Stmt] = labelRule("trait_definition")(
    ruleModifiers ++
      matchToken(KW_TRAIT) ++! (
        ruleIdentifier ++
          ruleMethodParameters ++
          matchToken(OP_SUBTYPE) ++
          ruleExpressionType.observeSource ++
          ruleStatementSeparator ++
          ruleStaticInstanceBody ++
          matchToken(KW_END)
      ) --> {
        case (modifiers, _, (name, parameters, _, baseType, _, (staticBody, instanceBody), _)) =>
          TraitDeclarationStmt(baseType, name, parameters, staticBody, instanceBody, modifiers)
      }
  )

  private val ruleDataConstructorDefinition: TGrammar[Stmt] = labelRule("data_ctor_definition")(
    ruleModifiers ++
      matchToken(KW_CONSTRUCTOR) ++! (
        ruleIdentifier ++
          skipNewLines ++
          ruleMethodParameters ++
          skipNewLines ++
          matchToken(OP_COLON) ++
          skipNewLines ++
          ruleExpressionType.observeSource ++
          ruleStatementSeparator ++
          ruleStatementList ++
          matchToken(KW_END)
      ) --> {
        case (modifiers, _, (name, _, params, _, _, _, returnType, _, body, _)) =>
          DataConstructorDeclarationStmt(name, params, returnType, body, modifiers)
      }
  )

  private val ruleClassDefinition: TGrammar[Stmt] = labelRule("ctor_definition")(
    ruleModifiers ++
      matchToken(KW_CLASS) ++! (
        ruleIdentifier ++
          ruleMethodParameters ++
          matchToken(OP_SUBTYPE) ++
          ruleExpressionType.observeSource ++
          ruleStatementSeparator ++
          ruleStaticInstanceBody ++
          matchToken(KW_END)
      ) --> {
        case (modifiers, _, (name, params, _, baseType, _, (staticBody, instanceBody), _)) =>
          ClassDeclarationStmt(baseType, name, params, staticBody, instanceBody, modifiers)
      }
  )

  private val ruleStatement: TGrammar[Stmt] = labelRule("statement")(
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
  )


  private lazy val ruleStatementList: TGrammar[Vector[WithSource[Stmt]]] = labelRule("statement_list")(
    (ruleStatementSeparator*) ++ (((ruleStatement.observeSource ++ (ruleStatementSeparator*)) --> { case (stmt, _) => stmt })*) --> {
      case (_, stmts) => stmts
    }
  )

  private val ruleNamespacePath: TGrammar[NamespacePath] = labelRule("namespace_path")(
    tokenIdentifier ++ ((matchToken(OP_DOT) ++ tokenIdentifier --> second)*) --> {
      case (head, tail) => NamespacePath(head +: tail)
    }
  )

  private val ruleNamespaceDeclaration: TGrammar[TopLevelStatement] =
    matchToken(KW_NAMESPACE) ++! ruleNamespacePath --> { case (_, ns) => TopLevelStatement.Namespace(ns) }

  private val ruleImportNamespace: TGrammar[TopLevelStatement] =
    matchToken(KW_IMPORT) ++! (ruleNamespacePath ++ matchToken(OP_DOT) ++ matchToken(KW_UNDERSCORE)) --> {
      case (_, (ns, _, _)) => TopLevelStatement.Import(ns)
    }

  private val ruleTopLevelStatement: TGrammar[TopLevelStatement] = labelRule("top_level_stmt")(
    ruleNamespaceDeclaration |
      ruleImportNamespace |
      ruleStatement.observeSource --> TopLevelStatement.Statement
  )

  private val ruleTopLevelStatementPadded: TGrammar[WithSource[TopLevelStatement]] =
    (ruleStatementSeparator*) ++ ruleTopLevelStatement.observeSource ++ (ruleStatementSeparator*) --> {
      case (_, stmt, _) => stmt
    }

  private def collectStmts(stmt: WithSource[WithSource[TopLevelStatement]]): Option[TopLevelStatement] =
    Some(stmt.value.value)

  def parse(tokens: Vector[WithSource[Token]]): Either[NonEmptyList[SyntaxError], Vector[TopLevelStatement]] =
    Grammar.parseAll(ruleTopLevelStatementPadded)(collectStmts)(tokens, FilePosition(1, 1)).toEither


}
