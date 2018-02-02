package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Token._
import com.mi3software.argon.parser._
import com.mi3software.argon.util.WithSource

import scala.reflect.ClassTag
import scala.language.postfixOps
import scalaz.{ICons, INil, NonEmptyList}

import Grammar.Operators._
import Function.const

object Parser {

  type TGrammar[T] = Grammar[Token, TokenCategory, T]

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

    matchToken(KW_IF) ++ ifRulePart --> { case (_, expr) => expr }
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
      rulePattern.observeSource ++ matchToken(OP_LAMBDA) ++ ruleStatementList.observeSource --> {
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
      matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> { _ => TupleExpr(Vector()) } |
      matchToken(OP_OPENPAREN) ++ ruleExpression ++ matchToken(OP_CLOSEPAREN) --> {
        case (_, expr, _) => expr
      } |
      matchToken(KW_TRUE) --> { _ => BoolValueExpr(true) } |
      matchToken(KW_FALSE) --> { _ => BoolValueExpr(false) } |
      ruleIfExpr |
      ruleExpressionMatch

  private val ruleExpressionL20 = ruleExpressionOther


  private trait ParenCallHandlerBase {
    def apply[T](nextRule: TGrammar[T])(f: (WithSource[T], WithSource[Expr]) => T): TGrammar[T]
  }

  private object ParenCallHandler extends ParenCallHandlerBase {

    private val ruleArgList: TGrammar[Expr] =
      matchToken(OP_OPENPAREN) ++ (ruleExpression?) ++ matchToken(OP_CLOSEPAREN) --> {
        case (_, Some(argList), _) => argList
        case (_, None, _) => TupleExpr(Vector.empty)
      }

    override def apply[T](nextRule: TGrammar[T])(f: (WithSource[T], WithSource[Expr]) => T): TGrammar[T] = {
      lazy val rule: TGrammar[T] =
        nextRule | rule.observeSource ++ ruleArgList.observeSource --> f.tupled

      rule
    }
  }

  private object SkipParenCallHandler extends ParenCallHandlerBase {
    override def apply[T](nextRule: TGrammar[T])(f: (WithSource[T], WithSource[Expr]) => T): TGrammar[T] = nextRule
  }


  private val ruleExpressionL19: TGrammar[Expr] =
    ParenCallHandler(ruleExpressionL20)(FunctionCallExpr.apply)

  private val ruleMemberAccess: TGrammar[WithSource[Expr] => Expr] =
    matchTokenFactory(Identifier) --> [WithSource[Expr] => Expr] { case Identifier(id) => baseExpr => DotExpr(baseExpr, id) } |
      matchToken(KW_NEW) --> const(ClassConstructorExpr.apply) |
      matchToken(KW_TYPE) --> const(TypeOfExpr.apply)

  private def ruleExpressionDot(nextRule: TGrammar[Expr], parenCallHandler: ParenCallHandlerBase): TGrammar[Expr] = {

    lazy val rule: TGrammar[Expr] =
      nextRule | rule.observeSource ++ matchToken(OP_DOT) ++ ruleMemberAccess --> {
        case (baseExpr, _, memberAccessFunc) => memberAccessFunc(baseExpr)
      }

    rule
  }

  private val ruleExpressionL18: TGrammar[Expr] =
    ruleExpressionDot(ruleExpressionL19, ParenCallHandler)

  private val ruleExpressionL18_SkipParenCalls: TGrammar[Expr] =
    ruleExpressionDot(ruleExpressionL20, SkipParenCallHandler)

  private val ruleExpressionL17: TGrammar[Expr] = {
    def matchPrefxOp[TToken <: TokenWithCategory[_ <: TokenCategory] with UnaryOperatorToken : ClassTag](token: TToken): TGrammar[Expr] =
      matchToken(token) ++ ruleExpressionL17.observeSource --> { case (_, inner) => UnaryOperatorExpr(token.unaryOperator, inner) }

    ruleExpressionL18 |
      matchPrefxOp(OP_BITNOT) |
      matchPrefxOp(OP_BOOLNOT) |
      matchPrefxOp(OP_ADD) |
      matchPrefxOp(OP_SUB)
  }

  private val ruleExpressionL16: TGrammar[Expr] =
    ruleExpressionL17 | ruleExpressionL16.observeSource ++ ruleExpressionL18_SkipParenCalls.observeSource --> FunctionCallExpr.tupled

  private def createLeftAssociativeOperatorRule(nextGrammar: => TGrammar[Expr], grammar: => TGrammar[Expr])(opGrammars: TGrammar[BinaryOperator]*): TGrammar[Expr] =
    opGrammars.foldLeft(nextGrammar) { case (accum, opGrammar) =>
      accum | nextGrammar.observeSource ++ opGrammar ++ grammar.observeSource --> { case (left, op, right) => BinaryOperatorExpr(op, left, right) }
    }

  private def ruleBinaryOperator[TToken <: TokenWithCategory[_ <: TokenCategory] with BinaryOperatorToken : ClassTag](token: TToken): TGrammar[BinaryOperator] =
    matchToken(token) --> const(token.binaryOperator)

  private val ruleExpressionL15: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL16, ruleExpressionL15)(
      ruleBinaryOperator(OP_MUL),
      ruleBinaryOperator(OP_DIV),
    )

  private val ruleExpressionL14: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL15, ruleExpressionL14)(
      ruleBinaryOperator(OP_ADD),
      ruleBinaryOperator(OP_SUB),
    )

  private val ruleExpressionL13: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL14, ruleExpressionL13)(
      ruleBinaryOperator(OP_SHIFTLEFT),
      ruleBinaryOperator(OP_SHIFTRIGHT),
    )

  private val ruleExpressionL12: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL13, ruleExpressionL12)(
      ruleBinaryOperator(OP_BITAND),
    )

  private val ruleExpressionL11: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL12, ruleExpressionL11)(
      ruleBinaryOperator(OP_BITXOR),
    )

  private val ruleExpressionL10: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL11, ruleExpressionL10)(
      ruleBinaryOperator(OP_BITOR),
    )

  private val ruleExpressionL09: TGrammar[Expr] =
    ruleExpressionL10 |
      matchToken(KW_TYPE) ++
        ((matchToken(OP_SUBTYPE) ++ ruleExpressionL10.observeSource --> { case (_, expr) => expr })?) ++
        ((matchToken(OP_SUPERTYPE) ++ ruleExpressionL10.observeSource --> { case (_, expr) => expr })?) ++
        ((matchToken(OP_COLON) ++ ruleExpressionL10.observeSource --> { case (_, expr) => expr })?) --> {
        case (_, subtypeOf, supertypeOf, instanceType) =>
          TypeExpr(instanceType, subtypeOf, supertypeOf)
      }

  private val ruleExpressionL08: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL09, ruleExpressionL08)(
      ruleBinaryOperator(OP_LESSTHAN),
      ruleBinaryOperator(OP_LESSTHANEQ),
      ruleBinaryOperator(OP_GREATERTHAN),
      ruleBinaryOperator(OP_GREATERTHANEQ),
    )

  private val ruleExpressionL07: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL08, ruleExpressionL07)(
      ruleBinaryOperator(OP_EQUALS),
      ruleBinaryOperator(OP_NOTEQUALS),
    )

  private val ruleExpressionL06: TGrammar[Expr] =
    ruleExpressionL07 | ruleIdentifier ++ matchToken(OP_LAMBDA) ++ ruleExpressionL04.observeSource --> {
      case (id, _, body) => LambdaExpr(id, body)
    }

  private val ruleExpressionL05: TGrammar[Expr] =
    ruleExpressionL06 | ruleExpressionL06.observeSource ++ matchToken(OP_ASSIGN) ++ ruleExpressionL06.observeSource --> {
      case (left, _, right) => BinaryOperatorExpr(BinaryOperator.Assign, left, right)
    }

  private lazy val ruleExpressionL04: TGrammar[Expr] =
    ruleExpressionL05 | ruleExpressionL05.observeSource ++ matchToken(KW_AS) ++ ruleExpressionL05.observeSource --> {
      case (left, _, right) => AsExpr(left, right)
    }

  private val ruleExpressionL03: TGrammar[Expr] =
    ruleExpressionL04 | ruleExpressionL04.observeSource ++ matchToken(OP_LAMBDA_TYPE) ++ ruleExpressionL03.observeSource --> {
      case (left, _, right) => LambdaTypeExpr(left, right)
    }

  private val ruleExpressionL02: TGrammar[Expr] =
    ruleExpressionL03.observeSource ++ ((matchToken(OP_COMMA) ++ ruleExpressionL03.observeSource --> { case (_, expr) => expr })*) --> {
      case (WithSource(expr, _), INil()) => expr
      case (head, tail) => TupleExpr(head +: tail.toVector)
    }

  private val ruleExpressionL01: TGrammar[Expr] = ruleExpressionL02

  private lazy val ruleExpression: TGrammar[Expr] = ruleExpressionL01
  private lazy val ruleExpressionStatement: TGrammar[Stmt] = ruleExpression --> identity

  private lazy val ruleExpressionType: TGrammar[Expr] = ???

  // Variable Declaration
  private val ruleVariableMutSpec: TGrammar[Boolean] =
    matchToken(KW_VAL) --> const(false) | matchToken(KW_VAR) --> const(true)

  private val ruleVariableDeclaration: TGrammar[Stmt] =
    ruleVariableMutSpec ++
      ruleIdentifier ++
      ((matchToken(OP_COLON) ++ ruleExpressionType.observeSource --> { case (_, expr) => expr })?) ++
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
        (matchToken(OP_EQUALS) ++ ruleExpression.observeSource --> { case (_, expr) => expr })?
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

}
