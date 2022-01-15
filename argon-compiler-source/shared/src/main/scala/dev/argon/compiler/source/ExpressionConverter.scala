package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.module.ModuleElementC
import dev.argon.compiler.signature.Signature
import dev.argon.util.{WithSource, SourceLocation}
import dev.argon.parser
import dev.argon.parser.{IdentifierExpr, FunctionParameterListType}
import dev.argon.expr.Evaluator
import dev.argon.expr.ImplicitResolver
import zio.Random
import dev.argon.util.UniqueIdentifier
import zio.*
import dev.argon.util.{*, given}
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.module.ModulePath

sealed abstract class ExpressionConverter extends UsingContext {

  val exprContext: ArgonExprContext with HasContext[context.type]
  import exprContext.{WrapExpr, ArExpr, ExprConstructor, Variable, LocalVariable}

  protected val evaluator: Evaluator[CompEnv, CompError] { val exprContext: ExpressionConverter.this.exprContext.type }
  protected val implicitResolver: ImplicitResolver[CompEnv, CompError] { val exprContext: ExpressionConverter.this.exprContext.type }
  protected val fuel: Int

  trait Scope {
    def lookup(id: IdentifierExpr): LookupResult

    final def addVariable(variable: Variable): Scope = new Scope {
      override def lookup(id: IdentifierExpr): LookupResult =
        if variable.name.contains(id) then
          LookupResult.Success(Seq(variable), LookupResult.NotFound())
        else
          Scope.this.lookup(id)
    }
  }

  object Scope {
    def fromImports(imports: Imports[context.type]): Scope = new Scope {
      override def lookup(id: IdentifierExpr): LookupResult =
        imports.get(id) match {
          case None | Some(Seq()) => LookupResult.NotFound()
          case Some(elements) => LookupResult.Success(elements, LookupResult.NotFound())
        }
    }
  }

  enum LookupResult {
    case Success(overloads: Seq[ScopeElement], nextPriority: LookupResult)
    case Suspended(suspendedResult: Comp[LookupResult])
    case NotFound()
  }

  type ScopeElement = ModuleElementC[context.type] | Variable



  final case class Env(
    scope: Scope,
  ) {
    def withScope(f: Scope => Scope): Env =
      copy(scope = f(scope))

    def mergeBranches(first: Env, second: Env): Env =
      this
  }

  final case class ArgumentInfo(arg: Env => ExprFactory, location: SourceLocation, listType: FunctionParameterListType)
  final case class MutatorValue(arg: Env => ExprFactory, location: SourceLocation)

  final case class ExprResult(expr: WrapExpr, env: Env)
  final case class ExprTypeResult(expr: WrapExpr, env: Env, exprType: WrapExpr)

  trait ExprFactory {
    def synth(): Comp[ExprTypeResult]
    def check(t: WrapExpr): Comp[ExprResult]

    def mutate(value: MutatorValue): ExprFactory = CompExprFactory(IO.fail(DiagnosticError.CanNotMutate()))
    def invoke(arg: ArgumentInfo): ExprFactory = ???
  }

  private trait ExprFactorySynth extends ExprFactory {
    override def check(t: WrapExpr): Comp[ExprResult] =
      for {
        res <- synth()
        env <- checkSubType(res.env, res.exprType, t)
      } yield ExprResult(res.expr, env)
  }

  private trait ExprFactoryCheck extends ExprFactory {
    override def synth(): Comp[ExprTypeResult] =
      IO.fail(DiagnosticError.UnknownTypeForExpression())
  }

  private final class ExprFactoryError(error: CompError) extends ExprFactory {
    override def synth(): Comp[ExprTypeResult] = IO.fail(error)
    override def check(t: WrapExpr): Comp[ExprResult] = IO.fail(error)
  }

  // Ensures that a <: b
  private def checkSubType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Env] =
    isSubType(env, a, b).flatMap {
      case Some(env) => IO.succeed(env)
      case None => IO.fail(DiagnosticError.TypeError())
    }

  private def isSubType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] = ???


  def convertStmtList(env: Env, stmts: WithSource[Vector[WithSource[parser.Stmt]]]): ExprFactory =
    stmts.value match {
      case Vector(WithSource(expr: parser.Expr, location)) =>
        EndOverloadExprFactory(convertExpr(env, WithSource(expr, location)))

      case head +: tail =>
        val tailStmts: WithSource[Vector[WithSource[parser.Stmt]]] =
          WithSource(
            tail,
            SourceLocation(
              tail.headOption
                .map { case WithSource(_, nextLoc) => nextLoc.start }
                .getOrElse { head.location.end },
              stmts.location.end
            )
          )

        val headResult: Comp[ExprResult] = head.value match {
          case expr: parser.Expr =>
            unitType.flatMap(convertExpr(env, WithSource(expr, head.location)).check)

          case varDecl: parser.VariableDeclarationStmt =>
            val localVarComp: Comp[(LocalVariable, WrapExpr, Env)] = varDecl.varType match {
              case Some(varType) =>
                for {
                  id <- UniqueIdentifier.make
                  tRes <- convertExpr(env, varType).check(anyType)
                  value <- convertExpr(tRes.env, varDecl.value).check(tRes.expr)
                  localVar = LocalVariable(id, tRes.expr, varDecl.name, varDecl.isMutable)
                } yield (localVar, value.expr, value.env)

              case None =>
                for {
                  id <- UniqueIdentifier.make
                  value <- convertExpr(env, varDecl.value).synth()
                  localVar = LocalVariable(id, value.exprType, varDecl.name, varDecl.isMutable)
                } yield (localVar, value.expr, value.env)
            }

            localVarComp.map { case (variable, value, env) =>
              val env2 = env.withScope(_.addVariable(variable))
              ExprResult(
                WrapExpr.OfExpr(ArExpr(ExprConstructor.BindVariable(variable), value)),
                env2
              )
            }

          case _ => IO.fail(DiagnosticError.InvalidStatementInFunction())
        }

        new ExprFactory {
          override def synth(): Comp[ExprTypeResult] =
            for {
              exprRes <- headResult
              tailRes <- convertStmtList(exprRes.env, tailStmts).synth()
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(exprRes.expr, tailRes.expr)))
            } yield ExprTypeResult(e, tailRes.env, tailRes.exprType)

          override def check(t: WrapExpr): Comp[ExprResult] =
            for {
              exprRes <- headResult
              tailRes <- convertStmtList(exprRes.env, tailStmts).check(t)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(exprRes.expr, tailRes.expr)))
            } yield ExprResult(e, tailRes.env)

          private def prependExpr(headExpr: WrapExpr, tailExpr: WrapExpr): WrapExpr =
            tailExpr match {
              case WrapExpr.OfExpr(e) =>
                (e.constructor : e.constructor.type & ExprConstructor) match {
                  case ctor: (e.constructor.type & ExprConstructor.Sequence.type) =>
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList.cons(headExpr, e.getArgs(ctor))))

                  case _ =>
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(headExpr, tailExpr)))
                }

              case _ =>
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(headExpr, tailExpr)))
            }

        }

      case _ => convertExpr(env, WithSource(parser.UnitLiteral, stmts.location))
    }

  def convertExpr(env: Env, expr: WithSource[parser.Expr]): ExprFactory =
    expr.value match {
      case parser.AsExpr(value, valueType) =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            convertExpr(env, valueType).check(anyType)
              .flatMap { case ExprResult(t, env) =>
                convertExpr(env, value).check(t)
                  .map { case ExprResult(e, env) =>
                    ExprTypeResult(e, env, t)
                  }
              }
        }

      case parser.BinaryOperatorExpr(op, left, right) =>
        EndOverloadExprFactory(
          OverloadExprFactory(env, env.scope.lookup(IdentifierExpr.OperatorIdentifier(op.value)), Seq(
            ArgumentInfo(convertExpr(_, left), left.location, FunctionParameterListType.NormalList),
            ArgumentInfo(convertExpr(_, right), right.location, FunctionParameterListType.NormalList),
          ))
        )

      case parser.BlockExpr(body, rescueCases, elseBody, Some(ensureBody)) =>
        new ExprFactory {
          override def synth(): Comp[ExprTypeResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              uType <- unitType
              innerRes <- convertExpr(env, inner).synth()
              ensuring <- convertStmtList(innerRes.env, ensureBody).check(uType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.EnsureExecuted, (innerRes.expr, ensuring.expr)))
            } yield ExprTypeResult(e, ensuring.env, innerRes.exprType)
          end synth


          override def check(t: WrapExpr): Comp[ExprResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              uType <- unitType
              innerRes <- convertExpr(env, inner).check(t)
              ensuring <- convertStmtList(innerRes.env, ensureBody).check(uType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.EnsureExecuted, (innerRes.expr, ensuring.expr)))
            } yield ExprResult(e, ensuring.env)
          end check
        }

      case parser.BlockExpr(body, Vector(), None, None) =>
        convertStmtList(env, body)

      case block: parser.BlockExpr => ???
        
      case parser.BoolValueExpr(b) =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            for {
              t <- boolType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantBool(b), EmptyTuple))
            } yield ExprTypeResult(e, env, t)
        }

      case parser.ClassConstructorExpr(classExpr) => ???
      case parser.DotExpr(left, id) => ???
      case parser.ExternExpr(specifier) => ???

      case parser.FunctionCallExpr(func, listType, arg) =>
        convertExpr(env, func)
          .invoke(ArgumentInfo(convertExpr(_, arg), arg.location, listType))

      case id: IdentifierExpr =>
        OverloadExprFactory(env, env.scope.lookup(id), Seq.empty)

      case parser.IfElseExpr(condition, ifBody, elseBody) =>
        new ExprFactory {
          override def synth(): Comp[ExprTypeResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(env, condition).check(bType)
              trueRes <- convertStmtList(condRes.env, ifBody).synth()
              falseRes <- convertStmtList(condRes.env, elseBody).synth()
              resEnv = condRes.env.mergeBranches(trueRes.env, falseRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.IfElse, (condRes.expr, trueRes.expr, falseRes.expr)))
              t = WrapExpr.OfExpr(ArExpr(ExprConstructor.UnionType, (trueRes.exprType, falseRes.exprType)))
            } yield ExprTypeResult(e, resEnv, t)


          override def check(t: WrapExpr): Comp[ExprResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(env, condition).check(bType)
              trueRes <- convertStmtList(condRes.env, ifBody).check(t)
              falseRes <- convertStmtList(condRes.env, elseBody).check(t)
              resEnv = condRes.env.mergeBranches(trueRes.env, falseRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.IfElse, (condRes.expr, trueRes.expr, falseRes.expr)))
            } yield ExprResult(e, resEnv)
        }

      case i: parser.IntValueExpr =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            for {
              t <- boolType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(i.value), EmptyTuple))
            } yield ExprTypeResult(e, env, t)
        }

      case parser.LambdaTypeExpr(argType, resultType) => ???

      case parser.LambdaExpr(varName, body) =>
        new ExprFactoryCheck {
          override def check(t: WrapExpr): Comp[ExprResult] =
            evaluator.normalizeTopLevelWrap(t, fuel).flatMap {
              case WrapExpr.OfExpr(normalizedType) =>
                normalizedType.constructor match {
                  case funcTypeCtor: (normalizedType.constructor.type & ExprConstructor.FunctionType.type) =>
                    val (argType, resType) = normalizedType.getArgs(funcTypeCtor)
                    for {
                      id <- UniqueIdentifier.make
                      paramVar = LocalVariable(id, argType, varName, isMutable = false)
                      bodyRes <- convertExpr(env, body).check(resType)
                      e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadLambda(paramVar), bodyRes.expr))
                    } yield ExprResult(e, bodyRes.env)

                  case _ => IO.fail(DiagnosticError.InvalidTypeForFunction())
                }

              case _ => IO.fail(DiagnosticError.InvalidTypeForFunction())
            }
        }

      case parser.MatchExpr(_, _) => ???

      case parser.RaiseExpr(exception) =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            for {
              exType <- exceptionType
              ex <- convertExpr(env, exception).check(exType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.RaiseException, ex.expr))
            } yield ExprTypeResult(e, ex.env, neverType)
        }

      case parser.StringValueExpr(parser.Token.StringToken(parser.Token.StringToken.StringPart(str) :: Nil)) =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            for {
              strType <- stringType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantString(str.value), EmptyTuple))
            } yield ExprTypeResult(e, env, strType)
        }

      case parser.StringValueExpr(_) => ???

      case parser.TupleExpr(values) =>
        new ExprFactory {
          override def synth(): Comp[ExprTypeResult] =
            synthPart(env, values, Nil, Nil)

          private def synthPart(env: Env, values: NonEmptyList[WithSource[parser.Expr]], valuesAcc: List[WrapExpr], typeAcc: List[WrapExpr]): Comp[ExprTypeResult] =
            convertExpr(env, values.head).synth().flatMap { currentRes =>
              values.tail match {
                case tail: ::[WithSource[parser.Expr]] =>
                  synthPart(currentRes.env, NonEmptyList.fromCons(tail), currentRes.expr :: valuesAcc, currentRes.exprType :: typeAcc)

                case Nil =>
                  val valuesExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadTuple, NonEmptyList.cons(currentRes.expr, valuesAcc).reverse))
                  val typesExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadTuple, NonEmptyList.cons(currentRes.exprType, typeAcc).reverse))
                  IO.succeed(ExprTypeResult(valuesExpr, currentRes.env, typesExpr))
              }
            }
          
          override def check(t: WrapExpr): Comp[ExprResult] =
            evaluator.normalizeTopLevelWrap(t, fuel).flatMap {
              case WrapExpr.OfExpr(normalizedType) =>
                normalizedType.constructor match {
                  case tupleTypeCtor: (normalizedType.constructor.type & ExprConstructor.LoadTuple.type) =>
                    val tupleType: NonEmptyList[WrapExpr] = normalizedType.getArgs(tupleTypeCtor)
                    checkPart(env, values, Nil, tupleType.toList)

                  case _ => IO.fail(DiagnosticError.InvalidTypeForFunction())
                }

              case _ => IO.fail(DiagnosticError.InvalidTypeForFunction())
            }
            

          private def checkPart(env: Env, values: NonEmptyList[WithSource[parser.Expr]], valuesAcc: List[WrapExpr], expectedTypes: List[WrapExpr]): Comp[ExprResult] =
            expectedTypes match {
              case currentExpected :: tailExpectedTypes =>
                convertExpr(env, values.head).check(currentExpected).flatMap { currentRes =>
                  values.tail match {
                    case tail: ::[WithSource[parser.Expr]] =>
                      checkPart(currentRes.env, NonEmptyList.fromCons(tail), currentRes.expr :: valuesAcc, tailExpectedTypes)

                    case Nil if tailExpectedTypes.nonEmpty =>
                      IO.fail(DiagnosticError.TupleSizeMismatch())

                    case Nil =>
                      val valuesExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadTuple, NonEmptyList.cons(currentRes.expr, valuesAcc).reverse))
                      IO.succeed(ExprResult(valuesExpr, currentRes.env))
                  }
                }

              case Nil =>
                IO.fail(DiagnosticError.TupleSizeMismatch())
            }

        }

      case parser.TypeExpr(level) => ???

      case parser.MetaTypeExpr(level) =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            val e = WrapExpr.OfExpr(ArExpr(ExprConstructor.OmegaTypeN(level), EmptyTuple))
            val t = WrapExpr.OfExpr(ArExpr(ExprConstructor.OmegaTypeN(level + 1), EmptyTuple))
            IO.succeed(ExprTypeResult(e, env, t))
          end synth
        }

      case parser.TypeOfExpr(ofExpr) => ???

      case parser.UnaryOperatorExpr(op, inner) =>
        EndOverloadExprFactory(
          OverloadExprFactory(env, env.scope.lookup(IdentifierExpr.OperatorIdentifier(op.value)), Seq(
            ArgumentInfo(convertExpr(_, inner), inner.location, FunctionParameterListType.NormalList),
          ))
        )

      case parser.UnitLiteral =>
        new ExprFactorySynth {
          override def synth(): Comp[ExprTypeResult] =
            for {
              uType <- unitType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadUnit, EmptyTuple))
            } yield ExprTypeResult(e, env, uType)
        }
    }

  private final class OverloadExprFactory(env: Env, lookup: LookupResult, args: Seq[ArgumentInfo]) extends ExprFactorySynth {
    override def synth(): Comp[ExprTypeResult] = resolveOverload(lookup)
    
    override def mutate(value: MutatorValue): ExprFactory =
      if args.isEmpty then
        CompExprFactory(mutateImpl(value, lookup))
      else
        super.mutate(value)
    override def invoke(arg: ArgumentInfo): ExprFactory = OverloadExprFactory(env, lookup, args :+ arg)

    private def resolveOverload(lookup: LookupResult): Comp[ExprTypeResult] =
      lookup match {
        case LookupResult.Success(overloads, next) =>

          def checkEachRank(overloads: Seq[Seq[ScopeElement]]): Comp[ExprTypeResult] =
            overloads match {
              case rank +: tail =>
                ZIO.foreach(rank)(checkOverload).map(_.flatten).flatMap {
                  case Seq(single) => IO.succeed(single)
                  case Seq() => checkEachRank(tail)
                  case _ => IO.fail(DiagnosticError.AmbiguousOverload())
                }

              case _ => resolveOverload(next)
            }

          rankOverloads(overloads).flatMap(checkEachRank)

        case LookupResult.Suspended(suspended) => suspended.flatMap(resolveOverload)
        
        case LookupResult.NotFound() =>
          IO.fail(DiagnosticError.LookupFailed())
      }

    // Ranks overloads based on parameter count relative to argument count, subtyping relation of parameters, etc.
    private def rankOverloads(overloads: Seq[ScopeElement]): Comp[Seq[Seq[ScopeElement]]] =
      def signatureOf(element: ScopeElement): Comp[Signature[WrapExpr, ?]] =
        element match {
          case variable: Variable =>
            IO.succeed(Signature.Result(()))

          case ModuleElementC.ClassElement(arClass) =>
            arClass.signature.map(convertSig(convertClassResult))

          case ModuleElementC.TraitElement(arTrait) =>
            arTrait.signature.map(convertSig(convertTraitResult))

          case ModuleElementC.FunctionElement(arFunc) =>
            arFunc.signature.map(convertSig(convertFunctionResult))
        }

      def isMoreSpecificType(a: WrapExpr, b: WrapExpr): Comp[Boolean] =
        isSubType(env, a, b).map { _.isDefined }
        

      def argumentDelta(args: List[ArgumentInfo], sig: Signature[WrapExpr, ?], acc: Int): Int =
        (args, sig) match {
          case (ArgumentInfo(_, _, FunctionParameterListType.InferrableList) :: tailArgs, Signature.Parameter(FunctionParameterListType.InferrableList, _, nextSig)) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (ArgumentInfo(_, _, FunctionParameterListType.InferrableList2) :: tailArgs, Signature.Parameter(FunctionParameterListType.InferrableList2, _, nextSig)) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (ArgumentInfo(_, _, FunctionParameterListType.RequiresList) :: tailArgs, Signature.Parameter(FunctionParameterListType.RequiresList, _, nextSig)) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (ArgumentInfo(_, _, FunctionParameterListType.NormalList) :: tailArgs, Signature.Parameter(_, _, nextSig)) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (ArgumentInfo(_, _, FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList) :: tailArgs, _) =>
            argumentDelta(tailArgs, sig, acc)

          case (ArgumentInfo(_, _, FunctionParameterListType.NormalList) :: tailArgs, Signature.Result(_)) =>
            argumentDelta(tailArgs, sig, acc + 1)

          case (Nil, Signature.Parameter(_, _, nextSig)) =>
            argumentDelta(args, sig, acc - 1)

          case (Nil, Signature.Result(_)) => acc
        }

      def checkBetterParams(a: WrapExpr, b: WrapExpr, nextA: Signature[WrapExpr, ?], nextB: Signature[WrapExpr, ?], hasFoundBetter: Boolean): Comp[Boolean] =
        if !hasFoundBetter then
          isMoreSpecificType(a, b).flatMap {
            case true => isBetterSig(nextA, nextB, hasFoundBetter = true)
            case false =>
              isMoreSpecificType(b, a).flatMap {
                case true => IO.succeed(false)
                case false => isBetterSig(nextA, nextB, hasFoundBetter)
              }
          }
        else
          isMoreSpecificType(b, a).flatMap {
            case true => IO.succeed(false)
            case false => isBetterSig(nextA, nextB, hasFoundBetter)
          }

      def isBetterSig(a: Signature[WrapExpr, ?], b: Signature[WrapExpr, ?], hasFoundBetter: Boolean): Comp[Boolean] =
        (a, b) match {
          case (Signature.Parameter(FunctionParameterListType.InferrableList, typeA, nextA), Signature.Parameter(FunctionParameterListType.InferrableList, typeB, nextB)) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (Signature.Parameter(FunctionParameterListType.InferrableList2, typeA, nextA), Signature.Parameter(FunctionParameterListType.InferrableList2, typeB, nextB)) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (Signature.Parameter(FunctionParameterListType.RequiresList, typeA, nextA), Signature.Parameter(FunctionParameterListType.RequiresList, typeB, nextB)) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (Signature.Parameter(FunctionParameterListType.NormalList, typeA, nextA), Signature.Parameter(FunctionParameterListType.NormalList, typeB, nextB)) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (Signature.Parameter(FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList, _, nextA), _) =>
            isBetterSig(nextA, b, hasFoundBetter)

          case (_, Signature.Parameter(FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList, _, nextB)) =>
            isBetterSig(a, nextB, hasFoundBetter)

          case (Signature.Result(_), Signature.Result(_)) =>
            IO.succeed(hasFoundBetter)

          case (Signature.Result(_), Signature.Parameter(FunctionParameterListType.NormalList, _, _)) =>
            IO.succeed(false)

          case (Signature.Parameter(FunctionParameterListType.NormalList, _, _), Signature.Result(_)) =>
            IO.succeed(false)
        }


      def isBetterThan(a: ScopeElement, b: ScopeElement): Comp[Boolean] =
        signatureOf(a).flatMap { sigA =>
          signatureOf(b).flatMap { sigB =>
            val argsList = args.toList
            val deltaA = argumentDelta(argsList, sigA, 0)
            val deltaB = argumentDelta(argsList, sigB, 0)

            if deltaA == deltaB then
              isBetterSig(sigA, sigB, hasFoundBetter = false)
            else
              IO.succeed(
                if deltaB == 0 then
                  false
                else if deltaA == 0 then
                  true
                else if deltaA > 0 && deltaB > 0 then
                  deltaB > deltaA
                else if deltaA > 0 then
                  true
                else if deltaB > 0 then
                  false
                else
                  deltaB < deltaA
              )
              
          }
        }

      def rankOverload(ranks: List[List[ScopeElement]], overload: ScopeElement): Comp[List[List[ScopeElement]]] =
        ranks match {
          case topRank :: lowerRanks =>
            ZIO.forall(topRank)(isBetterThan(overload, _)).flatMap {
              case true => IO.succeed(List(overload) :: ranks)
              case false =>
                ZIO.exists(topRank)(isBetterThan(_, overload)).flatMap {
                  case true => rankOverload(lowerRanks, overload).map { topRank :: _ }
                  case false => IO.succeed((overload :: topRank) :: lowerRanks)
                }
            }

          case Nil =>
            IO.succeed(List(List(overload)))
        }

      

      ZIO.foldLeft(overloads)(Nil)(rankOverload)
    end rankOverloads

    

    // Checks if the current overload is applicable
    private def checkOverload(overload: ScopeElement): Comp[Option[ExprTypeResult]] =
      overload match {
        case variable: Variable =>
          IO.succeed(Some(ExprTypeResult(
            WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple)),
            env,
            variable.varType
          )))

        case ModuleElementC.ClassElement(arClass) =>
          createSigResult(env, arClass.signature, args, Vector.empty)(convertClassResult) { case (env, args, (typeOfClassType, _, _)) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(arClass), args)),
              env,
              typeOfClassType
            )
          }

        case ModuleElementC.TraitElement(arTrait) =>
          createSigResult(env, arTrait.signature, args, Vector.empty)(convertTraitResult) { case (env, args, (typeOfTraitType, _)) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(arTrait), args)),
              env,
              typeOfTraitType
            )
          }

        case ModuleElementC.FunctionElement(func) =>
          createSigResult(env, func.signature, args, Vector.empty)(convertFunctionResult) { (env, args, returnType) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionCall(func), args)),
              env,
              returnType
            )
          }

      }

    private def createSigResult[Res1, Res2]
    (
      env: Env,
      sig: Comp[Signature[context.ExprContext.WrapExpr, Res1]],
      args: Seq[ArgumentInfo],
      convArgs: Vector[WrapExpr],
    )(
      convertRes: Res1 => Res2
    )(
      f: (Env, Vector[WrapExpr], Res2) => ExprTypeResult
    ): Comp[Option[ExprTypeResult]] = ???

    private def mutateImpl(value: MutatorValue, lookup: LookupResult): Comp[ExprFactory] =
      lookup match {
        case LookupResult.Success(Seq(variable: Variable), _) if variable.isMutable =>
          IO.succeed(MutateVariableExprFactory(variable, value))

        case LookupResult.Success(Seq(_), _) => IO.fail(DiagnosticError.CanNotMutate())

        case LookupResult.Success(Seq(), next) => mutateImpl(value, next)

        case LookupResult.Success(_, _) => IO.fail(DiagnosticError.AmbiguousOverload())

        case LookupResult.Suspended(suspended) => suspended.flatMap(mutateImpl(value, _))

        case LookupResult.NotFound() => IO.fail(DiagnosticError.LookupFailed())
      }

    private final class MutateVariableExprFactory(variable: Variable, value: MutatorValue) extends ExprFactorySynth {
      override def synth(): Comp[ExprTypeResult] =
        for {
          valueExpr <- value.arg(env).check(variable.varType)
          unitT <- unitType
          e = WrapExpr.OfExpr(ArExpr(ExprConstructor.StoreVariable(variable), valueExpr.expr))
        } yield ExprTypeResult(e, valueExpr.env, unitT)
    }

  }

  private final class EndOverloadExprFactory(inner: ExprFactory) extends ExprFactory {
    override def synth(): Comp[ExprTypeResult] = inner.synth()
    override def check(t: WrapExpr): Comp[ExprResult] = inner.check(t)
  }

  private final class CompExprFactory(inner: Comp[ExprFactory]) extends ExprFactory {
    override def synth(): Comp[ExprTypeResult] = inner.flatMap(_.synth())
    override def check(t: WrapExpr): Comp[ExprResult] = inner.flatMap(_.check(t))
    
    override def mutate(value: MutatorValue): ExprFactory = CompExprFactory(inner.map(_.mutate(value)))
    override def invoke(arg: ArgumentInfo): ExprFactory = CompExprFactory(inner.map(_.invoke(arg)))
  }

  private def convertSig[Res1, Res2](convertRes: Res1 => Res2)(sig: Signature[context.ExprContext.WrapExpr, Res1]): Signature[WrapExpr, Res2] =
    sig match {
      case Signature.Parameter(paramListType, paramType, next) =>
        Signature.Parameter(
          paramListType,
          ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(paramType),
          convertSig(convertRes)(next)
        )

      case Signature.Result(res) =>
        Signature.Result(convertRes(res))
    }

  private def convertClassResult(res: ArClass#ClassResult): (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]]) =
    val (level, baseClass, baseTraits) = res
    
    val level2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(level)
    val baseClass2 = baseClass.map(ArgonExprContext.convertClassType[Id](context)(context.ExprContext, exprContext)(identity))
    val baseTraits2 = baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

    (level2, baseClass2, baseTraits2)
  end convertClassResult

  private def convertTraitResult(res: ArTrait#TraitResult): (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]]) =
    val (level, baseTraits) = res

    val level2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(level)
    val baseTraits2 = baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

    (level2, baseTraits2)
  end convertTraitResult
  
  private def convertFunctionResult(res: context.ExprContext.WrapExpr): WrapExpr =
    ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(res)



  private def loadKnownExport(tubeName: TubeName, modulePath: ModulePath, id: IdentifierExpr): Comp[WrapExpr] =
    context.getTube(tubeName).flatMap { argonCore =>
      argonCore.module(modulePath).flatMap { rootModule =>
        rootModule.exports(id).flatMap { entries =>
          ZIO.foreach(entries.map { _.element }) {
            case ModuleElementC.ClassElement(arClass) =>
              arClass.signature.map { _.parameterCount == 0 }.map {
                case true => Some(WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(arClass), Vector.empty)))
                case false => None
              }

            case ModuleElementC.TraitElement(arTrait) =>
              arTrait.signature.map { _.parameterCount == 0 }.map {
                case true => Some(WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(arTrait), Vector.empty)))
                case false => None
              }

            case ModuleElementC.FunctionElement(arFunc) => IO.none
          }
            .map { _.flatten }
            .flatMap {
              case Seq(element) => IO.succeed(element)
              case Seq() => IO.fail(DiagnosticError.LookupFailed())
              case _ => IO.fail(DiagnosticError.AmbiguousOverload())
            }
        }
      }
    }

  private val argonCoreTubeName = TubeName("Argon.Core")

  private def boolType: Comp[WrapExpr] = loadKnownExport(argonCoreTubeName, ModulePath(Seq()), IdentifierExpr.Named("Bool"))
  private def intType: Comp[WrapExpr] = loadKnownExport(argonCoreTubeName, ModulePath(Seq()), IdentifierExpr.Named("Int"))
  private def stringType: Comp[WrapExpr] = loadKnownExport(argonCoreTubeName, ModulePath(Seq()), IdentifierExpr.Named("String"))
  private def unitType: Comp[WrapExpr] = loadKnownExport(argonCoreTubeName, ModulePath(Seq()), IdentifierExpr.Named("Unit"))
  private def exceptionType: Comp[WrapExpr] = loadKnownExport(argonCoreTubeName, ModulePath(Seq()), IdentifierExpr.Named("Exception"))
  
  
  private def anyType: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.AnyType, EmptyTuple))
  private def neverType: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.NeverType, EmptyTuple))

}
