package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.module.ModuleElementC
import dev.argon.compiler.signature.Signature
import dev.argon.util.{WithSource, SourceLocation}
import dev.argon.parser
import dev.argon.parser.{IdentifierExpr, FunctionParameterListType}
import dev.argon.expr.{Evaluator, ImplicitResolver, ExprConstraints}
import zio.Random
import dev.argon.util.UniqueIdentifier
import zio.*
import dev.argon.util.{*, given}
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.module.ModulePath
import dev.argon.prover.Proof

sealed abstract class ExpressionConverter extends UsingContext with ExprUtil {

  val exprContext: SourceCompilerExprContext with HasContext[context.type]
  import exprContext.{
    WrapExpr,
    ArExpr,
    ExprConstructor,
    Variable,
    LocalVariable,
    InstanceVariable,
    ParameterVariable,
  }

  protected val evaluator: Evaluator[CompEnv, CompError] { val exprContext: ExpressionConverter.this.exprContext.type }
  protected val implicitResolver: ImplicitResolver[CompEnv, CompError] { val exprContext: ExpressionConverter.this.exprContext.type }
  protected val fuel: Int

  type ClassResultConv = (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])
  type TraitResultConv = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])


  trait Scope {
    def lookup(id: IdentifierExpr): LookupResult[ScopeElement]

    final def addVariable(variable: Variable): Scope = new Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
        if variable.name.contains(id) then
          LookupResult.Success(Seq(variable), LookupResult.NotFound())
        else
          Scope.this.lookup(id)
    }
  }

  object Scope {
    def fromImports(imports: Imports[context.type]): Scope = new Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
        imports.get(id) match {
          case None | Some(Seq()) => LookupResult.NotFound()
          case Some(elements) => LookupResult.Success(elements, LookupResult.NotFound())
        }
    }
  }

  enum LookupResult[TElement] {
    case Success(overloads: Seq[TElement], nextPriority: LookupResult[TElement])
    case Suspended(suspendedResult: Comp[LookupResult[TElement]])
    case NotFound()
  }

  type ScopeElement = ModuleElementC[context.type] | Variable



  final case class Env(
    scope: Scope,
    model: Map[exprContext.THole, ExprConstraints[WrapExpr]],
  ) {
    def withScope(f: Scope => Scope): Env =
      copy(scope = f(scope))

    def mergeBranches(first: Env, second: Env): Env =
      this
  }

  final case class ArgumentInfo(arg: Env => ExprFactory, location: SourceLocation, listType: FunctionParameterListType)
  final case class MutatorValue(arg: Env => ExprFactory, location: SourceLocation)


  trait SignatureHandlerPlus[Res1, Res2] extends SignatureHandler[Res2] {
    def convertResult(res: Res1): Res2
  }


  final case class ExprResult(expr: WrapExpr, env: Env)
  final case class ExprTypeResult(expr: WrapExpr, env: Env, exprType: WrapExpr)

  trait ExprFactory {
    def synth: Comp[ExprTypeResult]
    def check(t: WrapExpr): Comp[ExprResult]

    def mutate(value: MutatorValue): ExprFactory = CompExprFactory(IO.fail(DiagnosticError.CanNotMutate()))
    
    def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadExprFactoryMethod.fromInstanceFactory(this, IdentifierExpr.Named("apply"))
  }

  private trait ExprFactorySynth extends ExprFactory {
    override def check(t: WrapExpr): Comp[ExprResult] =
      for {
        res <- synth
        env <- checkSubType(res.env, res.exprType, t)
      } yield ExprResult(res.expr, env)
  }

  private trait ExprFactoryCheck extends ExprFactory {
    override def synth: Comp[ExprTypeResult] =
      IO.fail(DiagnosticError.UnknownTypeForExpression())
  }

  private final class ExprFactoryError(error: CompError) extends ExprFactory {
    override def synth: Comp[ExprTypeResult] = IO.fail(error)
    override def check(t: WrapExpr): Comp[ExprResult] = IO.fail(error)
  }

  // Ensures that a <: b
  private def checkSubType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Env] =
    isSubType(env, a, b).flatMap {
      case Some(env) => IO.succeed(env)
      case None => IO.fail(DiagnosticError.TypeError())
    }

  private def isSubType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] =
    val prop = WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (a, b)))
    implicitResolver.tryResolve(prop, env.model, fuel).map(_.map {
      case implicitResolver.ResolvedImplicit(_, model) =>
        env.copy(model = model)
    })
  end isSubType

  private def proofToExpr(proof: Proof[implicitResolver.TCAtomicProof]): Comp[WrapExpr] =
    proof match {
      case Proof.Atomic(implicitResolver.TCAtomicProof.ExprProof(expr)) => IO.succeed(expr)
      case _ => ???
    }
    


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
                  value <- convertExpr(env, varDecl.value).synth
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
          override def synth: Comp[ExprTypeResult] =
            for {
              exprRes <- headResult
              tailRes <- convertStmtList(exprRes.env, tailStmts).synth
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
          override def synth: Comp[ExprTypeResult] =
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
          override def synth: Comp[ExprTypeResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              uType <- unitType
              innerRes <- convertExpr(env, inner).synth
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
          override def synth: Comp[ExprTypeResult] =
            for {
              t <- boolType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantBool(b), EmptyTuple))
            } yield ExprTypeResult(e, env, t)
        }

      case parser.ClassConstructorExpr(classExpr) => ???
      case parser.DotExpr(left, id) =>
        OverloadExprFactoryMethod.fromInstanceFactory(convertExpr(env, left), id)

      case parser.ExternExpr(specifier) => ???

      case parser.FunctionCallExpr(func, listType, arg) =>
        convertExpr(env, func)
          .invoke(ArgumentInfo(convertExpr(_, arg), arg.location, listType))

      case id: IdentifierExpr =>
        OverloadExprFactory(env, env.scope.lookup(id), Seq.empty)

      case parser.IfElseExpr(condition, ifBody, elseBody) =>
        new ExprFactory {
          override def synth: Comp[ExprTypeResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(env, condition).check(bType)
              trueRes <- convertStmtList(condRes.env, ifBody).synth
              falseRes <- convertStmtList(condRes.env, elseBody).synth
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
          override def synth: Comp[ExprTypeResult] =
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
          override def synth: Comp[ExprTypeResult] =
            for {
              exType <- exceptionType
              ex <- convertExpr(env, exception).check(exType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.RaiseException, ex.expr))
            } yield ExprTypeResult(e, ex.env, neverType)
        }

      case parser.StringValueExpr(parser.Token.StringToken(parser.Token.StringToken.StringPart(str) :: Nil)) =>
        new ExprFactorySynth {
          override def synth: Comp[ExprTypeResult] =
            for {
              strType <- stringType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantString(str.value), EmptyTuple))
            } yield ExprTypeResult(e, env, strType)
        }

      case parser.StringValueExpr(_) => ???

      case parser.TupleExpr(values) =>
        new ExprFactory {
          override def synth: Comp[ExprTypeResult] =
            synthPart(env, values, Nil, Nil)

          private def synthPart(env: Env, values: NonEmptyList[WithSource[parser.Expr]], valuesAcc: List[WrapExpr], typeAcc: List[WrapExpr]): Comp[ExprTypeResult] =
            convertExpr(env, values.head).synth.flatMap { currentRes =>
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
          override def synth: Comp[ExprTypeResult] =
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
          override def synth: Comp[ExprTypeResult] =
            for {
              uType <- unitType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadUnit, EmptyTuple))
            } yield ExprTypeResult(e, env, uType)
        }
    }

  private abstract class OverloadExprFactoryBase extends ExprFactorySynth {

    override def synth: Comp[ExprTypeResult] = resolveOverload(lookup)


    protected type TElement

    protected val env: Env
    protected val args: Seq[ArgumentInfo]
    protected def lookup: LookupResult[TElement]

    protected def signatureOf(element: TElement): Comp[Signature[WrapExpr, ?]]

    // Checks if the current overload is applicable
    protected def checkOverload(overload: TElement): Comp[Option[Comp[ExprTypeResult]]]

    protected def resolveOverload(lookup: LookupResult[TElement]): Comp[ExprTypeResult] =
      lookup match {
        case LookupResult.Success(overloads, next) =>

          def checkEachRank(overloads: Seq[Seq[TElement]]): Comp[ExprTypeResult] =
            overloads match {
              case rank +: tail =>
                ZIO.foreach(rank)(checkOverload).map(_.flatten).flatMap {
                  case Seq(single) => single
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
    protected def rankOverloads(overloads: Seq[TElement]): Comp[Seq[Seq[TElement]]] =
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


      def isBetterThan(a: TElement, b: TElement): Comp[Boolean] =
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

      def rankOverload(ranks: List[List[TElement]], overload: TElement): Comp[List[List[TElement]]] =
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

    protected def createSigResult[Res]
    (
      owner: exprContext.ParameterVariableOwner,
      env: Env,
      sig: Signature[WrapExpr, Res],
      args: List[ArgumentInfo],
      convArgs: Vector[WrapExpr],
    )(
      sigHandler: SignatureHandler[Res]
    )(
      f: (Env, Vector[WrapExpr], Res) => ExprTypeResult
    ): Comp[Option[Comp[ExprTypeResult]]] =

      def handleArg(arg: ArgumentInfo, paramType: WrapExpr, tailArgs: List[ArgumentInfo], next: Signature[WrapExpr, Res]): Comp[Option[Comp[ExprTypeResult]]] =
        arg.arg(env).check(paramType)
          .foldZIO(
            failure = _ => IO.none,
            success = result => {
              val variable = ParameterVariable(owner, convArgs.size, paramType)
              substituteArg(variable)(result.expr)(sigHandler)(next).flatMap { case (next, resultExpr) =>
                createSigResult(owner, env, next, tailArgs, convArgs :+ resultExpr)(sigHandler)(f)
              }
            }
          )

      def inferArg(paramType: WrapExpr, next: Signature[WrapExpr, Res]): Comp[Option[Comp[ExprTypeResult]]] =
        UniqueIdentifier.make.flatMap { hole =>
          val variable = ParameterVariable(owner, convArgs.size, paramType)
          substituteArg(variable)(WrapExpr.OfHole(hole))(sigHandler)(next).flatMap { case (next, resultExpr) =>
            createSigResult(owner, env, next, args, convArgs :+ resultExpr)(sigHandler)(f)
          }
        }

      def resolveReqArg(paramType: WrapExpr, next: Signature[WrapExpr, Res]): Comp[Option[Comp[ExprTypeResult]]] =
        implicitResolver.tryResolve(paramType, env.model, fuel).flatMap {
          case Some(implicitResolver.ResolvedImplicit(proof, model)) =>
            proofToExpr(proof).flatMap { paramValue =>
              val variable = ParameterVariable(owner, convArgs.size, paramType)
              substituteArg(variable)(paramValue)(sigHandler)(next).flatMap { case (next, resultExpr) =>
                createSigResult(owner, env, next, args, convArgs :+ resultExpr)(sigHandler)(f)
              }
            }

          case None => IO.none
        }

      (args, sig) match {
        case ((arg @ ArgumentInfo(_, _, FunctionParameterListType.NormalList)) :: tailArgs, Signature.Parameter(FunctionParameterListType.NormalList, paramType, next)) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.NormalList, paramType, next)) =>
          ???

        case ((arg @ ArgumentInfo(_, _, FunctionParameterListType.InferrableList)) :: tailArgs, Signature.Parameter(FunctionParameterListType.InferrableList, paramType, next)) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.InferrableList, paramType, next)) =>
          inferArg(paramType, next)

        case ((arg @ ArgumentInfo(_, _, FunctionParameterListType.InferrableList2)) :: tailArgs, Signature.Parameter(FunctionParameterListType.InferrableList2, paramType, next)) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.InferrableList2, paramType, next)) =>
          inferArg(paramType, next)

        case ((arg @ ArgumentInfo(_, _, FunctionParameterListType.RequiresList)) :: tailArgs, Signature.Parameter(FunctionParameterListType.RequiresList, paramType, next)) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.RequiresList, paramType, next)) =>
          resolveReqArg(paramType, next)

        case (_, Signature.Result(res)) =>
          val overloadExpr = f(env, convArgs, res)
          IO.succeed(Some(
            args.foldLeft(ConstExprFactory(overloadExpr) : ExprFactory)(_.invoke(_))
              .synth
          ))
      }

    end createSigResult



    // Returns substituted signature and (possibly) modified replacement expression
    protected def substituteArg[Res](variable: Variable)(replacement: WrapExpr)(sigHandler: SignatureHandler[Res])(sig: Signature[WrapExpr, Res]): Comp[(Signature[WrapExpr, Res], WrapExpr)] =
      if referencesVariableSig(variable)(sigHandler)(sig) then
        asStableExpression(replacement).map { case (replacement2, replacementStable) =>
          val sig2 = substituteSignature(variable)(replacement)(sigHandler)(sig)
          (sig2, replacement)
        }
      else
        IO.succeed((sig, replacement))

  }

  private final class OverloadExprFactory(protected val env: Env, protected val lookup: LookupResult[ScopeElement], protected val args: Seq[ArgumentInfo]) extends OverloadExprFactoryBase {
    
    override def mutate(value: MutatorValue): ExprFactory =
      if args.isEmpty then
        CompExprFactory(mutateImpl(value, lookup))
      else
        super.mutate(value)
        
    override def invoke(arg: ArgumentInfo): ExprFactory = OverloadExprFactory(env, lookup, args :+ arg)


    protected override type TElement = ScopeElement

    protected override def signatureOf(element: ScopeElement): Comp[Signature[WrapExpr, ?]] =
      element match {
        case variable: Variable =>
          IO.succeed(Signature.Result(()))

        case ModuleElementC.ClassElement(arClass) =>
          arClass.signature.map(convertSig(ClassSigHandler))

        case ModuleElementC.TraitElement(arTrait) =>
          arTrait.signature.map(convertSig(TraitSigHandler))

        case ModuleElementC.FunctionElement(arFunc) =>
          arFunc.signature.map(convertSig(FunctionSigHandler))
      }

    protected override def checkOverload(overload: ScopeElement): Comp[Option[Comp[ExprTypeResult]]] =
      overload match {
        case variable: Variable =>
          IO.succeed(Some(IO.succeed(ExprTypeResult(
            WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple)),
            env,
            variable.varType
          ))))

        case ModuleElementC.ClassElement(arClass) =>
          createSigResultConv(arClass, env, arClass.signature, args)(ClassSigHandler) { case (env, args, (typeOfClassType, _, _)) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(arClass), args)),
              env,
              typeOfClassType
            )
          }

        case ModuleElementC.TraitElement(arTrait) =>
          createSigResultConv(arTrait, env, arTrait.signature, args)(TraitSigHandler) { case (env, args, (typeOfTraitType, _)) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(arTrait), args)),
              env,
              typeOfTraitType
            )
          }

        case ModuleElementC.FunctionElement(func) =>
          createSigResultConv(func, env, func.signature, args)(FunctionSigHandler) { (env, args, returnType) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionCall(func), args)),
              env,
              returnType
            )
          }

      }

    protected def createSigResultConv[Res1, Res2]
    (
      owner: exprContext.ParameterVariableOwner,
      env: Env,
      sig: Comp[Signature[context.ExprContext.WrapExpr, Res1]],
      args: Seq[ArgumentInfo],
    )(
      sigHandler: SignatureHandlerPlus[Res1, Res2]
    )(
      f: (Env, Vector[WrapExpr], Res2) => ExprTypeResult
    ): Comp[Option[Comp[ExprTypeResult]]] =
      sig.flatMap { sig =>
        createSigResult(owner, env, convertSig(sigHandler)(sig), args.toList, Vector.empty)(sigHandler)(f)
      }
      

    private def mutateImpl(value: MutatorValue, lookup: LookupResult[ScopeElement]): Comp[ExprFactory] =
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
      override def synth: Comp[ExprTypeResult] =
        for {
          valueExpr <- value.arg(env).check(variable.varType)
          unitT <- unitType
          e = WrapExpr.OfExpr(ArExpr(ExprConstructor.StoreVariable(variable), valueExpr.expr))
        } yield ExprTypeResult(e, valueExpr.env, unitT)
    }

  }

  private final class OverloadExprFactoryMethod(protected val env: Env, instance: WrapExpr, instanceType: WrapExpr, memberName: IdentifierExpr, protected val args: Seq[ArgumentInfo]) extends OverloadExprFactoryBase {
    override def mutate(value: MutatorValue): ExprFactory =
      EndOverloadExprFactory(
        OverloadExprFactoryMethod(env, instance, instanceType, IdentifierExpr.Update(memberName), args :+ ArgumentInfo(value.arg, value.location, FunctionParameterListType.NormalList))
      )

    override def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadExprFactoryMethod(env, instance, instanceType, memberName, args :+ arg)


    protected override type TElement = ArMethod

    private enum InstanceType {
      case ByClass(c: ArExpr[ExprConstructor.ClassType])
      case ByTrait(t: ArExpr[ExprConstructor.TraitType])
      case ByClassStatic(c: ArExpr[ExprConstructor.ClassType])
      case ByTraitStatic(t: ArExpr[ExprConstructor.TraitType])
    }

    protected def lookup: LookupResult[ArMethod] =
      LookupResult.Suspended(
        evaluator.normalizeTopLevelWrap(instance, fuel)
          .flatMap {
            case WrapExpr.OfExpr(normalizedInstance) =>
              normalizedInstance.constructor match {
                case ctor: (normalizedInstance.constructor.type & ExprConstructor.ClassType) =>
                  lookupMethods(Seq(InstanceType.ByClassStatic(ArExpr(ctor, normalizedInstance.getArgs(ctor)))), Set.empty, Set.empty)

                case ctor: (normalizedInstance.constructor.type & ExprConstructor.TraitType) =>
                  lookupMethods(Seq(InstanceType.ByTraitStatic(ArExpr(ctor, normalizedInstance.getArgs(ctor)))), Set.empty, Set.empty)
                  
                case _ =>
                  getInstanceType(instanceType).flatMap { types =>
                    lookupMethods(types, Set.empty, Set.empty)
                  }
              }

            case WrapExpr.OfHole(_) => IO.succeed(LookupResult.NotFound())
          }
      )

    protected override def signatureOf(element: ArMethod): Comp[Signature[WrapExpr, ?]] =
      element.signatureUnsubstituted.map(convertSig(FunctionSigHandler))

    protected override def checkOverload(overload: ArMethod): Comp[Option[Comp[ExprTypeResult]]] =
      overload.signatureUnsubstituted.flatMap { sig =>
        val thisVar = InstanceVariable(overload, instanceType, None)
        val sig2 = convertSig(FunctionSigHandler)(sig)
        
        substituteArg(thisVar)(instance)(FunctionSigHandler)(sig2).flatMap { case (sig3, stableInstance) =>
          createSigResult(overload, env, sig3, args.toList, Vector.empty)(FunctionSigHandler) { (env, args, returnType) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.MethodCall(overload), (stableInstance, args))),
              env,
              returnType
            )
          }
        }
      }

    

    private def getInstanceType(instanceType: WrapExpr): Comp[Seq[InstanceType]] =
      evaluator.normalizeTopLevelWrap(instanceType, fuel).flatMap {
        case WrapExpr.OfExpr(normalizedType) =>
          normalizedType.constructor match {
            case ctor: (normalizedType.constructor.type & ExprConstructor.IntersectionType.type) =>
              val (a, b) = normalizedType.getArgs(ctor)
              for {
                a2 <- getInstanceType(a)
                b2 <- getInstanceType(b)
              } yield a2 ++ b2

            case ctor: (normalizedType.constructor.type & ExprConstructor.ClassType) =>
              IO.succeed(Seq(InstanceType.ByClass(ArExpr(ctor, normalizedType.getArgs(ctor)))))

            case ctor: (normalizedType.constructor.type & ExprConstructor.TraitType) =>
              IO.succeed(Seq(InstanceType.ByTrait(ArExpr(ctor, normalizedType.getArgs(ctor)))))

            case _ => IO.succeed(Seq.empty)
          }

        case _ => IO.succeed(Seq.empty)
      }

    private def methodsOfType(t: InstanceType): Comp[Seq[ArMethod]] =
      t match {
        case InstanceType.ByClass(classType) => classType.constructor.arClass.methods.map(_.getOrElse(Some(memberName), Seq.empty))
        case InstanceType.ByClassStatic(classType) => classType.constructor.arClass.staticMethods.map(_.getOrElse(Some(memberName), Seq.empty))
        case InstanceType.ByTrait(traitType) => traitType.constructor.arTrait.methods.map(_.getOrElse(Some(memberName), Seq.empty))
        case InstanceType.ByTraitStatic(traitType) => traitType.constructor.arTrait.staticMethods.map(_.getOrElse(Some(memberName), Seq.empty))
      }

    private def resolveTypeSignatureResult[Res](owner: exprContext.ParameterVariableOwner)(sigHandler: SignatureHandler[Res])(paramIndex: Int, args: List[WrapExpr])(sig: Signature[WrapExpr, Res]): Comp[Res] =
      (args, sig) match {
        case (arg :: tailArgs, Signature.Parameter(_, paramType, next)) =>
          val variable = ParameterVariable(owner, paramIndex, paramType)
          substituteArg(variable)(arg)(sigHandler)(next).flatMap { case (next, _) =>
            resolveTypeSignatureResult(owner)(sigHandler)(paramIndex + 1, tailArgs)(next)
          }

        case (Nil, Signature.Parameter(_, _, _)) => ???

        case (Nil, Signature.Result(res)) => IO.succeed(res)

        case (_ :: _, Signature.Result(_)) => ???
      }

    private def getBaseTypes(t: InstanceType): Comp[Seq[InstanceType]] =
      t match {
        case InstanceType.ByClass(classType) =>
          classType.constructor.arClass.signature
            .map(convertSig(ClassSigHandler))
            .flatMap(resolveTypeSignatureResult(classType.constructor.arClass)(ClassSigHandler)(0, classType.args.toList))
            .map { case (_, baseClass, baseTraits) =>
              baseClass.map(InstanceType.ByClass.apply).toSeq ++ baseTraits.map(InstanceType.ByTrait.apply)
            }
        
        case InstanceType.ByTrait(traitType) =>
          traitType.constructor.arTrait.signature
            .map(convertSig(TraitSigHandler))
            .flatMap(resolveTypeSignatureResult(traitType.constructor.arTrait)(TraitSigHandler)(0, traitType.args.toList))
            .map { case (_, baseTraits) =>
              baseTraits.map(InstanceType.ByTrait.apply)
            }
        
        case _ => IO.succeed(Seq.empty)
      }

    private def toSeenTypes(t: InstanceType): Set[ArClass | ArTrait] =
      t match {
        case InstanceType.ByClass(e) => Set(e.constructor.arClass)
        case InstanceType.ByTrait(e) => Set(e.constructor.arTrait)
        case _ => Set.empty
      }

    private def isSeenType(seenTypes: Set[ArClass | ArTrait])(t: InstanceType): Boolean =
      val asSeen = toSeenTypes(t)
      asSeen.nonEmpty && asSeen.subsetOf(seenTypes)
    end isSeenType

    private def lookupMethods(types: Seq[InstanceType], seenTypes: Set[ArClass | ArTrait], seenMethods: Set[ArMethod]): Comp[LookupResult[ArMethod]] =
      ZIO.foreach(types)(methodsOfType).map { methods =>
        val methods2 = methods.flatten.filterNot(seenMethods.contains).distinct
        
        val next = LookupResult.Suspended(
          ZIO.foreach(types)(getBaseTypes).flatMap { baseTypes =>
            val seenTypes2 = seenTypes ++ types.flatMap(toSeenTypes)
            val baseTypes2 = baseTypes.flatten.filter(isSeenType(seenTypes2))
            val seenMethods2 = seenMethods ++ methods2
            lookupMethods(baseTypes2, seenTypes2, seenMethods2)
          }
        )

        LookupResult.Success(methods2, next)
      }
      
  }

  object OverloadExprFactoryMethod {
    def fromInstanceFactory(instance: ExprFactory, memberName: IdentifierExpr): ExprFactory =
        CompExprFactory(
          for {
            instanceExpr <- instance.synth
          } yield OverloadExprFactoryMethod(instanceExpr.env, instanceExpr.expr, instanceExpr.exprType, memberName, Seq.empty)
        )
  }

  private final class EndOverloadExprFactory(inner: ExprFactory) extends ExprFactory {
    override def synth: Comp[ExprTypeResult] = inner.synth
    override def check(t: WrapExpr): Comp[ExprResult] = inner.check(t)
  }

  private final class CompExprFactory(inner: Comp[ExprFactory]) extends ExprFactory {
    override def synth: Comp[ExprTypeResult] = inner.flatMap(_.synth)
    override def check(t: WrapExpr): Comp[ExprResult] = inner.flatMap(_.check(t))
    
    override def mutate(value: MutatorValue): ExprFactory = CompExprFactory(inner.map(_.mutate(value)))
    override def invoke(arg: ArgumentInfo): ExprFactory = CompExprFactory(inner.map(_.invoke(arg)))
  }

  private final class ConstExprFactory(result: ExprTypeResult) extends ExprFactorySynth {
    def synth: Comp[ExprTypeResult] = IO.succeed(result)
  }

  private def convertSig[Res1, Res2](sigHandler: SignatureHandlerPlus[Res1, Res2])(sig: Signature[context.ExprContext.WrapExpr, Res1]): Signature[WrapExpr, Res2] =
    sig match {
      case Signature.Parameter(paramListType, paramType, next) =>
        Signature.Parameter(
          paramListType,
          ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(paramType),
          convertSig(sigHandler)(next)
        )

      case Signature.Result(res) =>
        Signature.Result(sigHandler.convertResult(res))
    }

  private object ClassSigHandler extends SignatureHandlerPlus[ArClass#ClassResult, ClassResultConv]:

    override def convertResult(res: ArClass#ClassResult): ClassResultConv =
      val (classType, baseClass, baseTraits) = res
      
      val classType2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(classType)
      val baseClass2 = baseClass.map(ArgonExprContext.convertClassType[Id](context)(context.ExprContext, exprContext)(identity))
      val baseTraits2 = baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

      (classType2, baseClass2, baseTraits2)
    end convertResult


    override def substituteResult(variable: Variable)(replacement: WrapExpr)(res: ClassResultConv): ClassResultConv =
      val (classType, baseClass, baseTraits) = res
      
      val classType2 = substituteWrapExpr(variable)(replacement)(classType)
      val baseClass2 = baseClass.map(substituteClassType(variable)(replacement))
      val baseTraits2 = baseTraits.map(substituteTraitType(variable)(replacement))

      (classType2, baseClass2, baseTraits2)
    end substituteResult

    override def resultReferences(variable: Variable)(res: ClassResultConv): Boolean =
      val (classType, baseClass, baseTraits) = res

      referencesVariable(variable)(classType) ||
        baseClass.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) } ||
        baseTraits.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) }
    end resultReferences
  end ClassSigHandler

  private object TraitSigHandler extends SignatureHandlerPlus[ArTrait#TraitResult, TraitResultConv]:
    override def convertResult(res: ArTrait#TraitResult): TraitResultConv =
      val (traitType, baseTraits) = res

      val traitType2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(traitType)
      val baseTraits2 = baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

      (traitType2, baseTraits2)
    end convertResult

    override def substituteResult(variable: Variable)(replacement: WrapExpr)(res: TraitResultConv): TraitResultConv =
      val (traitType, baseTraits) = res

      val traitType2 = substituteWrapExpr(variable)(replacement)(traitType)
      val baseTraits2 = baseTraits.map(substituteTraitType(variable)(replacement))

      (traitType2, baseTraits2)
    end substituteResult

    override def resultReferences(variable: Variable)(res: TraitResultConv): Boolean =
      val (traitType, baseTraits) = res

      referencesVariable(variable)(traitType) ||
        baseTraits.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) }
    end resultReferences
  end TraitSigHandler

  private object FunctionSigHandler extends SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr]:
    override def convertResult(res: context.ExprContext.WrapExpr): WrapExpr =
      ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(res)

    override def substituteResult(variable: Variable)(replacement: WrapExpr)(res: WrapExpr): WrapExpr =
      substituteWrapExpr(variable)(replacement)(res)

    override def resultReferences(variable: Variable)(res: WrapExpr): Boolean =
      referencesVariable(variable)(res)
  end FunctionSigHandler
  


  private def loadKnownExport(tubeName: TubeName, modulePath: ModulePath, id: IdentifierExpr): Comp[WrapExpr] =
    context.getTube(tubeName).flatMap { argonCore =>
      argonCore.module(modulePath).flatMap { rootModule =>
        rootModule.exports(id).flatMap { elements =>
          ZIO.foreach(elements) {
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
