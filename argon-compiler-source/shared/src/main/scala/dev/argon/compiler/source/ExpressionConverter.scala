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
import zio.stream.*
import dev.argon.util.{*, given}
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.module.ModulePath
import dev.argon.prover.Proof
import scala.reflect.TypeTest

sealed abstract class ExpressionConverter extends UsingContext with ExprUtil {

  val exprContext: SourceCompilerExprContext with HasContext[context.type] =
    new SourceCompilerExprContext {
      override val context: ExpressionConverter.this.context.type = ExpressionConverter.this.context
    }

  import exprContext.{WrapExpr, ArExpr, ExprConstructor, Variable, LocalVariable, InstanceVariable, ParameterVariable}

  val fuel: Int
  val evaluator: ArgonEvaluator.Aux[context.type, exprContext.type] = ArgonEvaluator(context)(exprContext)

  val implicitResolver
    : ImplicitResolver[CompEnv, CompError] {
      val exprContext: ExpressionConverter.this.exprContext.type
    } =
    new ImplicitResolver[CompEnv, CompError] {
      override val exprContext: ExpressionConverter.this.exprContext.type = ExpressionConverter.this.exprContext

      override def createHole: Comp[UniqueIdentifier] = UniqueIdentifier.make

      private def getResult[Res](sigHandler: SignatureHandler[Res])
        (owner: exprContext.ParameterVariableOwner, index: Int, sig: Signature[WrapExpr, Res], args: Seq[WrapExpr])
        : Comp[Res] =
        (sig, args) match {
          case (Signature.Parameter(_, paramType, nextSig), arg +: tailArgs) =>
            val variable = ParameterVariable(owner, index, paramType)
            val nextSigSubst = substituteSignature(variable)(arg)(sigHandler)(nextSig)
            getResult(sigHandler)(owner, index, nextSigSubst, tailArgs)

          case (Signature.Result(res), Seq()) => IO.succeed(res)
          case _ => ???
        }

      protected override def isSubClass
        (classA: ArClass, aArgs: Seq[WrapExpr], classB: ArClass, bArgs: Seq[WrapExpr], fuel: Int)
        : Comp[SubClassResult] =
        if classA.id == classB.id then
          ZIO.forall(aArgs.zip(bArgs)) { case (aArg, bArg) =>
            tryResolve(WrapExpr.OfExpr(ArExpr(ExprConstructor.EqualTo, (aArg, bArg))), Map.empty, fuel).map {
              _.isDefined
            }
          }
            .map {
              case true =>
                SubClassResult.SubClassProof(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
              case false => SubClassResult.Unknown
            }
        else
          classA.signature
            .map(convertSig(ClassSigHandler))
            .flatMap { sig =>
              getResult(ClassSigHandler)(classA, 0, sig, aArgs)
            }
            .flatMap {
              case (_, Some(baseClass), _) =>
                isSubClass(baseClass.constructor.arClass, baseClass.args, classB, bArgs, fuel)
              case (_, None, _) => IO.succeed(SubClassResult.NotSubClassProof(WrapExpr.OfExpr(ArExpr(
                  ExprConstructor.AssumeErasedValue,
                  EmptyTuple,
                ))))
            }

      private def getSubClassResultSink[E]: Sink[E, SubClassResult, SubClassResult, Option[SubClassResult]] =
        ZSink.fold(Option.empty[SubClassResult]) {
          case Some(SubClassResult.SubClassProof(_)) => false
          case _ => true
        } {
          case (state, SubClassResult.NotSubClassProof(_)) =>
            state

          case (_, result) => Some(result)
        }

      private def getOrNotSubClass(result: Option[SubClassResult]): SubClassResult =
        result match {
          case Some(result) => result
          case None =>
            SubClassResult.NotSubClassProof(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
        }

      protected override def isSubTrait
        (traitA: ArTrait, aArgs: Seq[WrapExpr], traitB: ArTrait, bArgs: Seq[WrapExpr], fuel: Int)
        : Comp[SubClassResult] =
        if traitA.id == traitB.id then
          ZIO.forall(aArgs.zip(bArgs)) { case (aArg, bArg) =>
            tryResolve(WrapExpr.OfExpr(ArExpr(ExprConstructor.EqualTo, (aArg, bArg))), Map.empty, fuel).map {
              _.isDefined
            }
          }
            .map {
              case true =>
                SubClassResult.SubClassProof(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
              case false => SubClassResult.Unknown
            }
        else
          traitA.signature
            .map(convertSig(TraitSigHandler))
            .flatMap { sig =>
              getResult(TraitSigHandler)(traitA, 0, sig, aArgs)
            }
            .flatMap { case (_, baseTraits) =>
              ZStream.fromIterable(baseTraits)
                .mapZIO { baseTrait =>
                  isSubTrait(baseTrait.constructor.arTrait, baseTrait.args, traitB, bArgs, fuel)
                }
                .run(getSubClassResultSink)
                .map(getOrNotSubClass)
            }

      protected override def classImplementsTrait
        (classA: ArClass, aArgs: Seq[WrapExpr], traitB: ArTrait, bArgs: Seq[WrapExpr], fuel: Int)
        : Comp[SubClassResult] =
        classA.signature
          .map(convertSig(ClassSigHandler))
          .flatMap { sig =>
            getResult(ClassSigHandler)(classA, 0, sig, aArgs)
          }
          .flatMap { case (_, baseClass, baseTraits) =>
            val baseTraitResults =
              ZStream.fromIterable(baseTraits)
                .mapZIO { baseTrait =>
                  isSubTrait(baseTrait.constructor.arTrait, baseTrait.args, traitB, bArgs, fuel)
                }

            val baseClassResult =
              ZStream.fromIterable(baseClass.toList)
                .mapZIO { baseClass =>
                  classImplementsTrait(baseClass.constructor.arClass, baseClass.args, traitB, bArgs, fuel)
                }

            (baseTraitResults ++ baseClassResult)
              .run(getSubClassResultSink)
              .map(getOrNotSubClass)
          }

      protected override def traitRelations(arTrait: ArTrait): Comp[Seq[ExprRelation]] =
        arTrait.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def classRelations(arClass: ArClass): Comp[Seq[ExprRelation]] =
        arClass.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def functionRelations(function: ArFunc): Comp[Seq[ExprRelation]] =
        function.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def methodRelations(method: ArMethod): Comp[Seq[ExprRelation]] =
        method.signatureUnsubstituted.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def classConstructorRelations(classCtor: ClassConstructor): Comp[Seq[ExprRelation]] =
        classCtor.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def boolType: Comp[WrapExpr] = ExpressionConverter.this.boolType

      protected override def natLessThanFunction: Comp[ArFunc] =
        natType
          .flatMap { nat =>
            loadKnownExport[ModuleElementC.FunctionElement[context.type]](
              argonCoreTubeName,
              ModulePath(Seq()),
              IdentifierExpr.OperatorIdentifier(parser.BinaryOperator.LessThan),
              Seq(nat, nat),
            )
          }
          .map { case ModuleElementC.FunctionElement(func) => func }

      protected override def invalidExpr: Comp[Nothing] = ???
      protected override def invalidPredicateExpr: Comp[Nothing] = ???

      protected override val evaluator
        : Evaluator[CompEnv, CompError] { val exprContext: ExpressionConverter.this.exprContext.type } =
        new Evaluator[CompEnv, CompError] {
          override val exprContext: ExpressionConverter.this.exprContext.type = ExpressionConverter.this.exprContext

          override def getFunctionBody(function: ArFunc, args: Vector[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] =
            ???

          override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Vector[WrapExpr], fuel: Int)
            : Comp[Option[WrapExpr]] = ???

        }

    }

  type ClassResultConv = (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])
  type TraitResultConv = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])

  trait Scope {
    def lookup(id: IdentifierExpr): LookupResult[ScopeElement]

    final def addVariable(variable: Variable): Scope =
      new Scope {

        override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
          if variable.name.contains(id) then
            LookupResult.Success(Seq(variable), LookupResult.NotFound())
          else
            Scope.this.lookup(id)

      }

  }

  object Scope {

    def fromImports(imports: Imports[context.type]): Scope =
      new Scope {

        override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
          imports.get(id) match {
            case None | Some(Seq()) => LookupResult.NotFound()
            case Some(elements) => LookupResult.Success(elements, LookupResult.NotFound())
          }

      }

  }

  enum LookupResult[+TElement] {
    case Success(overloads: Seq[TElement], nextPriority: LookupResult[TElement])
    case Suspended(suspendedResult: Comp[LookupResult[TElement]])
    case NotFound()
  }

  type ScopeElement = ModuleElementC[context.type] | Variable

  final case class Env
    (
      scope: Scope,
      model: Map[exprContext.THole, ExprConstraints[WrapExpr]],
    ) {
    def withScope(f: Scope => Scope): Env = copy(scope = f(scope))

    def mergeBranches(first: Env, second: Env): Env = this
  }

  final case class ArgumentInfo(arg: ExprFactory, location: SourceLocation, listType: FunctionParameterListType)
  final case class MutatorValue(arg: ExprFactory, location: SourceLocation)

  trait SignatureHandlerPlus[Res1, Res2] extends SignatureHandler[Res2] {
    def convertResult(res: Res1): Res2
    def resolveResultHoles(env: Env, res: Res2): Comp[(Res1, Env)]
  }

  final case class ExprResult(expr: WrapExpr, env: Env)
  final case class ExprTypeResult(expr: WrapExpr, env: Env, exprType: WrapExpr)

  trait ExprFactory {
    def synth(env: Env): Comp[ExprTypeResult]
    def check(env: Env, t: WrapExpr): Comp[ExprResult]

    def mutate(value: MutatorValue): ExprFactory = ExprFactoryError(DiagnosticError.CanNotMutate())

    def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadExprFactoryMethod.fromInstanceFactory(this, IdentifierExpr.Named("apply"))

  }

  private trait ExprFactorySynth extends ExprFactory {

    override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
      for {
        res <- synth(env)
        env <- checkSubType(res.env, res.exprType, t)
      } yield ExprResult(res.expr, env)

  }

  private trait ExprFactoryCheck extends ExprFactory {
    override def synth(env: Env): Comp[ExprTypeResult] = IO.fail(DiagnosticError.UnknownTypeForExpression())
  }

  private final class ExprFactoryError(error: CompError) extends ExprFactory {
    override def synth(env: Env): Comp[ExprTypeResult] = IO.fail(error)
    override def check(env: Env, t: WrapExpr): Comp[ExprResult] = IO.fail(error)
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

  private def isSameType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] =
    isSubType(env, a, b).flatMap {
      case Some(env) => isSubType(env, b, a)
      case None => IO.none
    }

  private def proofToExpr(proof: Proof[implicitResolver.TCAtomicProof]): Comp[WrapExpr] =
    proof match {
      case Proof.Atomic(implicitResolver.TCAtomicProof.ExprProof(expr)) => IO.succeed(expr)
      case _ => ???
    }

  def convertStmtList(stmts: WithSource[Vector[WithSource[parser.Stmt]]]): ExprFactory =
    stmts.value match {
      case Vector(WithSource(expr: parser.Expr, location)) =>
        EndOverloadExprFactory(convertExpr(WithSource(expr, location)))

      case head +: tail =>
        val tailStmts: WithSource[Vector[WithSource[parser.Stmt]]] =
          WithSource(
            tail,
            SourceLocation(
              tail.headOption
                .map { case WithSource(_, nextLoc) => nextLoc.start }
                .getOrElse { head.location.end },
              stmts.location.end,
            ),
          )

        def headResult(env: Env): Comp[ExprResult] =
          head.value match {
            case expr: parser.Expr =>
              unitType.flatMap { unitT =>
                convertExpr(WithSource(expr, head.location)).check(env, unitT)
              }

            case varDecl: parser.VariableDeclarationStmt =>
              val localVarComp: Comp[(LocalVariable, WrapExpr, Env)] =
                varDecl.varType match {
                  case Some(varType) =>
                    for {
                      id <- UniqueIdentifier.make
                      tRes <- convertExpr(varType).check(env, anyType)
                      value <- convertExpr(varDecl.value).check(tRes.env, tRes.expr)
                      localVar = LocalVariable(id, tRes.expr, varDecl.name, varDecl.isMutable)
                    } yield (localVar, value.expr, value.env)

                  case None =>
                    for {
                      id <- UniqueIdentifier.make
                      value <- convertExpr(varDecl.value).synth(env)
                      localVar = LocalVariable(id, value.exprType, varDecl.name, varDecl.isMutable)
                    } yield (localVar, value.expr, value.env)
                }

              localVarComp.map { case (variable, value, env) =>
                val env2 = env.withScope(_.addVariable(variable))
                ExprResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.BindVariable(variable), value)),
                  env2,
                )
              }

            case _ => IO.fail(DiagnosticError.InvalidStatementInFunction())
          }

        new ExprFactory {
          override def synth(env: Env): Comp[ExprTypeResult] =
            for {
              exprRes <- headResult(env)
              tailRes <- convertStmtList(tailStmts).synth(exprRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(exprRes.expr, tailRes.expr)))
            } yield ExprTypeResult(e, tailRes.env, tailRes.exprType)

          override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
            for {
              exprRes <- headResult(env)
              tailRes <- convertStmtList(tailStmts).check(exprRes.env, t)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(exprRes.expr, tailRes.expr)))
            } yield ExprResult(e, tailRes.env)

          private def prependExpr(headExpr: WrapExpr, tailExpr: WrapExpr): WrapExpr =
            tailExpr match {
              case WrapExpr.OfExpr(e) =>
                (e.constructor: e.constructor.type & ExprConstructor) match {
                  case ctor: (e.constructor.type & ExprConstructor.Sequence.type) =>
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList.cons(headExpr, e.getArgs(ctor))))

                  case _ =>
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(headExpr, tailExpr)))
                }

              case _ =>
                WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(headExpr, tailExpr)))
            }

        }

      case _ => convertExpr(WithSource(parser.TupleExpr(Vector.empty), stmts.location))
    }

  def convertExpr(expr: WithSource[parser.Expr]): ExprFactory =
    expr.value match {
      case parser.AsExpr(value, valueType) =>
        new ExprFactorySynth {

          override def synth(env: Env): Comp[ExprTypeResult] =
            convertExpr(valueType).check(env, anyType)
              .flatMap { case ExprResult(t, env) =>
                convertExpr(value).check(env, t)
                  .map { case ExprResult(e, env) =>
                    ExprTypeResult(e, env, t)
                  }
              }

        }

      case parser.BinaryOperatorExpr(op, left, right) =>
        EndOverloadExprFactory(
          OverloadExprFactory(_.scope.lookup(IdentifierExpr.OperatorIdentifier(op.value)))(Seq(
            ArgumentInfo(convertExpr(left), left.location, FunctionParameterListType.NormalList),
            ArgumentInfo(convertExpr(right), right.location, FunctionParameterListType.NormalList),
          ))
        )

      case parser.BlockExpr(body, rescueCases, elseBody, Some(ensureBody)) =>
        new ExprFactory {

          override def synth(env: Env): Comp[ExprTypeResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              unitT <- unitType
              innerRes <- convertExpr(inner).synth(env)
              ensuring <- convertStmtList(ensureBody).check(innerRes.env, unitT)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.EnsureExecuted, (innerRes.expr, ensuring.expr)))
            } yield ExprTypeResult(e, ensuring.env, innerRes.exprType)
          end synth

          override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              unitT <- unitType
              innerRes <- convertExpr(inner).check(env, t)
              ensuring <- convertStmtList(ensureBody).check(innerRes.env, unitT)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.EnsureExecuted, (innerRes.expr, ensuring.expr)))
            } yield ExprResult(e, ensuring.env)
          end check

        }

      case parser.BlockExpr(body, Vector(), None, None) =>
        convertStmtList(body)

      case block: parser.BlockExpr => ???

      case parser.BoolValueExpr(b) =>
        new ExprFactorySynth {

          override def synth(env: Env): Comp[ExprTypeResult] =
            for {
              t <- boolType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantBool(b), EmptyTuple))
            } yield ExprTypeResult(e, env, t)

        }

      case parser.ClassConstructorExpr(classExpr) => ???
      case parser.DotExpr(left, id) =>
        OverloadExprFactoryMethod.fromInstanceFactory(convertExpr(left), id)

      case parser.ExternExpr(specifier) => ???

      case parser.FunctionCallExpr(func, listType, arg) =>
        convertExpr(func)
          .invoke(ArgumentInfo(convertExpr(arg), arg.location, listType))

      case id: IdentifierExpr =>
        OverloadExprFactory(_.scope.lookup(id))(Seq.empty)

      case parser.IfElseExpr(condition, ifBody, elseBody) =>
        new ExprFactory {

          override def synth(env: Env): Comp[ExprTypeResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(condition).check(env, bType)
              trueRes <- convertStmtList(ifBody).synth(condRes.env)
              falseRes <- convertStmtList(elseBody).synth(condRes.env)
              resEnv = condRes.env.mergeBranches(trueRes.env, falseRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.IfElse, (condRes.expr, trueRes.expr, falseRes.expr)))
              t = WrapExpr.OfExpr(ArExpr(ExprConstructor.UnionType, (trueRes.exprType, falseRes.exprType)))
            } yield ExprTypeResult(e, resEnv, t)

          override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(condition).check(env, bType)
              trueRes <- convertStmtList(ifBody).check(condRes.env, t)
              falseRes <- convertStmtList(elseBody).check(condRes.env, t)
              resEnv = condRes.env.mergeBranches(trueRes.env, falseRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.IfElse, (condRes.expr, trueRes.expr, falseRes.expr)))
            } yield ExprResult(e, resEnv)

        }

      case i: parser.IntValueExpr =>
        new ExprFactorySynth {

          override def synth(env: Env): Comp[ExprTypeResult] =
            for {
              t <- intType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(i.value), EmptyTuple))
            } yield ExprTypeResult(e, env, t)

        }

      case parser.LambdaTypeExpr(argType, resultType) => ???

      case parser.LambdaExpr(varName, body) =>
        new ExprFactoryCheck {

          override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
            evaluator.normalizeTopLevelWrap(t, fuel).flatMap {
              case WrapExpr.OfExpr(normalizedType) =>
                normalizedType.constructor match {
                  case funcTypeCtor: (normalizedType.constructor.type & ExprConstructor.FunctionType.type) =>
                    val (argType, resType) = normalizedType.getArgs(funcTypeCtor)
                    for {
                      id <- UniqueIdentifier.make
                      paramVar = LocalVariable(id, argType, varName, isMutable = false)
                      bodyRes <- convertExpr(body).check(env, resType)
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

          override def synth(env: Env): Comp[ExprTypeResult] =
            for {
              exType <- exceptionType
              ex <- convertExpr(exception).check(env, exType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.RaiseException, ex.expr))
            } yield ExprTypeResult(e, ex.env, neverType)

        }

      case parser.StringValueExpr(parser.Token.StringToken(parser.Token.StringToken.StringPart(str) :: Nil)) =>
        new ExprFactorySynth {

          override def synth(env: Env): Comp[ExprTypeResult] =
            for {
              strType <- stringType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantString(str.value), EmptyTuple))
            } yield ExprTypeResult(e, env, strType)

        }

      case parser.StringValueExpr(_) => ???

      case parser.TupleExpr(values) =>
        new ExprFactory {
          override def synth(env: Env): Comp[ExprTypeResult] = synthPart(env, values.toList, Vector.empty, Vector.empty)

          private def synthPart
            (env: Env, values: List[WithSource[parser.Expr]], valuesAcc: Vector[WrapExpr], typeAcc: Vector[WrapExpr])
            : Comp[ExprTypeResult] =
            values match {
              case head :: tail =>
                convertExpr(head).synth(env).flatMap { currentRes =>
                  synthPart(
                    currentRes.env,
                    tail,
                    valuesAcc :+ currentRes.expr,
                    typeAcc :+ currentRes.exprType,
                  )
                }

              case Nil =>
                val valuesExpr =
                  WrapExpr.OfExpr(ArExpr(
                    ExprConstructor.LoadTuple,
                    valuesAcc,
                  ))
                val typesExpr =
                  WrapExpr.OfExpr(ArExpr(
                    ExprConstructor.LoadTuple,
                    typeAcc,
                  ))
                IO.succeed(ExprTypeResult(valuesExpr, env, typesExpr))
            }

          override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
            evaluator.normalizeTopLevelWrap(t, fuel).flatMap {
              case WrapExpr.OfExpr(normalizedType) =>
                normalizedType.constructor match {
                  case tupleTypeCtor: (normalizedType.constructor.type & ExprConstructor.LoadTuple.type) =>
                    val tupleType: Vector[WrapExpr] = normalizedType.getArgs(tupleTypeCtor)
                    checkPart(env, values.toList, Vector.empty, tupleType.toList)

                  case _ => IO.fail(DiagnosticError.InvalidTypeForFunction())
                }

              case _ => IO.fail(DiagnosticError.InvalidTypeForFunction())
            }

          private def checkPart
            (
              env: Env,
              values: List[WithSource[parser.Expr]],
              valuesAcc: Vector[WrapExpr],
              expectedTypes: List[WrapExpr],
            )
            : Comp[ExprResult] =
            expectedTypes match {
              case currentExpected :: tailExpectedTypes =>
                values match {
                  case head :: tail =>
                    convertExpr(head).check(env, currentExpected).flatMap { currentRes =>
                      checkPart(
                        currentRes.env,
                        tail,
                        valuesAcc :+ currentRes.expr,
                        tailExpectedTypes,
                      )
                    }

                  case Nil if tailExpectedTypes.nonEmpty =>
                    IO.fail(DiagnosticError.TupleSizeMismatch())

                  case Nil =>
                    val valuesExpr =
                      WrapExpr.OfExpr(ArExpr(
                        ExprConstructor.LoadTuple,
                        valuesAcc,
                      ))
                    IO.succeed(ExprResult(valuesExpr, env))
                }

              case Nil =>
                IO.fail(DiagnosticError.TupleSizeMismatch())
            }

        }

      case parser.TypeExpr(level) => ???

      case parser.MetaTypeExpr(level) =>
        new ExprFactorySynth {

          override def synth(env: Env): Comp[ExprTypeResult] =
            val e = WrapExpr.OfExpr(ArExpr(ExprConstructor.OmegaTypeN(level), EmptyTuple))
            val t = WrapExpr.OfExpr(ArExpr(ExprConstructor.OmegaTypeN(level + 1), EmptyTuple))
            IO.succeed(ExprTypeResult(e, env, t))
          end synth

        }

      case parser.TypeOfExpr(ofExpr) => ???

      case parser.UnaryOperatorExpr(op, inner) =>
        EndOverloadExprFactory(
          OverloadExprFactory(_.scope.lookup(IdentifierExpr.OperatorIdentifier(op.value)))(Seq(
            ArgumentInfo(convertExpr(inner), inner.location, FunctionParameterListType.NormalList)
          ))
        )
    }

  def resolveHoles(env: Env, expr: WrapExpr): Comp[(context.ExprContext.WrapExpr, Env)] = ???

  def resolveHolesClassType(env: Env, expr: ArExpr[ExprConstructor.ClassType])
    : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType], Env)] = ???

  def resolveHolesTraitType(env: Env, expr: ArExpr[ExprConstructor.TraitType])
    : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType], Env)] = ???

  private abstract class OverloadExprFactoryBase extends ExprFactorySynth {

    override def synth(env: Env): Comp[ExprTypeResult] = resolveOverload(env)(lookup(env))

    protected type TElement

    protected val args: Seq[ArgumentInfo]
    protected def lookup: Env => LookupResult[TElement]

    protected def signatureOf(element: TElement): Comp[Signature[WrapExpr, ?]]

    // Checks if the current overload is applicable
    protected def checkOverload(env: Env)(overload: TElement): Comp[Option[Comp[ExprTypeResult]]]

    protected def resolveOverload(env: Env)(lookup: LookupResult[TElement]): Comp[ExprTypeResult] =
      lookup match {
        case LookupResult.Success(overloads, next) =>
          def checkEachRank(overloads: Seq[Seq[TElement]]): Comp[ExprTypeResult] =
            overloads match {
              case rank +: tail =>
                ZIO.foreach(rank)(checkOverload(env)).map(_.flatten).flatMap {
                  case Seq(single) => single
                  case Seq() => checkEachRank(tail)
                  case _ => IO.fail(DiagnosticError.AmbiguousOverload())
                }

              case _ => resolveOverload(env)(next)
            }

          rankOverloads(env, overloads).flatMap(checkEachRank)

        case LookupResult.Suspended(suspended) => suspended.flatMap(resolveOverload(env))

        case LookupResult.NotFound() =>
          IO.fail(DiagnosticError.LookupFailed())
      }

    // Ranks overloads based on parameter count relative to argument count, subtyping relation of parameters, etc.
    protected def rankOverloads(env: Env, overloads: Seq[TElement]): Comp[Seq[Seq[TElement]]] =
      def isMoreSpecificType(a: WrapExpr, b: WrapExpr): Comp[Boolean] = isSubType(env, a, b).map { _.isDefined }

      def argumentDelta(args: List[ArgumentInfo], sig: Signature[WrapExpr, ?], acc: Int): Int =
        (args, sig) match {
          case (
                ArgumentInfo(_, _, FunctionParameterListType.InferrableList) :: tailArgs,
                Signature.Parameter(FunctionParameterListType.InferrableList, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(_, _, FunctionParameterListType.InferrableList2) :: tailArgs,
                Signature.Parameter(FunctionParameterListType.InferrableList2, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(_, _, FunctionParameterListType.RequiresList) :: tailArgs,
                Signature.Parameter(FunctionParameterListType.RequiresList, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(_, _, FunctionParameterListType.NormalList) :: tailArgs,
                Signature.Parameter(_, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(
                  _,
                  _,
                  FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList,
                ) :: tailArgs,
                _,
              ) =>
            argumentDelta(tailArgs, sig, acc)

          case (ArgumentInfo(_, _, FunctionParameterListType.NormalList) :: tailArgs, Signature.Result(_)) =>
            argumentDelta(tailArgs, sig, acc + 1)

          case (Nil, Signature.Parameter(_, _, nextSig)) =>
            argumentDelta(args, sig, acc - 1)

          case (Nil, Signature.Result(_)) => acc
        }

      def checkBetterParams
        (a: WrapExpr, b: WrapExpr, nextA: Signature[WrapExpr, ?], nextB: Signature[WrapExpr, ?], hasFoundBetter: Boolean)
        : Comp[Boolean] =
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
          case (
                Signature.Parameter(FunctionParameterListType.InferrableList, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.InferrableList, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(FunctionParameterListType.InferrableList2, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.InferrableList2, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(FunctionParameterListType.RequiresList, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.RequiresList, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(FunctionParameterListType.NormalList, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.NormalList, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(
                  FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList,
                  _,
                  nextA,
                ),
                _,
              ) =>
            isBetterSig(nextA, b, hasFoundBetter)

          case (
                _,
                Signature.Parameter(
                  FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList,
                  _,
                  nextB,
                ),
              ) =>
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
            end if

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
      )
      (
        sigHandler: SignatureHandler[Res]
      )
      (
        f: (Env, Vector[WrapExpr], Res) => ExprTypeResult
      )
      : Comp[Option[Comp[ExprTypeResult]]] =

      def handleArg
        (arg: ArgumentInfo, paramType: WrapExpr, tailArgs: List[ArgumentInfo], next: Signature[WrapExpr, Res])
        : Comp[Option[Comp[ExprTypeResult]]] =
        arg.arg.check(env, paramType)
          .foldZIO(
            failure = _ => IO.none,
            success =
              result => {
                val variable = ParameterVariable(owner, convArgs.size, paramType)
                substituteArg(variable)(result.expr)(sigHandler)(next).flatMap { case (next, resultExpr) =>
                  createSigResult(owner, result.env, next, tailArgs, convArgs :+ resultExpr)(sigHandler)(f)
                }
              },
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
            val env2 = env.copy(model = model)
            proofToExpr(proof).flatMap { paramValue =>
              val variable = ParameterVariable(owner, convArgs.size, paramType)
              substituteArg(variable)(paramValue)(sigHandler)(next).flatMap { case (next, resultExpr) =>
                createSigResult(owner, env2, next, args, convArgs :+ resultExpr)(sigHandler)(f)
              }
            }

          case None => IO.none
        }

      (args, sig) match {
        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.NormalList)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.NormalList, paramType, next),
            ) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.NormalList, paramType, next)) =>
          ???

        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.InferrableList)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.InferrableList, paramType, next),
            ) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.InferrableList, paramType, next)) =>
          inferArg(paramType, next)

        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.InferrableList2)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.InferrableList2, paramType, next),
            ) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.InferrableList2, paramType, next)) =>
          inferArg(paramType, next)

        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.RequiresList)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.RequiresList, paramType, next),
            ) =>
          handleArg(arg, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.RequiresList, paramType, next)) =>
          resolveReqArg(paramType, next)

        case (_, Signature.Result(res)) =>
          val overloadExpr = f(env, convArgs, res)
          IO.succeed(Some(
            args.foldLeft(ConstExprFactory(overloadExpr): ExprFactory)(_.invoke(_))
              .synth(env)
          ))
      }

    end createSigResult

    // Returns substituted signature and (possibly) modified replacement expression
    protected def substituteArg[Res](variable: Variable)(replacement: WrapExpr)(sigHandler: SignatureHandler[Res])
      (sig: Signature[WrapExpr, Res])
      : Comp[(Signature[WrapExpr, Res], WrapExpr)] =
      if referencesVariableSig(variable)(sigHandler)(sig) then
        asStableExpression(replacement).map { case (replacement2, replacementStable) =>
          val sig2 = substituteSignature(variable)(replacement)(sigHandler)(sig)
          (sig2, replacement)
        }
      else
        IO.succeed((sig, replacement))

  }

  private final class OverloadExprFactory(protected val lookup: Env => LookupResult[ScopeElement])
    (protected val args: Seq[ArgumentInfo])
      extends OverloadExprFactoryBase {

    override def mutate(value: MutatorValue): ExprFactory =
      if args.isEmpty then
        new ExprFactory {
          override def synth(env: Env): Comp[ExprTypeResult] = mutateImpl(value, lookup(env)).flatMap(_.synth(env))

          override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
            mutateImpl(value, lookup(env)).flatMap(_.check(env, t))

        }
      else
        super.mutate(value)

    override def invoke(arg: ArgumentInfo): ExprFactory = OverloadExprFactory(lookup)(args :+ arg)

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

    protected override def checkOverload(env: Env)(overload: ScopeElement): Comp[Option[Comp[ExprTypeResult]]] =
      overload match {
        case variable: Variable =>
          IO.succeed(Some(IO.succeed(ExprTypeResult(
            WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple)),
            env,
            variable.varType,
          ))))

        case ModuleElementC.ClassElement(arClass) =>
          createSigResultConv(arClass, env, arClass.signature, args)(ClassSigHandler) {
            case (env, args, (typeOfClassType, _, _)) =>
              ExprTypeResult(
                WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(arClass), args)),
                env,
                typeOfClassType,
              )
          }

        case ModuleElementC.TraitElement(arTrait) =>
          createSigResultConv(arTrait, env, arTrait.signature, args)(TraitSigHandler) {
            case (env, args, (typeOfTraitType, _)) =>
              ExprTypeResult(
                WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(arTrait), args)),
                env,
                typeOfTraitType,
              )
          }

        case ModuleElementC.FunctionElement(func) =>
          createSigResultConv(func, env, func.signature, args)(FunctionSigHandler) { (env, args, returnType) =>
            ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionCall(func), args)),
              env,
              returnType,
            )
          }

      }

    protected def createSigResultConv[Res1, Res2]
      (
        owner: exprContext.ParameterVariableOwner,
        env: Env,
        sig: Comp[Signature[context.ExprContext.WrapExpr, Res1]],
        args: Seq[ArgumentInfo],
      )
      (
        sigHandler: SignatureHandlerPlus[Res1, Res2]
      )
      (
        f: (Env, Vector[WrapExpr], Res2) => ExprTypeResult
      )
      : Comp[Option[Comp[ExprTypeResult]]] =
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

      override def synth(env: Env): Comp[ExprTypeResult] =
        for {
          valueExpr <- value.arg.check(env, variable.varType)
          unitT <- unitType
          e = WrapExpr.OfExpr(ArExpr(ExprConstructor.StoreVariable(variable), valueExpr.expr))
        } yield ExprTypeResult(e, valueExpr.env, unitT)

    }

  }

  private final class OverloadExprFactoryMethod private (
    instance: WrapExpr,
    instanceType: WrapExpr,
    memberName: IdentifierExpr,
    protected val args: Seq[ArgumentInfo],
  ) extends OverloadExprFactoryBase {

    protected override type TElement = ArMethod

    private enum InstanceType {
      case ByClass(c: ArExpr[ExprConstructor.ClassType])
      case ByTrait(t: ArExpr[ExprConstructor.TraitType])
      case ByClassStatic(c: ArExpr[ExprConstructor.ClassType])
      case ByTraitStatic(t: ArExpr[ExprConstructor.TraitType])
    }

    protected def lookup: Env => LookupResult[ArMethod] =
      _ =>
        LookupResult.Suspended(
          evaluator.normalizeTopLevelWrap(instance, fuel)
            .flatMap {
              case WrapExpr.OfExpr(normalizedInstance) =>
                normalizedInstance.constructor match {
                  case ctor: (normalizedInstance.constructor.type & ExprConstructor.ClassType) =>
                    lookupMethods(
                      Seq(InstanceType.ByClassStatic(ArExpr(ctor, normalizedInstance.getArgs(ctor)))),
                      Set.empty,
                      Set.empty,
                    )

                  case ctor: (normalizedInstance.constructor.type & ExprConstructor.TraitType) =>
                    lookupMethods(
                      Seq(InstanceType.ByTraitStatic(ArExpr(ctor, normalizedInstance.getArgs(ctor)))),
                      Set.empty,
                      Set.empty,
                    )

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

    protected override def checkOverload(env: Env)(overload: ArMethod): Comp[Option[Comp[ExprTypeResult]]] =
      overload.signatureUnsubstituted.flatMap { sig =>
        val thisVar = InstanceVariable(overload, instanceType, None)
        val sig2 = convertSig(FunctionSigHandler)(sig)

        substituteArg(thisVar)(instance)(FunctionSigHandler)(sig2).flatMap { case (sig3, stableInstance) =>
          createSigResult(overload, env, sig3, args.toList, Vector.empty)(FunctionSigHandler) {
            (env, args, returnType) =>
              ExprTypeResult(
                WrapExpr.OfExpr(ArExpr(ExprConstructor.MethodCall(overload), (stableInstance, args))),
                env,
                returnType,
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
        case InstanceType.ByClass(classType) =>
          classType.constructor.arClass.methods.map(_.getOrElse(Some(memberName), Seq.empty))
        case InstanceType.ByClassStatic(classType) =>
          classType.constructor.arClass.staticMethods.map(_.getOrElse(Some(memberName), Seq.empty))
        case InstanceType.ByTrait(traitType) =>
          traitType.constructor.arTrait.methods.map(_.getOrElse(Some(memberName), Seq.empty))
        case InstanceType.ByTraitStatic(traitType) =>
          traitType.constructor.arTrait.staticMethods.map(_.getOrElse(Some(memberName), Seq.empty))
      }

    private def resolveTypeSignatureResult[Res](owner: exprContext.ParameterVariableOwner)
      (sigHandler: SignatureHandler[Res])(paramIndex: Int, args: List[WrapExpr])(sig: Signature[WrapExpr, Res])
      : Comp[Res] =
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

    private def lookupMethods(types: Seq[InstanceType], seenTypes: Set[ArClass | ArTrait], seenMethods: Set[ArMethod])
      : Comp[LookupResult[ArMethod]] =
      ZIO.foreach(types)(methodsOfType).map { methods =>
        val methods2 = methods.flatten.filterNot(seenMethods.contains).distinct

        val next =
          LookupResult.Suspended(
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

    private final class WrappedMethodOverloadFactory
      (instance: ExprFactory, memberName: IdentifierExpr, args: Seq[ArgumentInfo])
        extends ExprFactory {

      override def synth(env: Env): Comp[ExprTypeResult] =
        instance.synth(env).flatMap { instanceExpr =>
          OverloadExprFactoryMethod(instanceExpr.expr, instanceExpr.exprType, memberName, args).synth(instanceExpr.env)
        }

      override def check(env: Env, t: WrapExpr): Comp[ExprResult] =
        instance.synth(env).flatMap { instanceExpr =>
          OverloadExprFactoryMethod(instanceExpr.expr, instanceExpr.exprType, memberName, args).check(
            instanceExpr.env,
            t,
          )
        }

      override def mutate(value: MutatorValue): ExprFactory =
        EndOverloadExprFactory(
          WrappedMethodOverloadFactory(
            instance,
            IdentifierExpr.Update(memberName),
            args :+ ArgumentInfo(value.arg, value.location, FunctionParameterListType.NormalList),
          )
        )

      override def invoke(arg: ArgumentInfo): ExprFactory =
        WrappedMethodOverloadFactory(instance, memberName, args :+ arg)

    }

    def fromInstanceFactory(instance: ExprFactory, memberName: IdentifierExpr): ExprFactory =
      WrappedMethodOverloadFactory(instance, memberName, Seq.empty)

  }

  private final class EndOverloadExprFactory(inner: ExprFactory) extends ExprFactory {
    override def synth(env: Env): Comp[ExprTypeResult] = inner.synth(env)
    override def check(env: Env, t: WrapExpr): Comp[ExprResult] = inner.check(env, t)
  }

  private final class ConstExprFactory(result: ExprTypeResult) extends ExprFactorySynth {
    def synth(env: Env): Comp[ExprTypeResult] = IO.succeed(result)
  }

  private def convertSig[Res1, Res2](sigHandler: SignatureHandlerPlus[Res1, Res2])
    (sig: Signature[context.ExprContext.WrapExpr, Res1])
    : Signature[WrapExpr, Res2] =
    sig match {
      case Signature.Parameter(paramListType, paramType, next) =>
        Signature.Parameter(
          paramListType,
          ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(paramType),
          convertSig(sigHandler)(next),
        )

      case Signature.Result(res) =>
        Signature.Result(sigHandler.convertResult(res))
    }

  object ClassSigHandler extends SignatureHandlerPlus[ArClass#ClassResult, ClassResultConv]:

    override def convertResult(res: ArClass#ClassResult): ClassResultConv =
      val (classType, baseClass, baseTraits) = res

      val classType2 =
        ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(classType)
      val baseClass2 =
        baseClass.map(ArgonExprContext.convertClassType[Id](context)(context.ExprContext, exprContext)(identity))
      val baseTraits2 =
        baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

      (classType2, baseClass2, baseTraits2)
    end convertResult

    override def resolveResultHoles(env: Env, res: ClassResultConv): Comp[(ArClass#ClassResult, Env)] =
      val (classType, baseClass, baseTraits) = res
      resolveHoles(env, classType).flatMap { case (classType2, env) =>
        ZIO.foreach(baseClass)(resolveHolesClassType(env, _))
          .map {
            case Some((baseClass2, env)) => (Some(baseClass2), env)
            case None => (None, env)
          }
          .flatMap { case (baseClass2, env) =>
            for {
              envState <- Ref.make(env)
              baseTraits2 <-
                ZIO.foreach(baseTraits) { baseTrait =>
                  for {
                    env <- envState.get
                    baseTrait2Res <- resolveHolesTraitType(env, baseTrait)
                    (baseTrait2, env) = baseTrait2Res
                    _ <- envState.set(env)
                  } yield baseTrait2
                }
              env <- envState.get
            } yield ((classType2, baseClass2, baseTraits2), env)

          }
      }
    end resolveResultHoles

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

  object TraitSigHandler extends SignatureHandlerPlus[ArTrait#TraitResult, TraitResultConv]:

    override def convertResult(res: ArTrait#TraitResult): TraitResultConv =
      val (traitType, baseTraits) = res

      val traitType2 =
        ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(traitType)
      val baseTraits2 =
        baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

      (traitType2, baseTraits2)
    end convertResult

    override def resolveResultHoles(env: Env, res: TraitResultConv): Comp[(ArTrait#TraitResult, Env)] =
      val (classType, baseTraits) = res
      resolveHoles(env, classType).flatMap { case (classType2, env) =>
        for {
          envState <- Ref.make(env)
          baseTraits2 <-
            ZIO.foreach(baseTraits) { baseTrait =>
              for {
                env <- envState.get
                baseTrait2Res <- resolveHolesTraitType(env, baseTrait)
                (baseTrait2, env) = baseTrait2Res
                _ <- envState.set(env)
              } yield baseTrait2
            }
          env <- envState.get
        } yield ((classType2, baseTraits2), env)
      }
    end resolveResultHoles

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

  object FunctionSigHandler extends SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr]:

    override def convertResult(res: context.ExprContext.WrapExpr): WrapExpr =
      ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(res)

    override def resolveResultHoles(env: Env, res: WrapExpr): Comp[(context.ExprContext.WrapExpr, Env)] =
      resolveHoles(env, res)

    override def substituteResult(variable: Variable)(replacement: WrapExpr)(res: WrapExpr): WrapExpr =
      substituteWrapExpr(variable)(replacement)(res)

    override def resultReferences(variable: Variable)(res: WrapExpr): Boolean = referencesVariable(variable)(res)
  end FunctionSigHandler

  private def loadKnownExport[TElement <: ModuleElement]
    (tubeName: TubeName, modulePath: ModulePath, id: IdentifierExpr, paramTypes: Seq[WrapExpr])
    (using TypeTest[ModuleElement, TElement])
    : Comp[TElement] =
    def matchesOverload(paramTypes: Seq[WrapExpr])(moduleElement: ModuleElement): Comp[Boolean] =
      (moduleElement match {
        case ModuleElementC.ClassElement(c) => c.signature.map(convertSig(ClassSigHandler))
        case ModuleElementC.TraitElement(t) => t.signature.map(convertSig(TraitSigHandler))
        case ModuleElementC.FunctionElement(f) => f.signature.map(convertSig(FunctionSigHandler))
      })
        .flatMap { sig =>
          val declParamTypes = sig.parameterTypes
          if paramTypes.size != declParamTypes.size then
            val env: Env =
              Env(
                scope = Scope.fromImports(Map.empty),
                model = Map.empty,
              )
            ZIO.forall(paramTypes.zip(declParamTypes)) { case (a, b) => isSameType(env, a, b).map(_.isDefined) }
          else
            IO.succeed(false)
          end if
        }

    context.getTube(tubeName)
      .flatMap(_.module(modulePath))
      .flatMap(_.exports(id))
      .flatMap { elements =>
        ZIO.filter(elements.collect { case element: TElement => element })(matchesOverload(paramTypes))
      }
      .flatMap {
        case Seq(element) => IO.succeed(element)
        case _ => ???
      }

  end loadKnownExport

  private val argonCoreTubeName = TubeName(NonEmptyList("Argon", "Core"))

  def boolType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type]](
      argonCoreTubeName,
      ModulePath(Seq()),
      IdentifierExpr.Named("Bool"),
      Seq(),
    )
      .map { case ModuleElementC.ClassElement(c) => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(c), Vector())) }

  def intType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type]](
      argonCoreTubeName,
      ModulePath(Seq()),
      IdentifierExpr.Named("Int"),
      Seq(),
    )
      .map { case ModuleElementC.ClassElement(c) => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(c), Vector())) }

  def natType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type]](
      argonCoreTubeName,
      ModulePath(Seq()),
      IdentifierExpr.Named("Nat"),
      Seq(),
    )
      .map { case ModuleElementC.ClassElement(c) => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(c), Vector())) }

  def stringType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type]](
      argonCoreTubeName,
      ModulePath(Seq()),
      IdentifierExpr.Named("String"),
      Seq(),
    )
      .map { case ModuleElementC.ClassElement(c) => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(c), Vector())) }

  def unitType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type]](
      argonCoreTubeName,
      ModulePath(Seq()),
      IdentifierExpr.Named("Unit"),
      Seq(),
    )
      .map { case ModuleElementC.ClassElement(c) => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(c), Vector())) }

  def exceptionType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type]](
      argonCoreTubeName,
      ModulePath(Seq()),
      IdentifierExpr.Named("Exception"),
      Seq(),
    )
      .map { case ModuleElementC.ClassElement(c) => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(c), Vector())) }

  def anyType: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.AnyType, EmptyTuple))
  def neverType: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.NeverType, EmptyTuple))

}

object ExpressionConverter {

  def make(ctx: Context): UIO[ExpressionConverter with HasContext[ctx.type]] =
    IO.succeed(
      new ExpressionConverter {
        override val context: ctx.type = ctx
        override val fuel: Int = 100
      }
    )

}
