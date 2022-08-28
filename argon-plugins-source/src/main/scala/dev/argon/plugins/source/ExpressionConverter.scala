package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.module.ModuleElementC
import dev.argon.compiler.signature.{ErasedSignature, ErasedSignatureType, ErasedSignatureWithResult, ImportSpecifier, Signature}
import dev.argon.util.{SourceLocation, WithSource}
import dev.argon.parser
import dev.argon.parser.{FunctionParameterListType, IdentifierExpr}
import dev.argon.expr.{Evaluator, ExprConstraints, ImplicitResolver}
import dev.argon.util.UniqueIdentifier
import zio.*
import zio.stream.*
import dev.argon.util.{*, given}
import dev.argon.compiler.tube.{TubeImporter, TubeName}
import dev.argon.compiler.module.ModulePath
import dev.argon.prover.Proof

import scala.reflect.TypeTest

sealed abstract class ExpressionConverter extends UsingContext with ExprUtilWithHoles {

  val exprContext: HolesExprContext & HasContext[context.type] =
    new HolesExprContext {
      override val context: ExpressionConverter.this.context.type = ExpressionConverter.this.context
    }

  import exprContext.{
    WrapExpr,
    ArExpr,
    ExprConstructor,

    ClassResult,
    TraitResult,

    Variable,
    LocalVariable,
    InstanceVariable,
    ParameterVariable,
    ParameterVariableOwner,
  }

  val evaluator: ArgonEvaluator.Aux[context.Env, context.Error, context.type, exprContext.type] = ArgonEvaluator(context)(exprContext)

  final case class ArgumentInfo(arg: ExprFactory, location: SourceLocation, listType: FunctionParameterListType)
  final case class MutatorValue(arg: ExprFactory, location: SourceLocation)

  final case class ExprResult(expr: WrapExpr, env: Env)
  final case class ExprTypeResult(expr: WrapExpr, env: Env, exprType: WrapExpr)

  trait ExprFactory {
    def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult]
    def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult]

    def mutate(value: MutatorValue): ExprFactory =
      ExprFactoryError(DiagnosticError.CanNotMutate(DiagnosticSource.Location(value.location)))

    def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadExprFactoryMethod.fromInstanceFactory(this, IdentifierExpr.Named("apply"), arg.location)
        .invoke(arg)

  }

  private trait ExprFactorySynth extends ExprFactory {

    protected def location: SourceLocation

    override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
      for {
        res <- synth(env, opt)
        env <- checkSubType(res.env, res.exprType, t, location)
      } yield ExprResult(res.expr, env)

  }

  private trait ExprFactoryCheck extends ExprFactory {
    override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] = ZIO.fail(DiagnosticError.UnknownTypeForExpression())
  }

  private final class ExprFactoryError(error: context.Error) extends ExprFactory {
    override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] = ZIO.fail(error)
    override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] = ZIO.fail(error)
  }

  def convertStmtList(stmts: WithSource[Seq[WithSource[parser.Stmt]]]): ExprFactory =
    stmts.value match {
      case Seq(WithSource(expr: parser.Expr, location)) =>
        EndOverloadExprFactory(convertExpr(WithSource(expr, location)))

      case head +: tail =>
        val tailStmts: WithSource[Seq[WithSource[parser.Stmt]]] =
          WithSource(
            tail,
            SourceLocation(
              stmts.location.fileName,
              tail.headOption
                .map { case WithSource(_, nextLoc) => nextLoc.start }
                .getOrElse { head.location.end },
              stmts.location.end,
            ),
          )

        def headResult(env: Env, opt: ExprOptions): Comp[ExprResult] =
          head.value match {
            case expr: parser.Expr =>
              convertExpr(WithSource(expr, head.location)).check(env, opt, unitType)

            case varDecl: parser.VariableDeclarationStmt =>
              val localVarComp: Comp[(LocalVariable, WrapExpr, Env)] =
                varDecl.varType match {
                  case Some(varType) =>
                    for {
                      id <- UniqueIdentifier.make
                      tRes <- convertExpr(varType).check(env, opt.forTypeExpr, anyType)
                      value <- convertExpr(varDecl.value).check(tRes.env, opt, tRes.expr)
                      localVar = LocalVariable(id, tRes.expr, varDecl.name, varDecl.isMutable, isErased = false)
                    } yield (localVar, value.expr, value.env)

                  case None =>
                    for {
                      id <- UniqueIdentifier.make
                      value <- convertExpr(varDecl.value).synth(env, opt)
                      localVar = LocalVariable(id, value.exprType, varDecl.name, varDecl.isMutable, isErased = false)
                    } yield (localVar, value.expr, value.env)
                }

              localVarComp.map { case (variable, value, env) =>
                val env2 = env.withScope(_.addVariable(variable))
                val env3 =
                  if varDecl.isGiven then
                    env2.withImplicitSource(_.addVariable(variable))
                  else
                    env2

                ExprResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.BindVariable(variable), value)),
                  env3,
                )
              }

            case _ => ZIO.fail(DiagnosticError.InvalidStatementInFunction())
          }

        new ExprFactory {
          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for {
              exprRes <- headResult(env, opt)
              tailRes <- convertStmtList(tailStmts).synth(exprRes.env, opt)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.Sequence, NonEmptyList(exprRes.expr, tailRes.expr)))
            } yield ExprTypeResult(e, tailRes.env, tailRes.exprType)

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            for {
              exprRes <- headResult(env, opt)
              tailRes <- convertStmtList(tailStmts).check(exprRes.env, opt, t)
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

  def createTypeOperatorFactory
  (location: SourceLocation)
  (ctor: ExprConstructor { type ConstructorArgs = (WrapExpr, WrapExpr) })
  (left: WithSource[parser.Expr], right: WithSource[parser.Expr])
  : ExprFactory =
    new ExprFactory {
      override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
        for
          _ <- ZIO.fail(DiagnosticError.ErasedExpressionNotAllowed(DiagnosticSource.Location(location))).unless(opt.checkErasure(true))
          ExprTypeResult(l, env, t1) <- convertExpr(left).synth(env, opt)
          ExprTypeResult(r, env, t2) <- convertExpr(right).synth(env, opt)
        yield ???


      override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
        for
          _ <- ZIO.fail(DiagnosticError.ErasedExpressionNotAllowed(DiagnosticSource.Location(location))).unless(opt.checkErasure(true))
          _ <- checkSubType(env, t, anyType, location)
          ExprResult(l, env) <- convertExpr(left).check(env, opt, t)
          ExprResult(r, env) <- convertExpr(right).check(env, opt, t)
        yield ExprResult(WrapExpr.OfExpr(ArExpr(ctor, (l, r))), env)
    }

  def convertExpr(expr: WithSource[parser.Expr]): ExprFactory =
    expr.value match {
      case parser.AsExpr(value, valueType) =>
        new ExprFactorySynth {

          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            convertExpr(valueType).check(env, opt.forTypeExpr, anyType)
              .flatMap { case ExprResult(t, env) =>
                convertExpr(value).check(env, opt, t)
                  .map { case ExprResult(e, env) =>
                    ExprTypeResult(e, env, t)
                  }
              }

        }

      case parser.BinaryOperatorExpr(WithSource(parser.BinaryOperator.Intersection, _), left, right) =>
        createTypeOperatorFactory(expr.location)(ExprConstructor.IntersectionType)(left, right)

      case parser.BinaryOperatorExpr(WithSource(parser.BinaryOperator.Union, _), left, right) =>
        createTypeOperatorFactory(expr.location)(ExprConstructor.UnionType)(left, right)

      case parser.BinaryOperatorExpr(WithSource(parser.BinaryOperator.SubType, _), left, right) =>
        createTypeOperatorFactory(expr.location)(ExprConstructor.SubtypeWitnessType)(left, right)

      case parser.BinaryOperatorExpr(WithSource(parser.BinaryOperator.Assign, assignLoc), left, right) =>
        convertExpr(left).mutate(MutatorValue(convertExpr(right), assignLoc))

      case parser.BinaryOperatorExpr(op, left, right) =>
        EndOverloadExprFactory(
          OverloadExprFactory(
            op.location,
            Seq(
              ArgumentInfo(convertExpr(left), left.location, FunctionParameterListType.NormalList),
              ArgumentInfo(convertExpr(right), right.location, FunctionParameterListType.NormalList),
            ),
            IdentifierExpr.OperatorIdentifier(op.value),
          )
        )

      case parser.BlockExpr(body, rescueCases, elseBody, Some(ensureBody)) =>
        new ExprFactory {

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              innerRes <- convertExpr(inner).synth(env, opt)
              ensuring <- convertStmtList(ensureBody).check(innerRes.env, opt, unitType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.EnsureExecuted, (innerRes.expr, ensuring.expr)))
            } yield ExprTypeResult(e, ensuring.env, innerRes.exprType)
          end synth

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            val inner = WithSource(parser.BlockExpr(body, rescueCases, elseBody, None), expr.location)
            for {
              innerRes <- convertExpr(inner).check(env, opt, t)
              ensuring <- convertStmtList(ensureBody).check(innerRes.env, opt, unitType)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.EnsureExecuted, (innerRes.expr, ensuring.expr)))
            } yield ExprResult(e, ensuring.env)
          end check

        }

      case parser.BlockExpr(body, Vector(), None, None) =>
        convertStmtList(body)

      case block: parser.BlockExpr => ???

      case parser.BoolValueExpr(b) =>
        new ExprFactorySynth {
          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for
              t <- boolType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantBool(b), EmptyTuple))
            yield ExprTypeResult(e, env, t)
        }

      case parser.ClassConstructorExpr(classExpr) =>
        class WrappedFactory(inner: (Env, ExprOptions) => Comp[(Env, ExprFactory)]) extends ExprFactory {
          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            inner(env, opt).flatMap { case (env, factory) => factory.synth(env, opt) }

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            inner(env, opt).flatMap { case (env, factory) => factory.check(env, opt, t) }

          override def mutate(value: MutatorValue): ExprFactory =
            WrappedFactory((env, opt) => inner(env, opt).map { case (env, factory) => (env, factory.mutate(value)) })

          override def invoke(arg: ArgumentInfo): ExprFactory =
            WrappedFactory((env, opt) => inner(env, opt).map { case (env, factory) => (env, factory.invoke(arg)) })
        }

        WrappedFactory { (env, opt) =>
          for
            classExprConv <- convertExpr(classExpr).check(env, opt, anyType)
            classExprNorm <- evaluator.normalizeTopLevelWrap(classExprConv.expr, fuel)
          yield (classExprConv.env, OverloadExprFactoryClassConstructor(classExprNorm, classExpr.location, Seq()))
        }


      case parser.DotExpr(left, WithSource(id, idLocation)) =>
        OverloadExprFactoryMethod.fromInstanceFactory(convertExpr(left), id, idLocation)

      case parser.ExternExpr(specifier) => ???

      case parser.FunctionCallExpr(func, listType, arg) =>
        convertExpr(func)
          .invoke(ArgumentInfo(convertExpr(arg), arg.location, listType))

      case id: IdentifierExpr =>
        OverloadExprFactory(
          expr.location,
          Seq.empty,
          id,
        )

      case parser.IfElseExpr(condition, ifBody, elseBody) =>
        new ExprFactory {

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(condition).check(env, opt, bType)
              trueRes <- convertStmtList(ifBody).synth(condRes.env, opt)
              falseRes <- convertStmtList(elseBody).synth(condRes.env, opt)
              resEnv = condRes.env.mergeBranches(trueRes.env, falseRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.IfElse, (condRes.expr, trueRes.expr, falseRes.expr)))
              t = WrapExpr.OfExpr(ArExpr(ExprConstructor.UnionType, (trueRes.exprType, falseRes.exprType)))
            } yield ExprTypeResult(e, resEnv, t)

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            for {
              bType <- boolType
              condRes <- convertExpr(condition).check(env, opt, bType)
              trueRes <- convertStmtList(ifBody).check(condRes.env, opt, t)
              falseRes <- convertStmtList(elseBody).check(condRes.env, opt, t)
              resEnv = condRes.env.mergeBranches(trueRes.env, falseRes.env)
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.IfElse, (condRes.expr, trueRes.expr, falseRes.expr)))
            } yield ExprResult(e, resEnv)

        }

      case i: parser.IntValueExpr =>
        new ExprFactorySynth {

          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for {
              t <- intType
              e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(i.value), EmptyTuple))
            } yield ExprTypeResult(e, env, t)

        }

      case parser.LambdaTypeExpr(argType, resultType) =>
        new ExprFactorySynth {

          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for
              argExpr <- convertExpr(argType).check(env, opt.forTypeExpr, anyType)
              resultExpr <- convertExpr(resultType).check(argExpr.env, opt.forTypeExpr, anyType)
            yield ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(
                ExprConstructor.FunctionType,
                (argExpr.expr, resultExpr.expr)
              )),
              resultExpr.env,
              anyType
            )
        }

      case parser.LambdaExpr(varName, body) =>
        new ExprFactoryCheck {

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            evaluator.normalizeTopLevelWrap(t, fuel).flatMap {
              case WrapExpr.OfExpr(normalizedType) =>
                normalizedType.constructor match {
                  case funcTypeCtor: (normalizedType.constructor.type & ExprConstructor.FunctionType.type) =>
                    val (argType, resType) = normalizedType.getArgs(funcTypeCtor)
                    for {
                      id <- UniqueIdentifier.make
                      paramVar = LocalVariable(id, argType, varName, isMutable = false, isErased = false)
                      bodyRes <- convertExpr(body).check(env.withScope(_.addVariable(paramVar)), opt.requirePure, resType)
                      e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadLambda(paramVar), bodyRes.expr))
                    } yield ExprResult(e, bodyRes.env)

                  case _ =>
                    ZIO.fail(DiagnosticError.InvalidTypeForFunction())
                }

              case _ =>
                ZIO.fail(DiagnosticError.InvalidTypeForFunction())
            }

        }

      case parser.MatchExpr(_, _) => ???

      case parser.RaiseExpr(exception) =>
        new ExprFactorySynth {

          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            if opt.purity then
              ZIO.fail(DiagnosticError.Purity(DiagnosticSource.Location(expr.location)))
            else
              for
                exType <- exceptionType
                ex <- convertExpr(exception).check(env, opt, exType)
                e = WrapExpr.OfExpr(ArExpr(ExprConstructor.RaiseException, ex.expr))
              yield ExprTypeResult(e, ex.env, neverType)

        }

      case parser.StringValueExpr(parser.Token.StringToken(tokenParts)) =>
        val headPart = tokenParts.head
        val tailParts = tokenParts.tail
        def convertPart(strType: WrapExpr, env: Env, opt: ExprOptions, part: parser.Token.StringToken.Part): Comp[ExprTypeResult] =
          part match {
            case parser.Token.StringToken.StringPart(str) =>
              val e = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantString(str.value), EmptyTuple))
              ZIO.succeed(ExprTypeResult(e, env, strType))

            case parser.Token.StringToken.ExprPart(None, expr) =>
              for {
                res <- convertExpr(expr).check(env, opt, strType)
              } yield ExprTypeResult(res.expr, res.env, strType)

            case parser.Token.StringToken.ExprPart(Some(format), expr) => ???
          }

        new ExprFactorySynth {

          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for {
              strType <- stringType
              concat <- loadKnownExport[ModuleElementC.FunctionElement[context.type, ?]](
                ImportSpecifier(
                  argonCoreTubeName,
                  ModulePath(Seq("String")),
                  Some(IdentifierExpr.OperatorIdentifier(parser.BinaryOperator.Concat)),
                  ErasedSignatureWithResult(
                    Seq(
                      ErasedSignatureType.Class(stringSpecifier, Seq()),
                      ErasedSignatureType.Class(stringSpecifier, Seq()),
                    ),
                    ErasedSignatureType.Class(stringSpecifier, Seq()),
                  )
                ))

              firstPartConv <- convertPart(strType, env, opt, headPart)

              convParts <- ZIO.foldLeft(tailParts)(firstPartConv) { (prevString, part) =>
                for
                  convPart <- convertPart(strType, prevString.env, opt, part)
                yield ExprTypeResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionCall(concat.func), Seq(prevString.expr, convPart.expr))),
                  convPart.env,
                  strType
                )
              }

            } yield convParts

        }


      case parser.TupleExpr(values) =>
        new ExprFactory {
          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] = synthPart(env, opt, values.toList, Vector.empty, Vector.empty)

          private def synthPart
            (env: Env, opt: ExprOptions, values: List[WithSource[parser.Expr]], valuesAcc: Vector[WrapExpr], typeAcc: Vector[WrapExpr])
            : Comp[ExprTypeResult] =
            values match {
              case head :: tail =>
                convertExpr(head).synth(env, opt).flatMap { currentRes =>
                  synthPart(
                    currentRes.env,
                    opt,
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
                ZIO.succeed(ExprTypeResult(valuesExpr, env, typesExpr))
            }

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            evaluator.normalizeTopLevelWrap(t, fuel).flatMap {
              case normalizedTypeWrap @ WrapExpr.OfExpr(normalizedType) =>
                normalizedType.constructor match {
                  case tupleTypeCtor: (normalizedType.constructor.type & ExprConstructor.LoadTuple.type) =>
                    val tupleType: Seq[WrapExpr] = normalizedType.getArgs(tupleTypeCtor)
                    checkPart(env, opt, values.toList, Vector.empty, tupleType.toList)

                  case _: (ExprConstructor.AnyType.type | ExprConstructor.OmegaTypeN | ExprConstructor.TypeN.type) =>
                    val tupleType = List.fill(values.size)(normalizedTypeWrap)
                    checkPart(env, opt, values.toList, Vector.empty, tupleType)

                  case _ =>
                    ZIO.fail(DiagnosticError.InvalidTypeForTuple())
                }

              case _ =>
                ZIO.fail(DiagnosticError.InvalidTypeForTuple())
            }

          private def checkPart
            (
              env: Env,
              opt: ExprOptions,
              values: List[WithSource[parser.Expr]],
              valuesAcc: Vector[WrapExpr],
              expectedTypes: List[WrapExpr],
            )
            : Comp[ExprResult] =
            (expectedTypes, values) match {
              case (t :: tailTypes, value :: valuesTail) =>
                convertExpr(value).check(env, opt, t).flatMap { currentRes =>
                  checkPart(
                    currentRes.env,
                    opt,
                    valuesTail,
                    valuesAcc :+ currentRes.expr,
                    tailTypes,
                  )
                }

              case (Nil, Nil) =>
                val valuesExpr =
                  WrapExpr.OfExpr(ArExpr(
                    ExprConstructor.LoadTuple,
                    valuesAcc,
                  ))
                ZIO.succeed(ExprResult(valuesExpr, env))

              case _ =>
                ZIO.fail(DiagnosticError.TupleSizeMismatch())
            }

        }

      case parser.TypeExpr(None) =>
        new ExprFactorySynth {
          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            val e = WrapExpr.OfExpr(ArExpr(ExprConstructor.TypeN, WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(0), EmptyTuple))))
            val t = WrapExpr.OfExpr(ArExpr(ExprConstructor.TypeN, WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(1), EmptyTuple))))
            ZIO.succeed(ExprTypeResult(e, env, t))
        }

      case parser.TypeExpr(level) => ???

      case parser.MetaTypeExpr(level) =>
        new ExprFactorySynth {

          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            val e = WrapExpr.OfExpr(ArExpr(ExprConstructor.OmegaTypeN(level), EmptyTuple))
            val t = WrapExpr.OfExpr(ArExpr(ExprConstructor.OmegaTypeN(level + 1), EmptyTuple))
            ZIO.succeed(ExprTypeResult(e, env, t))
          end synth

        }

      case parser.TypeOfExpr(ofExpr) => ???

      case parser.UnaryOperatorExpr(op, inner) =>
        EndOverloadExprFactory(
          OverloadExprFactory(
            op.location,
            Seq(
              ArgumentInfo(convertExpr(inner), inner.location, FunctionParameterListType.NormalList)
            ),
            IdentifierExpr.OperatorIdentifier(op.value),
          )
        )

      case parser.AssertExpr(assertionType) =>
        new ExprFactorySynth:
          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for
              ExprResult(t, env) <- convertExpr(assertionType).check(env, opt.forTypeExpr, anyType)
              (env, value) <- resolveImplicit(env, t, location)
              localId <- UniqueIdentifier.make
              variable = LocalVariable(
                localId,
                t,
                name = None,
                isMutable = false,
                isErased = true,
              )
            yield ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(ExprConstructor.BindVariable(variable), value)),
              env,
              unitType
            )
        end new

      case parser.SummonExpr(summonedType) =>
        new ExprFactorySynth:
          override protected def location: SourceLocation = expr.location

          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            for
              ExprResult(t, env) <- convertExpr(summonedType).check(env, opt.forTypeExpr, anyType)
              (env, value) <- resolveImplicit(env, t, location)
            yield ExprTypeResult(value, env, t)
        end new
    }

  private abstract class OverloadExprFactoryBase extends ExprFactorySynth {

    import ExprConstructor.MethodCallOwnerType

    override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] = resolveOverload(env, opt)(Seq.empty)(lookup(env, opt))

    protected type TElement

    protected val args: Seq[ArgumentInfo]
    protected def lookup(env: Env, opt: ExprOptions): LookupResult[TElement]
    protected def overloadLocation: SourceLocation
    protected def lookupName: IdentifierExpr | parser.ClassConstructorExpr.type

    override protected def location: SourceLocation = overloadLocation

    protected def signatureOf(element: TElement): Comp[Signature[WrapExpr, ?]]

    // Checks if the current overload is applicable
    protected def checkOverload(env: Env, opt: ExprOptions)(overload: TElement): Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]]

    private def resolveOverload(env: Env, opt: ExprOptions)(rejected: Seq[Cause[context.Error]])(lookup: LookupResult[TElement]): Comp[ExprTypeResult] =
      lookup match {
        case LookupResult.Success(overloads, next) =>
          def checkEachRank(rejected: Seq[Cause[context.Error]])(overloads: Seq[Seq[TElement]]): Comp[ExprTypeResult] =
            overloads match {
              case rank +: tail =>
                ZIO.foreach(rank)(checkOverload(env, opt)).flatMap { attemptedOverloads =>
                  val (invalidOverloads, validOverloads) = attemptedOverloads.partitionMap(identity)
                  validOverloads match {
                    case Seq(single) => single
                    case Seq() => checkEachRank(rejected ++ invalidOverloads.flatten)(tail)
                    case _ => ZIO.fail(DiagnosticError.AmbiguousOverload())
                  }
                }

              case _ => resolveOverload(env, opt)(rejected)(next)
            }

          rankOverloads(env, overloads).flatMap(checkEachRank(rejected))

        case LookupResult.Suspended(suspended) => suspended.flatMap(resolveOverload(env, opt)(rejected))

        case LookupResult.NotFound() =>
          rejected match {
            case Seq() => ZIO.fail(DiagnosticError.LookupFailed(DiagnosticSource.Location(overloadLocation), lookupName))
            case Seq(error) => ZIO.failCause(error)
            case _ => ZIO.fail(DiagnosticError.OverloadsFailed(DiagnosticSource.Location(overloadLocation), lookupName))
          }
      }

    // Ranks overloads based on parameter count relative to argument count, subtyping relation of parameters, etc.
    private def rankOverloads(env: Env, overloads: Seq[TElement]): Comp[Seq[Seq[TElement]]] =
      def isMoreSpecificType(a: WrapExpr, b: WrapExpr): Comp[Boolean] = isSubType(env, a, b).map { _.isDefined }

      def argumentDelta(args: List[ArgumentInfo], sig: Signature[WrapExpr, ?], acc: Int): Int =
        (args, sig) match {
          case (
                ArgumentInfo(_, _, FunctionParameterListType.InferrableList) :: tailArgs,
                Signature.Parameter(FunctionParameterListType.InferrableList, _, _, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(_, _, FunctionParameterListType.InferrableList2) :: tailArgs,
                Signature.Parameter(FunctionParameterListType.InferrableList2, _, _, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(_, _, FunctionParameterListType.RequiresList) :: tailArgs,
                Signature.Parameter(FunctionParameterListType.RequiresList, _, _, _, nextSig),
              ) =>
            argumentDelta(tailArgs, nextSig, acc)

          case (
                ArgumentInfo(_, _, FunctionParameterListType.NormalList) :: tailArgs,
                Signature.Parameter(_, _, _, _, nextSig),
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

          case (Nil, Signature.Parameter(_, _, _, _, nextSig)) =>
            argumentDelta(args, nextSig, acc - 1)

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
                case true => ZIO.succeed(false)
                case false => isBetterSig(nextA, nextB, hasFoundBetter)
              }
          }
        else
          isMoreSpecificType(b, a).flatMap {
            case true => ZIO.succeed(false)
            case false => isBetterSig(nextA, nextB, hasFoundBetter)
          }

      def isBetterSig(a: Signature[WrapExpr, ?], b: Signature[WrapExpr, ?], hasFoundBetter: Boolean): Comp[Boolean] =
        (a, b) match {
          case (
                Signature.Parameter(FunctionParameterListType.InferrableList, _, _, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.InferrableList, _, _, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(FunctionParameterListType.InferrableList2, _, _, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.InferrableList2, _, _, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(FunctionParameterListType.RequiresList, _, _, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.RequiresList, _, _, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(FunctionParameterListType.NormalList, _, _, typeA, nextA),
                Signature.Parameter(FunctionParameterListType.NormalList, _, _, typeB, nextB),
              ) =>
            checkBetterParams(typeA, typeB, nextA, nextB, hasFoundBetter)

          case (
                Signature.Parameter(
                  FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2 | FunctionParameterListType.RequiresList,
                  _,
                  _,
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
                  _,
                  _,
                  nextB,
                ),
              ) =>
            isBetterSig(a, nextB, hasFoundBetter)

          case (Signature.Result(_), Signature.Result(_)) =>
            ZIO.succeed(hasFoundBetter)

          case (Signature.Result(_), Signature.Parameter(FunctionParameterListType.NormalList, _, _, _, _)) =>
            ZIO.succeed(false)

          case (Signature.Parameter(FunctionParameterListType.NormalList, _, _, _, _), Signature.Result(_)) =>
            ZIO.succeed(false)
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
              ZIO.succeed(
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
              case true => ZIO.succeed(List(overload) :: ranks)
              case false =>
                ZIO.exists(topRank)(isBetterThan(_, overload)).flatMap {
                  case true => rankOverload(lowerRanks, overload).map { topRank :: _ }
                  case false => ZIO.succeed((overload :: topRank) :: lowerRanks)
                }
            }

          case Nil =>
            ZIO.succeed(List(List(overload)))
        }

      ZIO.foldLeft(overloads)(Nil)(rankOverload)
    end rankOverloads

    protected def createSigResult[Res]
      (
        owner: exprContext.ParameterVariableOwner,
        env: Env,
        opt: ExprOptions,
        sig: Signature[WrapExpr, Res],
        args: List[ArgumentInfo],
        convArgs: Vector[WrapExpr],
      )
      (
        sigHandler: SignatureHandler[Res]
      )
      (
        f: (Env, ExprOptions, Seq[WrapExpr], Res) => Comp[ExprTypeResult]
      )
      : Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =

      def handleArg
        (arg: ArgumentInfo, paramErased: Boolean, paramType: WrapExpr, tailArgs: List[ArgumentInfo], next: Signature[WrapExpr, Res])
        : Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
        arg.arg.check(env, opt.allowErased(paramErased), paramType)
          .foldCauseZIO(
            failure = ZIO.some(_).asLeft,
            success =
              result => {
                val variable = ParameterVariable(owner, convArgs.size, paramType, paramErased, None)
                substituteArg(variable)(result.expr)(sigHandler)(next).flatMap { case (next, resultExpr) =>
                  createSigResult(owner, result.env, opt, next, tailArgs, convArgs :+ resultExpr)(sigHandler)(f)
                }
              },
          )

      def inferArg(paramErased: Boolean, paramType: WrapExpr, next: Signature[WrapExpr, Res]): Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
        UniqueIdentifier.make.flatMap { hole =>
          val variable = ParameterVariable(owner, convArgs.size, paramType, paramErased, None)
          substituteArg(variable)(WrapExpr.OfHole(hole))(sigHandler)(next).flatMap { case (next, resultExpr) =>
            createSigResult(owner, env, opt, next, args, convArgs :+ resultExpr)(sigHandler)(f)
          }
        }

      def resolveReqArg(paramErased: Boolean, paramType: WrapExpr, next: Signature[WrapExpr, Res]): Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
        resolveImplicit(env, paramType, overloadLocation).flatMap {
          case (env, proof) =>
            val variable = ParameterVariable(owner, convArgs.size, paramType, paramErased, None)
            substituteArg(variable)(proof)(sigHandler)(next).flatMap { case (next, resultExpr) =>
              createSigResult(owner, env, opt, next, args, convArgs :+ resultExpr)(sigHandler)(f)
            }
        }

      (args, sig) match {
        case (
          (arg @ ArgumentInfo(_, _, FunctionParameterListType.RequiresList)) :: tailArgs,
          Signature.Parameter(FunctionParameterListType.RequiresList, paramErased, _, paramType, next),
        ) =>
          handleArg(arg, paramErased, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.RequiresList, paramErased, _, paramType, next)) =>
          resolveReqArg(paramErased, paramType, next)

        case (Nil, sig @ Signature.Parameter(_, _, _, _, _)) =>
          def impl(args: Seq[WrapExpr], sig: Signature[WrapExpr, Res]): Comp[ExprTypeResult] =
            sig match {
              case Signature.Parameter(_, isErased, _, paramType, next) =>
                val variable = ParameterVariable(owner, args.size, paramType, false, None)
                for
                  newVarId <- UniqueIdentifier.make
                  newVar = LocalVariable(newVarId, paramType, None, isMutable = false, isErased = isErased)
                  newVarExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(newVar), EmptyTuple))

                  (next, resultExpr) <- substituteArg(variable)(newVarExpr)(sigHandler)(next)

                  inner <- impl(args :+ resultExpr, next)
                yield ExprTypeResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadLambda(newVar), inner.expr)),
                  inner.env,
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionType, (paramType, inner.exprType)))
                )

              case Signature.Result(result) =>
                f(env, opt, args, result)
            }

          ZIO.right(impl(convArgs, sig))

        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.NormalList)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.NormalList, paramErased, _, paramType, next),
            ) =>
          handleArg(arg, paramErased, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.NormalList, _, _, paramType, next)) =>
          ???

        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.InferrableList)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.InferrableList, paramErased, _, paramType, next),
            ) =>
          handleArg(arg, paramErased, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.InferrableList, paramErased, _, paramType, next)) =>
          inferArg(paramErased, paramType, next)

        case (
              (arg @ ArgumentInfo(_, _, FunctionParameterListType.InferrableList2)) :: tailArgs,
              Signature.Parameter(FunctionParameterListType.InferrableList2, paramErased, _, paramType, next),
            ) =>
          handleArg(arg, paramErased, paramType, tailArgs, next)

        case (_, Signature.Parameter(FunctionParameterListType.InferrableList2, paramErased, _, paramType, next)) =>
          inferArg(paramErased, paramType, next)

        case (_, Signature.Result(res)) =>
          ZIO.right(
            f(env, opt, convArgs, res).flatMap { overloadExpr =>
              args.foldLeft(ConstExprFactory(overloadLocation, overloadExpr): ExprFactory)(_.invoke(_))
                .synth(env, opt)
            }
          )
      }

    end createSigResult


    protected def substituteInstanceTypeArgs[Res](sigHandler: SignatureHandlerPlus[?, Res])(callOwnerType: MethodCallOwnerType, sig: Signature[WrapExpr, Res]): Comp[Signature[WrapExpr, Res]] =
      val (owner: ParameterVariableOwner, ownerSigComp, args) = callOwnerType match {
        case MethodCallOwnerType.OwnedByClass(classType) =>
          (
            classType.constructor.arClass: ParameterVariableOwner,
            classType.constructor.arClass.signature.map(convertSig(classSigHandler)): Comp[Signature[WrapExpr, ?]],
            classType.args
          )
        case MethodCallOwnerType.OwnedByTrait(traitType) =>
          (
            traitType.constructor.arTrait: ParameterVariableOwner,
            traitType.constructor.arTrait.signature.map(convertSig(traitSigHandler)): Comp[Signature[WrapExpr, ?]],
            traitType.args
          )
      }

      def getInstanceTypeVars(ownerSig: Signature[WrapExpr, ?], prevVars: Seq[ParameterVariable]): Seq[ParameterVariable] =
        ownerSig match {
          case Signature.Parameter(_, isErased, name, paramType, next) =>
            getInstanceTypeVars(next, prevVars :+ ParameterVariable(owner, prevVars.size, paramType, isErased, name))
          case Signature.Result(resultType) => prevVars
        }

      ownerSigComp.map { ownerSig =>
        val params = getInstanceTypeVars(ownerSig, Seq.empty)
        substituteSignatureMany(params.zip(args).toMap)(sigHandler)(sig)
      }
    end substituteInstanceTypeArgs
  }

  private final class OverloadExprFactory
  (
    protected val overloadLocation: SourceLocation,
    protected val args: Seq[ArgumentInfo],
    protected val lookupName: IdentifierExpr,
  )
      extends OverloadExprFactoryBase {


    override protected def lookup(env: Env, opt: ExprOptions): LookupResult[ScopeElement] =
      checkLookupAccess(opt)(env.scope.lookup(lookupName))

    private def checkLookupAccess(opt: ExprOptions)(res: LookupResult[ScopeElement]): LookupResult[ScopeElement] =
      res match
        case LookupResult.Success(overloads, nextPriority) =>
          LookupResult.Success(
            overloads.filter {
              case elem: ModuleElement[?] => opt.accessToken.canAccessGlobal(elem.accessModifier, elem.module)
              case _: Variable => true
              case _: ParameterVariableElement => true
            },
            checkLookupAccess(opt)(nextPriority)
          )

        case LookupResult.Suspended(suspendedResult) =>
          LookupResult.Suspended(suspendedResult.map(checkLookupAccess(opt)))

        case LookupResult.NotFound() => LookupResult.NotFound()
      end match

    override def mutate(value: MutatorValue): ExprFactory =
      if args.isEmpty then
        new ExprFactory {
          override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
            mutateImpl(value, lookup(env, opt)).flatMap(_.synth(env, opt))

          override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
            mutateImpl(value, lookup(env, opt)).flatMap(_.check(env, opt, t))

        }
      else
        super.mutate(value)

    override def invoke(arg: ArgumentInfo): ExprFactory = OverloadExprFactory(overloadLocation, args :+ arg, lookupName)

    protected override type TElement = ScopeElement

    protected override def signatureOf(element: ScopeElement): Comp[Signature[WrapExpr, ?]] =
      element match {
        case _: Variable =>
          ZIO.succeed(Signature.Result(()))

        case _: ParameterVariableElement =>
          ZIO.succeed(Signature.Result(()))

        case ModuleElementC.ClassElement(arClass) =>
          arClass.signature.map(convertSig(classSigHandler))

        case ModuleElementC.TraitElement(arTrait) =>
          arTrait.signature.map(convertSig(traitSigHandler))

        case ModuleElementC.FunctionElement(arFunc) =>
          arFunc.signature.map(convertSig(functionSigHandler))

        case ModuleElementC.ExportedElement(inner) =>
          signatureOf(inner)
      }

    protected override def checkOverload(env: Env, opt: ExprOptions)(overload: ScopeElement): Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
      ZIO.logTrace(s"checkOverload $overload") *>
        (overload match {
          case variable: Variable =>
            val varFactory: ExprFactory = new ExprFactorySynth {
              override protected def location: SourceLocation = overloadLocation

              override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
                if !opt.checkErasure(variable.isErased) then
                  ZIO.fail(DiagnosticError.ErasedExpressionNotAllowed(DiagnosticSource.Location(overloadLocation)))
                else if !opt.checkPurity(!variable.isMutable) then
                  ZIO.fail(DiagnosticError.Purity(DiagnosticSource.Location(overloadLocation)))
                else
                  ZIO.succeed(ExprTypeResult(
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple)),
                    env,
                    variable.varType,
                  ))
            }

            ZIO.right(
              args.foldLeft(varFactory) { (varFactory, arg) => varFactory.invoke(arg) }
                .synth(env, opt)
            )

          case parameterVariableElement: ParameterVariableElement =>
            ZIO.right(ZIO.succeed(ExprTypeResult(
              WrapExpr.OfExpr(ArExpr(
                ExprConstructor.LoadTupleElement(parameterVariableElement.index),
                WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(parameterVariableElement.paramVar), EmptyTuple))
              )),
              env,
              parameterVariableElement.elementType
            )))

          case ModuleElementC.ClassElement(arClass) =>
            createSigResultConv(arClass, env, opt, arClass.signature, args)(classSigHandler) {
              case (env, opt, args, ClassResult(typeOfClassType, _, _)) =>
                ZIO.succeed(ExprTypeResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(arClass), args)),
                  env,
                  typeOfClassType,
                ))
            }

          case ModuleElementC.TraitElement(arTrait) =>
            createSigResultConv(arTrait, env, opt, arTrait.signature, args)(traitSigHandler) {
              case (env, opt, args, TraitResult(typeOfTraitType, _)) =>
                ZIO.succeed(ExprTypeResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(arTrait), args)),
                  env,
                  typeOfTraitType,
                ))
            }

          case ModuleElementC.FunctionElement(func) =>
            createSigResultConv(func, env, opt, func.signature, args)(functionSigHandler) { (env, opt, args, res) =>
              if opt.checkPurity(func.purity) then
                for
                  env <- ZIO.foldLeft(res.ensuresClauses)(env) { (env, ensuresType) =>
                    for
                      varId <- UniqueIdentifier.make
                      local = LocalVariable(varId, ensuresType, name = None, isMutable = false, isErased = false)
                    yield env.withImplicitSource(_.addVariable(local))
                  }
                yield ExprTypeResult(
                  WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionCall(func), args)),
                  env,
                  res.returnType,
                )
              else
                ZIO.fail(DiagnosticError.Purity(DiagnosticSource.Location(overloadLocation)))
            }

          case ModuleElementC.ExportedElement(inner) =>
            checkOverload(env, opt)(inner)

        })

    protected def createSigResultConv[Res1, Res2]
      (
        owner: exprContext.ParameterVariableOwner,
        env: Env,
        opt: ExprOptions,
        sig: Comp[Signature[context.ExprContext.WrapExpr, Res1]],
        args: Seq[ArgumentInfo],
      )
      (
        sigHandler: SignatureHandlerPlus[Res1, Res2]
      )
      (
        f: (Env, ExprOptions, Seq[WrapExpr], Res2) => Comp[ExprTypeResult]
      )
      : Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
      sig.flatMap { sig =>
        createSigResult(owner, env, opt, convertSig(sigHandler)(sig), args.toList, Vector.empty)(sigHandler)(f)
      }

    private def mutateImpl(value: MutatorValue, lookup: LookupResult[ScopeElement]): Comp[ExprFactory] =
      lookup match {
        case LookupResult.Success(Seq(variable: Variable), _) if variable.isMutable =>
          ZIO.succeed(MutateVariableExprFactory(variable, value))

        case LookupResult.Success(Seq(_), _) => ZIO.fail(DiagnosticError.CanNotMutate(DiagnosticSource.Location(value.location)))

        case LookupResult.Success(Seq(), next) => mutateImpl(value, next)

        case LookupResult.Success(_, _) => ZIO.fail(DiagnosticError.AmbiguousOverload())

        case LookupResult.Suspended(suspended) => suspended.flatMap(mutateImpl(value, _))

        case LookupResult.NotFound() => ZIO.fail(DiagnosticError.LookupFailed(DiagnosticSource.Location(overloadLocation), lookupName))
      }

    private final class MutateVariableExprFactory(variable: Variable, value: MutatorValue) extends ExprFactorySynth {

      override protected def location: SourceLocation = overloadLocation

      override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
        for {
          _ <- ZIO.fail(DiagnosticError.Purity(DiagnosticSource.Location(overloadLocation))).unless(opt.checkPurity(false))
          valueExpr <- value.arg.check(env, opt, variable.varType)
          e = WrapExpr.OfExpr(ArExpr(ExprConstructor.StoreVariable(variable), valueExpr.expr))
        } yield ExprTypeResult(e, valueExpr.env, unitType)

    }

  }

  private final class OverloadExprFactoryMethod private (
    instance: WrapExpr,
    instanceType: WrapExpr,
    memberName: IdentifierExpr,
    protected val overloadLocation: SourceLocation,
    protected val args: Seq[ArgumentInfo],
  ) extends OverloadExprFactoryBase {

    import ExprConstructor.MethodCallOwnerType

    protected override type TElement = MethodElement

    enum MethodElement {
      case Method(method: ArMethod, callOwnerType: MethodCallOwnerType)
      case FunctionObjectApply(argType: WrapExpr, resultType: WrapExpr)
    }


    override protected def lookupName: IdentifierExpr = memberName

    private enum InstanceType {
      case ByClass(c: ArExpr[ExprConstructor.ClassType])
      case ByTrait(t: ArExpr[ExprConstructor.TraitType])
      case ByClassStatic(c: ArExpr[ExprConstructor.ClassType])
      case ByTraitStatic(t: ArExpr[ExprConstructor.TraitType])

      def toAccessType: AccessToken.AccessType =
        this match {
          case ByClass(c) => c.constructor.arClass
          case ByTrait(t) => t.constructor.arTrait
          case ByClassStatic(c) => c.constructor.arClass
          case ByTraitStatic(t) => t.constructor.arTrait
        }
    }

    protected override def lookup(env: Env, opt: ExprOptions): LookupResult[MethodElement] =
      LookupResult.Suspended(
        evaluator.normalizeTopLevelWrap(instance, fuel)
          .flatMap {
            case WrapExpr.OfExpr(normalizedInstance) =>
              normalizedInstance.constructor match {
                case ctor: (normalizedInstance.constructor.type & ExprConstructor.ClassType) =>
                  lookupMethods(
                    opt.accessToken,
                    Seq(ctor.arClass),
                    Seq(InstanceType.ByClassStatic(ArExpr(ctor, normalizedInstance.getArgs(ctor)))),
                    Set.empty,
                    Set.empty,
                  )

                case ctor: (normalizedInstance.constructor.type & ExprConstructor.TraitType) =>
                  lookupMethods(
                    opt.accessToken,
                    Seq(ctor.arTrait),
                    Seq(InstanceType.ByTraitStatic(ArExpr(ctor, normalizedInstance.getArgs(ctor)))),
                    Set.empty,
                    Set.empty,
                  )

                case _ =>
                  evaluator.normalizeTopLevelWrap(instanceType, fuel)
                    .map {
                      case WrapExpr.OfExpr(normalizedInstanceType) =>
                        normalizedInstanceType.constructor match {
                          case ctor: (normalizedInstanceType.constructor.type & ExprConstructor.FunctionType.type) =>
                            if memberName == IdentifierExpr.Named("apply") then
                              val (argType, resType) = normalizedInstanceType.getArgs(ctor)
                              Some(LookupResult.Success(Seq(MethodElement.FunctionObjectApply(argType, resType)), LookupResult.NotFound()))
                            else
                              None

                          case _ => None
                        }
                      case WrapExpr.OfHole(_) => None
                    }
                    .flatMap {
                      case Some(res) => ZIO.succeed(res)
                      case None =>
                        getInstanceType(instanceType).flatMap { types =>
                          val instanceTypes = types.map { _.toAccessType }
                          lookupMethods(opt.accessToken, instanceTypes, types, Set.empty, Set.empty)
                        }
                    }
              }

            case WrapExpr.OfHole(_) => ZIO.succeed(LookupResult.NotFound())
          }
      )

    protected override def signatureOf(element: MethodElement): Comp[Signature[WrapExpr, ?]] =
      element match {
        case MethodElement.Method(method, _) =>
          method.signatureUnsubstituted.map(convertSig(functionSigHandler))

        case MethodElement.FunctionObjectApply(argType, resultType) =>
          ZIO.succeed(Signature.Parameter(
            FunctionParameterListType.NormalList,
            isErased = false,
            name = None,
            paramType = argType,
            next = Signature.Result(resultType)
          ))
      }

    protected override def checkOverload(env: Env, opt: ExprOptions)(overload: MethodElement): Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
      ZIO.logTrace(s"checkOverload method ${overload}") *> {
        overload match {
          case MethodElement.Method(method, callOwnerType) =>
            for
              sig <- method.signatureUnsubstituted
              thisVar = InstanceVariable(method, instanceType, None)
              sig <- ZIO.succeed(convertSig(functionSigHandler)(sig))
              sig <- substituteInstanceTypeArgs(functionSigHandler)(callOwnerType, sig)
              (sig, stableInstance) <- substituteArg(thisVar)(instance)(functionSigHandler)(sig)
              result <- createSigResult(method, env, opt, sig, args.toList, Vector.empty)(functionSigHandler) {
                (env, opt, args, res) =>
                  if opt.checkPurity(method.purity) then
                    for
                      env <- ZIO.foldLeft(res.ensuresClauses)(env) { (env, ensuresType) =>
                        for
                          varId <- UniqueIdentifier.make
                          local = LocalVariable(varId, ensuresType, name = None, isMutable = false, isErased = false)
                        yield env.withImplicitSource(_.addVariable(local))
                      }
                    yield ExprTypeResult(
                      WrapExpr.OfExpr(ArExpr(ExprConstructor.MethodCall(method), (stableInstance, callOwnerType, args))),
                      env,
                      res.returnType,
                    )
                  else
                    ZIO.fail(DiagnosticError.Purity(DiagnosticSource.Location(overloadLocation)))
              }
            yield result

          case MethodElement.FunctionObjectApply(argType, resultType) =>
            args match {
              case Seq(arg) =>
                ZIO.right(
                  for
                    argExprRes <- arg.arg.check(env, opt, argType)
                  yield ExprTypeResult(
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionObjectCall, (instance, argExprRes.expr))),
                    argExprRes.env,
                    resultType
                  )
                )

              case _ => ZIO.left(None)
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
              ZIO.succeed(Seq(InstanceType.ByClass(ArExpr(ctor, normalizedType.getArgs(ctor)))))

            case ctor: (normalizedType.constructor.type & ExprConstructor.TraitType) =>
              ZIO.succeed(Seq(InstanceType.ByTrait(ArExpr(ctor, normalizedType.getArgs(ctor)))))

            case _ => ZIO.succeed(Seq.empty)
          }

        case _ => ZIO.succeed(Seq.empty)
      }

    private def methodsOfType(t: InstanceType): Comp[Seq[(ArMethod, MethodCallOwnerType)]] =
      t match {
        case InstanceType.ByClass(classType) =>
          classType.constructor.arClass.methods
            .map { _.getOrElse(Some(memberName), Seq.empty) }
            .map { _.map { method => (method, MethodCallOwnerType.OwnedByClass(classType)) } }

        case InstanceType.ByClassStatic(classType) =>
          classType.constructor.arClass.staticMethods
            .map { _.getOrElse(Some(memberName), Seq.empty) }
            .map { _.map { method => (method, MethodCallOwnerType.OwnedByClass(classType)) } }

        case InstanceType.ByTrait(traitType) =>
          traitType.constructor.arTrait.methods
            .map { _.getOrElse(Some(memberName), Seq.empty) }
            .map { _.map { method => (method, MethodCallOwnerType.OwnedByTrait(traitType)) } }

        case InstanceType.ByTraitStatic(traitType) =>
          traitType.constructor.arTrait.staticMethods
            .map { _.getOrElse(Some(memberName), Seq.empty) }
            .map { _.map { method => (method, MethodCallOwnerType.OwnedByTrait(traitType)) } }
      }

    private def getBaseTypes(t: InstanceType): Comp[Seq[InstanceType]] =
      t match {
        case InstanceType.ByClass(classType) =>
          classType.constructor.arClass.signature
            .map(convertSig(classSigHandler))
            .flatMap(substituedSigResult(classType.constructor.arClass)(classSigHandler)(_, classType.args.toList))
            .flatMap { case ClassResult(_, baseClass, baseTraits) =>
              for
                baseClass <- baseClass
                baseTraits <- baseTraits
              yield baseClass.map(InstanceType.ByClass.apply).toSeq ++ baseTraits.map(InstanceType.ByTrait.apply)
            }

        case InstanceType.ByTrait(traitType) =>
          traitType.constructor.arTrait.signature
            .map(convertSig(traitSigHandler))
            .flatMap(substituedSigResult(traitType.constructor.arTrait)(traitSigHandler)(_, traitType.args.toList))
            .flatMap { case TraitResult(_, baseTraits) =>
              baseTraits.map { _.map(InstanceType.ByTrait.apply) }
            }

        case _ => ZIO.succeed(Seq.empty)
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

    private def lookupMethods
    (
      callerAccess: AccessToken,
      instanceTypes: Seq[AccessToken.AccessType],
      types: Seq[InstanceType],
      seenTypes: Set[ArClass | ArTrait],
      seenMethods: Set[ArMethod]
    )
    : Comp[LookupResult[MethodElement]] =
      if types.isEmpty then
        ZIO.succeed(LookupResult.NotFound())
      else
        ZIO.foreach(types)(methodsOfType)
          .flatMap { methods =>
            val methods2 = methods.flatten
              .filterNot { case (method, _) => seenMethods.contains(method) }
              .distinct

            ZIO.filter(methods2) { case (method, methodOwner) =>
              callerAccess.canAccessMember(
                ArMethodC.getAccessModifier(method.owner),
                instanceTypes,
                methodOwner.toAccessType
              )
            }
          }
          .map { methods =>
            val next =
              LookupResult.Suspended(
                ZIO.foreach(types)(getBaseTypes).flatMap { baseTypes =>
                  val seenTypes2 = seenTypes ++ types.flatMap(toSeenTypes)
                  val baseTypes2 = baseTypes.flatten.filterNot(isSeenType(seenTypes2))
                  val seenMethods2 = seenMethods ++ methods.map { case (method, _) => method }
                  lookupMethods(callerAccess, instanceTypes, baseTypes2, seenTypes2, seenMethods2)
                }
              )

            LookupResult.Success(
              methods.map { MethodElement.Method(_, _) },
              next,
            )
          }

  }

  object OverloadExprFactoryMethod {

    private final class WrappedMethodOverloadFactory
    (
      instance: ExprFactory,
      memberName: IdentifierExpr,
      memberNameLocation: SourceLocation,
      args: Seq[ArgumentInfo]
    )
        extends ExprFactory {

      override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] =
        instance.synth(env, opt).flatMap { instanceExpr =>
          OverloadExprFactoryMethod(instanceExpr.expr, instanceExpr.exprType, memberName, memberNameLocation, args).synth(instanceExpr.env, opt)
        }

      override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] =
        instance.synth(env, opt).flatMap { instanceExpr =>
          OverloadExprFactoryMethod(instanceExpr.expr, instanceExpr.exprType, memberName, memberNameLocation, args).check(
            instanceExpr.env,
            opt,
            t,
          )
        }

      override def mutate(value: MutatorValue): ExprFactory =
        EndOverloadExprFactory(
          WrappedMethodOverloadFactory(
            instance,
            IdentifierExpr.Update(memberName),
            memberNameLocation,
            args :+ ArgumentInfo(value.arg, value.location, FunctionParameterListType.NormalList),
          )
        )

      override def invoke(arg: ArgumentInfo): ExprFactory =
        WrappedMethodOverloadFactory(instance, memberName, memberNameLocation, args :+ arg)

    }

    def fromInstanceFactory(instance: ExprFactory, memberName: IdentifierExpr, memberNameLocation: SourceLocation): ExprFactory =
      WrappedMethodOverloadFactory(instance, memberName, memberNameLocation, Seq.empty)

  }

  private final class OverloadExprFactoryClassConstructor
  (
    instance: WrapExpr,
    protected val overloadLocation: SourceLocation,
    protected val args: Seq[ArgumentInfo],
  ) extends OverloadExprFactoryBase:

    import ExprConstructor.MethodCallOwnerType

    protected override type TElement = (ClassConstructor, ArExpr[ExprConstructor.ClassType])


    override protected def lookupName: parser.ClassConstructorExpr.type = parser.ClassConstructorExpr

    protected override def lookup(env: Env, opt: ExprOptions): LookupResult[TElement] =
      LookupResult.Suspended(
        evaluator.normalizeTopLevelWrap(instance, fuel)
          .flatMap {
            case WrapExpr.OfExpr(normalizedInstance) =>
              normalizedInstance.constructor match {
                case ctor: (normalizedInstance.constructor.type & ExprConstructor.ClassType) =>
                  ctor.arClass.constructors.map { classCtors =>
                    LookupResult.Suspended(
                      for
                        _ <- ZIO.fail(DiagnosticError.AbstractClassConstructorCalled(DiagnosticSource.Location(overloadLocation))).when(!opt.allowAbstractConstructorCall && ctor.arClass.isAbstract)

                        classCtors <- ZIO.filter(classCtors) { classCtor =>
                          opt.accessToken.canAccessMember(
                            classCtor.owner.accessModifier,
                            Seq(ctor.arClass),
                            ctor.arClass,
                          )
                        }
                      yield LookupResult.Success(
                        classCtors.map { classCtor => (classCtor, ArExpr(ctor, normalizedInstance.getArgs(ctor))) },
                        LookupResult.NotFound(),
                      )
                    )
                  }

                case _ => ???
              }

            case _ => ???
          }
      )

    protected override def signatureOf(element: TElement): Comp[Signature[WrapExpr, ?]] =
      element._1.signatureUnsubstituted.map(convertSig(classConstructorSigHandler))

    protected override def checkOverload(env: Env, opt: ExprOptions)(overload: TElement): Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]] =
      ZIO.logTrace(s"checkOverload method ${overload}") *> {
        val (classCtor, classType) = overload
        (
          for
            sig <- classCtor.signatureUnsubstituted
            sig <- ZIO.succeed(convertSig(classConstructorSigHandler)(sig))
            sig <- substituteInstanceTypeArgs(classConstructorSigHandler)(MethodCallOwnerType.OwnedByClass(classType), sig)

            result <- createSigResult(classCtor, env, opt, sig, args.toList, Vector.empty)(classConstructorSigHandler) {
              (env, opt, args, _) =>
                if opt.checkPurity(classCtor.purity) then
                  ZIO.succeed(ExprTypeResult(
                    WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassConstructorCall(classCtor), (classType, args))),
                    env,
                    WrapExpr.OfExpr(classType),
                  ))
                else
                  ZIO.fail(DiagnosticError.Purity(DiagnosticSource.Location(overloadLocation)))
            }
          yield result
        ) : Comp[Either[Option[Cause[context.Error]], Comp[ExprTypeResult]]]
      }

    override def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadExprFactoryClassConstructor(instance, overloadLocation, args :+ arg)

  end OverloadExprFactoryClassConstructor

  private final class EndOverloadExprFactory(inner: ExprFactory) extends ExprFactory {
    override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] = inner.synth(env, opt)
    override def check(env: Env, opt: ExprOptions, t: WrapExpr): Comp[ExprResult] = inner.check(env, opt, t)
  }

  private final class ConstExprFactory(protected val location: SourceLocation, result: ExprTypeResult) extends ExprFactorySynth {
    override def synth(env: Env, opt: ExprOptions): Comp[ExprTypeResult] = ZIO.succeed(result)
  }


}

object ExpressionConverter {

  def make(ctx: Context, importer: TubeImporter & HasContext[ctx.type]): UIO[ExpressionConverter & HasContext[ctx.type]] =
    ZIO.succeed(
      new ExpressionConverter {
        override val context: ctx.type = ctx
        override val tubeImporter: TubeImporter & HasContext[ctx.type] = importer
      }
    )

}
