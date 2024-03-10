package dev.argon.compiler

import dev.argon.util.{*, given}
import dev.argon.ast
import dev.argon.ast.{FunctionParameterListType, IdentifierExpr}
import dev.argon.expr.{BinaryBuiltin, ContextShifter, Evaluator, ExprContext, NullaryBuiltin, Substitution, UnaryBuiltin, Unification}
import dev.argon.util.{FilePosition, Location, UniqueIdentifier, WithLocation, WithSource}
import zio.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given
import zio.stream.ZStream

trait TypeResolver extends UsingContext {

  import context.{TRExprContext, TRSignatureContext, Scopes}
  import Scopes.{LookupResult, Overloadable}

  import TRExprContext.{Expr, Builtin, LocalVar, Var, Hole}
  private type Loc = Location[FilePosition]



  private final case class EmitState(
    model: Ref[TRExprContext.Model],
    scope: Scopes.LocalScope,
  )

  final def typeCheckExpr(scope: Scopes.Scope)(e: WithSource[ast.Expr], t: context.DefaultExprContext.Expr): Comp[context.DefaultExprContext.Expr] =
    for
      scope <- Scopes.LocalScope.make(scope)
      model <- Ref.make[TRExprContext.Model](TRExprContext.Model.empty)
      expr <- resolveExpr(e).check(DefaultToTRShifter[context.type](context).shiftExpr(t))(using EmitState(model, scope))
      model <- model.get
      shifted <- ResolvedHoleFiller(model).shiftExpr(expr)
    yield shifted

  final def typeCheckTypeExpr(scope: Scopes.Scope)(e: WithSource[ast.Expr]): Comp[context.DefaultExprContext.Expr] =
    for
      scope <- Scopes.LocalScope.make(scope)
      model <- Ref.make[TRExprContext.Model](TRExprContext.Model.empty)
      expr <- resolveType(e)(using EmitState(model, scope))
      model <- model.get
      shifted <- ResolvedHoleFiller(model).shiftExpr(expr)
    yield shifted

  private def normalizeToValue(e: Expr)(using EmitState): Comp[Expr] =
    TREvaluator().normalizeToValue(e, context.Config.fuel)

  private def unifyExpr(a: Expr, b: Expr)(using state: EmitState): Comp[Boolean] =
    Unification.unify[context.Env, context.Error](TRExprContext)(state.model, TREvaluator(), context.Config.fuel)(a, b)

  private def nestedScope[A](using state: EmitState)(f: EmitState ?=> Comp[A]): Comp[A] =
    for
      innerScope <- Scopes.LocalScope.make(state.scope)
      res <- f(using state.copy(scope = innerScope))
    yield res


  private def makeHole: Comp[Expr] = UniqueIdentifier.make.map(Expr.Hole.apply)

  private lazy val stringType: Expr = Expr.Builtin(Builtin.Nullary(NullaryBuiltin.StringType))
  private lazy val intType: Expr = Expr.Builtin(Builtin.Nullary(NullaryBuiltin.IntType))
  private lazy val boolType: Expr = Expr.Builtin(Builtin.Nullary(NullaryBuiltin.BoolType))
  private lazy val unitType: Expr = Expr.Tuple(Seq.empty)

  private abstract class ExprFactory {
    def loc: Loc

    def check(t: Expr)(using EmitState): Comp[Expr]
    def invoke(arg: ArgumentInfo): ExprFactory =
      new ExprFactory {
        override def loc: Loc = arg.callLocation

        override def check(t: Expr)(using EmitState): Comp[Expr] =
          for
            argType <- makeHole
            funcType = Expr.FunctionType(argType, t)
            f <- ExprFactory.this.check(funcType)
            arg <- arg.arg.check(argType)
          yield Expr.FunctionObjectCall(f, arg)
      }

    def assign(assignedValue: AssignedValue): ExprFactory =
      ErrorFactory(loc, CompilerError.InvalidAssignmentTarget(loc))
  }

  private abstract class InferFactory extends ExprFactory {
    def infer(using EmitState): Comp[InferredExpr]

    override final def check(t: Expr)(using EmitState): Comp[Expr] =
      infer.flatMap {
        case InferredExpr(e, inferredType) =>
          ErrorLog.logError(CompilerError.TypeError(
            loc,
            expected = TRToErrorShifter[context.type](context).shiftExpr(t),
            actual = TRToErrorShifter[context.type](context).shiftExpr(inferredType),
          ))
            .whenZIO(unifyExpr(t, inferredType).negate)
            .as(e)
      }
  }

  private final case class InferredExpr(e: Expr, t: Expr)

  private final case class ArgumentInfo(
    callLocation: Loc,
    arg: ExprFactory,
    listType: FunctionParameterListType,
  )

  private final case class AssignedValue(
    assignLocation: Loc,
    value: ExprFactory,
  )

  private def resolveStmtBlock(block: WithSource[Seq[WithSource[ast.Stmt]]]): ExprFactory =
    new ExprFactory {
      override def loc: Loc = block.location

      override def check(t: Expr)(using EmitState): Comp[Expr] =
        block.value match {
          case init :+ last =>
            for
              initExprs <- ZIO.foreach(init) { stmt => resolveStmt(stmt).check(Expr.Tuple(Seq.empty)) }
              lastExpr <- resolveStmt(last).check(t)
            yield Expr.Sequence(initExprs, lastExpr)

          case _ =>
            new InferFactory {
              override def loc: Loc = block.location

              override def infer(using EmitState): Comp[InferredExpr] =
                ZIO.succeed(InferredExpr(
                  Expr.Tuple(Seq.empty),
                  Expr.Tuple(Seq.empty),
                ))
            }.check(t)
        }
    }

  private def resolveStmt(stmt: WithSource[ast.Stmt]): ExprFactory =
    stmt.value match {
      case expr: ast.Expr => resolveExpr(WithLocation(expr, stmt.location))
      case decl: ast.VariableDeclarationStmt =>
        new InferFactory {
          override def loc: Loc = stmt.location

          override def infer(using state: EmitState): Comp[InferredExpr] =
            for
              isErased <- Ref.make(false)
              isProof <- Ref.make(false)
              _ <- ModifierParser()
                .withErased(isErased)
                .withProof(isProof)
                .parse(decl.modifiers)
              isErased <- isErased.get
              isProof <- isProof.get

              id <- UniqueIdentifier.make
              varType <- decl.varType.fold(makeHole)(resolveType)
              value <- resolveExpr(decl.value).check(varType)

              localVar = LocalVar(
                id,
                varType,
                decl.name,
                decl.isMutable,
                isErased = isErased,
                isProof = isProof,
              )

              _ <- state.scope.addVariable(localVar)

            yield InferredExpr(
              Expr.BindVariable(
                localVar,
                value,
              ),
              unitType,
            )
          end infer

        }

      case _ => ???
    }


  private def resolveExpr(expr: WithSource[ast.Expr]): ExprFactory =
    expr.value match {
      case ast.Expr.As(value, valueType) =>
        new InferFactory {
          override def loc: Loc = expr.location

          override def infer(using EmitState): Comp[InferredExpr] =
            for
              t <- resolveType(valueType)
              e <- resolveExpr(value).check(t)
            yield InferredExpr(e, t)
        }

      case ast.Expr.BinaryOperation(left, ast.BinaryOperator.Assign, right) =>
        resolveExpr(left).assign(AssignedValue(expr.location, resolveExpr(right)))

      case ast.Expr.BinaryOperation(
        left,
        op @ (
          ast.BinaryOperator.Plus | ast.BinaryOperator.Minus |ast.BinaryOperator.Mul | ast.BinaryOperator.Div |
          ast.BinaryOperator.Equal | ast.BinaryOperator.NotEqual |
          ast.BinaryOperator.LessThan | ast.BinaryOperator.LessThanEq |
          ast.BinaryOperator.GreaterThan | ast.BinaryOperator.GreaterThanEq |
          ast.BinaryOperator.BitOr | ast.BinaryOperator.BitAnd | ast.BinaryOperator.BitXOr |
          ast.BinaryOperator.ShiftLeft | ast.BinaryOperator.ShiftRight |
          ast.BinaryOperator.Concat
        ),
        right,
      ) =>
        LookupIdFactory(expr.location, IdentifierExpr.Op(op))
          .invoke(ArgumentInfo(
            callLocation = expr.location,
            arg = resolveExpr(left),
            listType = FunctionParameterListType.NormalList,
          ))
          .invoke(ArgumentInfo(
            callLocation = expr.location.copy(start = left.location.start),
            arg = resolveExpr(right),
            listType = FunctionParameterListType.NormalList,
          ))

      case ast.Expr.Block(body) =>
        resolveStmtBlock(body)

      case ast.Expr.BoolLiteral(b) =>
        new InferFactory {
          override def loc: Loc = expr.location
          override def infer(using state: EmitState): Comp[InferredExpr] =
            ZIO.succeed(InferredExpr(
              Expr.BoolLiteral(b),
              boolType,
            ))
        }

      case ast.Expr.Builtin(name) =>
        builtinExprFactory(expr.location, name)

      case ast.Expr.FunctionCall(f, listType, arg) =>
        resolveExpr(f).invoke(ArgumentInfo(
          callLocation = expr.location,
          arg = resolveExpr(arg),
          listType = listType,
        ))

      case id: IdentifierExpr =>
        LookupIdFactory(expr.location, id)

      case ast.Expr.IntLiteral(i) =>
        new InferFactory {
          override def loc: Loc = expr.location
          override def infer(using state: EmitState): Comp[InferredExpr] =
            ZIO.succeed(InferredExpr(
              Expr.IntLiteral(i),
              intType,
            ))
        }

      case ast.Expr.StringLiteral(parts) =>
        parts
          .map {
            case ast.StringFragment.Text(s) =>
              new InferFactory {
                override def loc: Loc = expr.location

                override def infer(using state: EmitState): Comp[InferredExpr] =
                  ZIO.succeed(InferredExpr(
                    Expr.StringLiteral(s),
                    stringType,
                  ))
              }

            case ast.StringFragment.Interpolate(_, _) => ???
          }
          .reduceLeft((a, b) =>
            new InferFactory {
              override def loc: Loc = a.loc.copy(end = b.loc.end)

              override def infer(using state: EmitState): Comp[InferredExpr] =
                for
                  aExpr <- a.check(stringType)
                  bExpr <- a.check(stringType)
                yield InferredExpr(
                  Expr.Builtin(Builtin.Binary(BinaryBuiltin.StringConcat, aExpr, bExpr)),
                  stringType,
                )
            }
          )

      case ast.Expr.Tuple(items) =>
        TupleExprFactory(expr.location, items.map(resolveExpr))

      case ast.Expr.Type(None) =>
        new InferFactory {
          override def loc: Loc = expr.location

          override def infer(using EmitState): Comp[InferredExpr] =
            ZIO.succeed(InferredExpr(
              Expr.TypeN(Expr.IntLiteral(0)),
              Expr.TypeN(Expr.IntLiteral(1)),
            ))
        }

      case ast.Expr.BigType(n) =>
        new InferFactory {
          override def loc: Loc = expr.location

          override def infer(using state: EmitState): Comp[InferredExpr] =
            ZIO.succeed(InferredExpr(
              Expr.TypeBigN(n),
              Expr.TypeBigN(n + 1),
            ))
        }

      case _ =>
        println(expr.value.getClass)
        ???
    }

  private def resolveType(expr: WithSource[ast.Expr])(using EmitState): Comp[Expr] =
    def isValidTypeOfType(t: Expr): Comp[Boolean] =
      normalizeToValue(t).flatMap {
        case Expr.Hole(h) => ZIO.succeed(true) // Assume the hole will be filled with a type
        case Expr.Error() => ZIO.succeed(false)

        case Expr.TypeN(_) | Expr.TypeBigN(_) => ZIO.succeed(true)

        case Expr.Tuple(items) =>
          ZIO.forall(items)(isValidTypeOfType)

        case _ => ZIO.succeed(false)
      }

    for
      t <- makeHole
      e <- resolveExpr(expr).check(t)
      _ <- ErrorLog.logError(CompilerError.InvalidTypeError(
        expr.location,
        expr = TRToErrorShifter[context.type](context).shiftExpr(e),
        exprType = TRToErrorShifter[context.type](context).shiftExpr(t),
      )).whenZIO(isValidTypeOfType(t).negate)
    yield e
  end resolveType


  private def builtinExprFactory(loc: Loc, name: String): ExprFactory =
    name match {
      case "int_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.IntType, Expr.TypeN(Expr.IntLiteral(0)))
      case "bool_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.BoolType, Expr.TypeN(Expr.IntLiteral(0)))
      case "string_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.StringType, Expr.TypeN(Expr.IntLiteral(0)))
      case "never_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.NeverType, Expr.TypeN(Expr.IntLiteral(0)))
  
      case "int_negate" => UnaryBuiltinFactory(loc, UnaryBuiltin.IntNegate, intType, intType)
  
      case "conjunction_type" => ???
      case "disjunction_type" => ???
      case "int_add" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntAdd, intType, intType, intType)
      case "int_sub" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntSub, intType, intType, intType)
      case "int_mul" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntMul, intType, intType, intType)
      case "int_eq" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntEQ, boolType, intType, intType)
      case "int_ne" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntNE, boolType, intType, intType)
      case "int_lt" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntLT, boolType, intType, intType)
      case "int_le" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntLE, boolType, intType, intType)
      case "int_gt" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntGT, boolType, intType, intType)
      case "int_ge" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntGE, boolType, intType, intType)
      case "string_concat" => BinaryBuiltinFactory(loc, BinaryBuiltin.StringConcat, stringType, stringType, stringType)
  
      case "equal_to_type" => ???
      case _ => ???
    }



  private final class ErrorFactory(override val loc: Loc, error: => CompilerError) extends ExprFactory {
    override def check(t: Expr)(using EmitState): Comp[Expr] =
      ErrorLog.logError(error).as(Expr.Error())
  }

  private abstract class WrappedFactory extends ExprFactory {

    protected def unwrap(using EmitState): Comp[ExprFactory]

    override def check(t: Expr)(using EmitState): Comp[Expr] =
      unwrap.flatMap(_.check(t))

    override def invoke(arg: ArgumentInfo): ExprFactory =
      new WrappedFactory {
        override def loc: Loc = arg.callLocation

        override protected def unwrap(using EmitState): Comp[ExprFactory] =
          WrappedFactory.this.unwrap.map(_.invoke(arg))
      }

    override def assign(assignedValue: AssignedValue): ExprFactory =
      new WrappedFactory {
        override def loc: Loc = assignedValue.assignLocation

        override protected def unwrap(using EmitState): Comp[ExprFactory] =
          WrappedFactory.this.unwrap.map(_.assign(assignedValue))
      }
  }

  private final class LookupIdFactory(override val loc: Loc, id: IdentifierExpr) extends WrappedFactory {
    override protected def unwrap(using state: EmitState): Comp[ExprFactory] =
      state.scope.lookup(id).map {
        case LookupResult.NotFound() =>
          ErrorFactory(loc, CompilerError.UnknownIdentifier(loc, id))

        case LookupResult.Variable(v) =>
          new InferFactory {
            override def loc: Loc = LookupIdFactory.this.loc

            override def infer(using EmitState): Comp[InferredExpr] =
              ZIO.succeed(InferredExpr(
                Expr.Variable(v),
                v.varType,
              ))
          }

        case LookupResult.VariableTupleElement(v, index, t) =>
          new InferFactory {
            override def loc: Loc = LookupIdFactory.this.loc

            override def infer(using EmitState): Comp[InferredExpr] =
              ZIO.succeed(InferredExpr(
                Expr.TupleElement(index, Expr.Variable(v)),
                t,
              ))
          }


        case lookup: LookupResult.Overloaded =>
          OverloadedExprFactory(loc, lookup, Seq())
      }
  }

  private final class OverloadedExprFactory(override val loc: Loc, lookup: LookupResult.Overloaded, args: Seq[ArgumentInfo]) extends ExprFactory {
    private def toAttemptedOverload(overload: Overloadable): AttemptedOverload =
      overload match {
        case Overloadable.Function(f) => AttemptedOverload.Function(f)
      }

    override def check(t: Expr)(using EmitState): Comp[Expr] =
      Ref.make(Seq.empty[Overloadable]).flatMap { rejectedOverloads =>
        val resolver = OverloadResolver(rejectedOverloads)

        def lookupOverloads(lookup: LookupResult.OverloadableOnly): ZStream[context.Env, context.Error, Seq[Overloadable]] =
          lookup match {
            case LookupResult.NotFound() => ZStream.empty
            case LookupResult.Overloaded(overloads, next) => ZStream(overloads) ++ ZStream.unwrap(next.map(lookupOverloads))
          }


        lookupOverloads(lookup)
          .flatMap { overloads => resolver.groupOverloads(overloads, args) }
          .mapZIO { overloads =>
            ZIO.foreach(overloads) { overload =>
              resolver.attemptOverload(overload, args).flatMap {
                case success: OverloadAttempt.Success =>
                  ZIO.succeed(Seq((overload, success)))

                case failure: OverloadAttempt.Failure =>
                  rejectedOverloads.update(_ :+ overload).as(Seq.empty)

              }
            }.map(_.flatten)
          }
          .filter(_.nonEmpty)
          .runHead
          .flatMap {
            case Some(overloads @ ((selectedOverload, overloadResult) +: _)) =>
              val factory0: ExprFactory = selectedOverload match {
                case Overloadable.Function(f) =>
                  if overloadResult.remainingSig.parameters.nonEmpty then ???
                  else
                    new InferFactory {
                      // TODO: Fix location
                      override def loc: Loc = OverloadedExprFactory.this.loc
                      override def infer(using EmitState): Comp[InferredExpr] =
                        ZIO.succeed(InferredExpr(
                          Expr.FunctionCall(f, overloadResult.arguments),
                          overloadResult.remainingSig.returnType
                        ))
                    }
              }

              val factory = overloadResult.remainingArguments.foldLeft(factory0)(_.invoke(_))

              ErrorLog.logError(CompilerError.AmbiguousOverload(loc, overloads.map { (overload, _) => toAttemptedOverload(overload) })).when(overloads.size > 1) *>
                factory.check(t)


            case _ =>
              for
                attempted <- rejectedOverloads.get
                _ <- ErrorLog.logError(CompilerError.InvalidOverload(loc, attempted.map(toAttemptedOverload)))
              yield Expr.Error()
          }

      }

    override def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadedExprFactory(loc, lookup, args :+ arg)
  }

  private sealed trait OverloadAttempt
  private object OverloadAttempt {
    final case class Success(
      model: TRExprContext.Model,
      partialScope: Scopes.PartialScope,
      arguments: Seq[Expr],
      remainingSig: TRSignatureContext.FunctionSignature,
      remainingArguments: Seq[ArgumentInfo],
    ) extends OverloadAttempt

    final case class Failure(errors: Seq[CompilerError]) extends OverloadAttempt
  }

  private final case class PartialScope(variables: Seq[Var])

  private final class OverloadResolver(rejectedOverloads: Ref[Seq[Overloadable]]) {

    def attemptOverload(overload: Overloadable, args: Seq[ArgumentInfo])(using state: EmitState): Comp[OverloadAttempt] =
      for
        errors <- Ref.make(Seq.empty[CompilerError])
        attemptErrorLog = new ErrorLog {
          override def logError(error: => CompilerError): UIO[Unit] =
            errors.update(_ :+ error)
        }

        attemptModel <- state.model.get
        attemptModel <- Ref.make(attemptModel)

        attemptScope <- Scopes.LocalScope.make(state.scope)

        attemptState = EmitState(
          model = attemptModel,
          scope = attemptScope,
        )

        sig <- overload.signature

        attemptRes <- attemptOverloadCheck(overload, sig, args, Seq.empty)(using attemptState).provideSomeEnvironment[context.Env](_.add[ErrorLog](attemptErrorLog))
        partialScope <- attemptScope.toPartialScope

        errors <- errors.get
        res <-
          if errors.nonEmpty then
            println(errors)
            ZIO.succeed(OverloadAttempt.Failure(errors))
          else
            for
              model <- attemptModel.get
            yield OverloadAttempt.Success(
              model = model,
              partialScope = partialScope,
              arguments = attemptRes.args,
              remainingSig = attemptRes.remainingSig,
              remainingArguments = attemptRes.remainingArgs,
            )

      yield res


    private final case class AttemptOverloadCheckResult(
      args: Seq[Expr],
      remainingSig: TRSignatureContext.FunctionSignature,
      remainingArgs: Seq[ArgumentInfo]
    )

    private def attemptOverloadCheck(overload: Overloadable, sig: TRSignatureContext.FunctionSignature, args: Seq[ArgumentInfo], callArgs: Seq[Expr])(using EmitState): Comp[AttemptOverloadCheckResult] =
      (sig.parameters, args) match {
        case (param +: tailParams, arg +: tailArgs) =>
          ((param.listType, arg.listType) match {
            case (FunctionParameterListType.NormalList, FunctionParameterListType.NormalList) |
                 (FunctionParameterListType.InferrableList, FunctionParameterListType.InferrableList) |
                 (FunctionParameterListType.QuoteList, FunctionParameterListType.QuoteList) |
                 (FunctionParameterListType.RequiresList, FunctionParameterListType.RequiresList) =>
              for
                argExpr <- arg.arg.check(param.paramType)
              yield (argExpr, tailArgs)

            case (FunctionParameterListType.InferrableList | FunctionParameterListType.QuoteList, _) =>
              for
                hole <- makeHole
              yield (hole, args)

            case (FunctionParameterListType.RequiresList, _) => ???

            case (FunctionParameterListType.NormalList, _) =>
              ZIO.die(RuntimeException("Parameter argument mismatches should have been filtered out already"))
          }).flatMap { (callArg, restArgs) =>
            val isProof = param.listType == FunctionParameterListType.RequiresList
            val paramVar = param.asParameterVar(overload.asOwner, callArgs.size)

            val mapping = Map[Var, Expr](paramVar -> callArg)
            val restParams = tailParams.map { tParam =>
              tParam.copy(paramType = Substitution.substitute(TRExprContext)(mapping)(tParam.paramType))
            }

            val restSig = sig.copy(parameters = restParams)

            attemptOverloadCheck(overload, restSig, restArgs, callArgs :+ callArg)
          }

        case (param +: tailParams, _) => ???

        case _ =>
          ZIO.succeed(AttemptOverloadCheckResult(callArgs, sig, args))
      }


    def groupOverloads(overloads: Seq[Overloadable], args: Seq[ArgumentInfo]): ZStream[context.Env, context.Error, Seq[Overloadable]] =
      groupOverloadsByParamLength(overloads, args)

    private def groupOverloadsByParamLength(overloads: Seq[Overloadable], args: Seq[ArgumentInfo]): ZStream[context.Env, context.Error, Seq[Overloadable]] =
      ZStream.unwrap(
        for
          rejectedGroup <- Ref.make(Seq.empty[Overloadable])
          exactGroup <- Ref.make(Seq.empty[Overloadable])
          moreGroups <- Ref.make(Map.empty[Int, Seq[Overloadable]])
          fewerGroups <- Ref.make(Map.empty[Int, Seq[Overloadable]])

          _ <- ZIO.foreachDiscard(overloads) { overload =>
            for
              sig <- overload.signature
              _ <- rankByParamLength(sig.parameters, args) match {
                case OverloadRank.Mismatch => rejectedGroup.update(_ :+ overload)
                case OverloadRank.Exact => exactGroup.update(_ :+ overload)
                case OverloadRank.More(n) => moreGroups.update(addToIntGroup(n)(overload))
                case OverloadRank.Fewer(n) => fewerGroups.update(addToIntGroup(n)(overload))
              }
            yield ()
          }


          rejectedGroup <- rejectedGroup.get
          exactGroup <- exactGroup.get
          moreGroups <- moreGroups.get
          fewerGroups <- fewerGroups.get

          _ <- rejectedOverloads.update(_ ++ rejectedGroup)

        yield ZStream(exactGroup) ++
          ZStream.fromIterable(
            moreGroups.toSeq
              .sortBy((n, _) => n)
              .map((_, group) => group)
          ) ++
          ZStream.fromIterable(
            fewerGroups.toSeq
              .sortBy((n, _) => n)(using Ordering[Int].reverse)
              .map((_, group) => group)
          )

      )

    private enum OverloadRank derives CanEqual {
      case Mismatch
      case Exact
      case More(n: Int)
      case Fewer(n: Int)
    }

    private def rankByParamLength(params: Seq[TRSignatureContext.SignatureParameter], args: Seq[ArgumentInfo]): OverloadRank =
      (params, args) match {
        case (param +: tailParams, arg +: tailArgs) =>
          (param.listType, arg.listType) match {
            case (FunctionParameterListType.NormalList, FunctionParameterListType.NormalList) |
                 (FunctionParameterListType.InferrableList, FunctionParameterListType.InferrableList) |
                 (FunctionParameterListType.QuoteList, FunctionParameterListType.QuoteList) |
                 (FunctionParameterListType.RequiresList, FunctionParameterListType.RequiresList) =>
              rankByParamLength(tailParams, tailArgs)

            case (FunctionParameterListType.InferrableList | FunctionParameterListType.QuoteList | FunctionParameterListType.RequiresList, _) =>
              rankByParamLength(tailParams, args)

            case (FunctionParameterListType.NormalList, _) =>
              OverloadRank.Mismatch
          }

        case (Seq(), _) => OverloadRank.More(args.size)
        case (_, Seq()) => OverloadRank.Fewer(params.size)

        case (_, _) => OverloadRank.Exact
      }

    private def addToIntGroup(n: Int)(overload: Overloadable)(m: Map[Int, Seq[Overloadable]]): Map[Int, Seq[Overloadable]] =
      m.updatedWith(n)(overloads => Some(overloads.toSeq.flatten :+ overload))
  }

  private final class TupleExprFactory(override val loc: Loc, items: Seq[ExprFactory]) extends ExprFactory {
    override def check(t: Expr)(using EmitState): Comp[Expr] =
      normalizeToValue(t).flatMap {
        case t @ (Expr.TypeN(_) | Expr.TypeBigN(_)) =>
          for
            itemExprs <- ZIO.foreach(items) { item =>
              item.check(t)
            }
          yield Expr.Tuple(itemExprs)


        case t =>
          for
            itemTypes <- ZIO.foreach(items) { _ => makeHole }
            actualType = Expr.Tuple(itemTypes)
            _ <- ErrorLog.logError(CompilerError.TypeError(
              loc,
              TRToErrorShifter[context.type](context).shiftExpr(t),
              TRToErrorShifter[context.type](context).shiftExpr(actualType)
            )).whenZIO(unifyExpr(t, actualType).negate)

            itemExprs <- ZIO.foreach(items.zip(itemTypes)) { (item, itemType) =>
              item.check(itemType)
            }
          yield Expr.Tuple(itemExprs)
      }
  }


  private abstract class FixedSizeFactory1 extends ExprFactory {
    protected def withArgs(a: ArgumentInfo): ExprFactory

    override final def check(t: Expr)(using EmitState): Comp[Expr] = ???

    override def invoke(arg: ArgumentInfo): ExprFactory =
      withArgs(arg)
  }

  private abstract class FixedSizeFactory2 extends ExprFactory {
    protected def withArgs(a: ArgumentInfo, b: ArgumentInfo): ExprFactory

    override def check(t: Expr)(using EmitState): Comp[Expr] = ???

    override def invoke(a: ArgumentInfo): ExprFactory =
      new FixedSizeFactory1 {
        override def loc: Loc = FixedSizeFactory2.this.loc

        override def withArgs(b: ArgumentInfo): ExprFactory =
          FixedSizeFactory2.this.withArgs(a, b)
      }
  }

  private final class NullaryBuiltinFactory(override val loc: Loc, builtin: NullaryBuiltin, resType: Expr) extends InferFactory {
    override def infer(using EmitState): Comp[InferredExpr] =
      ZIO.succeed(InferredExpr(
        Expr.Builtin(Builtin.Nullary(builtin)),
        resType
      ))
  }

  private final class UnaryBuiltinFactory(override val loc: Loc, builtin: UnaryBuiltin, resType: Expr, aType: Expr) extends FixedSizeFactory1 {
    protected override def withArgs(a: ArgumentInfo): ExprFactory =
      new InferFactory {
        override def loc: Loc = a.callLocation

        override def infer(using EmitState): Comp[InferredExpr] =
          for
            aExpr <- a.arg.check(aType)
          yield InferredExpr(
            Expr.Builtin(Builtin.Unary(builtin, aExpr)),
            resType
          )
      }
  }

  private final class BinaryBuiltinFactory(override val loc: Loc, builtin: BinaryBuiltin, resType: Expr, aType: Expr, bType: Expr) extends FixedSizeFactory2 {
    override protected def withArgs(a: ArgumentInfo, b: ArgumentInfo): ExprFactory =
      new InferFactory {
        override def loc: Loc = b.callLocation

        override def infer(using EmitState): Comp[InferredExpr] =
          for
            aExpr <- a.arg.check(aType)
            bExpr <- b.arg.check(bType)
          yield InferredExpr(
            Expr.Builtin(Builtin.Binary(builtin, aExpr, bExpr)),
            resType
          )

      }
  }

  private class TREvaluator(using state: EmitState) extends ArgonEvaluatorBase[context.Env, context.Error] {
    override val context: TypeResolver.this.context.type = TypeResolver.this.context
    override val exprContext: TRExprContext.type = TRExprContext

    override protected val signatureContext: TRSignatureContext.type = TRSignatureContext

    override protected def shiftExpr(expr: context.DefaultExprContext.Expr): exprContext.Expr =
      DefaultToTRShifter[context.type](context).shiftExpr(expr)

    override protected def shiftSig(sig: context.DefaultSignatureContext.FunctionSignature): signatureContext.FunctionSignature =
      TRSignatureContext.signatureFromDefault(sig)
    
    override def normalizeHole(hole: Hole): Comp[Expr] =
      state.model.get.map(_.resolveHole(hole).getOrElse(Expr.Hole(hole)))
  }

  private final class ResolvedHoleFiller(model: TRExprContext.Model) extends ContextShifter[Comp] {
    override val ec1: context.TRExprContext.type = context.TRExprContext
    override val ec2: context.DefaultExprContext.type = context.DefaultExprContext

    override protected def shiftHole(hole: UniqueIdentifier): Comp[ec2.Expr] =
      model.resolveHole(hole) match {
        case Some(e) => shiftExpr(e)
        case None => ???
      }
  }


}
