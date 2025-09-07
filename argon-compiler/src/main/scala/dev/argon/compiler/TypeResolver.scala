package dev.argon.compiler

import dev.argon.util.{*, given}
import dev.argon.ast
import dev.argon.ast.{FunctionParameterListType, IdentifierExpr}
import dev.argon.expr.*
import dev.argon.prover.Proof
import dev.argon.util.{FilePosition, Location, UniqueIdentifier, WithLocation, WithSource}
import zio.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given
import zio.stream.ZStream

trait TypeResolver extends UsingContext {

  import context.{TRExprContext, TRSignatureContext, Scopes}
  import Scopes.{LookupResult, Overloadable, ImplicitValue}

  import TRExprContext.{Expr, Builtin, LocalVar, Var, Hole, HoleInfo, AnnotatedExpr, RecordFieldLiteral, EffectInfo}
  private type Loc = Location[FilePosition]


  private final case class EmitState(
    model: Ref[TRExprContext.Model],
    scope: Scopes.LocalScope,
    effects: EffectInfo,
    erased: ErasureMode,
  )

  final def typeCheckExpr(scope: Scopes.Scope)(e: WithSource[ast.Expr], t: context.DefaultExprContext.Expr, effects: context.DefaultExprContext.EffectInfo, erased: Boolean): Comp[context.DefaultExprContext.Expr] =
    for
      scope <- Scopes.LocalScope.make(scope)
      model <- Ref.make[TRExprContext.Model](TRExprContext.Model.empty)
      shifter = DefaultToTRShifter[context.type](context)
      expr <- resolveExpr(e).check(shifter.shiftExpr(t))(using EmitState(model, scope, shifter.shiftEffectInfo(effects), erased = ErasureMode.fromBoolean(erased)))
      model <- model.get
      shifted <- ResolvedHoleFiller(model).shiftExpr(expr)
    yield shifted

  final def typeCheckTypeExpr(scope: Scopes.Scope)(e: WithSource[ast.Expr], erased: Boolean): Comp[context.DefaultExprContext.Expr] =
    for
      scope <- Scopes.LocalScope.make(scope)
      model <- Ref.make[TRExprContext.Model](TRExprContext.Model.empty)
      expr <- resolveType(e)(using EmitState(model, scope, EffectInfo.Pure, erased = if erased then ErasureMode.Erased else ErasureMode.TypeAnnotation))
      model <- model.get
      shifted <- ResolvedHoleFiller(model).shiftExpr(expr)
    yield shifted

  private def normalizeToValue(e: Expr)(using EmitState): Comp[Expr] =
    TREvaluator().normalizeToValue(e, context.Config.evaluatorFuel)


  private def tryUnifyExpr(a: Expr, b: Expr)(using state: EmitState): Comp[Boolean] =
    state.model.get.flatMap { model =>
      Ref.make(model)
        .flatMap { modelRef =>
          unifyExpr(a, b)(using state.copy(model = modelRef))
            .tap(modelRef.get.flatMap(state.model.set).whenDiscard(_))
        }

    }

  private def unifyExpr(a: Expr, b: Expr)(using state: EmitState): Comp[Boolean] =
    Unification.unify[context.Env, context.Error](TRExprContext)(state.model, TREvaluator(), context.Config.evaluatorFuel)(a, b)

  private def checkTypesMatch(loc: SourceLocation)(e: Expr)(a: Expr, b: Expr)(using state: EmitState): Comp[Expr] =
    normalizeToValue(a).flatMap { a =>
      normalizeToValue(b).flatMap { b =>
        def err = ErrorLog.logError(CompilerError.TypeError(
          loc,
          TRToErrorShifter[context.type](context).shiftExpr(a),
          TRToErrorShifter[context.type](context).shiftExpr(b)
        ))

        (a, b) match {
          case (Expr.AnyType(), Expr.AnyType() | Expr.TypeN(_) | Expr.TypeBigN(_)) =>
            ZIO.succeed(e)

          case (Expr.AnyType(), Expr.Tuple(items)) =>
            ZIO.foreachDiscard(items) { item =>
              checkTypesMatchNoBox(loc)(a, item)
            }.as(e)

          case (Expr.AnyType(), _) =>
            err.as(e)

          case (Expr.Boxed(a), Expr.Boxed(b)) =>
            checkTypesMatch(loc)(e)(a, b)

          case (Expr.Boxed(aInner), _) =>
            tryUnifyExpr(a, b).flatMap {
              case true => ZIO.succeed(e)
              case false =>
                unifyExpr(aInner, b).flatMap {
                  case true => ZIO.succeed(Expr.Box(aInner, e))
                  case false => err.as(e)
                }
            }

          case (_, Expr.Boxed(bInner)) =>
            tryUnifyExpr(a, b).flatMap {
              case true => ZIO.succeed(e)
              case false =>
                unifyExpr(a, bInner).flatMap {
                  case true => ZIO.succeed(Expr.Unbox(bInner, e))
                  case false => err.as(e)
                }
            }

          case _ =>
            err.whenZIODiscard(unifyExpr(a, b).negate).as(e)
        }
      }
    }

  private def checkTypesMatchNoBox(loc: SourceLocation)(a: Expr, b: Expr)(using state: EmitState): Comp[Unit] =
    normalizeToValue(a).flatMap { a =>
      normalizeToValue(b).flatMap { b =>
        def err = ErrorLog.logError(CompilerError.TypeError(
          loc,
          TRToErrorShifter[context.type](context).shiftExpr(a),
          TRToErrorShifter[context.type](context).shiftExpr(b)
        ))

        (a, b) match {
          case (Expr.AnyType(), Expr.AnyType() | Expr.TypeN(_) | Expr.TypeBigN(_)) =>
            ZIO.unit

          case (Expr.AnyType(), Expr.Tuple(items)) =>
            ZIO.foreachDiscard(items) { item =>
              checkTypesMatchNoBox(loc)(a, item)
            }

          case (Expr.AnyType(), _) =>
            err

          case _ =>
            err.whenZIODiscard(unifyExpr(a, b).negate)
        }
      }
    }


  private def nestedScope[A](using state: EmitState)(f: EmitState ?=> Comp[A]): Comp[A] =
    for
      innerScope <- Scopes.LocalScope.make(state.scope)
      res <- f(using state.copy(scope = innerScope))
    yield res


  private def makeHole(t: Expr, erased: ErasureMode, loc: Location[FilePosition]): Comp[Expr.Hole] =
    UniqueIdentifier.make.map(id => Expr.Hole(HoleInfo(id, t, erased, loc)))

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

        override def check(t: Expr)(using state: EmitState): Comp[Expr] =
          for
            argType <- makeHole(Expr.AnyType(), ErasureMode.Concrete, loc)
            paramId <- UniqueIdentifier.make
            param = LocalVar(paramId, argType, None, isMutable = false, isErased = false, isProof = false)
            funcType = Expr.FunctionType(param, t)
            f <- ExprFactory.this.check(funcType)
            arg <- arg.arg.check(argType)
          yield Expr.FunctionObjectCall(f, arg)
      }

    def assign(assignedValue: AssignedValue): ExprFactory =
      ErrorFactory(loc, CompilerError.InvalidAssignmentTarget(loc))

    def withRecordFields(fields: RecordLiteralInfo): ExprFactory =
      throw new RuntimeException("withRecordFields not implemented: " + getClass)

    final def accessMember(member: MemberInfo): ExprFactory =
      OverloadedExprFactory(member.memberAccessLocation, member.memberAccessLocation, memberLookupSource(member), Seq())

    def memberLookupSource(member: MemberInfo): OverloadLookupSource =
      MemberLookupSource(member.memberAccessLocation, this, hasExplicitAssign = false, member.memberName.value)

  }

  private abstract class InferFactory extends ExprFactory {
    def infer(using EmitState): Comp[InferredExpr]

    override final def check(t: Expr)(using EmitState): Comp[Expr] =
      infer.flatMap {
        case InferredExpr(e, inferredType) =>
          checkTypesMatch(loc)(e)(t, inferredType)
      }
  }

  private final case class InferredExpr(e: Expr, t: Expr)

  private final case class ArgumentInfo(
    callLocation: Loc,
    arg: ExprFactory,
    listType: FunctionParameterListType,
  )

  private final case class RecordFieldValueInfo(
    fieldLocation: Loc,
    fieldName: WithSource[IdentifierExpr],
    value: ExprFactory,
  )

  private final case class RecordLiteralInfo(
    recordLiteralLocation: Loc,
    fields: Seq[RecordFieldValueInfo],
  )

  private final case class RecordFieldResolved(
    field: RecordField,
    fieldType: Expr,
  )

  private final case class AssignedValue(
    assignLocation: Loc,
    value: ExprFactory,
  )

  private final case class MemberInfo(
    memberAccessLocation: Loc,
    memberName: WithSource[IdentifierExpr],
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
                .withWitness(isProof)
                .parse(decl.modifiers)
              isErased <- isErased.get
              isProof <- isProof.get

              _ <- ErrorLog.logError(CompilerError.ErasedMustBePure(loc))
                .whenDiscard(isErased && decl.isMutable)

              id <- UniqueIdentifier.make
              varType <- decl.varType.fold(makeHole(Expr.AnyType(), ErasureMode.TypeAnnotation, loc))(resolveType)
              value <- resolveExpr(decl.value).check(varType)(using state.copy(erased = state.erased || ErasureMode.fromBoolean(isErased)))

              localVar = LocalVar(
                id,
                varType,
                decl.name,
                decl.isMutable,
                isErased = isErased,
                isProof = isProof,
              )

              _ <- state.scope.addVariable(localVar, Some(value))

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

      case ast.Expr.Assert(t) =>
        new InferFactory {
          override def loc: Loc = expr.location

          override def infer(using state: EmitState): Comp[InferredExpr] =
            for
              t <- resolveType(t)(using state.copy(erased = ErasureMode.Erased))
              value <- resolveImplicit(t, loc)

              id <- UniqueIdentifier.make

              localVar = LocalVar(
                id,
                t,
                name = None,
                isMutable = false,
                isErased = true,
                isProof = true,
              )

              _ <- state.scope.addVariable(localVar, Some(value))

            yield InferredExpr(
              Expr.BindVariable(
                localVar,
                value,
              ),
              unitType,
            )
        }

      case ast.Expr.BoxedType(t) =>
        new InferFactory {
          override def loc: Loc = expr.location

          override def infer(using state: EmitState): Comp[InferredExpr] =
            for
              t <- resolveType(t)(using state.copy(erased = ErasureMode.Erased))
              tt <- exprType.getExprType(t)
            yield InferredExpr(Expr.Boxed(t), tt)
        }

      case ast.Expr.Box(value) =>
        new ExprFactory {
          override def loc: Loc = expr.location

          override def check(t: Expr)(using EmitState): Comp[Expr] =
            for
              tt <- exprType.getExprType(t)
              innerType <- makeHole(tt, ErasureMode.TypeAnnotation, loc)
              _ <- checkTypesMatchNoBox(loc)(t, Expr.Boxed(innerType))
              innerValue <- resolveExpr(value).check(innerType)
            yield Expr.Box(innerType, innerValue)
        }

      case ast.Expr.BinaryOperation(left, ast.BinaryOperator.Assign, right) =>
        resolveExpr(left).assign(AssignedValue(expr.location, resolveExpr(right)))

      case ast.Expr.BinaryOperation(
        left,
        op: ast.Operator.ValidIdentifier,
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


      case ast.Expr.BinaryOperation(left, ast.BinaryOperator.PropEqual, right) =>
        new InferFactory {
          override def loc: Loc = expr.location

          override def infer(using EmitState): Comp[InferredExpr] =
            for
              t <- makeHole(Expr.AnyType(), ErasureMode.TypeAnnotation, loc)
              left <- resolveExpr(left).check(t)
              right <- resolveExpr(right).check(t)
              tt <- exprType.getExprType(t)
            yield InferredExpr(
              Expr.Builtin(Builtin.EqualTo(t, left, right)),
              tt
            )

        }

      case ast.Expr.BinaryOperation(left, op, right) =>
        println("Unimplemented AST binary operation: " + op)
        ???

      case ast.Expr.Block(body, finallyBody) =>
        val bodyFac = resolveStmtBlock(body)

        finallyBody match {
          case Some(finallyBody) =>
            new ExprFactory {
              override def loc: Loc = expr.location

              override def check(t: Expr)(using EmitState): Comp[Expr] =
                for
                  bodyExpr <- bodyFac.check(t)
                  finallyExpr <- resolveStmtBlock(finallyBody).check(Expr.Tuple(Seq()))
                yield Expr.Finally(bodyExpr, finallyExpr)
            }

          case None => bodyFac
        }

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

      case ast.Expr.Dot(o, member) =>
        resolveExpr(o).accessMember(MemberInfo(expr.location, member))

      case ast.Expr.FunctionCall(f, listType, arg) =>
        resolveExpr(f).invoke(ArgumentInfo(
          callLocation = expr.location,
          arg = resolveExpr(arg),
          listType = listType,
        ))

      case ast.Expr.FunctionLiteral(parameterName, body) =>
        new LambdaExprFactory {
          override def loc: Loc = expr.location

          override def checkFunction(t: Expr.FunctionType)(using state: EmitState): Comp[Expr] =
            for
              paramId <- UniqueIdentifier.make
              param = LocalVar(paramId, t.a.varType, parameterName, isMutable = t.a.isMutable, isErased = t.a.isErased, isProof = t.a.isProof)

              nestedScope <- Scopes.LocalScope.make(state.scope)
              _ <- nestedScope.addVariable(param, None)
              body <- resolveExpr(body).check(t.r)(using state.copy(scope = nestedScope))
            yield Expr.Lambda(param, t.r, body)
        }

      case ast.Expr.FunctionType(a, r) =>
        new InferFactory {
          override def loc: Loc = expr.location
          override def infer(using state: EmitState): Comp[InferredExpr] =
            for
              a <- resolveType(a)
              r <- resolveType(r)
              paramId <- UniqueIdentifier.make
              param = LocalVar(paramId, a, None, isMutable = false, isErased = false, isProof = false)
            yield InferredExpr(Expr.FunctionType(param, r), Expr.TypeN(Expr.IntLiteral(0))) // TODO: Infer proper level
        }

      case id: IdentifierExpr =>
        LookupIdFactory(expr.location, id)

      case ast.Expr.IfElse(cond, whenTrue, whenFalse) =>
        new ExprFactory {
          override def loc: Loc = expr.location

          override def check(t: Expr)(using state: EmitState): Comp[Expr] =
            for
              condExpr <- resolveExpr(cond).check(Expr.Builtin(Builtin.Nullary(NullaryBuiltin.BoolType)))
              isPureCond = PurityScanner(context)(TRExprContext)(condExpr)

              whenTrueVar <- createIfBranchVar(condExpr, true).when(isPureCond)
              whenFalseVar <- createIfBranchVar(condExpr, false).when(isPureCond)

              trueBody <- nestedScope { state ?=>
                ZIO.foreach(whenTrueVar)(state.scope.addVariable(_, None)) *>
                  resolveStmtBlock(whenTrue).check(t)
              }
              falseBody <- nestedScope { state ?=>
                ZIO.foreach(whenFalseVar)(state.scope.addVariable(_, None)) *>
                  resolveStmtBlock(whenFalse).check(t)
              }
            yield Expr.IfElse(whenTrueVar, whenFalseVar, condExpr, trueBody, falseBody)

          private def createIfBranchVar(condExpr: Expr, value: Boolean)(using state: EmitState): Comp[LocalVar] =
            for
              id <- UniqueIdentifier.make 
            yield LocalVar(
              id,
              varType = Expr.Builtin(Builtin.EqualTo(boolType, condExpr, Expr.BoolLiteral(value))),
              name = None,
              isMutable = false,
              isErased = true,
              isProof = true,
            )
        }

      case ast.Expr.IntLiteral(i) =>
        new InferFactory {
          override def loc: Loc = expr.location
          override def infer(using state: EmitState): Comp[InferredExpr] =
            ZIO.succeed(InferredExpr(
              Expr.IntLiteral(i),
              intType,
            ))
        }

      case ast.Expr.RecordLiteral(rec, fields) =>
        resolveExpr(rec).withRecordFields(
          RecordLiteralInfo(
            recordLiteralLocation = fields.location,
            fields = fields.value.map { field =>
              RecordFieldValueInfo(
                fieldLocation = field.location,
                fieldName = field.value.name,
                value = resolveExpr(field.value.value),
              )
            },
          )
        )

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

            case ast.StringFragment.Interpolate(_) => ???
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

      case _: ast.Expr.Type.type =>
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

      case ast.Expr.Unbox(value) =>
        new ExprFactory {
          override def loc: Loc = expr.location

          override def check(t: Expr)(using EmitState): Comp[Expr] =
            for
              innerValue <- resolveExpr(value).check(Expr.Boxed(t))
            yield Expr.Unbox(t, innerValue)
        }

      case ast.Expr.UnaryOperation(op: ast.Operator.ValidIdentifier, a) =>
        LookupIdFactory(expr.location, IdentifierExpr.Op(op))
          .invoke(ArgumentInfo(
            callLocation = expr.location,
            arg = resolveExpr(a),
            listType = FunctionParameterListType.NormalList,
          ))

      case _ =>
        println("Unimplemented AST Expression type: " + expr.value.getClass)
        ???
    }

  private def resolveType(expr: WithSource[ast.Expr])(using state: EmitState): Comp[Expr] =
    resolveExpr(expr).check(Expr.AnyType())(using state.copy(erased = state.erased || ErasureMode.TypeAnnotation))


  private def builtinExprFactory(loc: Loc, name: String): ExprFactory =
    name match {
      case "int_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.IntType, Expr.TypeN(Expr.IntLiteral(0)))
      case "bool_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.BoolType, Expr.TypeN(Expr.IntLiteral(0)))
      case "string_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.StringType, Expr.TypeN(Expr.IntLiteral(0)))
      case "never_type" => NullaryBuiltinFactory(loc, NullaryBuiltin.NeverType, Expr.TypeN(Expr.IntLiteral(0)))
  
      case "int_negate" => UnaryBuiltinFactory(loc, UnaryBuiltin.IntNegate, intType, intType)
      case "int_bitnot" => UnaryBuiltinFactory(loc, UnaryBuiltin.IntBitNot, intType, intType)
      case "bool_not" => UnaryBuiltinFactory(loc, UnaryBuiltin.BoolNot, boolType, boolType)
  
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
      case "int_bitand" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntBitAnd, intType, intType, intType)
      case "int_bitor" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntBitOr, intType, intType, intType)
      case "int_bitxor" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntBitXOr, intType, intType, intType)
      case "int_bitshiftleft" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntBitShiftLeft, intType, intType, intType)
      case "int_bitshiftright" => BinaryBuiltinFactory(loc, BinaryBuiltin.IntBitShiftRight, intType, intType, intType)
      case "string_concat" => BinaryBuiltinFactory(loc, BinaryBuiltin.StringConcat, stringType, stringType, stringType)
      case "string_eq" => BinaryBuiltinFactory(loc, BinaryBuiltin.StringEQ, boolType, stringType, stringType)
      case "string_ne" => BinaryBuiltinFactory(loc, BinaryBuiltin.StringNE, boolType, stringType, stringType)
      case "bool_eq" => BinaryBuiltinFactory(loc, BinaryBuiltin.BoolEQ, boolType, boolType, boolType)
      case "bool_ne" => BinaryBuiltinFactory(loc, BinaryBuiltin.BoolNE, boolType, boolType, boolType)
  
      case "equal_to_type" => ???
      case _ =>
        scala.Console.err.println(name)
        ???
    }


  private def checkAllowedEffect(loc: Loc)(effect: EffectInfo)(using state: EmitState): Comp[Unit] =
    ErrorLog.logError(CompilerError.PurityError(loc))
      .whenDiscard(
        (state.effects, effect) match {
          case (EffectInfo.Effectful, _) | (_, EffectInfo.Pure) => false
          case (EffectInfo.Pure, EffectInfo.Effectful) => true
        }
      )

  private def checkErasure(loc: Loc)(expr: Expr)(erased: Boolean)(using state: EmitState): Comp[Expr] =
    state.erased match
      case ErasureMode.Erased => ZIO.succeed(expr)
      case ErasureMode.TypeAnnotation =>
        if erased then
          ZIO.succeed(Expr.Boxed(expr))
        else
          ZIO.succeed(expr)
      case ErasureMode.Concrete =>
        ErrorLog.logError(CompilerError.ErasedExpressionNotAllowed(loc))
          .whenDiscard(erased)
          .as(expr)
    end match


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

    override def withRecordFields(fields: RecordLiteralInfo): ExprFactory =
      new WrappedFactory {
        override def loc: Loc = fields.recordLiteralLocation

        override protected def unwrap(using EmitState): Comp[ExprFactory] =
          WrappedFactory.this.unwrap.map(_.withRecordFields(fields))
      }

    override def memberLookupSource(member: MemberInfo): OverloadLookupSource =
      new WrappedLookupSource {
        def unwrap(using EmitState): Comp[OverloadLookupSource] =
          WrappedFactory.this.unwrap.map(_.memberLookupSource(member))
      }
  }

  private abstract class LambdaExprFactory extends ExprFactory {
    override final def check(t: Expr)(using EmitState): Comp[Expr] =
      normalizeToValue(t).flatMap {
        case t: Expr.FunctionType =>
          checkFunction(t)

        case _ =>
          ErrorLog.logError(CompilerError.FunctionTypeRequired(
            loc,
            TRToErrorShifter[context.type](context).shiftExpr(t),
          )).as(Expr.Error())
      }


    def checkFunction(t: Expr.FunctionType)(using EmitState): Comp[Expr]

    protected def unifyParam(expected: Expr.FunctionType, actual: Expr.FunctionType)(using EmitState): Comp[Unit] =
      ErrorLog.logError(CompilerError.TypeError(
        loc,
        TRToErrorShifter[context.type](context).shiftExpr(expected),
        TRToErrorShifter[context.type](context).shiftExpr(actual)
      )).whenZIODiscard(
        unifyExpr(expected.a.varType, actual.a.varType).negate ||
          ZIO.succeed(expected.a.isErased != actual.a.isErased)
      )

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
              for
                _ <- checkAllowedEffect(loc)(if v.isMutable then EffectInfo.Effectful else EffectInfo.Pure)
                e = Expr.Variable(v)
                e <- checkErasure(loc)(e)(v.isErased)
              yield InferredExpr(
                e,
                v.varType,
              )

            override def assign(assignedValue: AssignedValue): ExprFactory =
              new InferFactory {
                override def loc: Loc = assignedValue.assignLocation

                override def infer(using state: EmitState): Comp[InferredExpr] =
                  for
                    _ <- ErrorLog.logError(CompilerError.CanNotMutate(loc)).whenDiscard(!v.isMutable)
                    _ <- checkAllowedEffect(loc)(EffectInfo.Effectful)

                    value <- assignedValue.value.check(v.varType)(using state.copy(
                      erased = state.erased || ErasureMode.fromBoolean(v.isErased),
                      effects = if v.isErased then EffectInfo.Pure else state.effects,
                    ))
                  yield InferredExpr(
                    Expr.VariableStore(v, value),
                    Expr.Tuple(Seq())
                  )
              }
          }

        case LookupResult.VariableTupleElement(v, index, t) =>
          new InferFactory {
            override def loc: Loc = LookupIdFactory.this.loc

            override def infer(using EmitState): Comp[InferredExpr] =
              val e = Expr.TupleElement(index, Expr.Variable(v))
              for
                e <- checkErasure(loc)(e)(v.isErased)
              yield InferredExpr(
                e,
                t,
              )
            end infer
          }


        case lookupOverload: LookupResult.Overloaded =>
          val scopeLookupSource = new OverloadLookupSource {
            override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
              ZIO.succeed(lookupOverload)

            override def assignmentTarget: OverloadLookupSource =
              RerunLookupSource(IdentifierExpr.Update(id))

            private class RerunLookupSource(id: IdentifierExpr) extends OverloadLookupSource {
              override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
                state.scope.lookup(id).map {
                  case res: LookupResult.OverloadableOnly => res
                  case _ => LookupResult.NotFound()
                }

              override def assignmentTarget: OverloadLookupSource = RerunLookupSource(IdentifierExpr.Update(id))
            }
          }

          OverloadedExprFactory(loc, loc, scopeLookupSource, Seq())
      }
  }

  private trait OverloadLookupSource {
    def lookup(using EmitState): Comp[LookupResult.OverloadableOnly]

    def assignmentTarget: OverloadLookupSource
  }

  private abstract class WrappedLookupSource extends OverloadLookupSource {
    def unwrap(using EmitState): Comp[OverloadLookupSource]

    override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
      unwrap.flatMap(_.lookup)

    override def assignmentTarget: OverloadLookupSource =
      new WrappedLookupSource {
        def unwrap(using EmitState): Comp[OverloadLookupSource] =
          WrappedLookupSource.this.unwrap.map(_.assignmentTarget)
      }
  }

  private class MemberLookupSource(
    loc: Loc,
    obj: ExprFactory,
    hasExplicitAssign: Boolean,
    memberName: IdentifierExpr,
  ) extends OverloadLookupSource {
    override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
      for
        t <- makeHole(Expr.AnyType(), ErasureMode.TypeAnnotation, loc)
        e <- obj.check(t)
        t <- normalizeToValue(t)
        overloads <- overloadsFromType(t, e, extensionMethods(t, e))
      yield overloads

    override def assignmentTarget: OverloadLookupSource =
      if !hasExplicitAssign then
        MemberLookupSource(loc, obj, hasExplicitAssign = true, IdentifierExpr.Update(memberName))
      else
        EmptyOverloadSource()

    private def overloadsFromType(t: Expr, e: Expr, next: Comp[LookupResult.OverloadableOnly]): Comp[LookupResult.OverloadableOnly] =
      def fieldRead = t match {
        case recType @ Expr.RecordType(record, args) =>
          for
            fields <- record.fields

            fieldOverloads = fields
              .filter { field => field.name == memberName }
              .map { field => Overloadable.RecordField(recType, field, e) }

          yield LookupResult.Overloaded(
            fieldOverloads,
            next
          )

        case _ => next
      }

      memberName match {
        case IdentifierExpr.Update(fieldName) if hasExplicitAssign =>
          t match {
            case recType @ Expr.RecordType(record, args) =>
              for
                fields <- record.fields
              yield LookupResult.Overloaded(
                fields
                  .filter { field => field.name == fieldName }
                  .map { field => Overloadable.RecordFieldUpdate(recType, field, e) },
                next
              )

            case _ => next
          }

        case _ =>
          fieldRead
      }

    end overloadsFromType


    private def extensionMethods(t: Expr, e: Expr)(using state: EmitState): Comp[LookupResult.OverloadableOnly] =
      state.scope.lookup(IdentifierExpr.Extension(memberName)).map {
        case LookupResult.Overloaded(overloads, next) =>
          val emOverloads = overloads.flatMap {
            case Overloadable.Function(f) =>
              Seq(Overloadable.ExtensionMethod(f, AnnotatedExpr(obj.loc, e, t)))

            case _ => Seq()
          }

          LookupResult.Overloaded(emOverloads, next)

        case _ => LookupResult.NotFound()
      }

  }

  private class ChainLookupSource(
    first: OverloadLookupSource,
    second: OverloadLookupSource,
  ) extends OverloadLookupSource {
    override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
      first.lookup.flatMap(_.chainZIO(second.lookup))

    override def assignmentTarget: OverloadLookupSource =
      ChainLookupSource(first.assignmentTarget, second.assignmentTarget)
  }

  private class EmptyOverloadSource() extends OverloadLookupSource {
    override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
      ZIO.succeed(LookupResult.NotFound())

    override def assignmentTarget: OverloadLookupSource =
      this
  }

  private class NoArgumentMembersLookupSource(
    lookupSource: OverloadLookupSource,
    memberName: IdentifierExpr,
  ) extends OverloadLookupSource {
    override def lookup(using EmitState): Comp[LookupResult.OverloadableOnly] =
      lookupSource.lookup.flatMap(lookupFrom)

    private def lookupFrom(lookup: LookupResult.OverloadableOnly)(using EmitState): Comp[LookupResult.OverloadableOnly] =
      lookup match {
        case LookupResult.NotFound() => ZIO.succeed(LookupResult.NotFound())
        case LookupResult.Overloaded(overloads, next) =>
          for
            noArgMembers <- ZIO.foreach(overloads)(_.noArgumentMembers(memberName))
          yield LookupResult.Overloaded(noArgMembers.flatten, next.flatMap(lookupFrom))
      }

    override def assignmentTarget: OverloadLookupSource = EmptyOverloadSource()
  }

  private final case class RejectedOverload(overloadable: Overloadable, errors: Seq[CompilerError])

  private final class OverloadedExprFactory(override val loc: Loc, overloadableLoc: Loc, lookupSource: OverloadLookupSource, args: Seq[ArgumentInfo]) extends ExprFactory {
    private def toAttemptedOverload(overload: Overloadable): AttemptedOverload =
      overload match {
        case Overloadable.Function(f) => AttemptedOverload.Function(f)
        case Overloadable.Record(r) => AttemptedOverload.Record(r)
        case Overloadable.Enum(e) => AttemptedOverload.Enum(e)
        case Overloadable.ExtensionMethod(f, _) => AttemptedOverload.Function(f)
        case Overloadable.RecordField(r, field, _) => AttemptedOverload.RecordField(r.record, field)
        case Overloadable.RecordFieldUpdate(r, field, _) => AttemptedOverload.RecordField(r.record, field)
        case Overloadable.EnumVariant(v) => AttemptedOverload.EnumVariant(v)
      }


    private def toAttemptedOverloadWithErrors(reject: RejectedOverload): AttemptedOverloadWithErrors =
      AttemptedOverloadWithErrors(toAttemptedOverload(reject.overloadable), reject.errors)


    private def resolveFactory(using EmitState): Comp[ExprFactory] =
      Ref.make(Seq.empty[RejectedOverload]).flatMap { rejectedOverloads =>
        val resolver = OverloadResolver(rejectedOverloads)

        def lookupOverloads(lookup: LookupResult.OverloadableOnly): ZStream[context.Env, context.Error, Seq[Overloadable]] =
          lookup match {
            case LookupResult.NotFound() => ZStream.empty
            case LookupResult.Overloaded(overloads, next) => ZStream(overloads) ++ ZStream.unwrap(next.map(lookupOverloads))
          }


        ZStream.fromZIO(lookupSource.lookup)
          .flatMap(lookupOverloads)
          .flatMap { overloads => resolver.groupOverloads(overloads, args) }
          .mapZIO { overloads =>
            ZIO.foreach(overloads) { overload =>
              resolver.attemptOverload(overload, overloadableLoc, args).flatMap {
                case success: OverloadAttempt.Success =>
                  ZIO.succeed(Seq((overload, success)))

                case failure: OverloadAttempt.Failure =>
                  rejectedOverloads.update(_ :+ RejectedOverload(overload, failure.errors)).as(Seq.empty)

              }
            }.map(_.flatten)
          }
          .filter(_.nonEmpty)
          .runHead
          .flatMap {
            case Some(overloads @ ((selectedOverload, overloadResult) +: _)) =>

              def functionOverload(f: ArFunc): ExprFactory =
                if overloadResult.remainingSig.parameters.nonEmpty then
                  ???
                else
                  new InferFactory {
                    override def loc: Loc = overloadableLoc

                    override def infer(using EmitState): Comp[InferredExpr] =
                      for
                        _ <- checkAllowedEffect(loc)(DefaultToTRShifter[context.type](context).shiftEffectInfo(f.effects))
                        e = Expr.FunctionCall(f, overloadResult.arguments)
                        e <- checkErasure(loc)(e)(f.isErased)
                      yield InferredExpr(
                        e,
                        overloadResult.remainingSig.returnType
                      )

                    override def toString: String = "Function ExprFactory"
                  }

              def recordOverload(r: ArRecord): ExprFactory =
                if overloadResult.remainingSig.parameters.nonEmpty then
                  ???
                else
                  new InferFactory {
                    override def loc: Loc = overloadableLoc

                    private def expr: Expr.RecordType = Expr.RecordType(r, overloadResult.arguments)

                    private def exprType: Expr = overloadResult.remainingSig.returnType

                    override def infer(using EmitState): Comp[InferredExpr] =
                      ZIO.succeed(InferredExpr(expr, exprType))

                    private def recordOverloadFactory: ExprFactory = this

                    override def withRecordFields(fields: RecordLiteralInfo): ExprFactory =
                      new InferFactory {
                        override def loc: Loc = overloadableLoc

                        override def infer(using EmitState): Comp[InferredExpr] =
                          for
                            _ <- ZIO.unit

                            recordExpr = expr
                            recordFields <- recordExpr.record.fields

                            fieldValues <- ZIO.foreach(fields.fields) { field =>
                              for
                                recordFieldDecl <- ZIO.fromEither(recordFields.find(_.name == field.fieldName.value).toRight {
                                  ???
                                })
                                fieldSig <- TRSignatureContext.recordFieldSig(recordExpr, recordFieldDecl)
                              yield RecordFieldResolved(recordFieldDecl, fieldSig.returnType)
                            }

                            recFields <- recordLiteralFields(fieldValues, fields)

                          yield InferredExpr(
                            Expr.RecordLiteral(recordExpr, recFields),
                            recordExpr,
                          )
                      }

                    override def toString: String = "Record ExprFactory"
                  }

              def enumOverload(e: ArEnum): ExprFactory =
                if overloadResult.remainingSig.parameters.nonEmpty then
                  ???
                else
                  new InferFactory {
                    override def loc: Loc = overloadableLoc

                    override def infer(using EmitState): Comp[InferredExpr] =
                      ZIO.succeed(InferredExpr(
                        Expr.EnumType(e, overloadResult.arguments),
                        overloadResult.remainingSig.returnType
                      ))

                    override def toString: String = "Enum ExprFactory"
                  }

              def recordFieldOverload(r: Expr.RecordType, field: RecordField, recordValue: Expr): ExprFactory =
                new InferFactory {
                  override def loc: Loc = overloadableLoc

                  override def infer(using EmitState): Comp[InferredExpr] =
                    for
                      _ <- checkAllowedEffect(loc)(
                        if field.isMutable then EffectInfo.Effectful
                        else EffectInfo.Pure
                      )
                    yield InferredExpr(
                      Expr.RecordFieldLoad(r, field, recordValue),
                      overloadResult.remainingSig.returnType
                    )

                  override def toString: String = s"Record Field ExprFactory: $r $field"
                }


              def recordFieldStoreOverload(r: Expr.RecordType, field: RecordField, recordValue: Expr): ExprFactory =
                new InferFactory {
                  override def loc: Loc = overloadableLoc

                  override def infer(using EmitState): Comp[InferredExpr] =
                    val Seq(arg) = overloadResult.arguments
                    for
                      _ <- ErrorLog.logError(CompilerError.CanNotMutate(loc)).whenDiscard(!field.isMutable)
                      _ <- checkAllowedEffect(loc)(EffectInfo.Effectful)

                      sig <- r.record.signature
                    yield InferredExpr(
                      Expr.RecordFieldStore(r, field, recordValue, arg),
                      Expr.Tuple(Seq())
                    )
                  end infer

                  override def toString: String = "Record Field Store ExprFactory"
                }

              def enumVariantOverload(v: EnumVariant): ExprFactory =
                if overloadResult.remainingSig.parameters.nonEmpty then
                  ???
                else
                  new InferFactory {
                    override def loc: Loc = overloadableLoc

                    override def infer(using EmitState): Comp[InferredExpr] =
                      withRecordFields(RecordLiteralInfo(loc, Seq.empty)).infer

                    override def withRecordFields(fields: RecordLiteralInfo): InferFactory =
                      new InferFactory {
                        override def loc: Loc = fields.recordLiteralLocation

                        override def infer(using EmitState): Comp[InferredExpr] =
                          for
                            _ <- ZIO.unit

                            recordFields <- v.fields

                            fieldValues <- ZIO.foreach(fields.fields) { field =>
                              for
                                recordFieldDecl <- ZIO.fromEither(recordFields.find(_.name == field.fieldName.value).toRight {
                                  ???
                                })
                                fieldSig <- TRSignatureContext.recordFieldSig(v, overloadResult.arguments, recordFieldDecl)
                              yield RecordFieldResolved(recordFieldDecl, fieldSig.returnType)
                            }

                            recFields <- recordLiteralFields(fieldValues, fields)
                            
                          yield InferredExpr(
                            Expr.EnumVariantLiteral(
                              overloadResult.remainingSig.returnType match {
                                case t: Expr.EnumType => t
                                case _ => ???
                              },
                              v,
                              overloadResult.arguments,
                              recFields
                            ),
                            overloadResult.remainingSig.returnType,
                          )

                      }

                    override def toString: String = "Enum Variant ExprFactory"
                  }


              val factory0: ExprFactory = selectedOverload match {
                case Overloadable.Function(f) => functionOverload(f)
                case Overloadable.Record(r) => recordOverload(r)
                case Overloadable.Enum(e) => enumOverload(e)
                case Overloadable.ExtensionMethod(f, _) => functionOverload(f)
                case Overloadable.RecordField(r, field, recordValue) => recordFieldOverload(r, field, recordValue)
                case Overloadable.RecordFieldUpdate(r, field, recordValue) => recordFieldStoreOverload(r, field, recordValue)
                case Overloadable.EnumVariant(v) => enumVariantOverload(v)
              }

              val factory1 = overloadResult.remainingArguments.foldLeft(factory0)(_.invoke(_))

              val factory =
                overloadResult.lambdaParameters.foldRight(factory1) { (param, bodyFactory) =>
                  new LambdaExprFactory {
                    override def loc: Loc = overloadableLoc

                    override def checkFunction(t: Expr.FunctionType)(using EmitState): Comp[Expr] =
                      for
                        _ <- unifyParam(t, Expr.FunctionType(param, t.r))
                        body <- bodyFactory.check(t.r)
                      yield Expr.Lambda(param, t.r, body)
                  }
                }


              ErrorLog.logError(CompilerError.AmbiguousOverload(loc, overloads.map { (overload, _) => toAttemptedOverload(overload) })).whenDiscard(overloads.size > 1) *>
                summon[EmitState].model.set(overloadResult.model) *>
                ZIO.succeed(factory)


            case _ =>
              for
                attempted <- rejectedOverloads.get
                _ <- attempted match {
                  case Seq(attempt) =>
                    ZIO.foreach(attempt.errors)(ErrorLog.logError(_))

                  case _ => ZIO.unit
                }
                _ <- ErrorLog.logError(CompilerError.InvalidOverload(loc, attempted.map(toAttemptedOverloadWithErrors)))
              yield new ExprFactory {
                override def loc: Loc = overloadableLoc

                override def check(t: Expr)(using EmitState): Comp[Expr] =
                  ZIO.succeed(Expr.Error())
              }
          }

      }

    override def check(t: Expr)(using EmitState): Comp[Expr] =
      resolveFactory.flatMap(_.check(t))

    override def invoke(arg: ArgumentInfo): ExprFactory =
      OverloadedExprFactory(loc, overloadableLoc, lookupSource, args :+ arg)

    override def assign(assignedValue: AssignedValue): ExprFactory =
      BlackBoxExprFactory(
        OverloadedExprFactory(
          loc,
          overloadableLoc,
          lookupSource.assignmentTarget,
          args :+ ArgumentInfo(
            assignedValue.assignLocation,
            assignedValue.value,
            FunctionParameterListType.NormalList
          ),
        )
      )

    override def withRecordFields(fields: RecordLiteralInfo): ExprFactory =
      new WrappedFactory {
        override def loc: Loc = OverloadedExprFactory.this.loc

        override protected def unwrap(using EmitState): Comp[ExprFactory] =
          resolveFactory.map(_.withRecordFields(fields))
      }

    override def memberLookupSource(member: MemberInfo): OverloadLookupSource =
      val baseSource = super.memberLookupSource(member)

      if args.isEmpty then
        ChainLookupSource(
          NoArgumentMembersLookupSource(
            lookupSource,
            member.memberName.value,
          ),
          baseSource,
        )
      else
        baseSource
    end memberLookupSource

  }

  private def recordLiteralFactory(
    recordFields: Seq[RecordFieldResolved],
    fieldValues: RecordLiteralInfo,
    createExpr: Seq[RecordFieldLiteral] => InferredExpr,
  ): ExprFactory =
    new InferFactory {
      override def loc: Loc = fieldValues.recordLiteralLocation

      override def infer(using state: EmitState): Comp[InferredExpr] =
        for
          _ <- ZIO.unit

          duplicateFieldNames = fieldValues.fields
            .groupBy(_.fieldName.value)
            .view
            .mapValues(_.size)
            .filter((_, v) => v > 1)
            .keySet

          _ <- if duplicateFieldNames.nonEmpty then ??? else ZIO.unit

          _ <- checkAllowedEffect(loc)(
            if recordFields.exists(_.field.isMutable) then
              EffectInfo.Effectful
            else
              EffectInfo.Pure
          )

          fieldNames = fieldValues.fields.map(_.fieldName.value).toSet

          _ <- ZIO.foreachDiscard(recordFields) { rf =>
            if !fieldNames.contains(rf.field.name) then
              ???
            else
              ZIO.unit
          }

          recFields <- ZIO.foreach(fieldValues.fields) { field =>
            for
              recordFieldDecl <- ZIO.fromEither(recordFields.find(_.field.name == field.fieldName.value).toRight {
                ???
              })
              value <- field.value.check(recordFieldDecl.fieldType)
            yield RecordFieldLiteral(recordFieldDecl.field, value)
          }
        yield createExpr(recFields)

    }

  private def recordLiteralFields(
    recordFields: Seq[RecordFieldResolved],
    fieldValues: RecordLiteralInfo,
  )(using EmitState): Comp[Seq[RecordFieldLiteral]] =
    for
      _ <- ZIO.unit

      duplicateFieldNames = fieldValues.fields
        .groupBy(_.fieldName.value)
        .view
        .mapValues(_.size)
        .filter((_, v) => v > 1)
        .keySet

      _ <- if duplicateFieldNames.nonEmpty then ??? else ZIO.unit

      _ <- checkAllowedEffect(fieldValues.recordLiteralLocation)(
        if recordFields.exists(_.field.isMutable) then
          EffectInfo.Effectful
        else
          EffectInfo.Pure
      )

      fieldNames = fieldValues.fields.map(_.fieldName.value).toSet

      _ <- ZIO.foreachDiscard(recordFields) { rf =>
        if !fieldNames.contains(rf.field.name) then
          ???
        else
          ZIO.unit
      }

      recFields <- ZIO.foreach(fieldValues.fields) { field =>
        for
          recordFieldDecl <- ZIO.fromEither(recordFields.find(_.field.name == field.fieldName.value).toRight {
            ???
          })
          value <- field.value.check(recordFieldDecl.fieldType)
        yield RecordFieldLiteral(recordFieldDecl.field, value)
      }
    yield recFields


  // Disables the inner factory's invoke and assign methods.
  private class BlackBoxExprFactory(inner: ExprFactory) extends ExprFactory {
    override def loc: Loc = inner.loc

    override def check(t: Expr)(using EmitState): Comp[Expr] =
      inner.check(t)
  }

  private sealed trait OverloadAttempt
  private object OverloadAttempt {
    final case class Success(
      model: TRExprContext.Model,
      partialScope: Scopes.PartialScope,
      arguments: Seq[Expr],
      remainingSig: TRSignatureContext.FunctionSignature,
      remainingArguments: Seq[ArgumentInfo],
      lambdaParameters: Seq[LocalVar],
    ) extends OverloadAttempt

    final case class Failure(errors: Seq[CompilerError]) extends OverloadAttempt
  }

  private final case class PartialScope(variables: Seq[Var])

  private final class OverloadResolver(rejectedOverloads: Ref[Seq[RejectedOverload]]) {

    def attemptOverload(overload: Overloadable, funcLocation: SourceLocation, args: Seq[ArgumentInfo])(using state: EmitState): Comp[OverloadAttempt] =
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
          effects = state.effects,
          erased = state.erased,
        )

        sig <- overload.signature

        attemptRes <- attemptOverloadCheck(overload, sig, funcLocation, overload.initialArgs.map(annExprToArg) ++ args, Seq.empty, Seq.empty)(using attemptState).provideSomeEnvironment[context.Env](_.add[ErrorLog](attemptErrorLog))
        partialScope <- attemptScope.toPartialScope

        errors <- errors.get
        res <-
          if errors.nonEmpty then
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
              lambdaParameters = attemptRes.lambdaParameters,
            )

      yield res


    private final case class AttemptOverloadCheckResult(
      args: Seq[Expr],
      remainingSig: TRSignatureContext.FunctionSignature,
      remainingArgs: Seq[ArgumentInfo],
      lambdaParameters: Seq[LocalVar],
    )

    private def attemptOverloadCheck(overload: Overloadable, sig: TRSignatureContext.FunctionSignature, funcLocation: SourceLocation, args: Seq[ArgumentInfo], callArgs: Seq[Expr], lambdaParams: Seq[LocalVar])(using state: EmitState): Comp[AttemptOverloadCheckResult] =
      def applyArg(param: TRSignatureContext.SignatureParameter, tailParams: Seq[TRSignatureContext.SignatureParameter], callLocation: SourceLocation, arg: Expr, restArgs: Seq[ArgumentInfo], lambdaParam: Option[LocalVar]): Comp[AttemptOverloadCheckResult] =
        val isProof = param.listType == FunctionParameterListType.RequiresList

        val restSig = overload.asOwner match {
          case Some(owner) =>
            val paramVar = param.asParameterVar(owner, callArgs.size)
            sig.copy(parameters = tailParams).substituteVar(paramVar, arg)

          case None =>
            sig.copy(parameters = tailParams)
        }

        attemptOverloadCheck(overload, restSig, callLocation, restArgs, callArgs :+ arg, lambdaParams ++ lambdaParam)
      end applyArg

      def paramErasure(param: TRSignatureContext.SignatureParameter): ErasureMode =
        if param.isErased then
          ErasureMode.Erased
        else
          state.erased match {
            case ErasureMode.TypeAnnotation if !ValueUtil.isTypeType(TRExprContext)(param.paramType) =>
              ErasureMode.Concrete

            case _ => state.erased
          }

      (sig.parameters, args) match {
        case (param +: tailParams, arg +: tailArgs) =>

          ((param.listType, arg.listType) match {
            case (FunctionParameterListType.NormalList, FunctionParameterListType.NormalList) |
                 (FunctionParameterListType.InferrableList, FunctionParameterListType.InferrableList) |
                 (FunctionParameterListType.QuoteList, FunctionParameterListType.QuoteList) |
                 (FunctionParameterListType.RequiresList, FunctionParameterListType.RequiresList) =>
              for
                argExpr <- arg.arg.check(param.paramType)(using state.copy(erased = paramErasure(param)))
              yield (argExpr, tailArgs, arg.callLocation)

            case (FunctionParameterListType.InferrableList | FunctionParameterListType.QuoteList, _) =>
              for
                hole <- makeHole(param.paramType, paramErasure(param), funcLocation)
              yield (hole, args, funcLocation)

            case (FunctionParameterListType.RequiresList, _) =>
              for
                resolvedValue <- resolveImplicit(param.paramType, funcLocation)
              yield (resolvedValue, args, funcLocation)

            case (FunctionParameterListType.NormalList, _) =>
              ZIO.die(RuntimeException("Parameter argument mismatches should have been filtered out already"))
          }).flatMap { (callArg, restArgs, callLocation) =>
            applyArg(param, tailParams, callLocation, callArg, restArgs, lambdaParam = None)
          }

        case (param +: tailParams, _) =>
          (param.listType match {
            case FunctionParameterListType.NormalList =>
              for
                  varId <- UniqueIdentifier.make
                  lambdaParam = LocalVar(
                    id = varId,
                    varType = param.paramType,
                    name = None,
                    isMutable = false,
                    isErased = param.isErased,
                    isProof = false,
                  )
              yield (Expr.Variable(lambdaParam), Some(lambdaParam))

            case FunctionParameterListType.InferrableList | FunctionParameterListType.QuoteList =>
              for
                hole <- makeHole(param.paramType, paramErasure(param), funcLocation)
              yield (hole, None)

            case FunctionParameterListType.RequiresList =>
              for
                resolvedValue <- resolveImplicit(param.paramType, funcLocation)
              yield (resolvedValue, None)

          }).flatMap { case (callArg, lambdaParam) =>
            applyArg(param, tailParams, funcLocation, callArg, args, lambdaParam)
          }

        case _ =>
          ZIO.succeed(AttemptOverloadCheckResult(callArgs, sig, args, lambdaParams))
      }
    end attemptOverloadCheck


    def groupOverloads(overloads: Seq[Overloadable], args: Seq[ArgumentInfo]): ZStream[context.Env, context.Error, Seq[Overloadable]] =
      groupOverloadsByParamLength(overloads, args)

    private def groupOverloadsByParamLength(overloads: Seq[Overloadable], args: Seq[ArgumentInfo]): ZStream[context.Env, context.Error, Seq[Overloadable]] =
      ZStream.unwrap(
        for
          rejectedGroup <- Ref.make(Seq.empty[RejectedOverload])
          exactGroup <- Ref.make(Seq.empty[Overloadable])
          moreGroups <- Ref.make(Map.empty[Int, Seq[Overloadable]])
          fewerGroups <- Ref.make(Map.empty[Int, Seq[Overloadable]])

          _ <- ZIO.foreachDiscard(overloads) { overload =>
            for
              sig <- overload.signature
              _ <- rankByParamLength(sig.parameters, overload.initialArgs.map(annExprToArg) ++ args) match {
                case OverloadRank.Mismatch => rejectedGroup.update(_ :+ RejectedOverload(overload, Seq()))
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

    private def annExprToArg(expr: AnnotatedExpr): ArgumentInfo =
      val factory = new InferFactory {
        override def loc: Loc = expr.location

        override def infer(using EmitState): Comp[InferredExpr] =
          ZIO.succeed(InferredExpr(expr.e, expr.t))
      }
      ArgumentInfo(expr.location, factory, FunctionParameterListType.NormalList)
    end annExprToArg
  }

  private final class TupleExprFactory(override val loc: Loc, items: Seq[ExprFactory]) extends ExprFactory {
    override def check(t: Expr)(using state: EmitState): Comp[Expr] =
      normalizeToValue(t).flatMap {
        case t @ (Expr.TypeN(_) | Expr.TypeBigN(_) | Expr.AnyType()) =>
          for
            itemExprs <- ZIO.foreach(items) { item =>
              item.check(t)
            }
          yield Expr.Tuple(itemExprs)

        case Expr.Boxed(t) =>
          for
            tuple <- check(t)
          yield Expr.Box(t, tuple)

        case Expr.Tuple(itemTypes) if items.size != itemTypes.size =>
          ErrorLog.logError(CompilerError.TupleSizeMismatch(
            loc,
            TRToErrorShifter[context.type](context).shiftExpr(t),
            items.size,
          )).as(Expr.Error())

        case Expr.Tuple(itemTypes) =>
          for
            itemExprs <- ZIO.foreach(items.zip(itemTypes)) { (item, itemType) =>
              item.check(itemType)
            }
          yield Expr.Tuple(itemExprs)

        case Expr.Hole(hole) =>
          ZIO.foreach(items) { _ => makeHole(Expr.AnyType(), ErasureMode.TypeAnnotation, loc) }
            .flatMap { types =>
              val t2 = Expr.Tuple(types)
              checkTypesMatchNoBox(loc)(t, t2).as(t2)
            }
            .flatMap(check)

        case t =>
          ErrorLog.logError(CompilerError.TupleTypeRequired(
            loc,
            TRToErrorShifter[context.type](context).shiftExpr(t),
          )).as(Expr.Error())
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


  private def resolveImplicit(t: Expr, funcLocation: SourceLocation)(using EmitState): Comp[Expr] =
    tryResolveImplicit(t, funcLocation)
      .flatMap(_.fold(ErrorLog.logError(CompilerError.ImplicitNotFound(funcLocation)).as(Expr.Error()))(ZIO.succeed(_)))

  private def tryResolveImplicit(t: Expr, funcLocation: SourceLocation)(using state: EmitState): Comp[Option[Expr]] =
    val ir = ImplicitResolverImpl(funcLocation)
    for
      model <- state.model.get
      givens <- state.scope.givenAssertions

      assertions = givens.map(buildImplicits(ir))
      fuel = ir.FuelSpecifiers(
        evaluatorFuel = context.Config.evaluatorFuel,
        prologFuel = context.Config.prologFuel,
        smtFuel = context.Config.smtFuel,
      )

      kvv <- state.scope.knownVarValues

      res <- ir.tryResolve(t, model, assertions, kvv, fuel)

      res <- ZIO.foreach(res) {
        case ir.ResolvedImplicit(proof, model) =>
          def extractProof(proof: Proof[Expr]): Comp[Expr] =
            proof match {
              case Proof.Atomic(expr) => ZIO.succeed(expr)

              case Proof.ModusPonens(implication, premise) =>
                for
                  implication <- extractProof(implication)
                  premise <- extractProof(premise)
                yield Expr.FunctionObjectCall(implication, premise)

              case _ =>
                // TODO: Implement actual types for these proofs
                ZIO.succeed(Expr.ErasedValue())
            }

          state.model.set(model) *> extractProof(proof)
      }
    yield res
  end tryResolveImplicit


  private def buildImplicits(ir: ImplicitResolverImpl)(value: ImplicitValue): ir.AssertionBuilder =
    new ir.AssertionBuilder {
      override def create(newVariable: ZIO[context.Env, context.Error, Hole]): ZIO[context.Env, context.Error, ir.Assertion] =
        value match {
          case ImplicitValue.OfVar(variable) =>
            val loadVar = Expr.Variable(variable)
            val assertion = ir.Assertion(loadVar, variable.varType)
            ZIO.succeed(assertion)

          case ImplicitValue.OfFunction(function) =>
            def buildCall(sig: TRSignatureContext.FunctionSignature, args: Seq[Expr]): Comp[ir.Assertion] =
              sig.parameters.toList match
                case (param@TRSignatureContext.SignatureParameter(FunctionParameterListType.InferrableList | FunctionParameterListType.QuoteList, _, _, _, _)) :: tailParams =>
                  val variable = param.asParameterVar(TRExprContext.ParameterOwner.Func(function), args.size)
                  for
                    hole <- newVariable
                    holeExpr = Expr.Hole(hole)
                    nextSubst = sig.copy(parameters = tailParams).substituteVar(variable, holeExpr)

                    assertion <- buildCall(nextSubst, args :+ holeExpr)
                  yield assertion

                case (param@TRSignatureContext.SignatureParameter(FunctionParameterListType.RequiresList, isErased, _, paramName, paramType)) :: tailParams =>
                  val variable = param.asParameterVar(TRExprContext.ParameterOwner.Func(function), args.size)
                  for
                    varId <- UniqueIdentifier.make
                    local = LocalVar(varId, paramType, paramName, isMutable = false, isErased = isErased, isProof = false)
                    loadLocal = Expr.Variable(local)
                    ir.Assertion(witness, assertionType) <- buildCall(sig.copy(parameters = tailParams).substituteVar(variable, loadLocal), args :+ loadLocal)
                  yield ir.Assertion(
                    witness = Expr.Lambda(local, assertionType, witness),
                    assertionType = Expr.FunctionType(local, assertionType),
                  )

                case (param@TRSignatureContext.SignatureParameter(FunctionParameterListType.NormalList, _, _, _, _)) :: tailParams => ???

                case Nil =>
                  ZIO.succeed(ir.Assertion(
                    witness = Expr.FunctionCall(function, args),
                    assertionType = sig.returnType,
                  ))
              end match

            function.signature.flatMap { sig =>
              val sig2 = TRSignatureContext.signatureFromDefault(sig)
              buildCall(sig2, Seq())
            }

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
      state.model.get.map(m => m.resolveHole(hole).getOrElse(Expr.Hole(hole)))
  }


  private lazy val exprType = new ExprType {
    override val context: TypeResolver.this.context.type = TypeResolver.this.context
    override val exprContext: TRExprContext.type = TRExprContext
    override val sigContext: TRSignatureContext.type = TRSignatureContext

    override protected def getHoleType(hole: exprContext.HoleInfo): exprContext.Expr =
      hole.holeType
  }

  private final class ResolvedHoleFiller(model: TRExprContext.Model) extends ContextShifter[Comp] {
    override val ec1: context.TRExprContext.type = context.TRExprContext
    override val ec2: context.DefaultExprContext.type = context.DefaultExprContext


    override protected def shiftHole(hole: Hole): Comp[ec2.Expr] =
      model.resolveHole(hole) match {
        case Some(e) =>
          for
            modelRef <- Ref.make(model)
            localScope <- Scopes.LocalScope.make(Scopes.Empty)
            given EmitState = EmitState(
              model = modelRef,
              scope = localScope,
              effects = EffectInfo.Pure,
              erased = hole.erased,
            )
            et <- exprType.getExprType(e)
            e <- checkTypesMatch(hole.location)(e)(hole.holeType, et)
            e2 <- shiftExpr(e)
            _ <- ErrorLog.logError(CompilerError.ErasedExpressionNotAllowed(hole.location))
              .whenZIODiscard(ErasedScanner(context)(ec2)(context.DefaultSignatureContext)(e2))
          yield e2

        case None =>
          for
            _ <- ErrorLog.logError(CompilerError.CouldNotInfer(hole.location))
          yield ec2.Expr.Error()
      }
  }

  private final class ImplicitResolverImpl(resolveLocation: SourceLocation)(using state: EmitState) extends ImplicitResolver[context.Env, context.Error] {
    override val exprContext: TRExprContext.type = TRExprContext

    override def createHole: Comp[Hole] =
      // TODO: Figure out how to get the type of the hole
      makeHole(Expr.AnyType(), ErasureMode.Erased, resolveLocation).map(_.hole)

    override protected def evaluator(model: Ref[TRExprContext.Model]): Evaluator[context.Env, context.Error] {val exprContext: TRExprContext.type } =
      TREvaluator(using state.copy(model = model))
      
  }

  

}
