package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import dev.argon.parser
import dev.argon.util.{FileSpec, NamespacePath, SourceLocation, WithSource}
import dev.argon.util.AnyExtensions._

import scala.collection.immutable.Set
import cats.{Id => _, _}
import cats.data._
import cats.implicits._
import PayloadSpecifiers._
import cats.evidence.{===, Is}
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.parser.{BindingPattern, DeconstructPattern, DiscardPattern, TuplePattern, TypeTestPattern}
import shapeless.{:: => _, _}
import shapeless.ops.nat.{LT, Pred}
import zio.{IO, Ref, UIO, ZIO}
import zio.interop.catz._

import Function.const
import scala.collection.immutable

sealed trait ExpressionConverter[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem.Aux[context.type]
  val scopeContext: ScopeContext[context.type] { val typeSystem: ExpressionConverter.this.typeSystem.type }
  val signatureContext: SignatureContext.Aux2[context.type, typeSystem.TTypeWrapper]

  import typeSystem.{ context => _, _ }
  import scopeContext.{ context => _, _ }
  import signatureContext.{ Signature, SignatureParameters, SignatureVisitor }

  def getAllHoles: Comp[Seq[HoleId]]
  def createHole: Comp[TType]
  def recordConstraint(info: SubTypeInfo[TType]): Comp[Unit]
  def resolveType(t: TType): Comp[TType]
  def attemptRun[A](value: Comp[A]): Comp[Either[ErrorList, Comp[A]]]

  type Env = ExpressionConverter.Env[context.type, Scope]

  final case class ArgumentInfo(argFactory: ExprFactory, env: Env, location: SourceLocation, style: ParameterStyle)

  abstract class ExprFactory {
    def forExpectedType(expectedType: TType): Comp[SimpleExpr]
    def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory =
      compFactory(inferExprType(ExprFactory.this).flatMap { thisExpr =>

        def simplifyInstanceType(t: TType): Comp[Option[SimpleExpr]] =
          resolveType(t)
            .flatMap { t =>
              unwrapType(t).flatTraverse { t =>
                reduceExprToValue(t).map(unwrapType)
              }
            }

        def resolveMethodOverloads(instanceType: TypeWithMethods[context.type, TTypeWrapper])(methods: Comp[OverloadResult[MemberValue[context.type]]]) =
          methods
            .flatMap { overloads =>
              overloads
                .toList
                .traverse { _.traverse {
                  case MemberValue.Method(method) =>
                    Compilation.require(env.effectInfo.canCall(method.value.method.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                      .flatMap { _ =>
                        method.value.method.signature(signatureContext)(instanceType)
                      }
                      .map {
                        case sig: Signature[FunctionResultInfo, len] =>
                          signatureFactory[FunctionResultInfo, len](env)(location)(method.value.method.descriptor)(sig) { (args, result) =>
                            MethodCall(AbsRef(method.value.method), fromSimpleType(thisExpr), args, result.returnType)
                          }
                      }
                } }
                .map { Vector(_) }
            }


        def overloadsOfType(memberName: MemberName)(t: TType): Comp[Vector[List[NonEmptyVector[OverloadExprFactory]]]] =
          simplifyInstanceType(t).flatMap {
            case Some(resolvedTypeWithMethods: TypeWithMethods[context.type, TTypeWrapper]) =>
              resolveMethodOverloads(resolvedTypeWithMethods)(
                MethodLookup.lookupMethods(context)(typeSystem)(resolvedTypeWithMethods)(env.descriptor, env.fileSpec)(memberName)
              )

            case Some(funcType @ FunctionType(argType, resultType)) if memberName === MemberName.Call =>
              Vector(List(NonEmptyVector.of(new OverloadExprFactory {

                override def overloadDescriptor: CallableDescriptor = FunctionTypeCallDescriptor

                override def usedParamTypes: Vector[TType] = Vector()
                override def remainingParameterTypes: Vector[TType] = Vector(argType)

                override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
                  ExprFactory.this.forExpectedType(expectedType)

                override def forArguments(argInfo: ArgumentInfo): OverloadExprFactory =
                  new OverloadExprFactory {
                    override def overloadDescriptor: CallableDescriptor = FunctionTypeCallDescriptor

                    override def usedParamTypes: Vector[TType] = Vector(argType)
                    override def remainingParameterTypes: Vector[TType] = Vector()

                    private def result: ExprFactory =
                      compFactory(
                        for {
                          funcExpr <- ExprFactory.this.forExpectedType(fromSimpleType(funcType))
                          argExpr <- argInfo.argFactory.forExpectedType(argType)
                        } yield factoryForExpr(argInfo.env)(argInfo.location)(FunctionObjectCall(fromSimpleType(funcExpr), fromSimpleType(argExpr), resultType))
                      )

                    override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
                      result.forExpectedType(expectedType)

                    override def forArguments(argInfo: ArgumentInfo): OverloadExprFactory =
                      wrapNonOverloadFactory(result.forArguments(argInfo))
                  }
              }))).pure[Comp]

            case Some(TypeOfType(thisType)) =>
              simplifyInstanceType(thisType).flatMap {
                _.toList.toVector.flatTraverse { t =>
                  reduceExprToValue(t).flatMap { t =>

                    def methodBindingsToOverloads
                    [TPayloadSpec[_, _]]
                    (bindings: Vector[MethodBinding[context.type, TPayloadSpec]])
                    : OverloadResult[MemberValue[context.type]] =
                      NonEmptyVector.fromVector(bindings) match {
                        case Some(bindings) =>
                          OverloadResult.List(
                            bindings.map { binding => MemberValue.Method(AbsRef(binding)) },
                            OverloadResult.End
                          )

                        case None =>
                          OverloadResult.End
                      }

                    unwrapType(t).collect[Comp[Vector[List[NonEmptyVector[OverloadExprFactory]]]]] {
                      case t @ ClassType(_, _) =>
                        memberName match {
                          case MemberName.New =>
                            if (!env.allowAbstractConstructor && t.arClass.value.isAbstract)
                              Compilation.forErrors(CompilationError.AbstractClassConstructorCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                            else
                              t.arClass.value.classConstructors.flatMap { constructors =>
                                constructors
                                  .traverse {
                                    case ClassConstructorBinding(_, _, classCtor) =>
                                      Compilation.require(env.effectInfo.canCall(classCtor.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                                        .flatMap { _ =>
                                          classCtor.signature(signatureContext)(t)
                                        }
                                        .map {
                                          case sig: Signature[ClassConstructor.ResultInfo, len] =>
                                            signatureFactory[ClassConstructor.ResultInfo, len](env)(location)(classCtor.descriptor)(sig) { (args, _) =>
                                              ClassConstructorCall(t, AbsRef(classCtor), args)
                                            }
                                        }
                                  }
                                  .map { overloads => Vector(NonEmptyVector.fromVector(overloads).toList) }
                              }

                          case methodName: MethodName =>
                            resolveMethodOverloads(t)(
                              t.arClass.value.staticMethods
                                .flatMap {
                                  _.filter { binding => binding.name === methodName }
                                    .filterA { binding =>
                                      AccessCheck.checkInstance[context.type, t.arClass.PayloadSpec](env.descriptor, env.fileSpec, binding)
                                    }
                                }
                                .map(methodBindingsToOverloads)
                            )

                        }

                      case t @ TraitType(_, _) =>
                        memberName match {
                          case MemberName.New => Vector.empty[List[NonEmptyVector[OverloadExprFactory]]].pure[Comp]
                          case methodName: MethodName =>
                            resolveMethodOverloads(t)(
                              t.arTrait.value.staticMethods
                                .flatMap {
                                  _.filter { binding => binding.name === methodName }
                                    .filterA { binding =>
                                      AccessCheck.checkInstance[context.type, t.arTrait.PayloadSpec](env.descriptor, env.fileSpec, binding)
                                    }
                                }
                                .map(methodBindingsToOverloads)
                            )

                        }
                    }
                      .getOrElse {
                        Vector.empty[List[NonEmptyVector[OverloadExprFactory]]].pure[Comp]
                      }
                  }
                }
              }

            case Some(IntersectionType(a, b)) =>
              for {
                aOverloads <- overloadsOfType(memberName)(a)
                bOverloads <- overloadsOfType(memberName)(b)
              } yield aOverloads ++ bOverloads


            case _ => Vector.empty[List[NonEmptyVector[OverloadExprFactory]]].pure[Comp]
          }

        def mergeOverloadLists(a: List[NonEmptyVector[OverloadExprFactory]], b: List[NonEmptyVector[OverloadExprFactory]]): List[NonEmptyVector[OverloadExprFactory]] =
          (a, b) match {
            case (aHead :: aTail, bHead :: bTail) => (aHead ++: bHead) :: mergeOverloadLists(aTail, bTail)
            case (_, Nil) => a
            case (_, _) => b
          }

        def overloadsForName(memberName: MemberName): Comp[ExprFactory] =
          getExprType(thisExpr, includeExtraTypeOfType = false)
            .flatMap(overloadsOfType(memberName))
            .flatMap { overloads =>
              val mergedOverloads =
                overloads
                  .reduceOption(mergeOverloadLists)
                  .toList
                  .flatten
                  .map { overloadList => NonEmptyVector.fromVectorUnsafe(overloadList.toVector.distinctBy { _.overloadDescriptor }) }

              NonEmptyList.fromList(mergedOverloads) match {
                case Some(mergedOverloads) => overloadSelectionFactory(env)(location)(mergedOverloads).pure[Comp]
                case None =>
                  Compilation.forErrors(CompilationError.LookupFailedError(LookupDescription.Member(LookupDescription.Other, memberName), CompilationMessageSource.SourceFile(env.fileSpec, location)))
              }
            }

        memberName match {
          case MemberName.Normal(name) =>
            new ExprFactory {
              override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
                overloadsForName(memberName).flatMap { _.forExpectedType(expectedType) }

              override def memberAccessExpr(memberName2: MemberName, env: Env, location: SourceLocation): ExprFactory =
                compFactory(overloadsForName(memberName).map { _.memberAccessExpr(memberName2, env, location) })

              override def forArguments(argInfo: ArgumentInfo): ExprFactory =
                compFactory(overloadsForName(memberName).map { _.forArguments(argInfo) })

              override def mutateValue(env: Env, location: SourceLocation, newValue: ExprFactory): ExprFactory =
                compFactory(overloadsForName(MemberName.Mutator(name))).forArguments(ArgumentInfo(newValue, env, location, ParameterStyle.Normal))
            }.pure[Comp]

          case _ =>
            overloadsForName(memberName)
        }
      })



    def forArguments(argInfo: ArgumentInfo): ExprFactory =
      memberAccessExpr(MemberName.Call, argInfo.env, argInfo.location).forArguments(argInfo)

    def mutateValue(env: Env, location: SourceLocation, newValue: ExprFactory): ExprFactory =
      compFactory(
        Compilation.forErrors(CompilationError.InvalidLValue(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      )

  }

  abstract class OverloadExprFactory {

    def overloadDescriptor: CallableDescriptor
    def usedParamTypes: Vector[TType]
    def remainingParameterTypes: Vector[TType]

    def forExpectedType(expectedType: TType): Comp[SimpleExpr]
    def forArguments(argInfo: ArgumentInfo): OverloadExprFactory

    def toExprFactory: ExprFactory = new ExprFactory {
      override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
        OverloadExprFactory.this.forExpectedType(expectedType)

      override def forArguments(argInfo: ArgumentInfo): ExprFactory =
        OverloadExprFactory.this.forArguments(argInfo).toExprFactory
    }

    protected final def wrapNonOverloadFactory(factory: ExprFactory): OverloadExprFactory =
      new OverloadExprFactory {

        override def overloadDescriptor: CallableDescriptor = OverloadExprFactory.this.overloadDescriptor

        override def usedParamTypes: Vector[TType] = OverloadExprFactory.this.usedParamTypes
        override def remainingParameterTypes: Vector[typeSystem.TType] = OverloadExprFactory.this.remainingParameterTypes

        override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
          factory.forExpectedType(expectedType)

        override def forArguments(argInfo: ArgumentInfo): OverloadExprFactory =
          wrapNonOverloadFactory(factory.forArguments(argInfo))
      }

  }

  def convertExpr(env: Env)(expr: WithSource[parser.Expr]): ExprFactory =
    expr.value match {
      case parser.AsExpr(value, valueTypeExpr) =>
        compFactory(
          for {
            expectedType <- evaluateTypeExprAST(env)(valueTypeExpr)
            result <- convertExpr(env)(value).forExpectedType(expectedType)
          } yield factoryForExpr(env)(expr.location)(result)
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Add, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.AddInt, fromSimpleType(leftExpr), fromSimpleType(rightExpr), intType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Sub, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.SubInt, fromSimpleType(leftExpr), fromSimpleType(rightExpr), intType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Mul, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.MulInt, fromSimpleType(leftExpr), fromSimpleType(rightExpr), intType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Equal, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            boolType <- resolveBoolClass(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.IntEqual, fromSimpleType(leftExpr), fromSimpleType(rightExpr), boolType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Union, left, right) =>
        compFactory(
          for {
            leftExpr <- evaluateTypeExprAST(env)(left)
            rightExpr <- evaluateTypeExprAST(env)(right)
          } yield factoryForExpr(env)(expr.location)(UnionType(leftExpr, rightExpr))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Intersection, left, right) =>
        compFactory(
          for {
            leftExpr <- evaluateTypeExprAST(env)(left)
            rightExpr <- evaluateTypeExprAST(env)(right)
          } yield factoryForExpr(env)(expr.location)(IntersectionType(leftExpr, rightExpr))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Assign, left, right) =>
        convertExpr(env)(left).mutateValue(env, expr.location, convertExpr(env)(right))

      case parser.BlockExpr(body, Vector(), None, None) =>
        convertStmts(env)(body)

      case parser.BlockExpr(_, Vector(), Some(_), _) =>
        compFactory(
          Compilation.forErrors(
            CompilationError.ElseClauseWithoutRescue(CompilationMessageSource.SourceFile(env.fileSpec, expr.location))
          )
        )

      case parser.BlockExpr(body, Vector(), None, Some(ensureBody)) =>
        new ExprFactory {
          override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
            for {
              bodyExpr <- convertStmts(env)(body).forExpectedType(expectedType)
              unitType <- resolveUnitType(env)(expr.location)
              ensureExpr <- convertStmts(env)(ensureBody).forExpectedType(unitType)
            } yield EnsureExecuted(fromSimpleType(bodyExpr), fromSimpleType(ensureExpr))
        }

      case parser.BoolValueExpr(b) =>
        compFactory(
          for {
            boolType <- resolveBoolClass(env)(expr.location)
          } yield factoryForExpr(env)(expr.location)(LoadConstantBool(b, boolType))
        )

      case parser.ClassConstructorExpr(classExpr) =>
        convertExpr(env)(classExpr).memberAccessExpr(MemberName.New, env, expr.location)

      case parser.DotExpr(obj, member) =>
        convertExpr(env)(obj).memberAccessExpr(MemberName.Normal(member), env, expr.location)

      case parser.FunctionCallExpr(func, listType, arg) =>
        convertExpr(env)(func).forArguments(ArgumentInfo(convertExpr(env)(arg), env, arg.location, ParameterStyle.fromParser(listType)))

      case parser.IdentifierExpr(name) =>
        compFactory(
          env.scope.findIdentifier(name, env.fileSpec, expr.location)
            .map(createLookupFactory(env)(LookupDescription.Identifier(name))(expr.location))
        )

      case parser.IfExpr(cond, ifBody) =>
        createIfExpr(env)(expr.location)(cond, ifBody, WithSource(Vector.empty, SourceLocation(expr.location.end, expr.location.end)))

      case parser.IfElseExpr(cond, ifBody, elseBody) =>
        createIfExpr(env)(expr.location)(cond, ifBody, elseBody)

      case intExpr @ parser.IntValueExpr(_, _, _) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
          } yield factoryForExpr(env)(expr.location)(LoadConstantInt(intExpr.value, intType))
        )

      case parser.LambdaExpr(varName, body) =>
        new ExprFactory {
          override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
            for {
              argHole <- createHole
              resultHole <- createHole

              exprConverter <- convertExprTypeDelay(env)(expr.location)(fromSimpleType(FunctionType(argHole, resultHole)))(expectedType)

              varId <- VariableIdentifier.make

              argVar = LocalVariable(
                VariableDescriptor(env.descriptor, varId),
                varName.map(VariableName.Normal).getOrElse(VariableName.Unnamed),
                Mutability.NonMutable,
                argHole
              )
              env2 = env.copy(scope = env.scope.addVariable(argVar), effectInfo = EffectInfo.pure)

              bodyExpr <- convertExpr(env2)(body).forExpectedType(resultHole)

            } yield exprConverter(LoadLambda(argVar, fromSimpleType(bodyExpr)))
        }

      case parser.LambdaTypeExpr(argType, resultType) =>
        compFactory(
          for {
            argTypeValue <- evaluateTypeExprAST(env)(argType)
            resultTypeValue <- evaluateTypeExprAST(env)(resultType)
          } yield factoryForExpr(env)(expr.location)(FunctionType(argTypeValue, resultTypeValue))
        )

      case parser.MatchExpr(value, cases) =>
        new ExprFactory {
          override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
            for {
              valueExpr <- inferExprType(convertExpr(env)(value))
              valueExprType <- getExprType(valueExpr)
              casesNE = NonEmptyList.fromList(cases.toList).getOrElse { ??? }
              patternCases <- casesNE.traverse {
                case WithSource(parser.MatchExprCase(pattern, body), _) =>
                  for {
                    (patternExpr, env2) <- convertPattern(env)(valueExprType)(pattern)
                    bodyExpr <- convertStmts(env2)(body).forExpectedType(expectedType)
                  } yield PatternCase(patternExpr, fromSimpleType(bodyExpr))
              }
            } yield PatternMatch(fromSimpleType(valueExpr), patternCases)

          private def convertPattern(env: Env)(t: TType)(pattern: WithSource[parser.Pattern]): Comp[(PatternExpr[context.type, TTypeWrapper], Env)] =
            pattern.value match {
              case DeconstructPattern(constructor, args) => ???
              case TuplePattern(values) => ???
              case DiscardPattern =>
                for {
                  varId <- VariableIdentifier.make

                  variable = LocalVariable(
                    VariableDescriptor(env.descriptor, varId),
                    VariableName.Unnamed,
                    Mutability.NonMutable,
                    t
                  )
                  env2 = env.copy(scope = env.scope.addVariable(variable))

                } yield (PatternExpr.Binding(variable), env2)

              case BindingPattern(name) =>
                for {
                  varId <- VariableIdentifier.make

                  variable = LocalVariable(
                    VariableDescriptor(env.descriptor, varId),
                    VariableName.Normal(name),
                    Mutability.NonMutable,
                    t
                  )
                  env2 = env.copy(scope = env.scope.addVariable(variable))

                } yield (PatternExpr.Binding(variable), env2)

              case TypeTestPattern(name, patternType) =>
                for {
                  patT <- evaluateTypeExprAST(env)(patternType)
                  varId <- VariableIdentifier.make
                  variable = LocalVariable(
                    VariableDescriptor(env.descriptor, varId),
                    name.map(VariableName.Normal).getOrElse(VariableName.Unnamed),
                    Mutability.NonMutable,
                    patT
                  )
                  env2 = env.copy(scope = env.scope.addVariable(variable))
                } yield (PatternExpr.CastBinding(variable), env2)

            }

        }

      case parser.StringValueExpr(str) =>
        compFactory(
          for {
            stringType <- resolveModuleClass(env)(expr.location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("String"))
          } yield factoryForExpr(env)(expr.location)(LoadConstantString(str, stringType))
        )

      case parser.UnitLiteral =>
        compFactory(
          for {
            unitType <- resolveUnitType(env)(expr.location)
          } yield factoryForExpr(env)(expr.location)(LoadUnit(unitType))
        )

      case parser.TupleExpr(values) =>
        new ExprFactory {
          override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
            values
              .traverse { elem =>
                createHole.map { elemHole =>
                  (elem, elemHole)
                }
              }
              .flatMap { elemPairs =>
                val tupleType = fromSimpleType(LoadTuple(elemPairs.map { case (_, elemHole) => TupleElement(elemHole) }))
                convertExprTypeDelay(env)(expr.location)(tupleType)(expectedType).flatMap { exprTypeConv =>
                  elemPairs
                    .traverse { case (elem, elemHole) =>
                        convertExpr(env)(elem).forExpectedType(elemHole).map { elemExpr =>
                          TupleElement(fromSimpleType(exprTypeConv(elemExpr)))
                        }
                    }
                    .map { tupleElements =>
                      LoadTuple(tupleElements)
                    }
                }
              }
        }

      case parser.TypeExpr(level, instanceType, subtypeOf, supertypeOf) =>
        compFactory(
          for {
            inst <- instanceType.traverse(evaluateTypeExprAST(env)(_))
            sub <- subtypeOf.traverse(evaluateTypeExprAST(env)(_))
            sup <- supertypeOf.traverse(evaluateTypeExprAST(env)(_))
            inferredUniverse <- (sub.toList ++ sup.toList)
              .traverse(universeOfWrapExpr)
              .map { _.foldLeft[UniverseExpr](FixedUniverse(0))(LargestUniverse) }

            universe <- level match {
              case Some(WithSource(levelExpr @ parser.IntValueExpr(_, _, _), _)) =>
                val declaredUniverse = FixedUniverse(levelExpr.value)
                universeSubsumes(declaredUniverse, inferredUniverse).flatMap {
                  case true => declaredUniverse.upcast[UniverseExpr].pure[Comp]
                  case false => ???
                }

              case Some(_) => ???
              case None => inferredUniverse.pure[Comp]
            }

            typeN = TypeN(NextLargestUniverse(universe), sub, sup)
          } yield factoryForExpr(env)(expr.location)(
            inst.foldLeft[SimpleExpr](typeN) { (a, b) => IntersectionType(fromSimpleType(a), b)}
          )
        )

      case parser.TypeOfExpr(ofExpr) =>
        compFactory(
          inferExprType(convertExpr(env)(ofExpr)).flatMap { convExpr =>
            getExprType(convExpr).map { convExprType =>
              unwrapType(convExprType) match {
                case Some(t) => factoryForExpr(env)(expr.location)(t)
                case None => ???
              }
            }
          }
        )

      case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented: ${e.dumpInfo}")
    }

  def createIfExpr(env: Env)(location: SourceLocation)(cond: WithSource[parser.Expr], ifBody: WithSource[Vector[WithSource[parser.Stmt]]], elseBody: WithSource[Vector[WithSource[parser.Stmt]]]) =
    new ExprFactory {
      override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
        for {
          boolType <- resolveBoolClass(env)(location)
          condTC <- convertExpr(env)(cond).forExpectedType(boolType)
          ifBodyTC <- convertStmts(env)(ifBody).forExpectedType(expectedType)
          elseBodyTC <- convertStmts(env)(elseBody).forExpectedType(expectedType)
        } yield IfElse(fromSimpleType(condTC), fromSimpleType(ifBodyTC), fromSimpleType(elseBodyTC))
    }

  def convertStmts(env: Env)(stmts: WithSource[Vector[WithSource[parser.Stmt]]]): ExprFactory =
    stmts.value match {
      case Vector() => loadUnitLiteral(env)(stmts.location)

      case WithSource(stmt: parser.VariableDeclarationStmt, location) +: tail =>
        new ExprFactory {
          override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] = {
            val mutability = Mutability.fromIsMutable(stmt.isMutable)
            val varName = stmt.name match {
              case Some(name) => VariableName.Normal(name)
              case None => VariableName.Unnamed
            }

            for {
              _ <- Compilation.require(env.effectInfo.canDeclareVariable(mutability))(CompilationError.MutableVariableNotPureError(varName, CompilationMessageSource.SourceFile(env.fileSpec, location)))

              exprType <- stmt.varType match {
                case Some(varTypeExpr) => evaluateTypeExprAST(env)(varTypeExpr)
                case None => createHole
              }
              valueExpr <- convertExpr(env)(stmt.value).forExpectedType(exprType)
              varType <- resolveType(exprType)

              varId <- VariableIdentifier.make

              variable = LocalVariable(
                VariableDescriptor(env.descriptor, varId),
                varName,
                mutability,
                varType
              )
              env2 = env.copy(scope = env.scope.addVariable(variable))



              secondStartPos = tail.headOption.map { _.location.start }.getOrElse(stmts.location.end)
              second <- convertStmts(env2)(WithSource(tail, SourceLocation(secondStartPos, stmts.location.end))).forExpectedType(expectedType)
            } yield LetBinding(variable, fromSimpleType(valueExpr), fromSimpleType(second))

          }
        }

      case Vector(stmt) => convertStmt(env)(stmt)
      case head +: tail =>
        new ExprFactory {
          override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
            for {
              unitType <- resolveUnitType(env)(head.location)
              first <- convertStmt(env)(head).forExpectedType(unitType)

              secondStartPos = tail.headOption.map { _.location.start }.getOrElse(stmts.location.end)
              second <- convertStmts(env)(WithSource(tail, SourceLocation(secondStartPos, stmts.location.end))).forExpectedType(expectedType)
            } yield Sequence(fromSimpleType(first), fromSimpleType(second))
        }
    }

  def convertStmt(env: Env)(stmt: WithSource[parser.Stmt]): ExprFactory =
    stmt.value match {
      case expr: parser.Expr =>
        convertExpr(env)(WithSource(expr, stmt.location))

      case _ => ???
    }

  def resolveModuleClassFactory
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  (args: Vector[ArgumentInfo])
  : ExprFactory = {

    def resolveClass[ClassPS[_, _]](arClassOptComp: Comp[Vector[ArClass[context.type, ClassPS]]]): ExprFactory =
      compFactory(
        for {
          classesVec <- arClassOptComp
          classes <- Compilation.requireSome(
            NonEmptyVector.fromVector(classesVec)
          )(CompilationError.NamespaceElementNotFound(moduleDesc, namespacePath, name, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          classFactories <- classes.traverse { arClass =>
            arClass.signature
              .flatMap {
                case classSig: context.signatureContext.Signature[ArClass.ResultInfo, len] =>
                  convertSignature[ArClass.ResultInfo, len](classSig).map { convSig =>
                    val classFactory =
                      signatureFactory[ArClass.ResultInfo, len](env)(location)(arClass.descriptor)(convSig) { (args, classResult) =>
                        ClassType(AbsRef[context.type, ClassPS, ArClass](arClass), args.map(TypeArgument.Expr.apply))
                      }

                    args.foldLeft(classFactory) { (factory, arg) => factory.forArguments(arg) }
                  }
              }
          }

        } yield overloadSelectionFactory(env)(location)(NonEmptyList.of(classFactories))
      )

    if(moduleDesc === env.currentModule.descriptor)
      resolveClass(ModuleLookup.lookupNamespaceValues(context)(env.currentModule)(namespacePath, name)(ModuleLookup.lookupGlobalClass))
    else
      resolveClass(ModuleLookup.lookupValues(context)(env.referencedModules)(moduleDesc)(namespacePath, name)(ModuleLookup.lookupGlobalClass))
  }

  def resolveModuleClass
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  : Comp[TType] =
    evaluateTypeExprFactory(env)(location)(resolveModuleClassFactory(env)(location)(moduleDesc)(namespacePath, name)(Vector.empty))

  def resolveBoolClass(env: Env)(location: SourceLocation): Comp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Bool"))

  def resolveUnitType(env: Env)(location: SourceLocation): Comp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))

  def resolveIntType(env: Env)(location: SourceLocation): Comp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Int"))

  def loadUnitLiteral(env: Env)(location: SourceLocation): ExprFactory =
    compFactory(
      for {
        unitType <- resolveUnitType(env)(location)
      } yield factoryForExpr(env)(location)(LoadUnit(unitType))
    )

  def factoryForExpr(env: Env)(location: SourceLocation)(expr: SimpleExpr): ExprFactory =
    new ExprFactory {
      override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
        convertExprType(env)(location)(expr)(expectedType)
    }

  def createLookupFactory(env: Env)(description: LookupDescription)(location: SourceLocation)(lookupResult: LookupResult): ExprFactory =
    lookupResult match {
      case LookupResult.ScopeResult(scope) =>
        new ExprFactory {
          private def error[A]: Comp[A] = Compilation.forErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
            error

          override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory =
            compFactory(
              (memberName match {
                case MemberName.Normal(name) => scope.findIdentifier(name, env.fileSpec, location)
                case _ => LookupResult.Failed.upcast[LookupResult].pure[Comp]
              }).map(createLookupFactory(env)(LookupDescription.Member(description, memberName))(location)(_))
            )

          override def forArguments(argInfo: ArgumentInfo): ExprFactory =
            compFactory(error)
        }

      case LookupResult.SingleValueResult(VariableScopeValue(variable)) =>
        new ExprFactory {
          override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
            convertExprType(env)(location)(LoadVariable(variable))(expectedType)

          override def mutateValue(env: Env, location: SourceLocation, newValue: ExprFactory): ExprFactory =
            variable.mutability match {
              case Mutability.Mutable =>
                compFactory(
                  for {
                    unitType <- resolveUnitType(env)(location)
                    newValueExpr <- newValue.forExpectedType(variable.varType)
                  } yield factoryForExpr(env)(location)(StoreVariable(variable, fromSimpleType(newValueExpr), unitType))
                )

              case Mutability.NonMutable => super.mutateValue(env, location, newValue)
            }
        }

      case LookupResult.SingleValueResult(ParameterElementScopeValue(paramElem)) =>
        factoryForExpr(env)(location)(LoadTupleElement(fromSimpleType(LoadVariable(paramElem.paramVar)), paramElem.elemType, paramElem.index))

      case LookupResult.ValuesResult(overloads) =>
        compFactory(
          overloads
            .toNonEmptyList
            .traverse { _.traverse[Comp, OverloadExprFactory] {
              case FunctionScopeValue(func) =>
                Compilation.require(env.effectInfo.canCall(func.value.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                  .flatMap { _ =>
                    func.value.signature
                  }
                  .flatMap {
                    case sig: context.signatureContext.Signature[FunctionResultInfo, len] =>
                      convertSignature[FunctionResultInfo, len](sig).map { convSig =>
                        signatureFactory[FunctionResultInfo, len](env)(location)(func.value.descriptor)(convSig) { (args, result) =>
                          FunctionCall(func, args, result.returnType)
                        }
                      }
                  }

              case TraitScopeValue(arTrait) =>
                arTrait.value.signature
                  .flatMap {
                    case sig: context.signatureContext.Signature[ArTrait.ResultInfo, len] =>
                      convertSignature[ArTrait.ResultInfo, len](sig).map { convSig =>
                        signatureFactory(env)(location)(arTrait.value.descriptor)(convSig) { (args, result) =>
                          TraitType(arTrait, args.map(TypeArgument.Expr.apply))
                        }
                      }
                  }

              case ClassScopeValue(arClass) =>
                arClass.value.signature
                  .flatMap {
                    case sig: context.signatureContext.Signature[ArClass.ResultInfo, len] =>
                      convertSignature[ArClass.ResultInfo, len](sig).map { convSig =>
                        signatureFactory(env)(location)(arClass.value.descriptor)(convSig) { (args, result) =>
                          ClassType(arClass, args.map(TypeArgument.Expr.apply))
                        }
                      }
                  }

              case DataConstructorScopeValue(ctor) =>
                ctor.value.signature
                  .flatMap {
                    case sig: context.signatureContext.Signature[DataConstructor.ResultInfo, len] =>
                      convertSignature[DataConstructor.ResultInfo, len](sig).map { convSig =>
                        signatureFactory(env)(location)(ctor.value.descriptor)(convSig) { (args, result) =>
                          DataConstructorCall(
                            DataConstructorType(
                              ctor,
                              args.map(TypeArgument.Expr.apply),
                              result.instanceType
                            ),
                            args
                          )
                        }
                      }
                  }

            } }
            .map { overloads => overloadSelectionFactory(env)(location)(overloads) }
        )

      case LookupResult.Failed =>
        new ExprFactory {
          private def error[A]: Comp[A] =
            Compilation.forErrors(CompilationError.LookupFailedError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] = error

          override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory = this
          override def forArguments(argInfo: ArgumentInfo): ExprFactory =
            this

        }
    }

  def overloadSelectionFactory(env: Env)(location: SourceLocation)(overloads: NonEmptyList[NonEmptyVector[OverloadExprFactory]]): ExprFactory = {

    final class OverloadSelectionFactory(argCount: Int)(overloads: NonEmptyList[NonEmptyVector[OverloadExprFactory]]) extends ExprFactory {

      private def prioritizedOverloads: Comp[NonEmptyList[NonEmptyVector[OverloadExprFactory]]] = {

        def insert(acc: NonEmptyList[NonEmptyVector[OverloadExprFactory]], item: OverloadExprFactory): Comp[NonEmptyList[NonEmptyVector[OverloadExprFactory]]] =
          acc.head.existsM(isBetterThan(_, item)).flatMap {
            case false => NonEmptyList(acc.head :+ item, acc.tail).pure[Comp]
            case true =>
              acc.tail match {
                case Nil => NonEmptyList(acc.head, NonEmptyVector.of(item) :: Nil).pure[Comp]
                case head :: tail =>
                  insert(NonEmptyList(head, tail), item).map { _.prepend(acc.head) }
              }
          }

        overloads.flatTraverse { overloadList =>
          overloadList.reduceLeftM(overload => NonEmptyList.of(NonEmptyVector.of(overload)).pure[Comp])(insert)
        }
      }


      // a is better than b
      private def isBetterThan(a: OverloadExprFactory, b: OverloadExprFactory): Comp[Boolean] =
        if(a.usedParamTypes.size === argCount && (b.usedParamTypes.size =!= argCount || b.remainingParameterTypes.nonEmpty))
          true.pure[Comp]
        else if(a.usedParamTypes.size > b.usedParamTypes.size)
          true.pure[Comp]
        else if(a.remainingParameterTypes.size < b.remainingParameterTypes.size)
          true.pure[Comp]
        else {
          val typePairs = (a.usedParamTypes ++ a.remainingParameterTypes)
            .zip(b.usedParamTypes ++ b.remainingParameterTypes)

          typePairs.existsM {
            case (aType, bType) => isSubType(aType, bType).map { _.isDefined }
          }.flatMap {
            case true => false.pure[Comp]
            case false =>
              typePairs.existsM {
                case (aType, bType) => isSubType(bType, aType).map { _.isDefined }
              }
          }
        }



      private def runSameLevelOverloads
      (overloads: NonEmptyVector[OverloadExprFactory])
      (expectedType: TType)
      : Comp[NonEmptyVector[(CallableDescriptor, Either[NonEmptyList[CompilationError], Comp[SimpleExpr]])]] =
        overloads.traverse { overload =>
          attemptRun(overload.forExpectedType(expectedType))
            .map { (overload.overloadDescriptor, _) }
        }

      type FailedOverload = (CallableDescriptor, NonEmptyList[CompilationError])
      type GoodOverload = (CallableDescriptor, Comp[SimpleExpr])

      private def splitCallsAndErrors
      (results: NonEmptyVector[(CallableDescriptor, Either[NonEmptyList[CompilationError], Comp[SimpleExpr]])])
      : Either[NonEmptyVector[FailedOverload], NonEmptyVector[GoodOverload]] = {

        def impl
        (results: Vector[(CallableDescriptor, Either[NonEmptyList[CompilationError], Comp[SimpleExpr]])])
        (acc: Either[NonEmptyVector[FailedOverload], NonEmptyVector[GoodOverload]])
        : Either[NonEmptyVector[FailedOverload], NonEmptyVector[GoodOverload]] =
          (results, acc) match {
            case ((_, Left(_)) +: tail, Right(_)) => impl(tail)(acc)
            case ((desc, Right(expr)) +: tail, Right(exprs)) => impl(tail)(Right(exprs :+ ((desc, expr))))
            case ((desc, Left(errors)) +: tail, Left(errorLists)) => impl(tail)(Left(errorLists :+ ((desc, errors))))
            case ((desc, Right(expr)) +: tail, Left(_)) => impl(tail)(Right(NonEmptyVector.of((desc, expr))))

            case (Vector(), _) => acc
          }

        results match {
          case NonEmptyVector((desc, Left(errors)), tail) => impl(tail)(Left(NonEmptyVector.of((desc, errors))))
          case NonEmptyVector((desc, Right(expr)), tail) => impl(tail)(Right(NonEmptyVector.of((desc, expr))))
        }
      }

      private def attemptOverloads(remaining: Ior[NonEmptyVector[FailedOverload], NonEmptyList[NonEmptyVector[OverloadExprFactory]]])(expectedType: TType): Comp[SimpleExpr] = {

        def attemptHead(head: NonEmptyVector[OverloadExprFactory])(fallback: NonEmptyVector[FailedOverload] => Comp[SimpleExpr]): Comp[SimpleExpr] =
          runSameLevelOverloads(head)(expectedType)
            .map(splitCallsAndErrors)
            .flatMap {
              case Right(NonEmptyVector((_, expr), Vector())) => expr

              case Right(exprs) =>
                Compilation.forErrors(CompilationError.AmbiguousLookupError(
                  exprs.map { case (desc, _) => desc },
                  CompilationMessageSource.SourceFile(env.fileSpec, location)
                ))

              case Left(failed) => fallback(failed)
            }

        remaining match {
          case Ior.Right(NonEmptyList(head, Nil)) =>
            attemptHead(head) { failed =>
              attemptOverloads(Ior.Left(failed))(expectedType)
            }

          case Ior.Right(NonEmptyList(head, thead :: ttail)) =>
            attemptHead(head) { failed =>
              attemptOverloads(Ior.Both(failed, NonEmptyList(thead, ttail)))(expectedType)
            }

          case Ior.Both(prevFailed, NonEmptyList(head, Nil)) =>
            attemptHead(head) { failed =>
              attemptOverloads(Ior.Left(prevFailed ++: failed))(expectedType)
            }

          case Ior.Both(prevFailed, NonEmptyList(head, thead :: ttail)) =>
            attemptHead(head) { failed =>
              attemptOverloads(Ior.Both(prevFailed ++: failed, NonEmptyList(thead, ttail)))(expectedType)
            }

          case Ior.Left(NonEmptyVector((_, errors), Vector())) =>
              Compilation.forErrors(errors)

          case Ior.Left(failed) =>
            Compilation.forErrors(CompilationError.OverloadedLookupFailed(
              failed,
              CompilationMessageSource.SourceFile(env.fileSpec, location)
            ))
        }
      }

      override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
        prioritizedOverloads.flatMap { overloads =>
          attemptOverloads(Ior.Right(overloads))(expectedType)
        }

      override def forArguments(argInfo: ArgumentInfo): ExprFactory =
        new OverloadSelectionFactory(argCount + 1)(overloads.map { _.map { _.forArguments(argInfo) } })

    }

    new OverloadSelectionFactory(argCount = 0)(overloads)
  }

  def compFactory(compFac: Comp[ExprFactory]): ExprFactory =
    new ExprFactory {
      override def forExpectedType(expectedType: typeSystem.TType): Comp[SimpleExpr] =
        compFac.flatMap { _.forExpectedType(expectedType) }

      override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory =
        compFactory(
          compFac.map { _.memberAccessExpr(memberName, env, location) }
        )

      override def forArguments(argInfo: ArgumentInfo): ExprFactory =
        compFactory(
          compFac.map { _.forArguments(argInfo) }
        )

      override def mutateValue(env: Env, location: SourceLocation, newValue: ExprFactory): ExprFactory =
        compFactory(
          compFac.map { _.mutateValue(env, location, newValue) }
        )
    }

  def signatureFactory[TResult[TContext2 <: Context with Singleton, Wrap[+_]], FullLen <: Nat]
  (env: Env)
  (location: SourceLocation)
  (descriptor: ParameterOwnerDescriptor)
  (fullSignature: Signature[TResult, FullLen])
  (f: (Vector[WrapExpr], TResult[context.type, TTypeWrapper]) => SimpleExpr)
  : OverloadExprFactory = {

    def signatureNextPart[RestLen <: Nat](sig: SignatureParameters[TResult, RestLen])(arg: WrapExpr): Comp[Signature[TResult, RestLen]] =
      if(isWrapExprPure(arg))
        sig.next(arg).pure[Comp]
      else
      sig.nextUnsubstituted.referencesParameter(sig.parameter).flatMap {
        case false => sig.nextUnsubstituted.pure[Comp]
        case true =>
          Compilation.forErrors(CompilationError.ArgumentToSignatureDependencyNotPureError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      }

    final class SigFactory[Len <: Nat](env: Env)(unsubSig: Signature[TResult, Len])(prevParamTypes: Vector[TType])(acc: Comp[(Signature[TResult, Len], Vector[WrapExpr])]) extends OverloadExprFactory {


      override def overloadDescriptor: ParameterOwnerDescriptor = descriptor


      override def usedParamTypes: Vector[TType] = prevParamTypes
      override lazy val remainingParameterTypes: Vector[TType] = unsubSig.unsubstitutedParameters.unsized.map { _.paramType }

      override def forExpectedType(expectedType: TType): Comp[SimpleExpr] =
        acc.flatMap {
          case (sig, args) =>
            sig.visit(new SignatureVisitor[TResult, Len, Comp[SimpleExpr]] {
              override def visitParameters[RestLen <: Nat](sigParams: signatureContext.SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): Comp[SimpleExpr] =
                partiallyApply(env, args, Vector(), fullSignature, identity).flatMap { expr =>
                  convertExprType(env)(location)(expr)(expectedType)
                }

              override def visitResult(sigResult: signatureContext.SignatureResult[TResult])(implicit lenEq: Len === _0): Comp[SimpleExpr] =
                convertExprType(env)(location)(f(args, sigResult.result))(expectedType)
            })
        }

      override def forArguments(argInfo: ArgumentInfo): OverloadExprFactory =
        unsubSig.visit(new SignatureVisitor[TResult, Len, OverloadExprFactory] {
          override def visitParameters[RestLen <: Nat](unsubSigParams: signatureContext.SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): OverloadExprFactory = {
            def createFactory(createExpr: TType => Comp[WrapExpr]) =
              new SigFactory(env)(unsubSigParams.nextUnsubstituted)(prevParamTypes :+ unsubSigParams.parameter.paramType)(
                acc.flatMap {
                  case (sig, prevArgs) =>
                    val sigParams = sig.toSignatureParameters

                    for {
                      argExpr <- createExpr(sigParams.parameter.paramType)
                      next <- signatureNextPart(sigParams)(argExpr)
                    } yield (next, prevArgs :+ argExpr)
                }
              )

            (argInfo.style, unsubSigParams.parameter.style) match {
              case (ParameterStyle.Normal, ParameterStyle.Normal) | (ParameterStyle.Inferrable, ParameterStyle.Inferrable) =>
                createFactory { expectedType => argInfo.argFactory.forExpectedType(expectedType).map(fromSimpleType) }

              case (_, ParameterStyle.Inferrable) =>
                createFactory { expectedType =>
                  createHole
                }.forArguments(argInfo)

              case (_, _) => ???
            }
          }

          override def visitResult(sigResult: signatureContext.SignatureResult[TResult])(implicit lenEq: Len === _0): OverloadExprFactory =
            wrapNonOverloadFactory(
              toExprFactory.memberAccessExpr(MemberName.Call, argInfo.env, argInfo.location).forArguments(argInfo)
            )
        }
        )
    }

    def partiallyApply[Len <: Nat](env: Env, prevArgs: Vector[WrapExpr], argVariables: Vector[WrapExpr], signature: Signature[TResult, Len], wrapInLambda: SimpleExpr => SimpleExpr): Comp[SimpleExpr] =
      signature.visit(new SignatureVisitor[TResult, Len, Comp[SimpleExpr]] {
        override def visitParameters[RestLen <: Nat](sigParams: signatureContext.SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): Comp[SimpleExpr] =
          prevArgs match {
            case Vector() =>
              for {
                varId <- VariableIdentifier.make
                newVar = LocalVariable(VariableDescriptor(env.descriptor, varId), VariableName.Unnamed, Mutability.NonMutable, sigParams.parameter.paramType)
                env2 = env.copy(scope = env.scope.addVariable(newVar))
                newVarExpr = LoadVariable(newVar)

                nextSig <- signatureNextPart(sigParams)(fromSimpleType(newVarExpr))
                result <- partiallyApply(env2, prevArgs, argVariables :+ fromSimpleType(newVarExpr), nextSig, inner => wrapInLambda(LoadLambda(newVar, fromSimpleType(inner))))
              } yield result

            case head +: tail =>
              for {
                varId <- VariableIdentifier.make
                newVar = LocalVariable(VariableDescriptor(env.descriptor, varId), VariableName.Unnamed, Mutability.NonMutable, sigParams.parameter.paramType)
                env2 = env.copy(scope = env.scope.addVariable(newVar))
                newVarExpr = LoadVariable(newVar)
                nextSig <- signatureNextPart(sigParams)(fromSimpleType(newVarExpr))
                result <- partiallyApply(env2, tail, argVariables :+ fromSimpleType(newVarExpr), nextSig, inner => wrapInLambda(LetBinding(newVar, head, fromSimpleType(inner))))
              } yield result
          }

        override def visitResult(sigResult: signatureContext.SignatureResult[TResult])(implicit lenEq: Len === _0): Comp[SimpleExpr] =
          wrapInLambda(f(argVariables, sigResult.result)).pure[Comp]
      })

    new SigFactory(env)(fullSignature)(Vector.empty)((fullSignature, Vector.empty[WrapExpr]).pure[Comp])
  }

  def convertSignature[TResult[TContext2 <: Context with Singleton, Wrap[+_]], Len <: Nat]
  (sig: context.signatureContext.Signature[TResult, Len])
  : Comp[Signature[TResult, Len]] =
    sig.convertTypeSystem(signatureContext)(ArTypeSystemConverter[TTypeWrapper](context))

  def convertExprType(env: Env)(location: SourceLocation)(expr: SimpleExpr)(t: typeSystem.TType): Comp[SimpleExpr] =
    getExprType(expr).flatMap { exprType =>
      convertExprTypeDelay(env)(location)(exprType)(t)
        .map { f => f(expr) }
    }

  def convertExprTypeDelay(env: Env)(location: SourceLocation)(exprType: typeSystem.TType)(t: typeSystem.TType): Comp[SimpleExpr => SimpleExpr] =
    typeSystem.isSubType(t, exprType).flatMap {
      case Some(info) => recordConstraint(info).as(identity)
      case None => Compilation.forErrors(CompilationError.CouldNotConvertType(typeSystem)(exprType, t)(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }

  def evaluateTypeExprFactory(env: Env)(location: SourceLocation)(factory: ExprFactory): Comp[TType] =
    inferExprType(factory).flatMap { expr =>
      validateTypeExpr(env)(location)(expr).map { _ => fromSimpleType(expr) }
    }

  def validateTypeExpr(env: Env)(location: SourceLocation)(expr: SimpleExpr): Comp[Unit] = {
    def invalidType[A]: Comp[A] = Compilation.forErrors(CompilationError.ExpressionNotTypeError(CompilationMessageSource.SourceFile(env.fileSpec, location)))

    expr match {

      case TypeOfType(_) | TypeN(_, _, _) |
           TraitType(_, _) | ClassType(_, _) |
           DataConstructorType(_, _, _) | FunctionType(_, _) |
           UnionType(_, _) | IntersectionType(_, _) => ().pure[Comp]

      case LoadTuple(values) =>
        values
          .traverse_ {
            case TupleElement(value) =>
              value.traverse(validateTypeExpr(env)(location)(_))
          }

      case expr if isExprPure(expr) =>
        def isMetaType(t: TType): Boolean = unwrapType(t) match {
          case Some(TypeOfType(_) | TypeN(_, _, _)) => true
          case Some(LoadTuple(values)) =>
            values.forall { elem => isMetaType(elem.value) }
          case Some(IntersectionType(first, second)) =>
            isMetaType(first) || isMetaType(second)

          case _ => false
        }

        getExprType(expr).flatMap { exprType =>
          if(isMetaType(exprType))
            ().pure[Comp]
          else
            invalidType
        }

      case _ => invalidType
    }
  }

  def evaluateTypeExprAST(env: Env)(expr: WithSource[parser.Expr]): Comp[TType] =
    evaluateTypeExprFactory(env)(expr.location)(convertExpr(env)(expr))


  private def inferExprType(factory: ExprFactory): Comp[SimpleExpr] =
    for {
      hole <- createHole
      expr <- factory.forExpectedType(hole)
      _ <- resolveType(hole)
    } yield expr



  def isExprPure(expr: SimpleExpr): Boolean =
    expr match {
      case ClassConstructorCall(_, _, args) => args.forall(isWrapExprPure)
      case DataConstructorCall(_, args) => args.forall(isWrapExprPure)
      case EnsureExecuted(body, ensuring) => isWrapExprPure(body) && isWrapExprPure(ensuring)
      case FunctionCall(_, args, _) => args.forall(isWrapExprPure)
      case FunctionObjectCall(function, arg, _) => isWrapExprPure(function) && isWrapExprPure(arg)
      case IfElse(condition, ifBody, elseBody) => isWrapExprPure(condition) && isWrapExprPure(ifBody) && isWrapExprPure(elseBody)
      case LetBinding(_, value, next) => isWrapExprPure(value) && isWrapExprPure(next)
      case LoadConstantBool(_, _) => true
      case LoadConstantInt(_, _) => true
      case LoadConstantString(_, _) => true
      case LoadLambda(_, _) => true
      case LoadTuple(values) => values.forall { case TupleElement(value) => isWrapExprPure(value) }
      case LoadTupleElement(tupleValue, _, _) => isWrapExprPure(tupleValue)
      case LoadUnit(_) => true
      case LoadVariable(variable) => !Mutability.toIsMutable(variable.mutability)
      case MethodCall(_, instance, args, _) => isWrapExprPure(instance) && args.forall(isWrapExprPure)
      case PatternMatch(expr, cases) => isWrapExprPure(expr) && cases.forall { case PatternCase(_, body) => isWrapExprPure(body) }
      case PrimitiveOp(_, left, right, _) => isWrapExprPure(left) && isWrapExprPure(right)
      case Sequence(first, second) => isWrapExprPure(first) && isWrapExprPure(second)
      case StoreVariable(_, _, _) => false
      case TraitType(_, args) => args.forall(isTypeArgPure)
      case ClassType(_, args) => args.forall(isTypeArgPure)
      case DataConstructorType(_, args, _) => args.forall(isTypeArgPure)
      case TypeOfType(inner) => isWrapExprPure(inner)
      case TypeN(_, subtypeConstraint, supertypeConstraint) => subtypeConstraint.forall(isWrapExprPure) && supertypeConstraint.forall(isWrapExprPure)
      case FunctionType(argumentType, resultType) => isWrapExprPure(argumentType) && isWrapExprPure(resultType)
      case UnionType(first, second) => isWrapExprPure(first) && isWrapExprPure(second)
      case IntersectionType(first, second) => isWrapExprPure(first) && isWrapExprPure(second)
    }

  def isWrapExprPure(expr: WrapExpr): Boolean =
    expr.traverse { t =>
      if(isExprPure(t)) Some(()) else None
    }.isDefined

  def isTypeArgPure(arg: TTypeArgument): Boolean =
    arg match {
      case TypeArgument.Expr(expr) => isWrapExprPure(expr)
      case TypeArgument.Wildcard(_) => true
    }



}

object ExpressionConverter {

  final case class Env[TContext <: Context with Singleton, TScope]
  (
    effectInfo: EffectInfo,
    descriptor: VariableOwnerDescriptor,
    fileSpec: FileSpec,
    currentModule: ArModule[TContext, DeclarationPayloadSpecifier],
    referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]],
    scope: TScope,
    allowAbstractConstructor: Boolean,
  )

  trait EnvCreator[TContext <: Context with Singleton] {
    def apply(context: TContext)(effectInfo: EffectInfo, descriptor: VariableOwnerDescriptor): Env[context.type, context.scopeContext.Scope]

    def addVariables(context: TContext)(variables: Vector[Variable[context.type, Id]]): EnvCreator[TContext]
    def addVariable(context: TContext)(variable: Variable[context.type, Id]): EnvCreator[TContext] =
      addVariables(context)(Vector(variable))

    def addParameters(context: TContext)(params: Vector[Parameter[context.type, Id]]): EnvCreator[TContext]

    val fileSpec: FileSpec
    val currentModule: ArModule[TContext, DeclarationPayloadSpecifier]
    val referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]]
  }

  private def createConverter(ctx: Context)(ts: HoleTypeSystem { val baseTypeSystem: ctx.typeSystem.type })
  : UIO[ExpressionConverter[ctx.type] { val typeSystem: ts.type }] = for {
    state <- Ref.make(TypeCheckState.default[ts.TType])
  } yield new ExpressionConverter[ctx.type] {
    override val context: ctx.type = ctx
    override val typeSystem: ts.type = ts

    import typeSystem.TType

    override def getAllHoles: Comp[Seq[HoleId]] =
      state.get.map { _.constraints.keys.toSeq }

    override def createHole: Comp[TType] =
      HoleId.make
        .tap { id =>
          state.update { oldState =>
            oldState.copy(
              constraints = oldState.constraints.updated(id, HoleBounds(Set.empty, Set.empty))
            )
          }
        }
        .map(HoleTypeHole.apply)

    private def addConstraint(id: HoleId, prop: Lens[HoleBounds[ts.TType], Set[ts.TType]], constraint: ts.TType): Comp[Unit] =
      state.update { oldState =>
        oldState.constraints.getOrElse(id, HoleBounds(Set.empty, Set.empty)) match {
          case HoleResolved(_) => oldState
          case bounds @ HoleBounds(_, _) =>
            oldState.copy(constraints = oldState.constraints.updated(id, prop.modify(bounds){ _ + constraint }))
        }
      }

    private val subTypeBoundsLens: Lens[HoleBounds[ts.TType], Set[ts.TType]] = new Lens[HoleBounds[ts.TType], Set[ts.TType]] {
      override def get(s: HoleBounds[ts.TType]): Set[ts.TType] = s.subTypeBounds
      override def set(s: HoleBounds[ts.TType])(a: Set[ts.TType]): HoleBounds[ts.TType] = s.copy(subTypeBounds = a)
    }

    private val superTypeBoundsLens: Lens[HoleBounds[ts.TType], Set[ts.TType]] = new Lens[HoleBounds[ts.TType], Set[ts.TType]] {
      override def get(s: HoleBounds[ts.TType]): Set[ts.TType] = s.superTypeBounds
      override def set(s: HoleBounds[ts.TType])(a: Set[ts.TType]): HoleBounds[ts.TType] = s.copy(superTypeBounds = a)
    }

    override def recordConstraint(info: SubTypeInfo[TType]): Comp[Unit] =
      (info.subType, info.superType) match {
        case (a @ HoleTypeHole(idA), b @ HoleTypeHole(idB)) =>
          addConstraint(idA, superTypeBoundsLens, b).flatMap { _ =>
            addConstraint(idB, subTypeBoundsLens, a)
          }

        case (HoleTypeHole(idA), b) =>
          addConstraint(idA, superTypeBoundsLens, b)

        case (a, HoleTypeHole(idB)) =>
          addConstraint(idB, subTypeBoundsLens, a)

        case (HoleTypeType(_), HoleTypeType(_)) =>
          info.args.traverse_(recordConstraint(_))
      }

    private def getConstraints(id: HoleId): Comp[HoleConstraint[ts.TType]] =
      state.get.map { stateValue =>
        stateValue.constraints.getOrElse(id, HoleBounds(Set.empty, Set.empty))
      }

    override def resolveType(t: TType): Comp[TType] =
      (new ResolverConverter).convertTypeSystem(t)

    private final class ResolverConverter extends TypeSystemConverter {

      override val context: ctx.type = ctx
      override type FromWrap[+A] = typeSystem.TTypeWrapper[A]
      override type ToWrap[+A] = typeSystem.TTypeWrapper[A]

      override protected val fromWrapInstances: WrapperInstance[HoleType] = implicitly
      override protected val toWrapInstances: WrapperInstance[HoleType] = implicitly

      override protected def convertType[A](fromExpr: ArExpr[context.type, FromWrap] => Comp[A])(t: ts.TTypeWrapper[A]): Comp[ts.TTypeWrapper[A]] =
        t match {
          case HoleTypeType(_) => IO.succeed(t)
          case HoleTypeHole(id) =>
            for {
              resolvedOuter <- resolveOuterHole(id)
              resolvedType <- convertTypeSystem(resolvedOuter)
              result <- resolvedType.traverse(fromExpr)
            } yield result
        }

    }

    private def updateConstraints(id: HoleId, constraints: HoleConstraint[ts.TType]): Comp[Unit] =
      state.update { stateValue =>
        stateValue.copy(constraints = stateValue.constraints.updated(id, constraints))
      }

    private def resolveOuterHole(id: HoleId): Comp[ts.TType] =
      getConstraints(id).flatMap  {
        case HoleResolved(hole) => IO.succeed(hole)
        case bounds @ HoleBounds(_, _) =>

          def resolveConstraints
          (pick: Lens[HoleBounds[ts.TType], Set[ts.TType]], otherSide: Lens[HoleBounds[ts.TType], Set[ts.TType]])
          (combine: (ts.TType, ts.TType) => ts.SimpleExpr)
          : Option[Comp[ts.TType]] =
            NonEmptyList.fromList(pick.get(bounds).toList).map {
              _.traverse {
                case t @ HoleTypeType(_) => IO.succeed(t)
                case t @ HoleTypeHole(constraintHoleId) =>
                  getConstraints(constraintHoleId)
                    .flatMap {
                      case HoleResolved(_) => IO.unit
                      case resBounds @ HoleBounds(_, _) =>
                        val newBounds = otherSide.set(resBounds)(
                          otherSide.get(resBounds).excl(HoleTypeHole(id)) ++ otherSide.get(bounds).excl(t)
                        )
                        updateConstraints(constraintHoleId, newBounds).map { _ =>  }
                    }
                    .map { _ => t }
              }
                .widen[NonEmptyList[ts.TType]]
                .map { _.reduceLeft { (a, b) => ts.fromSimpleType(combine(a, b)) } }
            }

          for {
            resolvedType <- resolveConstraints(subTypeBoundsLens, superTypeBoundsLens)(IntersectionType(_, _))
              .orElse { resolveConstraints(superTypeBoundsLens, subTypeBoundsLens)(UnionType(_, _)) }
              .getOrElse { ??? }

            _ <- updateConstraints(id, HoleResolved(resolvedType))
          } yield resolvedType
      }

    override def attemptRun[A](value: Comp[A]): Comp[Either[ErrorList, Comp[A]]] =
      for {
      prevState <- state.get
      v <- value.either
      newState <- state.getAndSet(prevState)
    } yield v.map(state.set(newState).as(_))

    override val scopeContext: ScopeContext[context.type] { val typeSystem: ts.type } =
      new ScopeContext[context.type] {
        override val context: ctx.type = ctx
        override val typeSystem: ts.type = ts
      }

    override val signatureContext: SignatureContext.Aux2[ctx.type, ts.TTypeWrapper] =
      new SignatureContext {
        override val context: ctx.type = ctx
        override type TTypeWrapper[+A] = ts.TTypeWrapper[A]
        override implicit val typeWrapperInstances: WrapperInstance[TTypeWrapper] = ts.typeWrapperInstances
      }
  }

  def convertStatementList
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (stmts: WithSource[Vector[WithSource[parser.Stmt]]])
  : Comp[ArExpr[context.type, Id]] = {

    val ts = HoleTypeSystem(context.typeSystem)

    for {
      converter <- createConverter(context)(ts)

      tsConverter = HoleTypeSystem.holeTypeConverter(context)(context.typeSystem)(ts)

      env2 = Env(
        effectInfo = env.effectInfo,
        descriptor = env.descriptor,
        fileSpec = env.fileSpec,
        currentModule = env.currentModule,
        referencedModules = env.referencedModules,
        scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
        allowAbstractConstructor = env.allowAbstractConstructor,
      )

      convExpectedType <- tsConverter.convertTypeSystem(expectedType)
      convExpr <- fillHoles(context)(ts)(converter)(
        converter
          .convertStmts(env2)(stmts)
          .forExpectedType(convExpectedType)
          .map(ts.fromSimpleType)
      )
    } yield convExpr
  }

  def convertExpression
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : Comp[ArExpr[context.type, Id]] =
    convertStatementList(context)(env)(expectedType)(WithSource(Vector(expr), expr.location))




  def convertTypeExpression
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (expr: WithSource[parser.Expr])
  : Comp[context.typeSystem.TType] = {

    val ts = HoleTypeSystem(context.typeSystem)

    for {
      converter <- createConverter(context)(ts)

      tsConverter = HoleTypeSystem.holeTypeConverter(context)(context.typeSystem)(ts)

      env2 = Env(
        effectInfo = env.effectInfo,
        descriptor = env.descriptor,
        fileSpec = env.fileSpec,
        currentModule = env.currentModule,
        referencedModules = env.referencedModules,
        scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
        allowAbstractConstructor = env.allowAbstractConstructor,
      )

      tcExpr = converter.evaluateTypeExprAST(env2)(expr)

      filled <- fillHoles(context)(ts)(converter)(tcExpr)
    } yield filled
  }


  def resolveUnitType
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (location: SourceLocation): Comp[context.typeSystem.TType] = {

    val ts = HoleTypeSystem(context.typeSystem)

    for {
      converter <- createConverter(context)(ts)

      tsConverter = HoleTypeSystem.holeTypeConverter(context)(context.typeSystem)(ts)

      env2 = Env(
        effectInfo = env.effectInfo,
        descriptor = env.descriptor,
        fileSpec = env.fileSpec,
        currentModule = env.currentModule,
        referencedModules = env.referencedModules,
        scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
        allowAbstractConstructor = env.allowAbstractConstructor,
      )

      tcExpr = converter.resolveUnitType(env2)(location)

      filled <- fillHoles(context)(ts)(converter)(tcExpr)
    } yield filled
  }

  private def fillHoles
  (context2: Context)
  (ts: HoleTypeSystem { val baseTypeSystem: context2.typeSystem.type })
  (converter: ExpressionConverter[context2.type] { val typeSystem: ts.type })
  (expr: Comp[ts.WrapExpr])
  : Comp[context2.typeSystem.SimpleExpr] = {
    final class FillConverter extends TypeSystemConverter {

      override val context: context2.type = context2
      override type FromWrap[+A] = HoleType[A]
      override type ToWrap[+A] = A

      override protected val fromWrapInstances: WrapperInstance[HoleType] = implicitly
      override protected val toWrapInstances: WrapperInstance[Id] = implicitly


      override protected def convertType[A](fromExpr: ArExpr[context.type, HoleType] => Comp[A])(t: HoleType[A]): Comp[A] =
        t match {
          case HoleTypeType(t) => IO.succeed(t)

          case HoleTypeHole(id) =>
            converter.resolveType(HoleTypeHole(id)).flatMap {
              case HoleTypeType(t) =>
                fromExpr(t)

              case HoleTypeHole(_) => ???
            }
        }

    }

    for {
      e <- expr
      ids <- converter.getAllHoles
      _ <- ZIO.foreach_(ids) { id =>
        converter.resolveType(HoleTypeHole(id))
      }
      convE <- (new FillConverter).convertTypeSystem(e)
    } yield convE
  }


  sealed trait HoleConstraint[TType]
  private final case class HoleResolved[TType](t: TType) extends HoleConstraint[TType]
  private final case class HoleBounds[TType](superTypeBounds: Set[TType], subTypeBounds: Set[TType]) extends HoleConstraint[TType]

  final case class TypeCheckState[TType]
  (
    constraints: Map[HoleId, HoleConstraint[TType]],
  )

  object TypeCheckState {
    def default[TType]: TypeCheckState[TType] = TypeCheckState(Map.empty)
  }


}
