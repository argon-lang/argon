package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import dev.argon.parser
import dev.argon.util.{FileSpec, NamespacePath, SourceLocation, WithSource}
import dev.argon.util.AnyExtensions._

import scala.collection.immutable.Set
import cats._
import cats.data._
import cats.implicits._
import cats.mtl._
import PayloadSpecifiers._
import cats.evidence.{===, Is}
import dev.argon.compiler.types.TypeSystem.PrimitiveOperation
import dev.argon.parser.{BindingPattern, DeconstructPattern, DiscardPattern, TuplePattern, TypeTestPattern}
import shapeless.{:: => _, Id => _, _}
import shapeless.ops.nat.{LT, Pred}

import Function.const
import scala.collection.immutable

sealed trait ExpressionConverter[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem[context.type]
  val scopeContext: ScopeContext[context.type] { val typeSystem: ExpressionConverter.this.typeSystem.type }
  val signatureContext: SignatureContext {
    val context: ExpressionConverter.this.context.type
    val typeSystem: ExpressionConverter.this.typeSystem.type
  }

  import ExpressionConverter.{ HoleType, TypeCheck => TypeCheckA }
  import typeSystem.{ context => _, _ }
  import scopeContext.{ context => _, _ }
  import signatureContext.{ Signature, SignatureParameters, SignatureVisitor }

  type TComp[A] = typeSystem.TSComp[A]
  type TypeCheck[F[_]] = TypeCheckA[context.type, TType, F]

  implicit val tcompTypeCheckInstance: TypeCheckA[context.type, TType, TComp]

  type Env = ExpressionConverter.Env[context.type, Scope]

  final case class ArgumentInfo(argFactory: ExprFactory, env: Env, location: SourceLocation, style: ParameterStyle)

  abstract class ExprFactory {
    def forExpectedType(expectedType: TType): TComp[ArExpr]
    def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory =
      compFactory(inferExprType(ExprFactory.this).flatMap { thisExpr =>

        def simplifyInstanceType(t: TType): TComp[Option[ArExpr]] =
          implicitly[TypeCheck[TComp]].resolveType(t)
            .flatMap { t =>
              unwrapType(t).flatTraverse { t =>
                reduceExprToValue(t).map(unwrapType)
              }
            }

        def resolveMethodOverloads(instanceType: TypeWithMethods)(methods: TComp[OverloadResult[MemberValue[context.type]]]) =
          methods
            .flatMap { overloads =>
              overloads
                .toList
                .traverse { _.traverse {
                  case MemberValue.Method(method) =>
                    Compilation[TComp].require(env.effectInfo.canCall(method.value.method.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                      .flatMap { _ =>
                        implicitly[TypeCheck[TComp]].fromContextComp(
                          method.value.method.signature(signatureContext)(instanceType)
                        )
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


        def overloadsOfType(memberName: MemberName)(t: TType): TComp[Vector[List[NonEmptyVector[OverloadExprFactory]]]] =
          simplifyInstanceType(t).flatMap {
            case Some(resolvedTypeWithMethods: TypeWithMethods) =>
              resolveMethodOverloads(resolvedTypeWithMethods)(
                MethodLookup.lookupMethods(context)(typeSystem)(resolvedTypeWithMethods)(env.descriptor, env.fileSpec)(memberName)
              )

            case Some(funcType @ FunctionType(argType, resultType)) if memberName === MemberName.Call =>
              Vector(List(NonEmptyVector.of(new OverloadExprFactory {

                override def overloadDescriptor: CallableDescriptor = FunctionTypeCallDescriptor

                override def usedParamTypes: Vector[TType] = Vector()
                override def remainingParameterTypes: Vector[TType] = Vector(argType)

                override def forExpectedType(expectedType: TType): TComp[ArExpr] =
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

                    override def forExpectedType(expectedType: TType): TComp[ArExpr] =
                      result.forExpectedType(expectedType)

                    override def forArguments(argInfo: ArgumentInfo): OverloadExprFactory =
                      wrapNonOverloadFactory(result.forArguments(argInfo))
                  }
              }))).pure[TComp]

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

                    unwrapType(t).collect[TComp[Vector[List[NonEmptyVector[OverloadExprFactory]]]]] {
                      case t: ClassType =>
                        memberName match {
                          case MemberName.New =>
                            if (!env.allowAbstractConstructor && t.arClass.value.isAbstract)
                              Compilation[TComp].forErrors(CompilationError.AbstractClassConstructorCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                            else
                              implicitly[TypeCheck[TComp]].fromContextComp(t.arClass.value.classConstructors).flatMap { constructors =>
                                constructors
                                  .traverse {
                                    case ClassConstructorBinding(_, _, classCtor) =>
                                      Compilation[TComp].require(env.effectInfo.canCall(classCtor.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                                        .flatMap { _ =>
                                          implicitly[TypeCheck[TComp]].fromContextComp(
                                            classCtor.signature(signatureContext)(t)
                                          )
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
                              implicitly[TypeCheck[TComp]].fromContextComp(t.arClass.value.staticMethods)
                                .flatMap {
                                  _.filter { binding => binding.name === methodName }
                                    .filterA { binding =>
                                      AccessCheck.checkInstance[TComp, context.type, t.arClass.PayloadSpec](env.descriptor, env.fileSpec, binding)
                                    }
                                }
                                .map(methodBindingsToOverloads)
                            )

                        }

                      case t: TraitType =>
                        memberName match {
                          case MemberName.New => Vector.empty[List[NonEmptyVector[OverloadExprFactory]]].pure[TComp]
                          case methodName: MethodName =>
                            resolveMethodOverloads(t)(
                              implicitly[TypeCheck[TComp]].fromContextComp(t.arTrait.value.staticMethods)
                                .flatMap {
                                  _.filter { binding => binding.name === methodName }
                                    .filterA { binding =>
                                      AccessCheck.checkInstance[TComp, context.type, t.arTrait.PayloadSpec](env.descriptor, env.fileSpec, binding)
                                    }
                                }
                                .map(methodBindingsToOverloads)
                            )

                        }
                    }
                      .getOrElse {
                        Vector.empty[List[NonEmptyVector[OverloadExprFactory]]].pure[TComp]
                      }
                  }
                }
              }

            case Some(IntersectionType(a, b)) =>
              for {
                aOverloads <- overloadsOfType(memberName)(a)
                bOverloads <- overloadsOfType(memberName)(b)
              } yield aOverloads ++ bOverloads


            case _ => Vector.empty[List[NonEmptyVector[OverloadExprFactory]]].pure[TComp]
          }

        def mergeOverloadLists(a: List[NonEmptyVector[OverloadExprFactory]], b: List[NonEmptyVector[OverloadExprFactory]]): List[NonEmptyVector[OverloadExprFactory]] =
          (a, b) match {
            case (aHead :: aTail, bHead :: bTail) => (aHead ++: bHead) :: mergeOverloadLists(aTail, bTail)
            case (_, Nil) => a
            case (_, _) => b
          }

        def overloadsForName(memberName: MemberName): TComp[ExprFactory] =
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
                case Some(mergedOverloads) => overloadSelectionFactory(env)(location)(mergedOverloads).pure[TComp]
                case None =>
                  Compilation[TComp].forErrors(CompilationError.LookupFailedError(LookupDescription.Member(LookupDescription.Other, memberName), CompilationMessageSource.SourceFile(env.fileSpec, location)))
              }
            }

        memberName match {
          case MemberName.Normal(name) =>
            new ExprFactory {
              override def forExpectedType(expectedType: TType): TComp[typeSystem.ArExpr] =
                overloadsForName(memberName).flatMap { _.forExpectedType(expectedType) }

              override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory =
                compFactory(overloadsForName(memberName).map { _.memberAccessExpr(memberName, env, location) })

              override def forArguments(argInfo: ArgumentInfo): ExprFactory =
                compFactory(overloadsForName(memberName).map { _.forArguments(argInfo) })

              override def mutateValue(env: Env, location: SourceLocation, newValue: ExprFactory): ExprFactory =
                compFactory(overloadsForName(MemberName.Mutator(name))).forArguments(ArgumentInfo(newValue, env, location, ParameterStyle.Normal))
            }.pure[TComp]

          case _ =>
            overloadsForName(memberName)
        }
      })



    def forArguments(argInfo: ArgumentInfo): ExprFactory =
      memberAccessExpr(MemberName.Call, argInfo.env, argInfo.location).forArguments(argInfo)

    def mutateValue(env: Env, location: SourceLocation, newValue: ExprFactory): ExprFactory =
      compFactory(
        Compilation[TComp].forErrors(CompilationError.InvalidLValue(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      )

  }

  abstract class OverloadExprFactory {

    def overloadDescriptor: CallableDescriptor
    def usedParamTypes: Vector[TType]
    def remainingParameterTypes: Vector[TType]

    def forExpectedType(expectedType: TType): TComp[ArExpr]
    def forArguments(argInfo: ArgumentInfo): OverloadExprFactory

    def toExprFactory: ExprFactory = new ExprFactory {
      override def forExpectedType(expectedType: TType): TComp[ArExpr] =
        OverloadExprFactory.this.forExpectedType(expectedType)

      override def forArguments(argInfo: ArgumentInfo): ExprFactory =
        OverloadExprFactory.this.forArguments(argInfo).toExprFactory
    }

    protected final def wrapNonOverloadFactory(factory: ExprFactory): OverloadExprFactory =
      new OverloadExprFactory {

        override def overloadDescriptor: CallableDescriptor = OverloadExprFactory.this.overloadDescriptor

        override def usedParamTypes: Vector[TType] = OverloadExprFactory.this.usedParamTypes
        override def remainingParameterTypes: Vector[typeSystem.TType] = OverloadExprFactory.this.remainingParameterTypes

        override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
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
          Compilation[TComp].forErrors(
            CompilationError.ElseClauseWithoutRescue(CompilationMessageSource.SourceFile(env.fileSpec, expr.location))
          )
        )

      case parser.BlockExpr(body, Vector(), None, Some(ensureBody)) =>
        new ExprFactory {
          override def forExpectedType(expectedType: TType): TComp[ArExpr] =
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
          implicitly[TypeCheck[TComp]].fromContextComp(env.scope.findIdentifier(name, env.fileSpec, expr.location))
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
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
            for {
              argHole <- implicitly[TypeCheck[TComp]].createHole
              resultHole <- implicitly[TypeCheck[TComp]].createHole

              exprConverter <- convertExprTypeDelay(env)(expr.location)(fromSimpleType(FunctionType(argHole, resultHole)))(expectedType)

              argVar = LocalVariable(
                VariableDescriptor(env.descriptor, env.scope.nextVariable),
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
          override def forExpectedType(expectedType: TType): TComp[ArExpr] =
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

          private def convertPattern(env: Env)(t: TType)(pattern: WithSource[parser.Pattern]): TComp[(PatternExpr, Env)] =
            pattern.value match {
              case DeconstructPattern(constructor, args) => ???
              case TuplePattern(values) => ???
              case DiscardPattern =>
                val variable = LocalVariable(
                  VariableDescriptor(env.descriptor, env.scope.nextVariable),
                  VariableName.Unnamed,
                  Mutability.NonMutable,
                  t
                )
                val env2 = env.copy(scope = env.scope.addVariable(variable))

                (PatternExpr.Binding(variable), env2).upcast[(PatternExpr, Env)].pure[TComp]

              case BindingPattern(name) =>
                val variable = LocalVariable(
                  VariableDescriptor(env.descriptor, env.scope.nextVariable),
                  VariableName.Normal(name),
                  Mutability.NonMutable,
                  t
                )
                val env2 = env.copy(scope = env.scope.addVariable(variable))

                (PatternExpr.Binding(variable), env2).upcast[(PatternExpr, Env)].pure[TComp]

              case TypeTestPattern(name, patternType) =>
                for {
                  patT <- evaluateTypeExprAST(env)(patternType)
                  variable = LocalVariable(
                    VariableDescriptor(env.descriptor, env.scope.nextVariable),
                    name.map(VariableName.Normal).getOrElse(VariableName.Unnamed),
                    Mutability.NonMutable,
                    patT
                  )
                  env2 = env.copy(scope = env.scope.addVariable(variable))
                } yield (PatternExpr.CastBinding(variable), env2).upcast[(PatternExpr, Env)]

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
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
            values
              .traverse { elem =>
                implicitly[TypeCheck[TComp]].createHole.map { elemHole =>
                  (elem, elemHole)
                }
              }
              .flatMap { elemPairs =>
                val tupleType = fromSimpleType(LoadTuple(elemPairs.map { case (_, elemHole) => TupleElement(elemHole) }))
                convertExprTypeDelay(env)(expr.location)(tupleType)(expectedType).flatMap { exprTypeConv =>
                  elemPairs
                    .traverse { case (elem, elemHole) =>
                        convertExpr(env)(elem).forExpectedType(elemHole).map { elemExpr =>
                          TupleElement(wrapType(exprTypeConv(elemExpr)))
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
                  case true => declaredUniverse.upcast[UniverseExpr].pure[TComp]
                  case false => ???
                }

              case Some(_) => ???
              case None => inferredUniverse.pure[TComp]
            }

            typeN = TypeN(NextLargestUniverse(universe), sub, sup)
          } yield factoryForExpr(env)(expr.location)(
            inst.foldLeft[ArExpr](typeN) { (a, b) => IntersectionType(fromSimpleType(a), b)}
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
      override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
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
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] = {
            val mutability = Mutability.fromIsMutable(stmt.isMutable)
            val varName = stmt.name match {
              case Some(name) => VariableName.Normal(name)
              case None => VariableName.Unnamed
            }

            for {
              _ <- Compilation[TComp].require(env.effectInfo.canDeclareVariable(mutability))(CompilationError.MutableVariableNotPureError(varName, CompilationMessageSource.SourceFile(env.fileSpec, location)))

              exprType <- stmt.varType match {
                case Some(varTypeExpr) => evaluateTypeExprAST(env)(varTypeExpr)
                case None => implicitly[TypeCheck[TComp]].createHole
              }
              valueExpr <- convertExpr(env)(stmt.value).forExpectedType(exprType)
              varType <- implicitly[TypeCheck[TComp]].resolveType(exprType)

              variable = LocalVariable(
                VariableDescriptor(env.descriptor, env.scope.nextVariable),
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
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
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

    def resolveClass[ClassPS[_, _]](arClassOptComp: context.Comp[Vector[ArClass[context.type, ClassPS]]]): ExprFactory =
      compFactory(
        for {
          classesVec <- implicitly[TypeCheck[TComp]].fromContextComp(arClassOptComp)
          classes <- Compilation[TComp].requireSome(
            NonEmptyVector.fromVector(classesVec)
          )(CompilationError.NamespaceElementNotFound(moduleDesc, namespacePath, name, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          classFactories <- classes.traverse { arClass =>
            implicitly[TypeCheck[TComp]].fromContextComp(arClass.signature)
              .flatMap {
                case classSig: context.signatureContext.Signature[ArClass.ResultInfo, len] =>
                  convertSignature[ArClass.ResultInfo, len](classSig).map { convSig =>
                    val classFactory =
                      signatureFactory[ArClass.ResultInfo, len](env)(location)(arClass.descriptor)(convSig) { (args, classResult) =>
                        ClassType(AbsRef[context.type, ClassPS, ArClass](arClass), args.map(TypeArgument.Expr))
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
  : TComp[TType] =
    evaluateTypeExprFactory(env)(location)(resolveModuleClassFactory(env)(location)(moduleDesc)(namespacePath, name)(Vector.empty))

  def resolveBoolClass(env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Bool"))

  def resolveUnitType(env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))

  def resolveIntType(env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Int"))

  def loadUnitLiteral(env: Env)(location: SourceLocation): ExprFactory =
    compFactory(
      for {
        unitType <- resolveUnitType(env)(location)
      } yield factoryForExpr(env)(location)(LoadUnit(unitType))
    )

  def factoryForExpr(env: Env)(location: SourceLocation)(expr: ArExpr): ExprFactory =
    new ExprFactory {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
        convertExprType(env)(location)(expr)(expectedType)
    }

  def createLookupFactory(env: Env)(description: LookupDescription)(location: SourceLocation)(lookupResult: LookupResult): ExprFactory =
    lookupResult match {
      case LookupResult.ScopeResult(scope) =>
        new ExprFactory {
          private def error[A]: TComp[A] = Compilation[TComp].forErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
            error

          override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory =
            compFactory(
              (memberName match {
                case MemberName.Normal(name) => implicitly[TypeCheck[TComp]].fromContextComp(scope.findIdentifier(name, env.fileSpec, location))
                case _ => LookupResult.Failed.upcast[LookupResult].pure[TComp]
              }).map(createLookupFactory(env)(LookupDescription.Member(description, memberName))(location)(_))
            )

          override def forArguments(argInfo: ArgumentInfo): ExprFactory =
            compFactory(error)
        }

      case LookupResult.SingleValueResult(VariableScopeValue(variable)) =>
        new ExprFactory {
          override def forExpectedType(expectedType: TType): TComp[typeSystem.ArExpr] =
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
            .traverse { _.traverse[TComp, OverloadExprFactory] {
              case FunctionScopeValue(func) =>
                Compilation[TComp].require(env.effectInfo.canCall(func.value.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                  .flatMap { _ =>
                    implicitly[TypeCheck[TComp]].fromContextComp(func.value.signature)
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
                implicitly[TypeCheck[TComp]].fromContextComp(arTrait.value.signature)
                  .flatMap {
                    case sig: context.signatureContext.Signature[ArTrait.ResultInfo, len] =>
                      convertSignature[ArTrait.ResultInfo, len](sig).map { convSig =>
                        signatureFactory(env)(location)(arTrait.value.descriptor)(convSig) { (args, result) =>
                          TraitType(arTrait, args.map(TypeArgument.Expr))
                        }
                      }
                  }

              case ClassScopeValue(arClass) =>
                implicitly[TypeCheck[TComp]].fromContextComp(arClass.value.signature)
                  .flatMap {
                    case sig: context.signatureContext.Signature[ArClass.ResultInfo, len] =>
                      convertSignature[ArClass.ResultInfo, len](sig).map { convSig =>
                        signatureFactory(env)(location)(arClass.value.descriptor)(convSig) { (args, result) =>
                          ClassType(arClass, args.map(TypeArgument.Expr))
                        }
                      }
                  }

              case DataConstructorScopeValue(ctor) =>
                implicitly[TypeCheck[TComp]].fromContextComp(ctor.value.signature)
                  .flatMap {
                    case sig: context.signatureContext.Signature[DataConstructor.ResultInfo, len] =>
                      convertSignature[DataConstructor.ResultInfo, len](sig).map { convSig =>
                        signatureFactory(env)(location)(ctor.value.descriptor)(convSig) { (args, result) =>
                          DataConstructorCall(
                            DataConstructorType(
                              ctor,
                              args.map(TypeArgument.Expr),
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
          private def error[A]: TComp[A] =
            Compilation[TComp].forErrors(CompilationError.LookupFailedError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] = error

          override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory = this
          override def forArguments(argInfo: ArgumentInfo): ExprFactory =
            this

        }
    }

  def overloadSelectionFactory(env: Env)(location: SourceLocation)(overloads: NonEmptyList[NonEmptyVector[OverloadExprFactory]]): ExprFactory = {

    final class OverloadSelectionFactory(argCount: Int)(overloads: NonEmptyList[NonEmptyVector[OverloadExprFactory]]) extends ExprFactory {

      private def prioritizedOverloads: TComp[NonEmptyList[NonEmptyVector[OverloadExprFactory]]] = {

        def insert(acc: NonEmptyList[NonEmptyVector[OverloadExprFactory]], item: OverloadExprFactory): TComp[NonEmptyList[NonEmptyVector[OverloadExprFactory]]] =
          acc.head.existsM(isBetterThan(_, item)).flatMap {
            case false => NonEmptyList(acc.head :+ item, acc.tail).pure[TComp]
            case true =>
              acc.tail match {
                case Nil => NonEmptyList(acc.head, NonEmptyVector.of(item) :: Nil).pure[TComp]
                case head :: tail =>
                  insert(NonEmptyList(head, tail), item).map { _.prepend(acc.head) }
              }
          }

        overloads.flatTraverse { overloadList =>
          overloadList.reduceLeftM(overload => NonEmptyList.of(NonEmptyVector.of(overload)).pure[TComp])(insert)
        }
      }


      // a is better than b
      private def isBetterThan(a: OverloadExprFactory, b: OverloadExprFactory): TComp[Boolean] =
        if(a.usedParamTypes.size === argCount && (b.usedParamTypes.size =!= argCount || b.remainingParameterTypes.nonEmpty))
          true.pure[TComp]
        else if(a.usedParamTypes.size > b.usedParamTypes.size)
          true.pure[TComp]
        else if(a.remainingParameterTypes.size < b.remainingParameterTypes.size)
          true.pure[TComp]
        else {
          val typePairs = (a.usedParamTypes ++ a.remainingParameterTypes)
            .zip(b.usedParamTypes ++ b.remainingParameterTypes)

          typePairs.existsM {
            case (aType, bType) => isSubType(aType, bType).map { _.isDefined }
          }.flatMap {
            case true => false.pure[TComp]
            case false =>
              typePairs.existsM {
                case (aType, bType) => isSubType(bType, aType).map { _.isDefined }
              }
          }
        }

      private def runSameLevelOverloads
      (overloads: NonEmptyVector[OverloadExprFactory])
      (expectedType: TType)
      : TComp[NonEmptyVector[(CallableDescriptor, Either[NonEmptyList[CompilationError], TComp[ArExpr]])]] =
        overloads.traverse { overload =>
          Compilation[TComp].attempt(overload.forExpectedType(expectedType))
            .map { (overload.overloadDescriptor, _) }
        }

      type FailedOverload = (CallableDescriptor, NonEmptyList[CompilationError])
      type GoodOverload = (CallableDescriptor, TComp[ArExpr])

      private def splitCallsAndErrors
      (results: NonEmptyVector[(CallableDescriptor, Either[NonEmptyList[CompilationError], TComp[ArExpr]])])
      : Either[NonEmptyVector[FailedOverload], NonEmptyVector[GoodOverload]] = {

        def impl
        (results: Vector[(CallableDescriptor, Either[NonEmptyList[CompilationError], TComp[ArExpr]])])
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

      private def attemptOverloads(remaining: Ior[NonEmptyVector[FailedOverload], NonEmptyList[NonEmptyVector[OverloadExprFactory]]])(expectedType: TType): TComp[ArExpr] = {

        def attemptHead(head: NonEmptyVector[OverloadExprFactory])(fallback: NonEmptyVector[FailedOverload] => TComp[ArExpr]): TComp[ArExpr] =
          runSameLevelOverloads(head)(expectedType)
            .map(splitCallsAndErrors)
            .flatMap {
              case Right(NonEmptyVector((_, expr), Vector())) => expr

              case Right(exprs) =>
                Compilation[TComp].forErrors(CompilationError.AmbiguousLookupError(
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
              Compilation[TComp].forErrors(errors)

          case Ior.Left(failed) =>
            Compilation[TComp].forErrors(CompilationError.OverloadedLookupFailed(
              failed,
              CompilationMessageSource.SourceFile(env.fileSpec, location)
            ))
        }
      }

      override def forExpectedType(expectedType: TType): TComp[ArExpr] =
        prioritizedOverloads.flatMap { overloads =>
          attemptOverloads(Ior.Right(overloads))(expectedType)
        }

      override def forArguments(argInfo: ArgumentInfo): ExprFactory =
        new OverloadSelectionFactory(argCount + 1)(overloads.map { _.map { _.forArguments(argInfo) } })

    }

    new OverloadSelectionFactory(argCount = 0)(overloads)
  }

  def compFactory(compFac: TComp[ExprFactory]): ExprFactory =
    new ExprFactory {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
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

  def signatureFactory[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], FullLen <: Nat]
  (env: Env)
  (location: SourceLocation)
  (descriptor: ParameterOwnerDescriptor)
  (fullSignature: Signature[TResult, FullLen])
  (f: (Vector[WrapExpr], TResult[context.type, typeSystem.type]) => ArExpr)
  : OverloadExprFactory = {

    def signatureNextPart[RestLen <: Nat](sig: SignatureParameters[TResult, RestLen])(arg: WrapExpr): TComp[Signature[TResult, RestLen]] =
      if(isWrapExprPure(arg))
        sig.next(arg).pure[TComp]
      else if(!sig.nextUnsubstituted.referencesParameter(sig.parameter))
        sig.nextUnsubstituted.pure[TComp]
      else
        Compilation[TComp].forErrors(CompilationError.ArgumentToSignatureDependencyNotPureError(CompilationMessageSource.SourceFile(env.fileSpec, location)))

    final class SigFactory[Len <: Nat](env: Env)(unsubSig: Signature[TResult, Len])(prevParamTypes: Vector[TType])(acc: TComp[(Signature[TResult, Len], Vector[WrapExpr])]) extends OverloadExprFactory {


      override def overloadDescriptor: ParameterOwnerDescriptor = descriptor


      override def usedParamTypes: Vector[TType] = prevParamTypes
      override lazy val remainingParameterTypes: Vector[TType] = unsubSig.unsubstitutedParameters.unsized.map { _.paramType }

      override def forExpectedType(expectedType: TType): TComp[ArExpr] =
        acc.flatMap {
          case (sig, args) =>
            sig.visit(new SignatureVisitor[TResult, Len, TComp[ArExpr]] {
              override def visitParameters[RestLen <: Nat](sigParams: signatureContext.SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): TComp[ArExpr] =
                partiallyApply(env, args, Vector(), fullSignature, identity).flatMap { expr =>
                  convertExprType(env)(location)(expr)(expectedType)
                }

              override def visitResult(sigResult: signatureContext.SignatureResult[TResult])(implicit lenEq: Len === _0): TComp[ArExpr] =
                convertExprType(env)(location)(f(args, sigResult.result))(expectedType)
            })
        }

      override def forArguments(argInfo: ArgumentInfo): OverloadExprFactory =
        unsubSig.visit(new SignatureVisitor[TResult, Len, OverloadExprFactory] {
          override def visitParameters[RestLen <: Nat](unsubSigParams: signatureContext.SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): OverloadExprFactory = {
            def createFactory(createExpr: TType => TComp[WrapExpr]) =
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
                  implicitly[TypeCheck[TComp]].createHole
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

    def partiallyApply[Len <: Nat](env: Env, prevArgs: Vector[WrapExpr], argVariables: Vector[WrapExpr], signature: Signature[TResult, Len], wrapInLambda: ArExpr => ArExpr): TComp[ArExpr] =
      signature.visit(new SignatureVisitor[TResult, Len, TComp[ArExpr]] {
        override def visitParameters[RestLen <: Nat](sigParams: signatureContext.SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): TComp[typeSystem.ArExpr] =
          prevArgs match {
            case Vector() =>
              val newVar = LocalVariable(VariableDescriptor(env.descriptor, env.scope.nextVariable), VariableName.Unnamed, Mutability.NonMutable, sigParams.parameter.paramType)
              val env2 = env.copy(scope = env.scope.addVariable(newVar))
              val newVarExpr = LoadVariable(newVar)
              signatureNextPart(sigParams)(fromSimpleType(newVarExpr)).flatMap { nextSig =>
                partiallyApply(env2, prevArgs, argVariables :+ fromSimpleType(newVarExpr), nextSig, inner => wrapInLambda(LoadLambda(newVar, fromSimpleType(inner))))
              }

            case head +: tail =>
              val newVar = LocalVariable(VariableDescriptor(env.descriptor, env.scope.nextVariable), VariableName.Unnamed, Mutability.NonMutable, sigParams.parameter.paramType)
              val env2 = env.copy(scope = env.scope.addVariable(newVar))
              val newVarExpr = LoadVariable(newVar)
              signatureNextPart(sigParams)(fromSimpleType(newVarExpr)).flatMap { nextSig =>
                partiallyApply(env2, tail, argVariables :+ fromSimpleType(newVarExpr), nextSig, inner => wrapInLambda(LetBinding(newVar, head, fromSimpleType(inner))))
              }
          }

        override def visitResult(sigResult: signatureContext.SignatureResult[TResult])(implicit lenEq: Len === _0): TComp[typeSystem.ArExpr] =
          wrapInLambda(f(argVariables, sigResult.result)).pure[TComp]
      })

    new SigFactory(env)(fullSignature)(Vector.empty)((fullSignature, Vector.empty[WrapExpr]).pure[TComp])
  }

  def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], Len <: Nat]
  (sig: context.signatureContext.Signature[TResult, Len])
  : TComp[Signature[TResult, Len]] = {
    import context.compCompilationInstance
    liftComp(sig.convertTypeSystem(signatureContext)(ArTypeSystemConverter(context)(typeSystem)))
  }

  def convertExprType(env: Env)(location: SourceLocation)(expr: ArExpr)(t: typeSystem.TType): TComp[ArExpr] =
    getExprType(expr).flatMap { exprType =>
      convertExprTypeDelay(env)(location)(exprType)(t)
        .map { f => f(expr) }
    }

  def convertExprTypeDelay(env: Env)(location: SourceLocation)(exprType: typeSystem.TType)(t: typeSystem.TType): TComp[ArExpr => ArExpr] =
    typeSystem.isSubType(t, exprType).flatMap {
      case Some(info) => implicitly[TypeCheck[TComp]].recordConstraint(info).map(const(identity))
      case None => Compilation[TComp].forErrors(CompilationError.CouldNotConvertType(context)(typeSystem)(exprType, t)(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }

  def evaluateTypeExprFactory(env: Env)(location: SourceLocation)(factory: ExprFactory): TComp[TType] =
    inferExprType(factory).flatMap { expr =>
      validateTypeExpr(env)(location)(expr).map { _ => fromSimpleType(expr) }
    }

  def validateTypeExpr(env: Env)(location: SourceLocation)(expr: ArExpr): TComp[Unit] = {
    def invalidType[A]: TComp[A] = Compilation[TComp].forErrors(CompilationError.ExpressionNotTypeError(CompilationMessageSource.SourceFile(env.fileSpec, location)))

    expr match {

      case _: TypeOfType | _: TypeN |
           _: TraitType | _: ClassType |
           _: DataConstructorType | _: FunctionType |
           _: UnionType | _: IntersectionType => ().pure[TComp]

      case LoadTuple(values) =>
        values
          .traverse_ {
            case TupleElement(value) =>
              traverseTypeWrapper(value)(validateTypeExpr(env)(location)(_))
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
            ().pure[TComp]
          else
            invalidType
        }

      case _ => invalidType
    }
  }

  def evaluateTypeExprAST(env: Env)(expr: WithSource[parser.Expr]): TComp[TType] =
    evaluateTypeExprFactory(env)(expr.location)(convertExpr(env)(expr))


  private def inferExprType(factory: ExprFactory): TComp[ArExpr] =
    for {
      hole <- implicitly[TypeCheck[TComp]].createHole
      expr <- factory.forExpectedType(hole)
      _ <- implicitly[TypeCheck[TComp]].resolveType(hole)
    } yield expr



  def isExprPure(expr: ArExpr): Boolean =
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
    traverseTypeWrapper(expr) { t =>
      if(isExprPure(t)) Some(()) else None
    }.isDefined

  def isTypeArgPure(arg: TypeArgument): Boolean =
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

    def addVariables(context: TContext)(variables: Vector[context.typeSystem.Variable]): EnvCreator[TContext]
    def addVariable(context: TContext)(variable: context.typeSystem.Variable): EnvCreator[TContext] =
      addVariables(context)(Vector(variable))

    def addParameters(context: TContext)(params: Vector[context.typeSystem.Parameter]): EnvCreator[TContext]

    val fileSpec: FileSpec
    val currentModule: ArModule[TContext, DeclarationPayloadSpecifier]
    val referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]]
  }

  private def createConverter(context: Context)(ts: TypeSystem[context.type])(implicit tcInstance: TypeCheck[context.type, ts.TType, ts.TSComp]): ExpressionConverter[context.type] {
    val typeSystem: ts.type
  } = {
    val ctx: context.type = context

    new ExpressionConverter[ctx.type] {
      override val context: ctx.type = ctx
      override val typeSystem: ts.type = ts

      override implicit val tcompTypeCheckInstance: ExpressionConverter.TypeCheck[context.type, typeSystem.TType, TComp] = tcInstance

      override val scopeContext: ScopeContext[context.type] { val typeSystem: ts.type } =
        new ScopeContext[context.type] {
          override val context: ctx.type = ctx
          override val typeSystem: ts.type = ts
        }

      override val signatureContext: SignatureContext { val context: ctx.type; val typeSystem: ts.type } =
        new SignatureContext {
          override val context: ctx.type = ctx
          override val typeSystem: ts.type = ts
        }
    }
  }

  def convertStatementList
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (stmts: WithSource[Vector[WithSource[parser.Stmt]]])
  : context.Comp[context.typeSystem.ArExpr] = {
    import context._

    val ts = HoleTypeSystem(context)
    implicit val tcInstance = typeCheckHoleTypeInstance(context)(ts)
    val converter = createConverter(context)(ts)


    val tsConverter = holeTypeConverter(context)(context.typeSystem)(ts)

    val env2 = Env(
      effectInfo = env.effectInfo,
      descriptor = env.descriptor,
      fileSpec = env.fileSpec,
      currentModule = env.currentModule,
      referencedModules = env.referencedModules,
      scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
      allowAbstractConstructor = env.allowAbstractConstructor,
    )

    tsConverter.convertTypeSystem(expectedType).flatMap { convExpectedType =>
      fillHoles(context)(ts)(
        converter
          .convertStmts(env2)(stmts)
          .forExpectedType(convExpectedType)
          .map(ts.wrapType)
      )
    }
  }

  def convertExpression
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : context.Comp[context.typeSystem.ArExpr] =
    convertStatementList(context)(env)(expectedType)(WithSource(Vector(expr), expr.location))




  def convertTypeExpression
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (expr: WithSource[parser.Expr])
  : context.Comp[context.typeSystem.TType] = {
    import context._

    val ts = HoleTypeSystem(context)
    implicit val tcInstance = typeCheckHoleTypeInstance(context)(ts)
    val converter = createConverter(context)(ts)

    val tsConverter = holeTypeConverter(context)(context.typeSystem)(ts)

    val env2 = Env(
      effectInfo = env.effectInfo,
      descriptor = env.descriptor,
      fileSpec = env.fileSpec,
      currentModule = env.currentModule,
      referencedModules = env.referencedModules,
      scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
      allowAbstractConstructor = env.allowAbstractConstructor,
    )

    val tcExpr = converter.evaluateTypeExprAST(env2)(expr)

    fillHoles(context)(ts)(tcExpr)
  }


  def resolveUnitType
  (context: Context)
  (env: Env[context.type, context.scopeContext.Scope])
  (location: SourceLocation): context.Comp[context.typeSystem.TType] = {
    import context._
    val ts = HoleTypeSystem(context)
    implicit val tcInstance = typeCheckHoleTypeInstance(context)(ts)
    val converter = createConverter(context)(ts)

    val tsConverter = holeTypeConverter(context)(context.typeSystem)(ts)

    val env2 = Env(
      effectInfo = env.effectInfo,
      descriptor = env.descriptor,
      fileSpec = env.fileSpec,
      currentModule = env.currentModule,
      referencedModules = env.referencedModules,
      scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
      allowAbstractConstructor = env.allowAbstractConstructor,
    )

    val tcExpr = converter.resolveUnitType(env2)(location)

    fillHoles(context)(ts)(tcExpr)
  }


  private def resolveHoles[TComp[_]]
  (context: Context)
  (ts: TypeSystem[context.type] { type TTypeWrapper[+A] = HoleType[A] })
  (remainingHoles: Int)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[Unit] =
    if(remainingHoles > 0)
      tcInstance.resolveType(HoleTypeHole(remainingHoles - 1))
        .flatMap { _ => resolveHoles(context)(ts)(remainingHoles - 1) }
    else
      ().pure[TComp]

  private def fillHoles
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (expr: HoleTypeCheckComp[context.Comp, ts.TType, ts.WrapExpr])
  (implicit tcInstance: TypeCheck[context.type, ts.TType, HoleTypeCheckComp[context.Comp, ts.TType, ?]])
  : context.Comp[context.typeSystem.ArExpr] = {
    import context._
    val context2: context.type = context
    val ts1: ts.type = ts

    type TCComp[A] = HoleTypeCheckComp[Comp, ts.TType, A]

    final class FillConverter extends TypeSystemConverter[TCComp] {

      override val context: context2.type = context2
      override val ts: ts1.type = ts1
      override val otherTS: context.typeSystem.type = context.typeSystem


      override protected def liftFromComp[A](compA: Comp[A]): TCComp[A] =
        StateT.liftF(compA)

      override protected def convertType[A](fromExpr: otherTS.ArExpr => A)(t: ts.TTypeWrapper[A]): TCComp[otherTS.TTypeWrapper[A]] =
        t match {
          case HoleTypeType(t) => t.pure[TCComp]

          case HoleTypeHole(id) =>
            tcInstance.resolveType(HoleTypeHole(id)).flatMap {
              case HoleTypeType(t) =>
                convertExprTypeSystem(t)
                  .map(fromExpr)

              case HoleTypeHole(_) => ???
            }
        }

    }

    expr
      .flatMap { e =>
        StateT.get[Comp, TypeCheckState[ts.TType]]
          .flatMap { state =>
            resolveHoles(context)(ts)(state.nextHoleId)
          }
          .flatMap { _ =>
            (new FillConverter).convertTypeSystem(e)
          }
      }
      .runA(TypeCheckState.default)
  }



  sealed trait HoleType[+T]
  private final case class HoleTypeType[+T](t: T) extends HoleType[T]
  private final case class HoleTypeHole[+T](id: Int) extends HoleType[T]



  trait HoleTypeSystem[TContext <: Context with Singleton] extends TypeSystem[TContext] {

    override type TSComp[A] = HoleTypeCheckComp[context.Comp, TType, A]

    import context.compCompilationInstance

    override def liftComp[A](value: context.Comp[A]): HoleTypeCheckComp[context.Comp, TType, A] =
      StateT.liftF(value)

    override type TTypeWrapper[+A] = HoleType[A]

    override def wrapType[A](a: A): HoleType[A] =
      HoleTypeType(a)

    override def unwrapType[A](t: HoleType[A]): Option[A] =
      t match {
        case HoleTypeType(t) => Some(t)
        case HoleTypeHole(_) => None
      }

    override def mapTypeWrapper[A, B](t: HoleType[A])(f: A => B): HoleType[B] =
      t match {
        case HoleTypeType(a) => HoleTypeType(f(a))
        case HoleTypeHole(id) => HoleTypeHole(id)
      }

    override def flatMapTypeWrapper[A, B](t: HoleType[A])(f: A => HoleType[B]): HoleType[B] =
      t match {
        case HoleTypeType(a) => f(a)
        case HoleTypeHole(id) => HoleTypeHole(id)
      }

    override def traverseTypeWrapper[A, B, F[_] : Applicative](t: HoleType[A])(f: A => F[B]): F[HoleType[B]] =
      t match {
        case HoleTypeType(a) => f(a).map(HoleTypeType.apply)
        case HoleTypeHole(id) => (HoleTypeHole(id) : TTypeWrapper[B]).pure[F]
      }

    override def flatTraverseTypeWrapper[A, B, F[_] : Applicative](t: HoleType[A])(f: A => F[HoleType[B]]): F[HoleType[B]] =
      t match {
        case HoleTypeType(a) => f(a)
        case HoleTypeHole(id) => (HoleTypeHole(id) : TTypeWrapper[B]).pure[F]
      }

    override def wrapExprType(expr: WrapExpr): TSComp[TType] =
      expr match {
        case HoleTypeType(t) => getExprType(t)
        case HoleTypeHole(_) => fromSimpleType(TypeOfType(expr)).pure[TSComp]
      }

    override def isSubTypeWrapper(a: TType, b: TType): TSComp[Option[SubTypeInfo[TType]]] =
      (a, b) match {
        case (HoleTypeType(aInner), HoleTypeType(bInner)) => isSimpleSubType(aInner, bInner)
        case (_, _) => (Some(SubTypeInfo(a, b, Vector.empty)) : Option[SubTypeInfo[TType]]).pure[TSComp]
      }

    override def universeOfWrapExpr(expr: WrapExpr): TSComp[UniverseExpr] =
      expr match {
        case HoleTypeHole(_) => AbstractUniverse().upcast[UniverseExpr].pure[TSComp]
        case HoleTypeType(t) => universeOfExpr(t)
      }

  }

  object HoleTypeSystem {

    def apply(ctx: Context): HoleTypeSystem[ctx.type] = new HoleTypeSystem[ctx.type] {
      override val context: ctx.type = ctx
      override val contextProof: ctx.type === context.type = Is.refl

      override implicit lazy val tscompCompilationInstance: Compilation[TSComp] =
        typeCheckHoleTypeInstance(ctx)(this)

      override def liftSignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](sig: context.signatureContext.Signature[TResult, _ <: Nat], args: Vector[TypeArgument]): TSComp[TResult[ctx.type, this.type]] = {

        val self: this.type = this

        val sigContext: SignatureContext {
          val context: ctx.type
          val typeSystem: self.type
        } =
          new SignatureContext {
            override val context: ctx.type = ctx
            override val typeSystem: self.type = self
          }

        val conv = holeTypeConverter(context)(context.typeSystem)(this)

        import context.compCompilationInstance

        for {
          convSig <- liftComp(sig.convertTypeSystem[context.Comp](sigContext)(conv))
          convParams <- liftComp(sig.unsubstitutedParameters.unsized.traverse(conv.convertParameterTypeSystem))
        } yield convSig.substituteTypeArguments(convParams)(args).unsubstitutedResult
      }
    }

  }

  private def holeTypeConverter
  (context: Context)
  (innerTS: TypeSystem[context.type])
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  })
  : TypeSystemConverter.Aux[context.type, innerTS.type, holeTS.type, context.Comp] = {
    import context.{ Comp, compCompilationInstance }
    val context2: context.type = context

    new TypeSystemConverter[Comp] {

      override val context: context2.type = context2
      override val ts: innerTS.type = innerTS
      override val otherTS: holeTS.type = holeTS

      override protected def liftFromComp[A](compA: Comp[A]): Comp[A] = compA

      override protected def convertType[A](fromExpr: otherTS.ArExpr => A)(t: ts.TTypeWrapper[A]): Comp[otherTS.TTypeWrapper[A]] =
        HoleTypeType(t).pure[Comp]
    }
  }


  sealed trait HoleConstraint[TType]
  private final case class HoleResolved[TType](t: TType) extends HoleConstraint[TType]
  private final case class HoleBounds[TType](superTypeBounds: Set[TType], subTypeBounds: Set[TType]) extends HoleConstraint[TType]

  trait TypeCheck[TContext <: Context with Singleton, TType, TComp[_]] extends Compilation[TComp] {
    def fromContextComp[A](comp: TContext#Comp[A]): TComp[A]
    def createHole: TComp[TType]
    def recordConstraint(info: SubTypeInfo[TType]): TComp[Unit]
    def resolveType(t: TType): TComp[TType]
  }

  final case class TypeCheckState[TType]
  (
    nextHoleId: Int,
    constraints: Map[Int, HoleConstraint[TType]],
  )

  object TypeCheckState {
    def default[TType]: TypeCheckState[TType] = TypeCheckState(0, Map.empty)
  }

  type HoleTypeCheckComp[TComp[_], TType, A] = StateT[TComp, TypeCheckState[TType], A]
  implicit def holeTypeCheckMonad[TComp[_]: Monad, TType]: Monad[HoleTypeCheckComp[TComp, TType, ?]] =
    cats.data.IndexedStateT.catsDataMonadForIndexedStateT[TComp, TypeCheckState[TType]]




  private def typeCheckHoleTypeInstance
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  : TypeCheck[context.type, ts.TType, HoleTypeCheckComp[context.Comp, ts.TType, ?]] =
    new TypeCheck[context.type, ts.TType, HoleTypeCheckComp[context.Comp, ts.TType, ?]] {

      import context._

      implicit val holeTypeCheckMonadTType = holeTypeCheckMonad[Comp, ts.TType]

      override def fromContextComp[A](comp: context.Comp[A]): HoleTypeCheckComp[Comp, ts.TType, A] =
        StateT((s: TypeCheckState[ts.TType]) => comp.map { a => (s, a) })

      override def createHole: HoleTypeCheckComp[Comp, ts.TType, ts.TType] =
        for {
          state <- StateT.get[Comp, TypeCheckState[ts.TType]]
          _ <- StateT.set[Comp, TypeCheckState[ts.TType]](state.copy(nextHoleId = state.nextHoleId + 1))
        } yield HoleTypeHole(state.nextHoleId)

      private def addConstraint(id: Int, prop: Lens[HoleBounds[ts.TType], Set[ts.TType]], constraint: ts.TType): HoleTypeCheckComp[Comp, ts.TType, Unit] =
        StateT.modify[Comp, TypeCheckState[ts.TType]] { state =>
          state.constraints.getOrElse(id, HoleBounds(Set.empty, Set.empty)) match {
            case HoleResolved(_) => state
            case bounds @ HoleBounds(_, _) =>
              state.copy(constraints = state.constraints.updated(id, prop.modify(bounds){ _ + constraint }))
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

      override def recordConstraint(info: SubTypeInfo[ts.TType]): HoleTypeCheckComp[Comp, ts.TType, Unit] =
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
            info.args.traverse_(recordConstraint(_))(this)
        }

      override def resolveType(t: ts.TType): HoleTypeCheckComp[Comp, ts.TType, ts.TType] =
        (new ResolverConverter).convertTypeSystem(t)

      private def getConstraints(id: Int): HoleTypeCheckComp[Comp, ts.TType, HoleConstraint[ts.TType]] =
        StateT.get[Comp, TypeCheckState[ts.TType]].map { state =>
          state.constraints.getOrElse(id, HoleBounds(Set.empty, Set.empty))
        }

      private def updateConstraints(id: Int, constraints: HoleConstraint[ts.TType]): HoleTypeCheckComp[Comp, ts.TType, Unit] =
        StateT.modify[Comp, TypeCheckState[ts.TType]] { state =>
          state.copy(constraints = state.constraints.updated(id, constraints))
        }

      private def resolveOuterHole(id: Int): HoleTypeCheckComp[Comp, ts.TType, ts.TType] =
        getConstraints(id).flatMap  {
          case HoleResolved(hole) => hole.pure[HoleTypeCheckComp[Comp, ts.TType, ?]]
          case bounds @ HoleBounds(_, _) =>

            def resolveConstraints
            (pick: Lens[HoleBounds[ts.TType], Set[ts.TType]], otherSide: Lens[HoleBounds[ts.TType], Set[ts.TType]])
            (combine: (ts.TType, ts.TType) => ts.ArExpr)
            : Option[HoleTypeCheckComp[Comp, ts.TType, ts.TType]] =
              NonEmptyList.fromList(pick.get(bounds).toList).map {
                _.traverse[HoleTypeCheckComp[Comp, ts.TType, ?], ts.TType] {
                  case t @ HoleTypeType(_) => (t : ts.TType).pure[HoleTypeCheckComp[Comp, ts.TType, ?]]
                  case t @ HoleTypeHole(constraintHoleId) =>
                    getConstraints(constraintHoleId)
                      .flatMap {
                        case HoleResolved(_) => ().pure[HoleTypeCheckComp[Comp, ts.TType, ?]]
                        case resBounds @ HoleBounds(_, _) =>
                          val newBounds = otherSide.set(resBounds)(
                            otherSide.get(resBounds).excl(HoleTypeHole(id)) ++ otherSide.get(bounds).excl(t)
                          )
                          updateConstraints(constraintHoleId, newBounds).map { _ =>  }
                      }
                      .map { _ => t }
                }
                  .map { _.reduceLeft { (a, b) => ts.fromSimpleType(combine(a, b)) } }
              }

            for {
              resolvedType <- resolveConstraints(subTypeBoundsLens, superTypeBoundsLens)(ts.IntersectionType)
                .orElse { resolveConstraints(superTypeBoundsLens, subTypeBoundsLens)(ts.UnionType) }
                .getOrElse { ??? }

              _ <- updateConstraints(id, HoleResolved(resolvedType))
            } yield resolvedType
        }

      type ResolverState[A] = HoleTypeCheckComp[Comp, ts.TType, A]

      val context2: context.type = context
      val ts1: ts.type = ts

      private final class ResolverConverter extends TypeSystemConverter[ResolverState] {

        override val context: context2.type = context2
        override val ts: ts1.type = ts1
        override val otherTS: ts1.type = ts1

        override protected def liftFromComp[A](compA: Comp[A]): ResolverState[A] =
          StateT.liftF(compA)

        override protected def convertType[A](fromExpr: otherTS.ArExpr => A)(t: ts.TTypeWrapper[A]): ResolverState[otherTS.TTypeWrapper[A]] =
          t match {
            case HoleTypeType(_) => t.pure[ResolverState]
            case HoleTypeHole(id) =>
              for {
                resolvedOuter <- resolveOuterHole(id)
                resolvedType <- convertTypeSystem(resolvedOuter)
              } yield ts.mapTypeWrapper(resolvedType)(fromExpr)
          }

      }

      override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[Nothing]): HoleTypeCheckComp[Comp, ts.TType, A] =
        StateT((s: TypeCheckState[ts.TType]) => Compilation[Comp].forErrors[A](errors, messages).map { a => (s, a) })

      override def createCache[A]: HoleTypeCheckComp[Comp, ts.TType, HoleTypeCheckComp[Comp, ts.TType, A] => HoleTypeCheckComp[Comp, ts.TType, A]] =
        pure(identity _)

      override def createMemo[A, B]: HoleTypeCheckComp[Comp, ts.TType, (A => HoleTypeCheckComp[Comp, ts.TType, B]) => A => HoleTypeCheckComp[Comp, ts.TType, B]] =
        pure(identity _)

      override def attempt[A]
      (action: HoleTypeCheckComp[context.Comp, ts.TType, A])
      : HoleTypeCheckComp[context.Comp, ts.TType, Either[NonEmptyList[CompilationError], HoleTypeCheckComp[context.Comp, ts.TType, A]]] =
        StateT.get[Comp, TypeCheckState[ts.TType]].flatMap { state =>
          StateT.liftF(Compilation[Comp].attempt(action.run(state)))
            .map { _.map { attemptAction =>
              StateT.liftF(attemptAction).flatMap {
                case (state2, a) =>
                  StateT.set[Comp, TypeCheckState[ts.TType]](state2)
                    .map { _ => a }
              }
            } }
        }

      override def flatMap[A, B](fa: HoleTypeCheckComp[Comp, ts.TType, A])(f: A => HoleTypeCheckComp[Comp, ts.TType, B]): HoleTypeCheckComp[Comp, ts.TType, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => HoleTypeCheckComp[Comp, ts.TType, Either[A, B]]): HoleTypeCheckComp[Comp, ts.TType, B] =
        holeTypeCheckMonadTType.tailRecM(a)(f)

      override def pure[A](x: A): HoleTypeCheckComp[Comp, ts.TType, A] =
        holeTypeCheckMonadTType.pure(x)

    }

}
