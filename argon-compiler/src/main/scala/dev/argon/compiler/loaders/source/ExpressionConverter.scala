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

import Function.const

sealed trait ExpressionConverter[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem[context.type]
  val scopeContext: ScopeContext[context.type] { val typeSystem: ExpressionConverter.this.typeSystem.type }
  val signatureContext: SignatureContext[context.type] { val typeSystem: ExpressionConverter.this.typeSystem.type }

  import ExpressionConverter.{ HoleType, TypeCheck => TypeCheckA, TypeConstraint }
  import typeSystem.{ context => _, _ }
  import scopeContext.{ context => _, _ }
  import signatureContext.Signature



  type Env = ExpressionConverter.Env[context.type, Scope]
  private type TypeCheckT[TCType, TComp[_]] = TypeCheckA[context.type, TCType, TComp]
  private type TypeCheck[TComp[_]] = TypeCheckT[typeSystem.TType, TComp]

  final case class ArgumentInfo[TComp[_]](argFactory: ExprFactory[TComp], env: Env, location: SourceLocation)

  abstract class ExprFactory[TComp[_]: TypeCheck] {
    def forExpectedType(expectedType: TType): TComp[ArExpr]
    def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory[TComp] =
      compFactory(inferExprType(ExprFactory.this).flatMap { thisExpr =>
        implicitly[TypeCheck[TComp]].resolveType(thisExpr.exprType).flatMap { resolvedType =>
          unwrapType(resolvedType) match {
            case Some(resolvedTypeWithMethods: TypeWithMethods) =>
              implicitly[TypeCheck[TComp]].fromContextComp(context)(MethodLookup.lookupMethods(context)(typeSystem)(resolvedTypeWithMethods)(env.descriptor, env.fileSpec)(memberName)).flatMap {
                case OverloadResult.List(Vector(MemberValue.Method(method)), _) =>
                  for {
                    _ <- Compilation[TComp].require(env.effectInfo.canCall(method.value.method.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                    sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(method.value.method.signature)
                    convSig = convertSignature(sig)
                  } yield signatureFactory(env)(location)(convSig) { (args, result) => MethodCall(AbsRef(method.value.method), thisExpr, args, result.returnType).upcast[ArExpr].pure[TComp] }

                case methods => ???
              }

            case Some(funcType @ FunctionType(argType, resultType)) if memberName === MemberName.Call =>
              new ExprFactory[TComp] {
                override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
                  ExprFactory.this.forExpectedType(expectedType)

                override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory[TComp] =
                  ???

                override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
                  compFactory(
                    for {
                      funcExpr <- ExprFactory.this.forExpectedType(fromSimpleType(funcType))
                      argExpr <- argInfo.argFactory.forExpectedType(argType)
                    } yield factoryForExpr(argInfo.env)(argInfo.location)(FunctionObjectCall(funcExpr, argExpr, resultType))
                  )
              }.pure[TComp]

            case Some(TypeOfType(thisType, _)) =>
              unwrapType(thisType) match {
                case Some(t: ClassType) =>
                  memberName match {
                    case MemberName.New =>
                      if(!env.allowAbstractConstructor && t.arClass.value.isAbstract)
                        Compilation[TComp].forErrors(CompilationError.AbstractClassConstructorCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                      else
                        implicitly[TypeCheck[TComp]].fromContextComp(context)(t.arClass.value.classConstructors).flatMap {
                          case Vector(ClassConstructorBinding(_, _, classCtor)) =>
                            for {
                              _ <- Compilation[TComp].require(env.effectInfo.canCall(classCtor.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                              sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(classCtor.signature)
                              convSig = convertSignature(sig)
                            } yield signatureFactory(env)(location)(convSig) { (args, result) => ClassConstructorCall(t, AbsRef(classCtor), args).upcast[ArExpr].pure[TComp] }

                          case _ => ???
                        }

                    case methodName: MethodName =>
                      implicitly[TypeCheck[TComp]].fromContextComp(context)(t.arClass.value.staticMethods)
                        .map { _.filter { binding => binding.name === methodName } }
                        .flatMap {
                          case Vector(MethodBinding(_, _, _, method)) =>
                            for {
                              _ <- Compilation[TComp].require(env.effectInfo.canCall(method.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                              sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(method.signature)
                              convSig = convertSignature(sig)
                            } yield signatureFactory(env)(location)(convSig) { (args, result) => MethodCall(AbsRef(method), thisExpr, args, result.returnType).upcast[ArExpr].pure[TComp] }

                          case _ => ???
                        }

                  }

                case _ => ???
              }

            case _ => ???
          }
        }
      })

    def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
      memberAccessExpr(MemberName.Call, argInfo.env, argInfo.location).forArguments(argInfo)
  }

  def convertExpr[TComp[_] : TypeCheck](env: Env)(expr: WithSource[parser.Expr]): ExprFactory[TComp] =
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
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.AddInt, leftExpr, rightExpr, intType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Sub, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.SubInt, leftExpr, rightExpr, intType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Mul, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.MulInt, leftExpr, rightExpr, intType))
        )

      case parser.BinaryOperatorExpr(parser.BinaryOperator.Equal, left, right) =>
        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
            boolType <- resolveBoolClass(env)(expr.location)
            leftExpr <- convertExpr(env)(left).forExpectedType(intType)
            rightExpr <- convertExpr(env)(right).forExpectedType(intType)
          } yield factoryForExpr(env)(expr.location)(PrimitiveOp(PrimitiveOperation.IntEqual, leftExpr, rightExpr, boolType))
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

      case parser.FunctionCallExpr(func, arg) =>
        convertExpr[TComp](env)(func).forArguments(ArgumentInfo[TComp](convertExpr(env)(arg), env, arg.location))

      case parser.IdentifierExpr(name) =>
        compFactory(
          implicitly[TypeCheck[TComp]].fromContextComp(context)(env.scope.findIdentifier(name, env.fileSpec, expr.location))
            .map(createLookupFactory(env)(LookupDescription.Identifier(name))(expr.location))
        )

      case parser.IfExpr(cond, ifBody) =>
        createIfExpr(env)(expr.location)(cond, ifBody, WithSource(Vector.empty, SourceLocation(expr.location.end, expr.location.end)))

      case parser.IfElseExpr(cond, ifBody, elseBody) =>
        createIfExpr(env)(expr.location)(cond, ifBody, elseBody)

      case parser.IntValueExpr(sign, base, digits) =>
        val value = sign * digits.foldLeft(0 : BigInt) { (acc, digit) => acc * base + digit }

        compFactory(
          for {
            intType <- resolveIntType(env)(expr.location)
          } yield factoryForExpr(env)(expr.location)(LoadConstantInt(value, intType))
        )

      case parser.LambdaExpr(varName, body) =>
        new ExprFactory[TComp] {
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
              env2 = env.copy(scope = env.scope.addVariable(argVar))

              bodyExpr <- convertExpr(env2)(body).forExpectedType(resultHole)

            } yield exprConverter(LoadLambda(argVar, bodyExpr))
        }

      case parser.LambdaTypeExpr(argType, resultType) =>
        compFactory(
          for {
            argTypeValue <- evaluateTypeExprAST(env)(argType)
            resultTypeValue <- evaluateTypeExprAST(env)(resultType)
          } yield factoryForExpr(env)(expr.location)(FunctionType(argTypeValue, resultTypeValue))
        )

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
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
            values
              .traverse { elem =>
                implicitly[TypeCheck[TComp]].createHole.map { elemHole =>
                  (elem, elemHole)
                }
              }
              .flatMap { elemPairs =>
                val tupleType = fromSimpleType(LoadTupleType(elemPairs.map { case (_, elemHole) => TupleElement[SimpleType](elemHole) }))
                convertExprTypeDelay(env)(expr.location)(tupleType)(expectedType).flatMap { exprTypeConv =>
                  elemPairs
                    .traverse { case (elem, elemHole) =>
                        convertExpr(env)(elem).forExpectedType(elemHole).map { elemExpr =>
                          TupleElement[ArExpr](wrapType(exprTypeConv(elemExpr)))
                        }
                    }
                    .map { tupleElements =>
                      LoadTuple(tupleElements)
                    }
                }
              }
        }

      case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented: $e")
    }

  def createIfExpr[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(cond: WithSource[parser.Expr], ifBody: WithSource[Vector[WithSource[parser.Stmt]]], elseBody: WithSource[Vector[WithSource[parser.Stmt]]]) =
    new ExprFactory[TComp] {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
        for {
          boolType <- resolveBoolClass(env)(location)
          condTC <- convertExpr(env)(cond).forExpectedType(boolType)
          ifBodyTC <- convertStmts(env)(ifBody).forExpectedType(expectedType)
          elseBodyTC <- convertStmts(env)(elseBody).forExpectedType(expectedType)
        } yield IfElse(condTC, ifBodyTC, elseBodyTC)
    }

  def convertStmts[TComp[_] : TypeCheck](env: Env)(stmts: WithSource[Vector[WithSource[parser.Stmt]]]): ExprFactory[TComp] =
    stmts.value match {
      case Vector() => loadUnitLiteral(env)(stmts.location)

      case WithSource(stmt: parser.VariableDeclarationStmt, location) +: tail =>
        new ExprFactory[TComp] {
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
            } yield LetBinding(variable, valueExpr, second)

          }
        }

      case Vector(stmt) => convertStmt(env)(stmt)
      case head +: tail =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
            for {
              unitType <- resolveUnitType[TComp](env)(head.location)
              first <- convertStmt(env)(head).forExpectedType(unitType)

              secondStartPos = tail.headOption.map { _.location.start }.getOrElse(stmts.location.end)
              second <- convertStmts(env)(WithSource(tail, SourceLocation(secondStartPos, stmts.location.end))).forExpectedType(expectedType)
            } yield Sequence(first, second)
        }
    }

  def convertStmt[TComp[_] : TypeCheck](env: Env)(stmt: WithSource[parser.Stmt]): ExprFactory[TComp] =
    stmt.value match {
      case expr: parser.Expr =>
        convertExpr(env)(WithSource(expr, stmt.location))

      case _ => ???
    }

  def resolveModuleClassFactory[TComp[_] : TypeCheck]
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  (args: Vector[ArgumentInfo[TComp]])
  : ExprFactory[TComp] = {

    def resolveClass[ClassPS[_, _]](arClassOptComp: context.Comp[Option[ArClass[context.type, ClassPS]]]): ExprFactory[TComp] =
      compFactory(
        for {
          arClassOpt <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClassOptComp)
          arClass <- Compilation[TComp].requireSome(
            arClassOpt
          )(CompilationError.NamespaceElementNotFound(moduleDesc, namespacePath, name, CompilationMessageSource.SourceFile(env.fileSpec, location)))
          classSig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClass.signature)

          classFactory = signatureFactory[TComp, ArClass.ResultInfo](env)(location)(
            convertSignature(classSig)
          ) { (args, classResult) =>
            for {
              argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
            } yield ClassType(AbsRef[context.type, ClassPS, ArClass](arClass), argsAsTypes, classResult.baseTypes)
          }

        } yield args.foldLeft(classFactory) { (factory, arg) => factory.forArguments(arg) }
      )

    if(moduleDesc === env.currentModule.descriptor)
      resolveClass(ModuleLookup.lookupNamespaceValue(context)(env.currentModule)(namespacePath, name)(ModuleLookup.lookupGlobalClass))
    else
      resolveClass(ModuleLookup.lookupValue(context)(env.referencedModules)(moduleDesc)(namespacePath, name)(ModuleLookup.lookupGlobalClass))
  }

  def resolveModuleClass[TComp[_] : TypeCheck]
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  : TComp[TType] =
    evaluateTypeExprFactory(env)(location)(resolveModuleClassFactory(env)(location)(moduleDesc)(namespacePath, name)(Vector.empty))

  def resolveBoolClass[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Bool"))

  def resolveUnitType[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))

  def resolveIntType[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor(LookupNames.argonCoreLib))(NamespacePath(Vector("Ar")), GlobalName.Normal("Int"))

  def loadUnitLiteral[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): ExprFactory[TComp] =
    compFactory(
      for {
        unitType <- resolveUnitType(env)(location)
      } yield factoryForExpr(env)(location)(LoadUnit(unitType))
    )

  def factoryForExpr[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr): ExprFactory[TComp] =
    new ExprFactory[TComp] {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
        convertExprType(env)(location)(expr)(expectedType)
    }

  def createLookupFactory[TComp[_] : TypeCheck](env: Env)(description: LookupDescription)(location: SourceLocation)(lookupResult: LookupResult): ExprFactory[TComp] =
    lookupResult match {
      case LookupResult.ScopeResult(scope) =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
            Compilation[TComp].forErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory[TComp] =
            compFactory(
              (memberName match {
                case MemberName.Normal(name) => implicitly[TypeCheck[TComp]].fromContextComp(context)(scope.findIdentifier(name, env.fileSpec, location))
                case _ => LookupResult.Failed.upcast[LookupResult].pure[TComp]
              }).map(createLookupFactory(env)(LookupDescription.Member(description, memberName))(location)(_)(implicitly[TypeCheck[TComp]]))
            )

          override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
            compFactory(
              Compilation[TComp].forErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))
            )
        }

      case LookupResult.ValuesResult(OverloadResult.List(Vector(result), _)) =>
        result match {
          case VariableScopeValue(variable) =>
            factoryForExpr(env)(location)(LoadVariable(variable))

          case FunctionScopeValue(func) =>
            compFactory(
              for {
                _ <- Compilation[TComp].require(env.effectInfo.canCall(func.value.effectInfo))(CompilationError.ImpureFunctionCalledError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(func.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(location)(convSig) { (args, result) => FunctionCall(func, args, result.returnType).upcast[ArExpr].pure[TComp] }
            )

          case TraitScopeValue(arTrait) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arTrait.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(location)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
                } yield TraitType(arTrait, argsAsTypes, result.baseTypes)
              }
            )

          case ClassScopeValue(arClass) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClass.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(location)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
                } yield ClassType(arClass, argsAsTypes, result.baseTypes)
              }
            )

          case DataConstructorScopeValue(ctor) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(ctor.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(location)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
                } yield DataConstructorCall(
                  DataConstructorType(
                    ctor,
                    argsAsTypes,
                    result.instanceType
                  ),
                  args
                )
              }
            )
        }

      case LookupResult.ValuesResult(_) => ???

      case LookupResult.Failed =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
            Compilation[TComp].forErrors(CompilationError.LookupFailedError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory[TComp] = this
          override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] = this
        }
    }

  def compFactory[TComp[_] : TypeCheck](compFac: TComp[ExprFactory[TComp]]): ExprFactory[TComp] =
    new ExprFactory[TComp] {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
        compFac.flatMap { _.forExpectedType(expectedType) }

      override def memberAccessExpr(memberName: MemberName, env: Env, location: SourceLocation): ExprFactory[TComp] =
        compFactory(
          compFac.map { _.memberAccessExpr(memberName, env, location) }
        )

      override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
        compFactory(
          compFac.map { _.forArguments(argInfo) }
        )
    }

  def signatureFactory[TComp[_] : TypeCheck, TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (env: Env)
  (location: SourceLocation)
  (fullSignature: Signature[TResult])
  (f: (Vector[ArExpr], TResult[context.type, typeSystem.type]) => TComp[ArExpr])
  : ExprFactory[TComp] = {

    final class SigFactory(env: Env)(signature: Signature[TResult])(prevArgs: Vector[ArExpr]) extends ExprFactory[TComp] {
      override def forExpectedType(expectedType: TType): TComp[ArExpr] =
        signature.visit(
          sigParams => partiallyApply(env, prevArgs, Vector(), fullSignature, identity).flatMap { expr =>
            convertExprType(env)(location)(expr)(expectedType)
          },
          sigResult =>
            f(prevArgs, sigResult.result).flatMap { expr =>
              convertExprType(env)(location)(expr)(expectedType)
            }
        )

      override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
        signature.visit(
          sigParams =>
            compFactory(
              for {
                argExpr <- argInfo.argFactory.forExpectedType(sigParams.parameter.paramType)
                next <- sigParams.next(argExpr)
              } yield new SigFactory(env)(next)(prevArgs :+ argExpr)
            ),
          _ => super.forArguments(argInfo)
        )
    }

    def partiallyApply(env: Env, prevArgs: Vector[ArExpr], argVariables: Vector[ArExpr], signature: Signature[TResult], wrapInLambda: ArExpr => ArExpr): TComp[ArExpr] =
      signature.visit(
        sigParams => prevArgs match {
          case Vector() =>
            val newVar = LocalVariable(VariableDescriptor(env.descriptor, env.scope.nextVariable), VariableName.Unnamed, Mutability.NonMutable, sigParams.parameter.paramType)
            val env2 = env.copy(scope = env.scope.addVariable(newVar))
            val newVarExpr = LoadVariable(newVar)
            sigParams.next(newVarExpr).flatMap { nextSig =>
              partiallyApply(env2, prevArgs, argVariables :+ newVarExpr, nextSig, inner => wrapInLambda(LoadLambda(newVar, inner)))
            }

          case head +: tail =>
            val newVar = LocalVariable(VariableDescriptor(env.descriptor, env.scope.nextVariable), VariableName.Unnamed, Mutability.NonMutable, sigParams.parameter.paramType)
            val env2 = env.copy(scope = env.scope.addVariable(newVar))
            val newVarExpr = LoadVariable(newVar)
            sigParams.next(newVarExpr).flatMap { nextSig =>
              partiallyApply(env2, tail, argVariables :+ newVarExpr, nextSig, inner => wrapInLambda(LetBinding(newVar, head, inner)))
            }
        },
        sigResult => f(argVariables, sigResult.result).map(wrapInLambda)
      )

    new SigFactory(env)(fullSignature)(Vector.empty)
  }

  def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (sig: context.signatureContext.Signature[TResult])
  : Signature[TResult] =
    sig.convertTypeSystem(signatureContext)(ArTypeSystemConverter(context)(typeSystem))

  def convertExprType[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr)(t: typeSystem.TType): TComp[ArExpr] =
    convertExprTypeDelay(env)(location)(expr.exprType)(t).map { f => f(expr) }

  def convertExprTypeDelay[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(exprType: typeSystem.TType)(t: typeSystem.TType): TComp[ArExpr => ArExpr] =
    typeSystem.isSubType[TComp](t, exprType).flatMap {
      case Some(info) => implicitly[TypeCheck[TComp]].recordConstraint(info).map(const(identity))
      case None =>
        implicitly[TypeCheck[TComp]].createHole.flatMap { newHole =>
          typeSystem.isSubType(t, fromSimpleType(LoadTupleType(NonEmptyList.of(TupleElement(newHole))))).flatMap {
            case Some(stTupleInfo) =>
              convertExprTypeDelay(env)(location)(exprType)(newHole).map { innerConverter =>
                expr => LoadTuple(NonEmptyList.of(TupleElement(wrapType(innerConverter(expr)))))
              }

            case None =>
              Compilation[TComp].forErrors(CompilationError.CouldNotConvertType(context)(typeSystem)(exprType, t)(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        }
    }

  def evaluateTypeExprFactory[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(factory: ExprFactory[TComp]): TComp[TType] =
    inferExprType(factory).flatMap { expr =>
      evaluateTypeExpr(env)(location)(expr)
    }

  def evaluateTypeExpr[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr): TComp[TType] =
    expr match {
      case t: SimpleType => typeSystem.fromSimpleType(t).pure[TComp]
      case _ => Compilation[TComp].forErrors(CompilationError.ExpressionNotTypeError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }

  def evaluateTypeExprAST[TComp[_] : TypeCheck](env: Env)(expr: WithSource[parser.Expr]): TComp[TType] =
    evaluateTypeExprFactory(env)(expr.location)(convertExpr(env)(expr))


  private def inferExprType[TComp[_] : TypeCheck](factory: ExprFactory[TComp]): TComp[ArExpr] =
    for {
      hole <- implicitly[TypeCheck[TComp]].createHole
      expr <- factory.forExpectedType(hole)
      _ <- implicitly[TypeCheck[TComp]].resolveType(hole)
    } yield expr

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

    val fileSpec: FileSpec
    val currentModule: ArModule[TContext, DeclarationPayloadSpecifier]
    val referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]]
  }

  private def createConverter(context: Context)(ts: TypeSystem[context.type]): ExpressionConverter[context.type] {
    val typeSystem: ts.type
  } = {
    val ctx: context.type = context

    new ExpressionConverter[ctx.type] {
      override val context: ctx.type = ctx
      override val typeSystem: ts.type = ts

      override val scopeContext: ScopeContext[context.type] { val typeSystem: ts.type } =
        new ScopeContext[context.type] {
          override val context: ctx.type = ctx
          override val typeSystem: ts.type = ts
        }

      override val signatureContext: SignatureContext[context.type] { val typeSystem: ts.type } =
        new SignatureContext[context.type] {
          override val context: ctx.type = ctx
          override val typeSystem: ts.type = ts
        }
    }
  }

  def convertStatementList[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (stmts: WithSource[Vector[WithSource[parser.Stmt]]])
  : TComp[context.typeSystem.ArExpr] = {
    val ts = new HoleTypeSystem[context.type](context)
    val converter = createConverter(context)(ts)

    implicit val tcInstance = typeCheckHoleTypeInstance[TComp](context)(ts)

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

    fillHoles(context)(ts)(
      converter
        .convertStmts[HoleTypeCheckComp[TComp, ts.TType, ?]](env2)(stmts)(tcInstance)
        .forExpectedType(TypeSystem.convertTypeSystem(context)(context.typeSystem)(ts)(tsConverter)(expectedType))
    )(expectedType)
  }

  def convertExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.ArExpr] =
    convertStatementList(context)(env)(expectedType)(WithSource(Vector(expr), expr.location))




  def convertTypeExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.TType] = {
    val ts = new HoleTypeSystem[context.type](context)
    val converter = createConverter(context)(ts)

    implicit val tcInstance = typeCheckHoleTypeInstance[TComp](context)(ts)

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

    val tcExpr = converter.evaluateTypeExprAST[HoleTypeCheckComp[TComp, ts.TType, ?]](env2)(expr)(tcInstance)

    fillHolesType[TComp](context)(ts)(tcExpr)
  }


  def resolveUnitType[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (location: SourceLocation): TComp[context.typeSystem.TType] = {
    val ts = new HoleTypeSystem[context.type](context)
    val converter = createConverter(context)(ts)

    implicit val tcInstance = typeCheckHoleTypeInstance[TComp](context)(ts)

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

    val tcExpr = converter.resolveUnitType[HoleTypeCheckComp[TComp, ts.TType, ?]](env2)(location)(tcInstance)

    fillHolesType[TComp](context)(ts)(tcExpr)
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

  private def fillHoles[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (ts: HoleTypeSystem[context.type])
  (expr: HoleTypeCheckComp[TComp, ts.TType, ts.ArExpr])
  (expectedType: context.typeSystem.TType)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, HoleTypeCheckComp[TComp, ts.TType, ?]])
  : TComp[context.typeSystem.ArExpr] =
    expr
      .flatMap { e =>
        StateT.get[TComp, TypeCheckState[ts.TType]]
          .flatMap { state =>
            resolveHoles(context)(ts)(state.nextHoleId)
          }
          .flatMap { _ =>
            fillHolesExpr(context)(ts)(e)(expectedType)
          }
      }
      .runA(TypeCheckState.default)


  private def fillHolesType[TComp[_] : Compilation]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (t: HoleTypeCheckComp[TComp, ts.TType, ts.TType])
  (implicit tcInstance: TypeCheck[context.type, ts.TType, HoleTypeCheckComp[TComp, ts.TType, ?]])
  : TComp[context.typeSystem.TType] =
    t
      .flatMap { t =>
        StateT.get[TComp, TypeCheckState[ts.TType]]
          .flatMap { state =>
            resolveHoles(context)(ts)(state.nextHoleId)
          }
          .flatMap { _ =>
            fillHolesTypeChildren(context)(ts)(t)
          }
      }
      .runA(TypeCheckState.default)

  private def fillHolesExpr[TComp[_]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (expr: ts.ArExpr)
  (expectedType: context.typeSystem.TType)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[context.typeSystem.ArExpr] = for {
    convExpr <- fillHolesExprChildren(context)(ts)(expr)
    stInfo <- context.typeSystem.isSubType(expectedType, convExpr.exprType)
    _ <- stInfo match {
      case Some(_) => ().pure[TComp]
      case None => Compilation[TComp].forErrors(CompilationError.CouldNotConvertType(context)(context.typeSystem)(convExpr.exprType, expectedType)(???))
    }
  } yield convExpr


  private def fillHolesExprChildren[TComp[_]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (expr: ts.ArExpr)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[context.typeSystem.ArExpr] = expr match {
    case t: ts.SimpleType => fillHolesSimpleTypeChildren(context)(ts)(t).map(identity)

    case ts.ClassConstructorCall(classType, ctor, args) =>
      for {
        newClassType <-  fillHolesClassType(context)(ts)(classType)
        sig <- tcInstance.fromContextComp(context)(ctor.value.signature)
        (newArgs, _) <- fillSignatureArgs(context)(ts)(sig)(args)
      } yield context.typeSystem.ClassConstructorCall(newClassType, ctor, newArgs)

    case ts.PrimitiveOp(ts.PrimitiveOperation.AddInt, left, right, intType) =>
      for {
        newIntType <- fillHolesTypeChildren(context)(ts)(intType)
        newLeft <- fillHolesExpr(context)(ts)(left)(newIntType)
        newRight <- fillHolesExpr(context)(ts)(right)(newIntType)
      } yield context.typeSystem.PrimitiveOp(context.typeSystem.PrimitiveOperation.AddInt, newLeft, newRight, newIntType)

    case ts.PrimitiveOp(ts.PrimitiveOperation.SubInt, left, right, intType) =>
      for {
        newIntType <- fillHolesTypeChildren(context)(ts)(intType)
        newLeft <- fillHolesExpr(context)(ts)(left)(newIntType)
        newRight <- fillHolesExpr(context)(ts)(right)(newIntType)
      } yield context.typeSystem.PrimitiveOp(context.typeSystem.PrimitiveOperation.SubInt, newLeft, newRight, newIntType)

    case ts.PrimitiveOp(ts.PrimitiveOperation.MulInt, left, right, intType) =>
      for {
        newIntType <- fillHolesTypeChildren(context)(ts)(intType)
        newLeft <- fillHolesExpr(context)(ts)(left)(newIntType)
        newRight <- fillHolesExpr(context)(ts)(right)(newIntType)
      } yield context.typeSystem.PrimitiveOp(context.typeSystem.PrimitiveOperation.MulInt, newLeft, newRight, newIntType)

    case ts.PrimitiveOp(ts.PrimitiveOperation.IntEqual, left, right, boolType) =>
      for {
        newBoolType <- fillHolesTypeChildren(context)(ts)(boolType)
        newLeft <- fillHolesExprChildren(context)(ts)(left)
        newRight <- fillHolesExprChildren(context)(ts)(right)
      } yield context.typeSystem.PrimitiveOp(context.typeSystem.PrimitiveOperation.IntEqual, newLeft, newRight, newBoolType)

    case ts.FunctionCall(function, args, _) =>
      for {
        sig <- tcInstance.fromContextComp(context)(function.value.signature)
        (newArgs, result) <- fillSignatureArgs(context)(ts)(sig)(args)
      } yield context.typeSystem.FunctionCall(function, newArgs, result.returnType)

    case ts.FunctionObjectCall(funcExpr, args, _) =>
      for {
        newFuncExpr <- fillHolesExprChildren(context)(ts)(funcExpr)
        newFuncType = newFuncExpr.exprType match {
          case t @ context.typeSystem.FunctionType(_, _) => t
          case _ => ???
        }

        newArgs <- fillHolesExpr(context)(ts)(args)(newFuncType.argumentType)
      } yield context.typeSystem.FunctionObjectCall(newFuncExpr, newArgs, newFuncType.resultType)

    case ts.IfElse(condition, ifBody, elseBody) =>
      for {
        newCondition <- fillHolesExprChildren(context)(ts)(condition)
        newIfBody <- fillHolesExprChildren(context)(ts)(ifBody)
        newElseBody <- fillHolesExprChildren(context)(ts)(elseBody)
      } yield context.typeSystem.IfElse(newCondition, newIfBody, newElseBody)

    case ts.LetBinding(variable, value, next) =>
      for {
        newVarType <- fillHolesTypeChildren(context)(ts)(variable.varType)
        newVar = context.typeSystem.LocalVariable(variable.descriptor, variable.name, variable.mutability, newVarType)

        newValue <- fillHolesExpr(context)(ts)(value)(newVarType)
        newNext <- fillHolesExprChildren(context)(ts)(next)
      } yield context.typeSystem.LetBinding(newVar, newValue, newNext)

    case ts.LoadConstantInt(i, intType) =>
      for {
        newIntType <- fillHolesTypeChildren(context)(ts)(intType)
      } yield context.typeSystem.LoadConstantInt(i, newIntType)

    case ts.LoadConstantString(str, stringType) =>
      for {
        newStringType <- fillHolesTypeChildren(context)(ts)(stringType)
      } yield context.typeSystem.LoadConstantString(str, newStringType)

    case ts.LoadLambda(argVariable, body) =>
      for {
        newVarType <- fillHolesTypeChildren(context)(ts)(argVariable.varType)
        newVar = context.typeSystem.LocalVariable(argVariable.descriptor, argVariable.name, argVariable.mutability, newVarType)
        newBody <- fillHolesExprChildren(context)(ts)(body)
      } yield context.typeSystem.LoadLambda(newVar, newBody)

    case t: ts.LoadTuple =>
      for {
        elems <- t.values.traverse { elem =>
          fillHolesWrapExprChildren(context)(ts)(elem.value)
            .map(context.typeSystem.TupleElement(_))
        }
      } yield context.typeSystem.LoadTuple(elems)

    case ts.LoadUnit(unitType) =>
      for {
        newUnitType <- fillHolesTypeChildren(context)(ts)(unitType)
      } yield context.typeSystem.LoadUnit(newUnitType)

    case ts.LoadVariable(ts.LocalVariable(descriptor, name, mutability, varType)) =>
      for {
        newVarType <- fillHolesTypeChildren(context)(ts)(varType)
        newVar = context.typeSystem.LocalVariable(descriptor, name, mutability, newVarType)
      } yield context.typeSystem.LoadVariable(newVar)

    case ts.LoadVariable(ts.ParameterVariable(descriptor, name, mutability, varType)) =>
      for {
        newVarType <- fillHolesTypeChildren(context)(ts)(varType)
        newVar = context.typeSystem.ParameterVariable(descriptor, name, mutability, newVarType)
      } yield context.typeSystem.LoadVariable(newVar)

    case ts.LoadVariable(ts.ParameterElementVariable(descriptor, name, mutability, varType)) =>
      for {
        newVarType <- fillHolesTypeChildren(context)(ts)(varType)
        newVar = context.typeSystem.ParameterElementVariable(descriptor, name, mutability, newVarType)
      } yield context.typeSystem.LoadVariable(newVar)

    case ts.LoadVariable(ts.FieldVariable(descriptor, ownerClass, name, mutability, varType)) =>
      for {
        newVarType <- fillHolesTypeChildren(context)(ts)(varType)
        newVar = context.typeSystem.FieldVariable(descriptor, ownerClass, name, mutability, newVarType)
      } yield context.typeSystem.LoadVariable(newVar)

    case ts.MethodCall(method, instance, args, _) =>
      for {
        newInstance <- fillHolesExprChildren(context)(ts)(instance)
        sig <- tcInstance.fromContextComp(context)(method.value.signature)
        (newArgs, result) <- fillSignatureArgs(context)(ts)(sig)(args)
      } yield context.typeSystem.MethodCall(method, newInstance, newArgs, result.returnType)

    case ts.Sequence(first, second) =>
      for {
        newFirst <- fillHolesExprChildren(context)(ts)(first)
        newSecond <- fillHolesExprChildren(context)(ts)(second)
      } yield context.typeSystem.Sequence(newFirst, newSecond)

    case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented")
  }

  private def fillHolesWrapExprChildren[TComp[_]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (expr: ts.WrapExpr)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[context.typeSystem.ArExpr] = expr match {
    case HoleTypeHole(id) =>
      tcInstance.resolveType(HoleTypeHole(id))
        .flatMap(fillHolesTypeChildren(context)(ts)(_))
        .map(identity)

    case HoleTypeType(e) =>
      fillHolesExprChildren(context)(ts)(e).map(context.typeSystem.wrapType(_))
  }

  private def fillHolesTypeChildren[TComp[_]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (t: ts.TType)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[context.typeSystem.TType] = t match {
    case HoleTypeHole(_) => tcInstance.resolveType(t).flatMap(fillHolesTypeChildren(context)(ts)(_))
    case HoleTypeType(t) => fillHolesSimpleTypeChildren(context)(ts)(t).map(context.typeSystem.fromSimpleType(_))
  }


  private def fillHolesSimpleTypeChildren[TComp[_]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (t: ts.SimpleType)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[context.typeSystem.SimpleType] = t match {
    case t: ts.ClassType => fillHolesClassType(context)(ts)(t).map(identity)

    case ts.TraitType(arTrait, args, baseTypes) =>
      for {
        sig <- tcInstance.fromContextComp(context)(arTrait.value.signature)
        (filledArgs, resultInfo) <- fillSignatureArgsTypes(context)(ts)(sig)(args)
      } yield context.typeSystem.TraitType(arTrait, filledArgs, resultInfo.baseTypes)

    case ts.FunctionType(argumentType, resultType) =>
      for {
        newArgType <- fillHolesTypeChildren(context)(ts)(argumentType)
        newResultType <- fillHolesTypeChildren(context)(ts)(resultType)
      } yield context.typeSystem.FunctionType(newArgType, newResultType)

    case t: ts.LoadTupleType =>
      t.typeValues
        .traverse { case ts.TupleElement(elemType) =>
            fillHolesTypeChildren(context)(ts)(elemType)
              .map(context.typeSystem.TupleElement(_))
        }
        .map(context.typeSystem.LoadTupleType(_))

    case ts.UnionType(first, second) =>
      for {
        newFirst <- fillHolesTypeChildren(context)(ts)(first)
        newSecond <- fillHolesTypeChildren(context)(ts)(second)
      } yield context.typeSystem.UnionType(newFirst, newSecond)

    case ts.IntersectionType(first, second) =>
      for {
        newFirst <- fillHolesTypeChildren(context)(ts)(first)
        newSecond <- fillHolesTypeChildren(context)(ts)(second)
      } yield context.typeSystem.IntersectionType(newFirst, newSecond)

    case ts.TypeOfType(inner, universe) =>
      for {
        newInner <- fillHolesTypeChildren(context)(ts)(inner)
      } yield context.typeSystem.TypeOfType(newInner, universe)

    case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented")
  }

  private def fillHolesClassType[TComp[_]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (t: ts.ClassType)
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[context.typeSystem.ClassType] =
    for {
      sig <- tcInstance.fromContextComp(context)(t.arClass.value.signature)
      (filledArgs, resultInfo) <- fillSignatureArgsTypes(context)(ts)(sig)(t.args)
    } yield context.typeSystem.ClassType(t.arClass, filledArgs, resultInfo.baseTypes)

  private def fillSignatureArgs[TComp[_], TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (sig: context.signatureContext.Signature[TResult])
  (args: Vector[ts.ArExpr])
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[(Vector[context.typeSystem.ArExpr], TResult[context.type, context.typeSystem.type])] = {

    def impl
    (sig: context.signatureContext.Signature[TResult])
    (args: Vector[ts.ArExpr])
    (newArgs: Vector[context.typeSystem.ArExpr])
    : TComp[(Vector[context.typeSystem.ArExpr], TResult[context.type, context.typeSystem.type])] =
      sig.visit(
        sigParam => args match {
          case Vector() => ???
          case arg +: tailArgs =>
            fillHolesExpr(context)(ts)(arg)(sigParam.parameter.paramType).flatMap { convArg =>
              sigParam.next(convArg).flatMap { next =>
                impl(next)(tailArgs)(newArgs :+ convArg)
              }
            }
        },
        sigResult => if(args.nonEmpty) ??? else (newArgs, sigResult.result).pure[TComp]
      )

    impl(sig)(args)(Vector.empty)
  }

  private def fillSignatureArgsTypes[TComp[_], TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (context: Context)
  (ts: HoleTypeSystem[context.type])
  (sig: context.signatureContext.Signature[TResult])
  (args: Vector[ts.TType])
  (implicit tcInstance: TypeCheck[context.type, ts.TType, TComp])
  : TComp[(Vector[context.typeSystem.TType], TResult[context.type, context.typeSystem.type])] = {

    def resolveOuterHole(t: ts.TType): TComp[ts.SimpleType] = t match {
      case HoleTypeType(t) => t.pure[TComp]
      case t @ HoleTypeHole(_) => tcInstance.resolveType(t).flatMap(resolveOuterHole(_))
    }

    for {
      simpleTypeArgs <- args.traverse(resolveOuterHole(_))
      (newArgs, result) <- fillSignatureArgs(context)(ts)(sig)(simpleTypeArgs)
      argsAsTypes <- newArgs.traverse {
        case t: context.typeSystem.SimpleType => context.typeSystem.fromSimpleType(t).pure[TComp]
        case _ => ???
      }
    } yield (argsAsTypes, result)

  }



  sealed trait HoleType[+T]
  private final case class HoleTypeType[+T](t: T) extends HoleType[T]
  private final case class HoleTypeHole[+T](id: Int) extends HoleType[T]



  private final class HoleTypeSystem[TContext <: Context with Singleton](override val context: TContext) extends TypeSystem[TContext] {
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

    override def traverseTypeWrapper[A, B, F[_] : Applicative](t: HoleType[A])(f: A => F[B]): F[HoleType[B]] =
      t match {
        case HoleTypeType(a) => f(a).map(HoleTypeType.apply)
        case HoleTypeHole(id) => (HoleTypeHole(id) : TTypeWrapper[B]).pure[F]
      }

    override def wrapExprType(expr: WrapExpr): TType = expr match {
      case HoleTypeType(t) => t.exprType
      case HoleTypeHole(_) => ???
    }


    override def isSubTypeWrapper[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[SubTypeInfo[TType]]] =
      (a, b) match {
        case (HoleTypeType(aInner), HoleTypeType(bInner)) => isSimpleSubType(aInner, bInner)
        case (_, _) => (Some(SubTypeInfo(a, b, Vector.empty)) : Option[SubTypeInfo[TType]]).pure[TComp]
      }

    override def universeOfExpr(expr: WrapExpr): Universe = ???

    override def universeOfType(t: TType): TypeUniverse = t match {
      case HoleTypeHole(_) => ???
      case HoleTypeType(t) => t.universe
    }
  }

  private def holeTypeConverter
  (context: Context)
  (innerTS: TypeSystem[context.type])
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  })
  : TypeSystemConverter[context.type, innerTS.type, holeTS.type, Id] =
    new TypeSystemConverter[context.type, innerTS.type, holeTS.type, Id] {
      override def convertType[A](ts1: innerTS.type)(ts2: holeTS.type)(fromSimpleType: ts2.SimpleType => A)(t: ts1.TTypeWrapper[A]): HoleType[innerTS.TTypeWrapper[A]] =
        HoleTypeType(t)
    }


  sealed trait HoleConstraint[TType]
  private final case class HoleResolved[TType](t: TType) extends HoleConstraint[TType]
  private final case class HoleBounds[TType](bounds: Set[TypeConstraint[TType]]) extends HoleConstraint[TType]

  private sealed trait TypeConstraint[TType]
  private final case class SuperTypeConstraint[TType](superType: TType) extends TypeConstraint[TType]
  private final case class SubTypeConstraint[TType](subType: TType) extends TypeConstraint[TType]

  private trait TypeCheck[TContext <: Context with Singleton, TType, TComp[_]] extends Compilation[TComp] {
    def fromContextComp[A](context: TContext)(comp: context.Comp[A]): TComp[A]
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

  private def typeCheckHoleTypeInstance[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (ts: HoleTypeSystem[context.type])
  : TypeCheck[context.type, ts.TType, HoleTypeCheckComp[TComp, ts.TType, ?]] =
    new TypeCheck[context.type, ts.TType, HoleTypeCheckComp[TComp, ts.TType, ?]] {


      implicit val holeTypeCheckMonadTType = holeTypeCheckMonad[TComp, ts.TType]

      override def fromContextComp[A](context2: context.type)(comp: context.Comp[A]): HoleTypeCheckComp[TComp, ts.TType, A] =
        StateT((s: TypeCheckState[ts.TType]) => comp.map { a => (s, a) })

      override def createHole: HoleTypeCheckComp[TComp, ts.TType, ts.TType] =
        for {
          state <- StateT.get[TComp, TypeCheckState[ts.TType]]
          _ <- StateT.set[TComp, TypeCheckState[ts.TType]](state.copy(nextHoleId = state.nextHoleId + 1))
        } yield HoleTypeHole(state.nextHoleId)

      private def addConstraint(id: Int, constraint: TypeConstraint[ts.TType]): HoleTypeCheckComp[TComp, ts.TType, Unit] =
        StateT.modify[TComp, TypeCheckState[ts.TType]] { state =>
          state.constraints.getOrElse(id, HoleBounds(Set.empty[TypeConstraint[ts.TType]])) match {
            case HoleResolved(_) => state
            case HoleBounds(bounds) =>
              state.copy(constraints = state.constraints.updated(id, HoleBounds(bounds + constraint)))
          }
        }

      override def recordConstraint(info: SubTypeInfo[ts.TType]): HoleTypeCheckComp[TComp, ts.TType, Unit] =
        (info.subType, info.superType) match {
          case (a @ HoleTypeHole(idA), b @ HoleTypeHole(idB)) =>
            addConstraint(idA, SuperTypeConstraint(b)).flatMap { _ =>
              addConstraint(idB, SubTypeConstraint(a))
            }

          case (HoleTypeHole(idA), b) =>
            addConstraint(idA, SuperTypeConstraint(b))

          case (a, HoleTypeHole(idB)) =>
            addConstraint(idB, SubTypeConstraint(a))

          case (HoleTypeType(_), HoleTypeType(_)) =>
            info.args.traverse_(recordConstraint(_))(this)
        }

      override def resolveType(t: ts.TType): HoleTypeCheckComp[TComp, ts.TType, ts.TType] =
        TypeSystem.convertTypeSystem(context)(ts)(ts)(new ResolverConverter)(t).runA(Set.empty)

      private def resolveOuterHole(id: Int): HoleTypeCheckComp[TComp, ts.TType, ts.TType] =
        StateT.get[TComp, TypeCheckState[ts.TType]].flatMap { state =>
          state.constraints.getOrElse(id, HoleBounds(Set.empty[TypeConstraint[ts.TType]])) match {
            case HoleResolved(hole) => hole.pure[HoleTypeCheckComp[TComp, ts.TType, ?]]
            case HoleBounds(bounds) =>
              val superTypeConstraints = bounds.collect {
                case SuperTypeConstraint(superType) => superType
              }

              val resolvedType = superTypeConstraints.toVector match {
                case head +: tail =>
                  tail.fold(head) { (a, b) => ts.fromSimpleType(ts.IntersectionType(a, b)) }

                case _ =>
                  val subTypeConstraints = bounds.collect {
                    case SubTypeConstraint(subType) => subType
                  }

                  subTypeConstraints.toVector match {
                    case head +: tail =>
                      tail.fold(head) { (a, b) => ts.fromSimpleType(ts.UnionType(a, b)) }

                    case _ => ???
                  }
              }

              for {
                _ <- StateT.set[TComp, TypeCheckState[ts.TType]](
                  state.copy(constraints = state.constraints.updated(id, HoleResolved(resolvedType)))
                )
              } yield resolvedType
          }
        }

      type ResolverState[A] = StateT[HoleTypeCheckComp[TComp, ts.TType, ?], Set[Int], A]
      implicit val resolverStateMonad: Monad[ResolverState] =
        cats.data.IndexedStateT.catsDataMonadForIndexedStateT[HoleTypeCheckComp[TComp, ts.TType, ?], Set[Int]]

      private final class ResolverConverter extends TypeSystemConverter[context.type, ts.type, ts.type, ResolverState] {
        override def convertType[A](ts1: ts.type)(ts2: ts.type)(fromSimpleType: ts2.SimpleType => A)(t: HoleType[A]): ResolverState[HoleType[A]] =
          t match {
            case HoleTypeType(_) => t.pure[ResolverState]
            case HoleTypeHole(id) =>
              for {
                seenHoles <- StateT.get[HoleTypeCheckComp[TComp, ts.TType, ?], Set[Int]]
                _ <-
                  if(seenHoles.contains(id))
                    ???
                  else
                    StateT.set[HoleTypeCheckComp[TComp, ts.TType, ?], Set[Int]](seenHoles + id)


                resolvedOuter <- StateT((s: Set[Int]) => resolveOuterHole(id).map { a => (s, a) })
                resolvedType <- TypeSystem.convertTypeSystem(context)(ts)(ts)(this)(resolvedOuter)
              } yield ts.mapTypeWrapper(resolvedType)(fromSimpleType)
          }

      }

      override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[Nothing]): HoleTypeCheckComp[TComp, ts.TType, A] =
        StateT((s: TypeCheckState[ts.TType]) => Compilation[TComp].forErrors[A](errors, messages).map { a => (s, a) })

      override def createCache[A]: HoleTypeCheckComp[TComp, ts.TType, HoleTypeCheckComp[TComp, ts.TType, A] => HoleTypeCheckComp[TComp, ts.TType, A]] =
        pure(identity _)

      override def createMemo[A, B]: HoleTypeCheckComp[TComp, ts.TType, (A => HoleTypeCheckComp[TComp, ts.TType, B]) => A => HoleTypeCheckComp[TComp, ts.TType, B]] =
        pure(identity _)


      override def flatMap[A, B](fa: HoleTypeCheckComp[TComp, ts.TType, A])(f: A => HoleTypeCheckComp[TComp, ts.TType, B]): HoleTypeCheckComp[TComp, ts.TType, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => HoleTypeCheckComp[TComp, ts.TType, Either[A, B]]): HoleTypeCheckComp[TComp, ts.TType, B] =
        holeTypeCheckMonadTType.tailRecM(a)(f)

      override def pure[A](x: A): HoleTypeCheckComp[TComp, ts.TType, A] =
        holeTypeCheckMonadTType.pure(x)

    }

}
