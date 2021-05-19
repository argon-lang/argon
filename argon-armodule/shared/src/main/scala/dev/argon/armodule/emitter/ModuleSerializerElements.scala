package dev.argon.armodule.emitter

import cats.evidence.===
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.module
import dev.argon.util.{FileID, NamespacePath}
import shapeless.{Nat, _0}
import zio.{IO, ZIO}
import cats.implicits._
import com.google.protobuf.ByteString
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.util.Id
import shapeless.ops.nat.{LT, Pred}
import zio.interop.catz.core._

private[emitter] trait ModuleSerializerElements {

  val context: Context
  protected val emitEnv: EmitEnv[context.type]
  import context.typeSystem
  import context.signatureContext.{Signature, SignatureParameters, SignatureResult, SignatureVisitor}




  def convertSignature[TResult[_ <: Context with Singleton, Wrap[+_]], A]
  (sig: Signature[TResult, _ <: Nat])
  (f: (Vector[module.Parameter], TResult[context.type, context.typeSystem.TTypeWrapper]) => Comp[A])
  : Comp[A] = {

    def impl[Len <: Nat](sig: Signature[TResult, Len], prevParams: Vector[module.Parameter]): Comp[A] =
      sig.visit(new SignatureVisitor[TResult, Len, Comp[A]] {
        override def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): Comp[A] =
          sigParams.parameter.elements
            .traverse { elem =>
              convertExpr(elem.elemType).map { paramType =>
                module.ParameterElement(
                  elem.name match {
                    case VariableName.Normal(name) => Some(name)
                    case VariableName.Unnamed => None
                  },
                  paramType
                )
              }
            }
            .flatMap { elems =>
              val style = sigParams.parameter.style match {
                case ParameterStyle.Normal => None
                case ParameterStyle.Inferrable => Some(module.ParameterStyle.Inferrable)
              }

              impl(sigParams.nextUnsubstituted, prevParams :+ module.Parameter(style, isErased = if(sigParams.parameter.paramVar.isErased) Some(true) else None, elems))
            }

        override def visitResult(sigResult: SignatureResult[TResult])(implicit lenEq: Len === _0): Comp[A] =
          f(prevParams, sigResult.result)

      })

    impl(sig, Vector.empty)
  }

  def convertClassType(classType: typeSystem.TClassType): Comp[module.ClassType] = for {
    id <- emitEnv.classes.getIdNumForAbsRef(classType.arClass)
    args <- classType.args.traverse(convertExpr(_))
  } yield module.ClassType(id, args)

  def convertTraitType(traitType: typeSystem.TTraitType): Comp[module.TraitType] = for {
    id <- emitEnv.traits.getIdNumForAbsRef(traitType.arTrait)
    args <- traitType.args.traverse(convertExpr(_))
  } yield module.TraitType(id, args)

  def convertDataCtorType(dataCtorType: typeSystem.TDataConstructorType): Comp[module.DataConstructorType] = for {
    id <- emitEnv.dataConstructors.getIdNumForAbsRef(dataCtorType.ctor)
    args <- dataCtorType.args.traverse(convertExpr(_))
  } yield module.DataConstructorType(id, args)

  def convertVariableName(name: VariableName): module.VariableName = name match {
    case VariableName.Normal(name) => module.VariableName(module.VariableName.Name.Normal(name))
    case VariableName.Unnamed => module.VariableName(module.VariableName.Name.Unnamed(com.google.protobuf.empty.Empty()))
  }

  def convertFieldVariable(variable: FieldVariable[context.type, Id]): Comp[module.FieldVariable] =
    for {
      ownerId <- emitEnv.classes.getIdNumForAbsRef(variable.owner.ownerClass)
      convVarType <- convertExpr(variable.varType)
    } yield module.FieldVariable(
      ownerClass = ownerId,
      name = convertVariableName(variable.name),
      mutability = convertMutability(variable.mutability),
      varType = convVarType,
    )

  def convertExpr(expr: typeSystem.SimpleExpr): Comp[module.Expression] = {
    def convertBigInt(i: BigInt): module.BigInt =
      module.BigInt(
        if(i >= Int.MinValue && i <= Int.MaxValue)
          module.BigInt.IntType.Signed32(i.toInt)
        else if(i >= Long.MinValue && i <= Long.MaxValue)
          module.BigInt.IntType.Signed64(i.toLong)
        else
          module.BigInt.IntType.Bigint(ByteString.copyFrom(i.toByteArray))
      )

    def convertUniverse(universe: UniverseExpr): module.UniverseExpr =
      module.UniverseExpr(
        universe match {
          case FixedUniverse(u) => module.UniverseExpr.ExprType.Fixed(convertBigInt(u))
          case AbstractUniverse() => module.UniverseExpr.ExprType.AbstractUniverse(com.google.protobuf.empty.Empty())
          case LargestUniverse(a, b) =>
            module.UniverseExpr.ExprType.LargestUniverse(
              module.UniversePair(convertUniverse(a), convertUniverse(b))
            )
          case NextLargestUniverse(a) =>
            module.UniverseExpr.ExprType.NextLargestUniverse(convertUniverse(a))

          case PreviousUniverse(a) =>
            module.UniverseExpr.ExprType.PreviousUniverse(convertUniverse(a))
        }
      )

    def convertLocalVariable(variable: LocalVariable[context.type, Id]): Comp[module.LocalVariable] =
      for {
        varIdNum <- emitEnv.getLocalVariableIdNum(variable)

        ownerId <- variable.owner match {
          case LocalVariableOwner.ByClass(ownerClass) =>
            emitEnv.classes.getIdNumForAbsRef(ownerClass)
              .map { module.LocalVariableOwner.Owner.ByClass(_) }

          case LocalVariableOwner.ByTrait(ownerTrait) =>
            emitEnv.traits.getIdNumForAbsRef(ownerTrait)
              .map { module.LocalVariableOwner.Owner.ByTrait(_) }

          case LocalVariableOwner.ByDataConstructor(ownerCtor) =>
            emitEnv.dataConstructors.getIdNumForAbsRef(ownerCtor)
              .map { module.LocalVariableOwner.Owner.ByDataConstructor(_) }

          case LocalVariableOwner.ByFunction(ownerFunc) =>
            emitEnv.functions.getIdNumForAbsRef(ownerFunc)
              .map { module.LocalVariableOwner.Owner.ByFunction(_) }

          case LocalVariableOwner.ByMethod(ownerMethod) =>
            emitEnv.methods.getIdNumForAbsRef(ownerMethod)
              .map { module.LocalVariableOwner.Owner.ByMethod(_) }

          case LocalVariableOwner.ByClassConstructor(ownerCtor) =>
            emitEnv.classConstructors.getIdNumForAbsRef(ownerCtor)
              .map { module.LocalVariableOwner.Owner.ByClassConstructor(_) }
        }

        convVarType <- convertExpr(variable.varType)
      } yield module.LocalVariable(
        id = varIdNum,
        owner = module.LocalVariableOwner(ownerId),
        name = convertVariableName(variable.name),
        mutability = convertMutability(variable.mutability),
        varType = convVarType,
        isErased = if(variable.isErased) Some(true) else None
      )

    def convertVariable(variable: Variable[context.type, Id]): Comp[module.Variable] =
      variable match {
        case variable: LocalVariable[context.type, Id] =>
          convertLocalVariable(variable).map { convVar =>
            module.Variable(module.Variable.VariableType.Local(convVar))
          }

        case ParameterVariable(owner, index, name, mutability, isErased, varType) =>
          for {
            ownerId <- owner match {
              case ParameterVariableOwner.ByClass(ownerClass) =>
                emitEnv.classes.getIdNumForAbsRef(ownerClass)
                  .map { module.ParameterVariableOwner.Owner.ByClass(_) }

              case ParameterVariableOwner.ByTrait(ownerTrait) =>
                emitEnv.traits.getIdNumForAbsRef(ownerTrait)
                  .map { module.ParameterVariableOwner.Owner.ByTrait(_) }

              case ParameterVariableOwner.ByDataConstructor(ownerCtor) =>
                emitEnv.dataConstructors.getIdNumForAbsRef(ownerCtor)
                  .map { module.ParameterVariableOwner.Owner.ByDataConstructor(_) }

              case ParameterVariableOwner.ByFunction(ownerFunc) =>
                emitEnv.functions.getIdNumForAbsRef(ownerFunc)
                  .map { module.ParameterVariableOwner.Owner.ByFunction(_) }

              case ParameterVariableOwner.ByMethod(ownerMethod) =>
                emitEnv.methods.getIdNumForAbsRef(ownerMethod)
                  .map { module.ParameterVariableOwner.Owner.ByMethod(_) }

              case ParameterVariableOwner.ByClassConstructor(ownerCtor) =>
                emitEnv.classConstructors.getIdNumForAbsRef(ownerCtor)
                  .map { module.ParameterVariableOwner.Owner.ByClassConstructor(_) }
            }

            convVarType <- convertExpr(varType)
          } yield module.Variable(module.Variable.VariableType.Param(module.ParameterVariable(
            owner = module.ParameterVariableOwner(ownerId),
            index = index,
            name = convertVariableName(name),
            mutability = convertMutability(mutability),
            varType = convVarType,
            isErased = if(isErased) Some(true) else None
          )))

        case ThisParameterVariable(_, _, _, _) => ???

        case variable: FieldVariable[context.type, Id] =>
          convertFieldVariable(variable).map { convVar =>
            module.Variable(module.Variable.VariableType.Field(convVar))
          }



      }

    def impl(expr: typeSystem.SimpleExpr): Comp[(module.Expression.ExprType, Vector[module.Expression])] =
      expr match {
        case TraitType(arTrait, args) =>
          for {
            idNum <- emitEnv.traits.getIdNumForAbsRef(arTrait)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.TraitType(idNum), convArgs)

        case ClassType(arClass, args) =>
          for {
            idNum <- emitEnv.classes.getIdNumForAbsRef(arClass)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.ClassType(idNum), convArgs)

        case DataConstructorType(ctor, args, _) =>
          for {
            idNum <- emitEnv.dataConstructors.getIdNumForAbsRef(ctor)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.DataConstructorType(idNum), convArgs)

        case TypeOfType(inner) =>
          for {
            convInner <- convertExpr(inner)
          } yield (module.Expression.ExprType.TypeOfType(com.google.protobuf.empty.Empty()), Vector(convInner))

        case TypeN(universe, subtypeConstraint, supertypeConstraint) =>
          for {
            convSubtype <- ZIO.foreach(subtypeConstraint)(convertExpr(_))
            convSupertype <- ZIO.foreach(supertypeConstraint)(convertExpr(_))
          } yield (
            module.Expression.ExprType.TypeN(
              module.TypeN(convertUniverse(universe), convSubtype, convSupertype)
            ),
            Vector.empty
          )

        case FunctionType(argumentType, resultType) =>
          for {
            convArg <- convertExpr(argumentType)
            convRes <- convertExpr(resultType)
          } yield (module.Expression.ExprType.FunctionType(com.google.protobuf.empty.Empty()), Vector(convArg, convRes))

        case UnionType(a, b) =>
          for {
            convA <- convertExpr(a)
            convB <- convertExpr(b)
          } yield (module.Expression.ExprType.UnionType(com.google.protobuf.empty.Empty()), Vector(convA, convB))

        case IntersectionType(a, b) =>
          for {
            convA <- convertExpr(a)
            convB <- convertExpr(b)
          } yield (module.Expression.ExprType.IntersectionType(com.google.protobuf.empty.Empty()), Vector(convA, convB))

        case ExistentialType(variable, inner) =>
          for {
            convVar <- convertLocalVariable(variable)
            convInner <- convertExpr(inner)
          } yield (module.Expression.ExprType.ExistentialType(convVar), Vector(convInner))

        case ClassConstructorCall(classType, classCtor, args) =>
          for {
            idNum <- emitEnv.classConstructors.getIdNumForAbsRef(classCtor)
            convClassType <- convertExpr(classType)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.ClassConstructorCall(idNum), convClassType +: convArgs)

        case DataConstructorCall(dataCtorInstanceType, args) =>
          for {
            convCtorType <- convertExpr(dataCtorInstanceType)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.DataConstructorCall(com.google.protobuf.empty.Empty()), convCtorType +: convArgs)

        case EnsureExecuted(body, ensuring) =>
          for {
            convBody <- convertExpr(body)
            convEnsuring <- convertExpr(ensuring)
          } yield (module.Expression.ExprType.EnsureExecuted(com.google.protobuf.empty.Empty()), Vector(convBody, convEnsuring))

        case FunctionCall(function, args, _) =>
          for {
            idNum <- emitEnv.functions.getIdNumForAbsRef(function)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.FunctionCall(idNum), convArgs)

        case FunctionObjectCall(function, arg, returnType) =>
          for {
            convFunc <- convertExpr(function)
            convArg <- convertExpr(arg)
            convReturnType <- convertExpr(returnType)
          } yield (module.Expression.ExprType.FunctionObjectCall(com.google.protobuf.empty.Empty()), Vector(convFunc, convArg, convReturnType))

        case IfElse(condition, ifBody, elseBody) =>
          for {
            convCond <- convertExpr(condition)
            convIfBody <- convertExpr(ifBody)
            convElseBody <- convertExpr(elseBody)
          } yield (module.Expression.ExprType.IfElse(com.google.protobuf.empty.Empty()), Vector(convCond, convIfBody, convElseBody))

        case LetBinding(variable, value, next) =>
          for {
            convVar <- convertLocalVariable(variable)
            convValue <- convertExpr(value)
            convNext <- convertExpr(next)
          } yield (module.Expression.ExprType.LetBinding(convVar), Vector(convValue, convNext))

        case LoadConstantBool(value, exprType) =>
          for {
            convExprType <- convertExpr(exprType)
          } yield (module.Expression.ExprType.LoadConstantBool(value), Vector(convExprType))

        case LoadConstantInt(value, exprType) =>
          for {
            convExprType <- convertExpr(exprType)
          } yield (module.Expression.ExprType.LoadConstantInt(convertBigInt(value)), Vector(convExprType))

        case LoadConstantString(value, exprType) =>
          for {
            convExprType <- convertExpr(exprType)
          } yield (module.Expression.ExprType.LoadConstantString(value), Vector(convExprType))

        case LoadLambda(argVariable, body) =>
          for {
            convVar <- convertLocalVariable(argVariable)
            convBody <- convertExpr(body)
          } yield (module.Expression.ExprType.LoadLambda(convVar), Vector(convBody))

        case LoadTuple(values) =>
          val tupleElements = values.toList.toVector.map { _ => module.TupleElement() }
          for {
            convValues <- values.toList.toVector.traverse { case TupleElement(value) => convertExpr(value) }
          } yield (
            module.Expression.ExprType.LoadTuple(module.TupleMetadata(tupleElements)),
            convValues
          )

        case LoadTupleElement(tupleValue, elemType, index) =>
          for {
            convValue <- convertExpr(tupleValue)
            convElemType <- convertExpr(elemType)
          } yield (module.Expression.ExprType.LoadTupleElement(index), Vector(convValue, convElemType))

        case LoadUnit(exprType) =>
          for {
            convExprType <- convertExpr(exprType)
          } yield (module.Expression.ExprType.LoadUnit(com.google.protobuf.empty.Empty()), Vector(convExprType))

        case LoadVariable(variable) =>
          for {
            convVar <- convertVariable(variable)
          } yield (module.Expression.ExprType.LoadVariable(convVar), Vector.empty)

        case MethodCall(method, instance, instanceType, args, _) =>
          for {
            idNum <- emitEnv.methods.getIdNumForAbsRef(method)
            convInstance <- convertExpr(instance)
            convInstanceType <- convertExpr(instanceType)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.MethodCall(idNum), convInstance +: convInstanceType +: convArgs)

        case PatternMatch(expr, cases) =>
          def convertPattern(pattern: PatternExpr[context.type, Id]): Comp[module.PatternExpr] = ???

          for {
            convExpr <- convertExpr(expr)
            patterns <- cases.toList.toVector.traverse { case PatternCase(pattern, _) => convertPattern(pattern) }
            caseBodies <- cases.toList.toVector.traverse { case PatternCase(_, body) => convertExpr(body) }
          } yield (
            module.Expression.ExprType.PatternMatch(
              module.PatternMatch(patterns)
            ),
            convExpr +: caseBodies
          )

        case Sequence(first, second) =>
          for {
            convFirst <- convertExpr(first)
            convSecond <- convertExpr(second)
          } yield (module.Expression.ExprType.Sequence(com.google.protobuf.empty.Empty()), Vector(convFirst, convSecond))

        case StoreVariable(variable, value, exprType) =>
          for {
            convVar <- convertVariable(variable)
            convExprType <- convertExpr(exprType)
            convValue <- convertExpr(value)
          } yield (module.Expression.ExprType.StoreVariable(convVar), Vector(convExprType, convValue))
      }

    impl(expr).map { case (exprType, args) =>
      module.Expression(exprType, args)
    }
  }

  def convertAccessModifier(accessModifier: AccessModifier): module.AccessModifier =
    accessModifier match {
      case AccessModifier.Public => module.AccessModifier.Public
      case AccessModifier.Internal => module.AccessModifier.Internal
      case AccessModifier.Protected => module.AccessModifier.Protected
      case AccessModifier.ProtectedInternal => module.AccessModifier.ProtectedInternal
      case AccessModifier.Private => module.AccessModifier.Private
      case AccessModifier.PrivateInternal => module.AccessModifier.PrivateInternal
    }

  def convertFileId(id: FileID): module.FileID =
    module.FileID(id.id)

  def convertNamespace(ns: NamespacePath): module.Namespace =
    module.Namespace(ns.ns)

  def convertGlobalName(name: GlobalName): module.GlobalName =
    name match {
      case GlobalName.Normal(name) => module.GlobalName(module.GlobalName.GlobalName.NormalName(name))
      case GlobalName.Operator(op) => module.GlobalName(module.GlobalName.GlobalName.Operator(op))
      case GlobalName.Unnamed => ???
    }



  def convertErasedSigType(sigType: ErasedSignature.SigType[context.type]): Comp[module.SigType] =
    (sigType match {
      case ErasedSignature.BlankType() => IO.succeed(module.SigType.SigType._Empty(com.google.protobuf.empty.Empty()))
      case ErasedSignature.TraitType(arTrait, typeArgs) =>
        for {
          idNum <- emitEnv.traits.getIdNumForAbsRef(arTrait)
          args <- typeArgs.traverse(convertErasedSigType)
        } yield module.SigType.SigType.TraitType(module.SigTypeTrait(idNum, args))

      case ErasedSignature.ClassType(arClass, typeArgs) =>
        for {
          idNum <- emitEnv.classes.getIdNumForAbsRef(arClass)
          args <- typeArgs.traverse(convertErasedSigType)
        } yield module.SigType.SigType.ClassType(module.SigTypeClass(idNum, args))

      case ErasedSignature.DataConstructorType(ctor, typeArgs) =>
        for {
          idNum <- emitEnv.dataConstructors.getIdNumForAbsRef(ctor)
          args <- typeArgs.traverse(convertErasedSigType)
        } yield module.SigType.SigType.DataCtorType(module.SigTypeDataConstructor(idNum, args))

      case ErasedSignature.TupleType(elements) =>
        for {
          elems <- elements.toList.toVector.traverse(convertErasedSigType)
        } yield module.SigType.SigType.TupleType(module.SigTypeTuple(elems))

      case ErasedSignature.FunctionType(argumentType, resultType) =>
        for {
          argType <- convertErasedSigType(argumentType)
          resType <- convertErasedSigType(resultType)
        } yield module.SigType.SigType.FunctionType(module.SigTypeFunction(argType, resType))

    }).map(module.SigType.apply)

  def convertErasedSignatureParameterOnly(sig: ErasedSignature.ParameterOnlySignature[context.type]): Comp[module.ErasedSignatureParameterOnly] =
    sig.paramTypes.traverse(convertErasedSigType)
      .map(module.ErasedSignatureParameterOnly.apply)

  def convertErasedSignatureParameterOnly(sigOpt: Option[ErasedSignature.ParameterOnlySignature[context.type]], sig: Comp[ErasedSignature.ParameterOnlySignature[context.type]]): Comp[module.ErasedSignatureParameterOnly] =
    sigOpt.fold(sig)(IO.succeed(_)).flatMap(convertErasedSignatureParameterOnly)

  def convertErasedSignature(sig: ErasedSignature[context.type]): Comp[module.ErasedSignature] = sig match {
    case ErasedSignature.Parameter(paramType, next) =>
      for {
        paramTypeConv <- convertErasedSigType(paramType)
        nextConv <- convertErasedSignature(next)
      } yield module.ErasedSignature(parameterTypes = paramTypeConv +: nextConv.parameterTypes, resultType = nextConv.resultType)

    case ErasedSignature.Result(resultType) =>
      for {
        resultTypeConv <- convertErasedSigType(resultType)
      } yield module.ErasedSignature(parameterTypes = Vector.empty, resultType = resultTypeConv)
  }

  def convertErasedSignature(sigOpt: Option[ErasedSignature[context.type]], sig: Comp[ErasedSignature[context.type]]): Comp[module.ErasedSignature] =
    sigOpt.fold(sig)(IO.succeed(_)).flatMap(convertErasedSignature)

  def convertTraitOwner[TPayloadSpec[_, _]: PayloadSpecInfo](owner: TraitOwner[context.type, TPayloadSpec]): Comp[module.TraitOwner] =
    owner match {
      case TraitOwner.ByNamespace(arModule, namespace, name) =>
        emitEnv.getModuleIdNum(arModule).map { moduleIdNum =>
          module.TraitOwner(module.TraitOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertClassOwner[TPayloadSpec[_, _]: PayloadSpecInfo](owner: ClassOwner[context.type, TPayloadSpec]): Comp[module.ClassOwner] =
    owner match {
      case ClassOwner.ByNamespace(arModule, namespace, name) =>
        emitEnv.getModuleIdNum(arModule).map { moduleIdNum =>
          module.ClassOwner(module.ClassOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertDataCtorOwner[TPayloadSpec[_, _]: PayloadSpecInfo](owner: DataConstructorOwner[context.type, TPayloadSpec]): Comp[module.DataConstructorOwner] =
    owner match {
      case DataConstructorOwner.ByNamespace(arModule, namespace, name) =>
        emitEnv.getModuleIdNum(arModule).map { moduleIdNum =>
          module.DataConstructorOwner(module.DataConstructorOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertFuncOwner[TPayloadSpec[_, _]: PayloadSpecInfo](owner: FunctionOwner[context.type, TPayloadSpec]): Comp[module.FunctionOwner] =
    owner match {
      case FunctionOwner.ByNamespace(arModule, namespace, name) =>
        emitEnv.getModuleIdNum(arModule).map { moduleIdNum =>
          module.FunctionOwner(module.FunctionOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertMethodOwner[TPayloadSpec[_, _]: PayloadSpecInfo](owner: MethodOwner[context.type, TPayloadSpec]): Comp[module.MethodOwner] =
    for {
      ownerInfo <- owner match {
        case MethodOwner.ByClass(ownerClass) =>
          emitEnv.classes.getIdNum(ownerClass).map(module.MethodOwner.Owner.ByClass.apply)

        case MethodOwner.ByClassObject(ownerClass) =>
          emitEnv.classes.getIdNum(ownerClass).map(module.MethodOwner.Owner.ByClassObject.apply)

        case MethodOwner.ByTrait(ownerTrait) =>
          emitEnv.traits.getIdNum(ownerTrait).map(module.MethodOwner.Owner.ByTrait.apply)

        case MethodOwner.ByTraitObject(ownerTrait) =>
          emitEnv.traits.getIdNum(ownerTrait).map(module.MethodOwner.Owner.ByTraitObject.apply)

        case MethodOwner.ByDataCtor(dataCtor) =>
          emitEnv.dataConstructors.getIdNum(dataCtor).map(module.MethodOwner.Owner.ByDataConstructor.apply)
      }
    } yield module.MethodOwner(ownerInfo)

  def convertMutability(mutability: Mutability): module.Mutability =
    mutability match {
      case Mutability.Mutable => module.Mutability.Mutable
      case Mutability.NonMutable => module.Mutability.NonMutable
    }

  def convertMethodName(name: MethodName): module.MethodName =
    name match {
      case MemberName.Normal(name) => module.MethodName(module.MethodName.Name.Normal(name))
      case MemberName.Mutator(name) => module.MethodName(module.MethodName.Name.Mutator(name))
      case MemberName.Unnamed => module.MethodName(module.MethodName.Name.SpecialName(module.SpecialMethodName.Unnamed))
      case MemberName.Call => module.MethodName(module.MethodName.Name.SpecialName(module.SpecialMethodName.Call))
    }

  def convertTraitSignature(sig: Signature[ArTrait.ResultInfo, _ <: Nat]): Comp[module.TraitSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        baseTypes <- result.baseTypes
        baseTraits <- baseTypes.baseTraits.traverse(convertTraitType(_))
      } yield module.TraitSignature(
        parameters = params,
        baseTraits = baseTraits,
      )
    }

  def createTraitDefMessage(arTrait: ArTrait[context.type, DeclarationPayloadSpecifier]): Comp[module.TraitDefinition] =
    for {
      sig <- arTrait.signature
      convSig <- convertTraitSignature(sig)

      instMethods <- createMethodMembers(arTrait.methods)
      staticMethods <- createMethodMembers(arTrait.staticMethods)

      convOwner <- convertTraitOwner(arTrait.owner)

    } yield module.TraitDefinition(
      owner = convOwner,
      fileId = convertFileId(arTrait.fileId),
      signature = convSig,
      isSealed = Some(arTrait.isSealed).filter(identity),
      methods = instMethods,
      staticMethods = staticMethods,
    )

  def createTraitRefMessage(arTrait: ArTrait[context.type, ReferencePayloadSpecifier]): Comp[module.TraitReference] =
    for {
      owner <- convertTraitOwner(arTrait.owner)
      sig <- arTrait.signature
      convSig <- convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig))
    } yield module.TraitReference(owner, convSig)

  def createMethodMembers(methods: Comp[Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]]]): Comp[Vector[module.MethodMember]] =
    methods.flatMap {
      _.traverse { method =>
        for {
          methodId <- emitEnv.methods.getIdNum(method.method)
        } yield module.MethodMember(methodId, convertAccessModifier(method.accessModifier))
      }
    }

  def convertClassSignature(sig: Signature[ArClass.ResultInfo, _ <: Nat]): Comp[module.ClassSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        baseTypes <- result.baseTypes
        baseClass <- baseTypes.baseClass.traverse(convertClassType(_))
        baseTraits <- baseTypes.baseTraits.traverse(convertTraitType(_))
      } yield module.ClassSignature(
        parameters = params,
        baseClass = baseClass,
        baseTraits = baseTraits,
      )
    }

  def createClassDefMessage(arClass: ArClass[context.type, DeclarationPayloadSpecifier]): Comp[module.ClassDefinition] =
    for {
      sig <- arClass.signature
      convSig <- convertClassSignature(sig)
      fields <- arClass.fields
      convFields <- fields.traverse { field =>
        convertExpr(field.varType).map { fieldType =>
          module.ClassField(convertMutability(field.mutability), field.name.name, fieldType)
        }
      }

      instMethods <- createMethodMembers(arClass.methods)
      staticMethods <- createMethodMembers(arClass.staticMethods)

      classCtors <- arClass.classConstructors
      ctors <- classCtors.traverse { ctor =>
        emitEnv.classConstructors.getIdNum(ctor.ctor).map { ctorIdNum =>
          module.MethodMember(ctorIdNum, convertAccessModifier(ctor.accessModifier))
        }
      }

      convOwner <- convertClassOwner(arClass.owner)
      _ <- emitEnv.vtableBuilder.fromClass(arClass)

    } yield module.ClassDefinition(
      owner = convOwner,
      fileId = convertFileId(arClass.fileId),
      signature = convSig,
      isOpen = Some(arClass.isOpen).filter(identity),
      isAbstract = Some(arClass.isAbstract).filter(identity),
      isSealed = Some(arClass.isSealed).filter(identity),
      fields = convFields,
      methods = instMethods,
      staticMethods = staticMethods,
      constructors = ctors
    )

  def createClassRefMessage(arClass: ArClass[context.type, ReferencePayloadSpecifier]): Comp[module.ClassReference] =
    for {
      owner <- convertClassOwner(arClass.owner)
      sig <- arClass.signature
      convSig <- convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig))
    } yield module.ClassReference(owner, convSig)

  def convertDataCtorSignature(sig: Signature[DataConstructor.ResultInfo, _ <: Nat]): Comp[module.DataConstructorSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        instanceType <- convertTraitType(result.instanceType)
      } yield module.DataConstructorSignature(
        parameters = params,
        instanceType = instanceType,
      )
    }

  def createDataCtorDefMessage(dataCtor: DataConstructor[context.type, DeclarationPayloadSpecifier]): Comp[module.DataConstructorDefinition] =
    for {
      sig <- dataCtor.signature
      convSig <- convertDataCtorSignature(sig)

      methods <- createMethodMembers(dataCtor.methods)

      convOwner <- convertDataCtorOwner(dataCtor.owner)
      _ <- emitEnv.vtableBuilder.fromDataConstructor(dataCtor)

    } yield module.DataConstructorDefinition(
      owner = convOwner,
      fileId = convertFileId(dataCtor.fileId),
      signature = convSig,
      methods = methods,
    )

  def createDataCtorRefMessage(dataCtor: DataConstructor[context.type, ReferencePayloadSpecifier]): Comp[module.DataConstructorReference] =
    for {
      owner <- convertDataCtorOwner(dataCtor.owner)
      sig <- dataCtor.signature
      convSig <- convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig))
    } yield module.DataConstructorReference(owner, convSig)

  def convertFunctionSignature(sig: Signature[FunctionResultInfo, _ <: Nat]): Comp[module.FunctionSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        returnType <- convertExpr(result.returnType)
      } yield module.FunctionSignature(
        parameters = params,
        returnType = returnType,
      )
    }

  def createFuncDefMessage(func: ArFunc[context.type, DeclarationPayloadSpecifier]): Comp[module.FunctionDefinition] =
    for {
      sig <- func.signature
      convSig <- convertFunctionSignature(sig)

      convOwner <- convertFuncOwner(func.owner)

      funcBody <- emitEnv.options.moduleType match {
        case ModuleEmitOptions.ReferenceModule => IO.none
        case ModuleEmitOptions.DeclarationModule => func.payload.asSome
      }
      convFuncBody <- ZIO.foreach(funcBody)(convertFunctionBody(_))

    } yield module.FunctionDefinition(
      owner = convOwner,
      fileId = convertFileId(func.fileId),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = func.effectInfo.isPure
      ),

      body = convFuncBody,
    )

  def createFuncRefMessage(func: ArFunc[context.type, ReferencePayloadSpecifier]): Comp[module.FunctionReference] =
    for {
      owner <- convertFuncOwner(func.owner)
      sig <- func.signature
      convSig <- convertErasedSignature(ErasedSignature.fromSignature(context)(sig))
    } yield module.FunctionReference(owner, convSig)

  def convertMethodSignature(sig: Signature[FunctionResultInfo, _ <: Nat]): Comp[module.MethodSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        returnType <- convertExpr(result.returnType)
      } yield module.MethodSignature(
        parameters = params,
        returnType = returnType,
      )
    }

  def createMethodDefMessage(method: ArMethod[context.type, DeclarationPayloadSpecifier]): Comp[module.MethodDefinition] =
    for {
      sig <- method.signatureUnsubstituted
      convSig <- convertMethodSignature(sig)

      methodOwner <- convertMethodOwner(method.owner)

      methodBody <- emitEnv.options.moduleType match {
        case ModuleEmitOptions.ReferenceModule => IO.none
        case ModuleEmitOptions.DeclarationModule => method.payload.asSome
      }
      convMethodBody <- methodBody.flatTraverse(convertMethodBody(_))

    } yield module.MethodDefinition(
      owner = methodOwner,
      name = convertMethodName(method.name),
      fileId = convertFileId(method.fileId),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = method.effectInfo.isPure
      ),

      isVirtual = Some(method.isVirtual).filter(identity),
      isAbstract = Some(method.isAbstract).filter(identity),
      isImplicitOverride = Some(method.isImplicitOverride).filter(identity),
      isFinal = Some(method.isFinal).filter(identity),

      body = convMethodBody,
    )

  def createMethodRefMessage(method: ArMethod[context.type, ReferencePayloadSpecifier]): Comp[module.MethodReference] =
    for {
      owner <- convertMethodOwner(method.owner)
      sig <- method.signatureUnsubstituted
      convSig <- convertErasedSignature(ErasedSignature.fromSignature(context)(sig))
    } yield module.MethodReference(owner, convertMethodName(method.name), convSig)

  def convertClassCtorSignature(sig: Signature[ClassConstructor.ResultInfo, _ <: Nat]): Comp[module.ClassConstructorSignature] =
    convertSignature(sig) { (params, _) =>
      module.ClassConstructorSignature(params).pure[Comp]
    }

  def createClassCtorDefMessage(ctor: ClassConstructor[context.type, DeclarationPayloadSpecifier]): Comp[module.ClassConstructorDefinition] =
    for {
      sig <- ctor.signatureUnsubstituted
      convSig <- convertClassCtorSignature(sig)

      ownerClassIdNum <- emitEnv.classConstructors.getIdNum(ctor)

      convCtorBody <- createClassCtorBody(ctor)

    } yield module.ClassConstructorDefinition(
      ownerClass = ownerClassIdNum,
      fileId = convertFileId(ctor.fileId),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = ctor.effectInfo.isPure
      ),
      body = convCtorBody,
    )

  def createClassCtorRefMessage(ctor: ClassConstructor[context.type, ReferencePayloadSpecifier]): Comp[module.ClassConstructorReference] =
    for {
      ownerClassIdNum <- emitEnv.classes.getIdNum(ctor.ownerClass)
      sig <- ctor.signatureUnsubstituted
      convSig <- convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig))
    } yield module.ClassConstructorReference(ownerClassIdNum, convSig)


  def convertClassConstructorExpressionBody(body: ClassConstructorBody[context.type]): Comp[module.ClassConstructorBody] = {

    def convertPreInitStmt(stmt: ClassConstructorStatement[context.type]): Comp[module.PreInitClassConstructorStatement] =
      stmt match {
        case ClassConstructorStatementExpr(expr) => convertExpr(expr).map { convExpr => module.PreInitClassConstructorStatement(expr = convExpr) }
        case InitializeFieldStatement(field, value) =>
          for {
            convField <- convertFieldVariable(field)
            convValue <- convertExpr(value)
          } yield module.PreInitClassConstructorStatement(field = Some(convField), expr = convValue)
      }

    def convertBaseCtorCall(baseCall: ClassConstructorCall[context.type, Id]): Comp[module.BaseClassConstructorCall] =
      for {
        baseCtorIdNum <- emitEnv.classConstructors.getIdNumForAbsRef(baseCall.classCtor)
        baseCtorClassType <- convertClassType(baseCall.classType)
        convArgs <- baseCall.args.traverse(convertExpr(_))
      } yield module.BaseClassConstructorCall(
        baseConstructorId = baseCtorIdNum,
        instanceClassType = baseCtorClassType,
        args = convArgs,
      )


    for {
      preInitStmts <- body.initStatements.traverse(convertPreInitStmt(_))
      baseCall <- ZIO.foreach(body.baseConstructorCall)(convertBaseCtorCall(_))
      endExpr <- convertExpr(body.endExpr)

    } yield module.ClassConstructorBody(module.ClassConstructorBody.BodyType.ExpressionBody(
      module.ClassConstructorExpressionBody(
        preInitStatements = preInitStmts,
        baseConstructorCall = baseCall,
        endExpr = endExpr,
      )
    ))
  }

  def createClassCtorBody(ctor: ClassConstructor[context.type, DeclarationPayloadSpecifier]): Comp[Option[module.ClassConstructorBody]] =
    emitEnv.options.moduleType match {
      case ModuleEmitOptions.ReferenceModule => IO.none
      case ModuleEmitOptions.DeclarationModule =>
        ctor.payload.flatMap(convertClassConstructorBody).asSome
    }

  private def convertFunctionBody(body: context.TFunctionImplementation): Comp[module.FunctionBody] = body match {
    case FunctionImplementation.Extern(_, _) =>
      IO.succeed(module.FunctionBody(module.FunctionBody.BodyType.ExternalImplementation(com.google.protobuf.empty.Empty())))

    case FunctionImplementation.Expression(body) =>
      convertExpr(body).map { expr => module.FunctionBody(module.FunctionBody.BodyType.ExpressionBody(expr)) }
  }

  def convertMethodBody(body: context.TMethodImplementation): Comp[Option[module.FunctionBody]] = body match {
    case MethodImplementation.Abstract => IO.none
    case MethodImplementation.Extern(_, _) =>
      IO.succeed(Some(module.FunctionBody(module.FunctionBody.BodyType.ExternalImplementation(com.google.protobuf.empty.Empty()))))

    case MethodImplementation.Expression(body) =>
      convertExpr(body).map { expr =>
        Some(module.FunctionBody(module.FunctionBody.BodyType.ExpressionBody(expr)))
      }

  }

  def convertClassConstructorBody(body: context.TClassConstructorImplementation): Comp[module.ClassConstructorBody] =
    convertClassConstructorExpressionBody(body.body)
}

private[emitter] object ModuleSerializerElements {
  type Aux[TContext <: Context] = ModuleSerializerElements { val context: TContext }

  def apply(ctx: Context)(emitEnv2: EmitEnv[ctx.type]): ModuleSerializerElements.Aux[ctx.type] = new ModuleSerializerElements {
    override val context: ctx.type = ctx;
    override val emitEnv: EmitEnv[context.type] = emitEnv2
  }
}
