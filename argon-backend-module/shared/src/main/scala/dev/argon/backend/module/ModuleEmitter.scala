package dev.argon.backend.module

import dev.argon.compiler.{core, _}
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats.{Id => _, _}
import cats.data._
import cats.evidence.===
import cats.implicits._
import com.google.protobuf.ByteString
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.loaders.armodule.{ModuleFormatVersion, ModulePaths}
import dev.argon.compiler.types.TypeSystem
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.compiler.vtable.VTableBuilder.Aux
import dev.argon.util.{FileID, NamespacePath}
import dev.argon.module
import dev.argon.module.Metadata
import dev.argon.stream.builder.Source
import shapeless._
import dev.argon.util.AnyExtensions._
import shapeless.ops.nat.{LT, Pred}
import zio.{IO, Ref, UIO, URIO, ZIO, stream => zstream}
import zio.interop.catz.core._

sealed abstract class ModuleEmitter private() {

  val context: ModuleContext

  import context.typeSystem
  import context.signatureContext.{ Signature, SignatureResult, SignatureParameters, SignatureVisitor }

  trait EmitEnv {
    val options: ModuleEmitOptions
    val vtableBuilder: VTableBuilder.Aux[context.type]

    def getModuleIdNum[TPayloadSpec[_, _]](arModule: ArModule[context.type, TPayloadSpec]): Comp[Option[Int]]

    def getClassIdNum[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): Comp[Int]
    def getTraitIdNum[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): Comp[Int]
    def getDataCtorIdNum[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Comp[Int]
    def getFunctionIdNum[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Comp[Int]
    def getMethodIdNum[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Comp[Int]
    def getClassCtorIdNum[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Comp[Int]

    def getLocalVariableIdNum[TPayloadSpec[_, _]](variable: LocalVariable[context.type, Id]): Comp[Int]

    def updateMetadataElement(f: module.Metadata => module.Metadata): UIO[Unit]
    def produceStreamElement[R](path: String)(element: => RComp[R, GeneratedMessage]): RComp[R, Unit]
  }

  type Emit[A] = RComp[EmitEnv, A]

  def addGlobalDeclaration(lens: scalapb.lenses.Lens[module.Metadata, module.Metadata] => scalapb.lenses.Lens[module.Metadata, Vector[module.GlobalDeclaration]])(declaration: module.GlobalDeclaration): URIO[EmitEnv, Unit] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      emitEnv.updateMetadataElement { metadata =>
        metadata.update { metadataLens => lens(metadataLens).modify(_ :+ declaration) }
      }
    }

  def produceGlobalElement[TElem[_ <: Context with Singleton, _[_, _]]]
  (accessor: EmitEnv => TElem[context.type, DeclarationPayloadSpecifier] => Comp[Int])
  (elem: TElem[context.type, DeclarationPayloadSpecifier])
  (accessModifier: AccessModifierGlobal)
  (metadataLens: scalapb.lenses.Lens[module.Metadata, module.Metadata] => scalapb.lenses.Lens[module.Metadata, Vector[module.GlobalDeclaration]])
  (pathTypeName: String)
  (message: TElem[context.type, DeclarationPayloadSpecifier] => Emit[GeneratedMessage])
  : Emit[Unit] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      accessor(emitEnv)(elem).flatMap { idNum =>
        val path = ModulePaths.elementDef(pathTypeName, idNum)
        emitEnv.produceStreamElement(path)(message(elem)) *>
          addGlobalDeclaration(metadataLens)(module.GlobalDeclaration(idNum, convertAccessModifier(accessModifier)))
      }
    }

  def produceElement[TElem[_ <: Context with Singleton, _[_, _]]]
  (accessor: EmitEnv => TElem[context.type, DeclarationPayloadSpecifier] => Comp[Int])
  (elem: TElem[context.type, DeclarationPayloadSpecifier])
  (pathTypeName: String)
  (message: TElem[context.type, DeclarationPayloadSpecifier] => Emit[GeneratedMessage])
  : Emit[Unit] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      accessor(emitEnv)(elem).flatMap { idNum =>
        val path = ModulePaths.elementDef(pathTypeName, idNum)
        emitEnv.produceStreamElement(path)(message(elem))
      }
    }

  def processModule(armodule: ArModule[context.type, DeclarationPayloadSpecifier]): Emit[Unit] = {

    def processNamespace(ns: Namespace[context.type, DeclarationPayloadSpecifier]): Emit[Unit] =
      ZIO.foreach_(ns.bindings) {
        case GlobalBinding.NestedNamespace(_, nestedNS) => processNamespace(nestedNS)
        case GlobalBinding.GlobalTrait(_, access, arTrait) =>
          for {
            _ <- produceGlobalElement[ArTrait](_.getTraitIdNum)(arTrait)(access)(_.globalTraits)(ModulePaths.traitTypeName)(createTraitDefMessage)

            instMethods <- arTrait.methods
            _ <- processMethods(instMethods)
          } yield ()

        case GlobalBinding.GlobalClass(_, access, arClass) =>
          for {
            _ <- produceGlobalElement[ArClass](_.getClassIdNum)(arClass)(access)(_.globalClasses)(ModulePaths.classTypeName)(createClassDefMessage)

            instMethods <- arClass.methods
            _ <- processMethods(instMethods)

            staticMethods <- arClass.staticMethods
            _ <- processMethods(staticMethods)

            ctors <- arClass.classConstructors
            _ <- processClassCtors(ctors)
          } yield ()

        case GlobalBinding.GlobalDataConstructor(_, access, dataCtor) =>
          for {
            _ <- produceGlobalElement[DataConstructor](_.getDataCtorIdNum)(dataCtor)(access)(_.globalDataConstructors)(ModulePaths.dataCtorTypeName)(createDataCtorDefMessage)

            instMethods <- dataCtor.methods
            instEntries <- processMethods(instMethods)
          } yield instEntries

        case GlobalBinding.GlobalFunction(_, access, func) =>
          produceGlobalElement[ArFunc](_.getFunctionIdNum)(func)(access)(_.globalFunctions)(ModulePaths.funcTypeName)(createFuncDefMessage)

      }

    def processMethods(methods: Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]]): Emit[Unit] =
      methods.traverse_ {
        case MethodBinding(_, method) =>
          produceElement[ArMethod](_.getMethodIdNum)(method)(ModulePaths.methodTypeName)(createMethodDefMessage)
      }

    def processClassCtors(ctors: Vector[ClassConstructorBinding[context.type, DeclarationPayloadSpecifier]]): Emit[Unit] =
      ctors.traverse_ {
        case ClassConstructorBinding(_, ctor) =>
          produceElement[ClassConstructor](_.getClassCtorIdNum)(ctor)(ModulePaths.classCtorTypeName)(createClassCtorDefMessage)
      }


    armodule.globalNamespace
      .flatMap(processNamespace(_))
  }


  def convertSignature[TResult[_ <: Context with Singleton, Wrap[+_]], A]
  (sig: Signature[TResult, _ <: Nat])
  (f: (Vector[module.Parameter], TResult[context.type, context.typeSystem.TTypeWrapper]) => Emit[A])
  : Emit[A] = {

    def impl[Len <: Nat](sig: Signature[TResult, Len], prevParams: Vector[module.Parameter]): Emit[A] =
      sig.visit(new SignatureVisitor[TResult, Len, Emit[A]] {
        override def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): Emit[A] =
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

        override def visitResult(sigResult: SignatureResult[TResult])(implicit lenEq: Len === _0): Emit[A] =
          f(prevParams, sigResult.result)

      })

    impl(sig, Vector.empty)
  }

  def convertClassType(classType: typeSystem.TClassType): Emit[module.ClassType] = for {
    id <- ZIO.accessM[EmitEnv](_.getClassIdNum(classType.arClass.value))
    args <- classType.args.traverse(convertExpr(_))
  } yield module.ClassType(id, args)

  def convertTraitType(traitType: typeSystem.TTraitType): Emit[module.TraitType] = for {
    id <- ZIO.accessM[EmitEnv](_.getTraitIdNum(traitType.arTrait.value))
    args <- traitType.args.traverse(convertExpr(_))
  } yield module.TraitType(id, args)

  def convertDataCtorType(dataCtorType: typeSystem.TDataConstructorType): Emit[module.DataConstructorType] = for {
    id <- ZIO.accessM[EmitEnv](_.getDataCtorIdNum(dataCtorType.ctor.value))
    args <- dataCtorType.args.traverse(convertExpr(_))
  } yield module.DataConstructorType(id, args)

  def convertVariableName(name: VariableName): module.VariableName = name match {
    case VariableName.Normal(name) => module.VariableName(module.VariableName.Name.Normal(name))
    case VariableName.Unnamed => module.VariableName(module.VariableName.Name.Unnamed(com.google.protobuf.empty.Empty()))
  }

  def convertFieldVariable(variable: FieldVariable[context.type, Id]): Emit[module.FieldVariable] =
    for {
      ownerId <- ZIO.accessM[EmitEnv](_.getClassIdNum(variable.owner.ownerClass.value))
      convVarType <- convertExpr(variable.varType)
    } yield module.FieldVariable(
      ownerClass = ownerId,
      name = convertVariableName(variable.name),
      mutability = convertMutability(variable.mutability),
      varType = convVarType,
    )

  def convertExpr(expr: typeSystem.SimpleExpr): Emit[module.Expression] = {
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

    def convertLocalVariable(variable: LocalVariable[context.type, Id]): Emit[module.LocalVariable] =
      for {
        varIdNum <- ZIO.accessM[EmitEnv](_.getLocalVariableIdNum(variable))

        ownerId <- variable.owner match {
          case LocalVariableOwner.ByClass(ownerClass) =>
            ZIO.accessM[EmitEnv](_.getClassIdNum(ownerClass.value))
              .map { module.LocalVariableOwner.Owner.ByClass(_) }

          case LocalVariableOwner.ByTrait(ownerTrait) =>
            ZIO.accessM[EmitEnv](_.getTraitIdNum(ownerTrait.value))
              .map { module.LocalVariableOwner.Owner.ByTrait(_) }

          case LocalVariableOwner.ByDataConstructor(ownerCtor) =>
            ZIO.accessM[EmitEnv](_.getDataCtorIdNum(ownerCtor.value))
              .map { module.LocalVariableOwner.Owner.ByDataConstructor(_) }

          case LocalVariableOwner.ByFunction(ownerFunc) =>
            ZIO.accessM[EmitEnv](_.getFunctionIdNum(ownerFunc.value))
              .map { module.LocalVariableOwner.Owner.ByFunction(_) }

          case LocalVariableOwner.ByMethod(ownerMethod) =>
            ZIO.accessM[EmitEnv](_.getMethodIdNum(ownerMethod.value))
              .map { module.LocalVariableOwner.Owner.ByMethod(_) }

          case LocalVariableOwner.ByClassConstructor(ownerCtor) =>
            ZIO.accessM[EmitEnv](_.getClassCtorIdNum(ownerCtor.value))
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

    def convertVariable(variable: Variable[context.type, Id]): Emit[module.Variable] =
      variable match {
        case variable: LocalVariable[context.type, Id] =>
          convertLocalVariable(variable).map { convVar =>
            module.Variable(module.Variable.VariableType.Local(convVar))
          }

        case ParameterVariable(owner, index, name, mutability, isErased, varType) =>
          for {
            ownerId <- owner match {
              case ParameterVariableOwner.ByClass(ownerClass) =>
                ZIO.accessM[EmitEnv](_.getClassIdNum(ownerClass.value))
                  .map { module.ParameterVariableOwner.Owner.ByClass(_) }

              case ParameterVariableOwner.ByTrait(ownerTrait) =>
                ZIO.accessM[EmitEnv](_.getTraitIdNum(ownerTrait.value))
                  .map { module.ParameterVariableOwner.Owner.ByTrait(_) }

              case ParameterVariableOwner.ByDataConstructor(ownerCtor) =>
                ZIO.accessM[EmitEnv](_.getDataCtorIdNum(ownerCtor.value))
                  .map { module.ParameterVariableOwner.Owner.ByDataConstructor(_) }

              case ParameterVariableOwner.ByFunction(ownerFunc) =>
                ZIO.accessM[EmitEnv](_.getFunctionIdNum(ownerFunc.value))
                  .map { module.ParameterVariableOwner.Owner.ByFunction(_) }

              case ParameterVariableOwner.ByMethod(ownerMethod) =>
                ZIO.accessM[EmitEnv](_.getMethodIdNum(ownerMethod.value))
                  .map { module.ParameterVariableOwner.Owner.ByMethod(_) }

              case ParameterVariableOwner.ByClassConstructor(ownerCtor) =>
                ZIO.accessM[EmitEnv](_.getClassCtorIdNum(ownerCtor.value))
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

        case ThisParameterVariable(owner, name, mutability, varType) => ???

        case variable: FieldVariable[context.type, Id] =>
          convertFieldVariable(variable).map { convVar =>
            module.Variable(module.Variable.VariableType.Field(convVar))
          }



      }

    def impl(expr: typeSystem.SimpleExpr): Emit[(module.Expression.ExprType, Vector[module.Expression])] =
      expr match {
        case TraitType(arTrait, args) =>
          for {
            idNum <- ZIO.accessM[EmitEnv](_.getTraitIdNum(arTrait.value))
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.TraitType(idNum), convArgs)

        case ClassType(arClass, args) =>
          for {
            idNum <- ZIO.accessM[EmitEnv](_.getClassIdNum(arClass.value))
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.ClassType(idNum), convArgs)

        case DataConstructorType(ctor, args, _) =>
          for {
            idNum <- ZIO.accessM[EmitEnv](_.getDataCtorIdNum(ctor.value))
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
            idNum <- ZIO.accessM[EmitEnv](_.getClassCtorIdNum(classCtor.value))
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
            idNum <- ZIO.accessM[EmitEnv](_.getFunctionIdNum(function.value))
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
            idNum <- ZIO.accessM[EmitEnv](_.getMethodIdNum(method.value))
            convInstance <- convertExpr(instance)
            convInstanceType <- convertExpr(instanceType)
            convArgs <- args.traverse(convertExpr(_))
          } yield (module.Expression.ExprType.MethodCall(idNum), convInstance +: convInstanceType +: convArgs)

        case PatternMatch(expr, cases) =>
          def convertPattern(pattern: PatternExpr[context.type, Id]): Emit[module.PatternExpr] = ???

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

  def convertErasedSigType(sigType: ErasedSignature.SigType[context.type]): Emit[module.SigType] =
    (sigType match {
      case ErasedSignature.BlankType() => IO.succeed(module.SigType.SigType._Empty(com.google.protobuf.empty.Empty()))
      case ErasedSignature.TraitType(arTrait, typeArgs) =>
        for {
          idNum <- ZIO.accessM[EmitEnv](_.getTraitIdNum(arTrait.value))
          args <- typeArgs.traverse(convertErasedSigType)
        } yield module.SigType.SigType.TraitType(module.SigTypeTrait(idNum, args))

      case ErasedSignature.ClassType(arClass, typeArgs) =>
        for {
          idNum <- ZIO.accessM[EmitEnv](_.getClassIdNum(arClass.value))
          args <- typeArgs.traverse(convertErasedSigType)
        } yield module.SigType.SigType.ClassType(module.SigTypeClass(idNum, args))

      case ErasedSignature.DataConstructorType(ctor, typeArgs) =>
        for {
          idNum <- ZIO.accessM[EmitEnv](_.getDataCtorIdNum(ctor.value))
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

  def convertErasedSignatureParameterOnly(sig: ErasedSignature.ParameterOnlySignature[context.type]): Emit[module.ErasedSignatureParameterOnly] =
    sig.paramTypes.traverse(convertErasedSigType)
      .map(module.ErasedSignatureParameterOnly.apply)

  def convertErasedSignature(sig: ErasedSignature[context.type]): Emit[module.ErasedSignature] = sig match {
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

  def convertTraitOwner[TPayloadSpec[_, _]](owner: TraitOwner[context.type, TPayloadSpec]): Emit[module.TraitOwner] =
    owner match {
      case TraitOwner.ByNamespace(arModule, namespace, name) =>
        ZIO.accessM[EmitEnv](_.getModuleIdNum(arModule)).map { moduleIdNum =>
          module.TraitOwner(module.TraitOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertClassOwner[TPayloadSpec[_, _]](owner: ClassOwner[context.type, TPayloadSpec]): Emit[module.ClassOwner] =
    owner match {
      case ClassOwner.ByNamespace(arModule, namespace, name) =>
        ZIO.accessM[EmitEnv](_.getModuleIdNum(arModule)).map { moduleIdNum =>
          module.ClassOwner(module.ClassOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertDataCtorOwner[TPayloadSpec[_, _]](owner: DataConstructorOwner[context.type, TPayloadSpec]): Emit[module.DataConstructorOwner] =
    owner match {
      case DataConstructorOwner.ByNamespace(arModule, namespace, name) =>
        ZIO.accessM[EmitEnv](_.getModuleIdNum(arModule)).map { moduleIdNum =>
          module.DataConstructorOwner(module.DataConstructorOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertFuncOwner[TPayloadSpec[_, _]](owner: FunctionOwner[context.type, TPayloadSpec]): Emit[module.FunctionOwner] =
    owner match {
      case FunctionOwner.ByNamespace(arModule, namespace, name) =>
        ZIO.accessM[EmitEnv](_.getModuleIdNum(arModule)).map { moduleIdNum =>
          module.FunctionOwner(module.FunctionOwner.Owner.InNamespace(
            module.ByNamespaceOwner(
              module = moduleIdNum,
              ns = convertNamespace(namespace),
              name = convertGlobalName(name),
            )
          ))
        }
    }

  def convertMethodOwner[TPayloadSpec[_, _]](owner: MethodOwner[context.type, TPayloadSpec]): Emit[module.MethodOwner] =
    for {
      ownerInfo <- owner match {
        case MethodOwner.ByClass(ownerClass) =>
          ZIO.accessM[EmitEnv](_.getClassIdNum(ownerClass)).map(module.MethodOwner.Owner.ByClass.apply)

        case MethodOwner.ByClassObject(ownerClass) =>
          ZIO.accessM[EmitEnv](_.getClassIdNum(ownerClass)).map(module.MethodOwner.Owner.ByClassObject.apply)

        case MethodOwner.ByTrait(ownerTrait) =>
          ZIO.accessM[EmitEnv](_.getTraitIdNum(ownerTrait)).map(module.MethodOwner.Owner.ByTrait.apply)

        case MethodOwner.ByTraitObject(ownerTrait) =>
          ZIO.accessM[EmitEnv](_.getTraitIdNum(ownerTrait)).map(module.MethodOwner.Owner.ByTraitObject.apply)

        case MethodOwner.ByDataCtor(dataCtor) =>
          ZIO.accessM[EmitEnv](_.getDataCtorIdNum(dataCtor)).map(module.MethodOwner.Owner.ByDataConstructor.apply)
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

  def convertTraitSignature(sig: Signature[ArTrait.ResultInfo, _ <: Nat]): Emit[module.TraitSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        baseTypes <- result.baseTypes
        baseTraits <- baseTypes.baseTraits.traverse(convertTraitType(_))
      } yield module.TraitSignature(
        parameters = params,
        baseTraits = baseTraits,
      )
    }

  def createTraitDefMessage(arTrait: ArTrait[context.type, DeclarationPayloadSpecifier]): Emit[module.TraitDefinition] =
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

  def createMethodMembers(methods: Comp[Vector[MethodBinding[context.type, DeclarationPayloadSpecifier]]]): Emit[Vector[module.MethodMember]] =
    methods.flatMap {
      _.traverse { method =>
        for {
          methodId <- ZIO.accessM[EmitEnv](_.getMethodIdNum(method.method))
        } yield module.MethodMember(methodId, convertAccessModifier(method.accessModifier))
      }
    }

  def convertClassSignature(sig: Signature[ArClass.ResultInfo, _ <: Nat]): Emit[module.ClassSignature] =
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

  def createClassDefMessage(arClass: ArClass[context.type, DeclarationPayloadSpecifier]): Emit[module.ClassDefinition] =
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
        ZIO.accessM[EmitEnv](_.getClassCtorIdNum(ctor.ctor)).map { ctorIdNum =>
          module.MethodMember(ctorIdNum, convertAccessModifier(ctor.accessModifier))
        }
      }

      convOwner <- convertClassOwner(arClass.owner)
      _ <- ZIO.accessM[EmitEnv](_.vtableBuilder.fromClass(arClass))

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

  def convertDataCtorSignature(sig: Signature[DataConstructor.ResultInfo, _ <: Nat]): Emit[module.DataConstructorSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        instanceType <- convertTraitType(result.instanceType)
      } yield module.DataConstructorSignature(
        parameters = params,
        instanceType = instanceType,
      )
    }

  def createDataCtorDefMessage(dataCtor: DataConstructor[context.type, DeclarationPayloadSpecifier]): Emit[module.DataConstructorDefinition] =
    for {
      sig <- dataCtor.signature
      convSig <- convertDataCtorSignature(sig)

      methods <- createMethodMembers(dataCtor.methods)

      convOwner <- convertDataCtorOwner(dataCtor.owner)
      _ <- ZIO.accessM[EmitEnv](_.vtableBuilder.fromDataConstructor(dataCtor))

    } yield module.DataConstructorDefinition(
      owner = convOwner,
      fileId = convertFileId(dataCtor.fileId),
      signature = convSig,
      methods = methods,
    )

  def convertFunctionSignature(sig: Signature[FunctionResultInfo, _ <: Nat]): Emit[module.FunctionSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        returnType <- convertExpr(result.returnType)
      } yield module.FunctionSignature(
        parameters = params,
        returnType = returnType,
      )
    }

  def createFuncDefMessage(func: ArFunc[context.type, DeclarationPayloadSpecifier]): Emit[module.FunctionDefinition] =
    for {
      sig <- func.signature
      convSig <- convertFunctionSignature(sig)

      convOwner <- convertFuncOwner(func.owner)

      funcBody <- ZIO.accessM[EmitEnv](_.options.moduleType match {
        case ModuleEmitOptions.ReferenceModule => IO.succeed(None)
        case ModuleEmitOptions.DeclarationModule => func.payload.map(Some.apply)
      })
      convFuncBody <- ZIO.foreach(funcBody)(convertExpr(_))

    } yield module.FunctionDefinition(
      owner = convOwner,
      fileId = convertFileId(func.fileId),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = func.effectInfo.isPure
      ),

      body = convFuncBody.map { expr => module.FunctionBody(module.FunctionBody.BodyType.ExpressionBody(expr)) },
    )

  def convertMethodSignature(sig: Signature[FunctionResultInfo, _ <: Nat]): Emit[module.MethodSignature] =
    convertSignature(sig) { (params, result) =>
      for {
        returnType <- convertExpr(result.returnType)
      } yield module.MethodSignature(
        parameters = params,
        returnType = returnType,
      )
    }

  def createMethodDefMessage(method: ArMethod[context.type, DeclarationPayloadSpecifier]): Emit[module.MethodDefinition] =
    for {
      sig <- method.signatureUnsubstituted
      convSig <- convertMethodSignature(sig)

      methodOwner <- convertMethodOwner(method.owner)

      methodBody <- ZIO.accessM[EmitEnv](_.options.moduleType match {
        case ModuleEmitOptions.ReferenceModule => IO.succeed(None)
        case ModuleEmitOptions.DeclarationModule => method.payload
      })
      convMethodBody <- ZIO.foreach(methodBody)(convertExpr(_))

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

      body = convMethodBody.map { expr => module.FunctionBody(module.FunctionBody.BodyType.ExpressionBody(expr)) },
    )

  def convertClassCtorSignature(sig: Signature[ClassConstructor.ResultInfo, _ <: Nat]): Emit[module.ClassConstructorSignature] =
    convertSignature(sig) { (params, _) =>
      module.ClassConstructorSignature(params).pure[Emit]
    }

  def createClassCtorDefMessage(ctor: ClassConstructor[context.type, DeclarationPayloadSpecifier]): Emit[module.ClassConstructorDefinition] =
    for {
      sig <- ctor.signatureUnsubstituted
      convSig <- convertClassCtorSignature(sig)

      ownerClassIdNum <- ZIO.accessM[EmitEnv](_.getClassCtorIdNum(ctor))

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

  def createClassCtorBody(ctor: ClassConstructor[context.type, DeclarationPayloadSpecifier]): Emit[Option[module.ClassConstructorBody]] =
    ZIO.accessM[EmitEnv](_.options.moduleType match {
      case ModuleEmitOptions.ReferenceModule => IO.succeed(None)
      case ModuleEmitOptions.DeclarationModule =>
        def convertPreInitStmt(stmt: ClassConstructorStatement[context.type]): Emit[module.PreInitClassConstructorStatement] =
          stmt match {
            case ClassConstructorStatementExpr(expr) => convertExpr(expr).map { convExpr => module.PreInitClassConstructorStatement(expr = convExpr) }
            case InitializeFieldStatement(field, value) =>
              for {
                convField <- convertFieldVariable(field)
                convValue <- convertExpr(value)
              } yield module.PreInitClassConstructorStatement(field = Some(convField), expr = convValue)
          }

        def convertBaseCtorCall(baseCall: ClassConstructorCall[context.type, Id]): Emit[module.BaseClassConstructorCall] =
          for {
            baseCtorIdNum <- ZIO.accessM[EmitEnv](_.getClassCtorIdNum(baseCall.classCtor.value))
            baseCtorClassType <- convertClassType(baseCall.classType)
            convArgs <- baseCall.args.traverse(convertExpr(_))
          } yield module.BaseClassConstructorCall(
            baseConstructorId = baseCtorIdNum,
            instanceClassType = baseCtorClassType,
            args = convArgs,
          )

        for {
          body <- ctor.payload

          preInitStmts <- body.initStatements.traverse(convertPreInitStmt(_))
          baseCall <- ZIO.foreach(body.baseConstructorCall)(convertBaseCtorCall(_))
          endExpr <- convertExpr(body.endExpr)

        } yield Some(module.ClassConstructorBody(module.ClassConstructorBody.BodyType.ExpressionBody(
          module.ClassConstructorExpressionBody(
            preInitStatements = preInitStmts,
            baseConstructorCall = baseCall,
            endExpr = endExpr,
          )
        )))
    })
}

object ModuleEmitter {

  type StreamElem = (String, GeneratedMessage)

  def emitModule(context: ModuleContext)(emitOptions: ModuleEmitOptions)(currentModule: ArModule[context.type, DeclarationPayloadSpecifier]): zstream.Stream[ErrorList, StreamElem] =
    new Source[Any, ErrorList, StreamElem] {

      override def foreach(f: StreamElem => Comp[Unit]): Comp[Unit] = for {
        metadata <- Ref.make(module.Metadata(
          formatVersion = ModuleFormatVersion.currentVersion,
          name = currentModule.id.name,
          moduleType = emitOptions.moduleType match {
            case ModuleEmitOptions.ReferenceModule => module.ModuleType.ReferenceModule
            case ModuleEmitOptions.DeclarationModule => module.ModuleType.DefinitionModule
          },
        ))

        vtableBuilderObj <- VTableBuilder(context)

        moduleIds <- Ref.make(IdentifierState.initial[ModuleId])

        classIds <- Ref.make(IdentifierState.initial[ClassId])
        traitIds <- Ref.make(IdentifierState.initial[TraitId])
        dataCtorIds <- Ref.make(IdentifierState.initial[DataConstructorId])
        functionIds <- Ref.make(IdentifierState.initial[FunctionId])
        methodIds <- Ref.make(IdentifierState.initial[MethodId])
        classCtorIds <- Ref.make(IdentifierState.initial[ClassConstructorId])

        classRefIds <- Ref.make(IdentifierState.initial[ClassId])
        traitRefIds <- Ref.make(IdentifierState.initial[TraitId])
        dataCtorRefIds <- Ref.make(IdentifierState.initial[DataConstructorId])
        functionRefIds <- Ref.make(IdentifierState.initial[FunctionId])
        methodRefIds <- Ref.make(IdentifierState.initial[MethodId])
        classCtorRefIds <- Ref.make(IdentifierState.initial[ClassConstructorId])

        localVarIds <- Ref.make(IdentifierState.initial[LocalVariableId])

        emittedPaths <- Ref.make(Set.empty[String])

        context2: context.type = context
        moduleEmitter = new ModuleEmitter {
          override val context: context2.type = context2
        }

        emitEnv = new moduleEmitter.EmitEnv {

          override val options: ModuleEmitOptions = emitOptions

          override val vtableBuilder: Aux[context.type] = vtableBuilderObj

          private def isCurrentModule[TPayloadSpec[_, _]](arModule: ArModule[context.type, TPayloadSpec]): Boolean =
            arModule.id === currentModule.id

          private def classFromCurrentModule[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): Boolean =
            arClass.owner match {
              case ClassOwner.ByNamespace(arModule, _, _) => isCurrentModule(arModule)
            }

          private def traitFromCurrentModule[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): Boolean =
            arTrait.owner match {
              case TraitOwner.ByNamespace(arModule, _, _) => isCurrentModule(arModule)
            }

          private def dataCtorFromCurrentModule[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Boolean =
            ctor.owner match {
              case DataConstructorOwner.ByNamespace(arModule, _, _) => isCurrentModule(arModule)
            }

          private def funcFromCurrentModule[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Boolean =
            func.owner match {
              case FunctionOwner.ByNamespace(arModule, _, _) => isCurrentModule(arModule)
            }

          private def methodFromCurrentModule[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Boolean =
            method.owner match {
              case MethodOwner.ByClass(ownerClass) => classFromCurrentModule(ownerClass)
              case MethodOwner.ByClassObject(ownerClass) => classFromCurrentModule(ownerClass)
              case MethodOwner.ByTrait(ownerTrait) => traitFromCurrentModule(ownerTrait)
              case MethodOwner.ByTraitObject(ownerTrait) => traitFromCurrentModule(ownerTrait)
              case MethodOwner.ByDataCtor(dataCtor) => dataCtorFromCurrentModule(dataCtor)
            }

          private def classCtorFromCurrentModule[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Boolean =
            classFromCurrentModule(ctor.ownerClass)


          private def getDefOrRefId[TElem, ID]
          (elem: TElem)
          (fromCurrentModule: TElem => Boolean)
          (id: TElem => ID)
          (defIds: Ref[IdentifierState[ID]], refIds: Ref[IdentifierState[ID]])
          (elemTypeName: String)
          (createReference: => Comp[GeneratedMessage])
          : Comp[Int] =
            if(fromCurrentModule(elem))
              getElementIdNum(defIds)(id(elem))
            else
              for {
                idNum <- getElementIdNum(refIds)(id(elem))
                _ <- produceStreamElement(ModulePaths.elementRef(elemTypeName, idNum))(createReference)
              } yield idNum


          override def getModuleIdNum[TPayloadSpec[_, _]](arModule: ArModule[context.type, TPayloadSpec]): Comp[Option[Int]] =
            if(isCurrentModule(arModule))
              IO.succeed(None)
            else
              getElementIdNum(moduleIds)(arModule.id).map(Some.apply)

          override def getClassIdNum[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(arClass)(classFromCurrentModule)(_.id)(classIds, classRefIds)(ModulePaths.classTypeName)(
              for {
                owner <- moduleEmitter.convertClassOwner(arClass.owner).provide(this)
                sig <- arClass.signature
                convSig <- moduleEmitter.convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig)).provide(this)
              } yield module.ClassReference(owner, convSig)
            )

          override def getTraitIdNum[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(arTrait)(traitFromCurrentModule)(_.id)(traitIds, traitRefIds)(ModulePaths.traitTypeName)(
              for {
                owner <- moduleEmitter.convertTraitOwner(arTrait.owner).provide(this)
                sig <- arTrait.signature
                convSig <- moduleEmitter.convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig)).provide(this)
              } yield module.TraitReference(owner, convSig)
            )

          override def getDataCtorIdNum[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(ctor)(dataCtorFromCurrentModule)(_.id)(dataCtorIds, dataCtorRefIds)(ModulePaths.dataCtorTypeName)(
              for {
                owner <- moduleEmitter.convertDataCtorOwner(ctor.owner).provide(this)
                sig <- ctor.signature
                convSig <- moduleEmitter.convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig)).provide(this)
              } yield module.DataConstructorReference(owner, convSig)
            )

          override def getFunctionIdNum[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(func)(funcFromCurrentModule)(_.id)(functionIds, functionRefIds)(ModulePaths.funcTypeName)(
              for {
                owner <- moduleEmitter.convertFuncOwner(func.owner).provide(this)
                sig <- func.signature
                convSig <- moduleEmitter.convertErasedSignature(ErasedSignature.fromSignature(context)(sig)).provide(this)
              } yield module.FunctionReference(owner, convSig)
            )

          override def getMethodIdNum[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(method)(methodFromCurrentModule)(_.id)(methodIds, methodRefIds)(ModulePaths.methodTypeName)(
              for {
                owner <- moduleEmitter.convertMethodOwner(method.owner).provide(this)
                sig <- method.signatureUnsubstituted
                convSig <- moduleEmitter.convertErasedSignature(ErasedSignature.fromSignature(context)(sig)).provide(this)
              } yield module.MethodReference(owner, moduleEmitter.convertMethodName(method.name), convSig)
            )

          override def getClassCtorIdNum[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(ctor)(classCtorFromCurrentModule)(_.id)(classCtorIds, classCtorRefIds)(ModulePaths.classCtorTypeName)(
              for {
                ownerClassIdNum <- getClassIdNum(ctor.ownerClass)
                sig <- ctor.signatureUnsubstituted
                convSig <- moduleEmitter.convertErasedSignatureParameterOnly(ErasedSignature.fromSignatureParameters(context)(sig)).provide(this)
              } yield module.ClassConstructorReference(ownerClassIdNum, convSig)
            )

          override def getLocalVariableIdNum[TPayloadSpec[_, _]](variable: LocalVariable[moduleEmitter.context.type, Id]): Comp[Int] =
            getElementIdNum(localVarIds)(variable.id)

          override def updateMetadataElement(f: Metadata => Metadata): UIO[Unit] =
            metadata.update(f)

          override def produceStreamElement[R](path: String)(element: => RComp[R, GeneratedMessage]): RComp[R, Unit] =
            emittedPaths.modifySome(false) {
              case emPaths if !emPaths.contains(path) =>
                (true, emPaths + path)
            }
              .flatMap {
                case true =>
                  element.flatMap { msg =>
                    f((path, msg))
                  }

                case false =>
                  IO.unit
              }
        }

        _ <- moduleEmitter.processModule(currentModule).provide(emitEnv)
        metadataValue <- metadata.get
        _ <- f(ModulePaths.metadata -> metadataValue)
      } yield ()

    }.toZStream



  private[this] final case class IdentifierState[ID](nextIdNum: Int, idMap: Map[ID, Int])
  private[this] object IdentifierState {
    def initial[ID]: IdentifierState[ID] = IdentifierState(1, Map.empty[ID, Int])
  }

  private[this] def getElementIdNum[ID](ref: Ref[IdentifierState[ID]])(id: ID): UIO[Int] =
    ref.modify { case idState @ IdentifierState(nextIdNum, idMap) =>
      idMap.get(id) match {
        case Some(idNum) => (idNum, idState)
        case None =>
          (nextIdNum, IdentifierState(nextIdNum + 1, idMap + (id -> nextIdNum)))
      }
    }

}
