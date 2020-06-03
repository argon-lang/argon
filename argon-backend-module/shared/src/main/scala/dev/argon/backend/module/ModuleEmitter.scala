package dev.argon.backend.module

import dev.argon.compiler.{core, _}
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats._
import cats.data._
import cats.evidence.===
import cats.implicits._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.loaders.armodule.{ModuleFormatVersion, ModulePaths}
import dev.argon.compiler.types.TypeSystem
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
    def getModuleIdNum[TPayloadSpec[_, _]](arModule: ArModule[context.type, TPayloadSpec]): Comp[Option[Int]]

    def getClassIdNum[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): Comp[Int]
    def getTraitIdNum[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): Comp[Int]
    def getDataCtorIdNum[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Comp[Int]
    def getFunctionIdNum[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Comp[Int]
    def getMethodIdNum[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Comp[Int]
    def getClassCtorIdNum[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Comp[Int]

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

              impl(sigParams.nextUnsubstituted, prevParams :+ module.Parameter(style, elems))
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

  def convertExpr(expr: typeSystem.SimpleExpr): Emit[module.Expression] =
    ???

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
      case GlobalName.Unnamed => ???
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

    } yield module.FunctionDefinition(
      owner = convOwner,
      fileId = convertFileId(func.fileId),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = func.effectInfo.isPure
      ),
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

    } yield module.ClassConstructorDefinition(
      ownerClass = ownerClassIdNum,
      fileId = convertFileId(ctor.fileId),
      signature = convSig,
      effects = module.EffectInfo(
        isPure = ctor.effectInfo.isPure
      ),
    )


}

object ModuleEmitter {

  type StreamElem = (String, GeneratedMessage)

  def emitModule(context: ModuleContext)(currentModule: ArModule[context.type, DeclarationPayloadSpecifier]): zstream.Stream[ErrorList, StreamElem] =
    new Source[Any, ErrorList, StreamElem] {

      override def foreach(f: StreamElem => Comp[Unit]): Comp[Unit] = for {
        metadata <- Ref.make(module.Metadata(
          formatVersion = ModuleFormatVersion.currentVersion,
          name = currentModule.id.name,
          isInterfaceModule = Some(true),
        ))

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

        emittedPaths <- Ref.make(Set.empty[String])

        context2: context.type = context
        moduleEmitter = new ModuleEmitter {
          override val context: context2.type = context2
        }

        emitEnv = new moduleEmitter.EmitEnv {

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
                convSig <- moduleEmitter.convertClassSignature(sig).provide(this)
              } yield module.ClassReference(owner, convSig)
            )

          override def getTraitIdNum[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(arTrait)(traitFromCurrentModule)(_.id)(traitIds, traitRefIds)(ModulePaths.traitTypeName)(
              for {
                owner <- moduleEmitter.convertTraitOwner(arTrait.owner).provide(this)
                sig <- arTrait.signature
                convSig <- moduleEmitter.convertTraitSignature(sig).provide(this)
              } yield module.TraitReference(owner, convSig)
            )

          override def getDataCtorIdNum[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(ctor)(dataCtorFromCurrentModule)(_.id)(dataCtorIds, dataCtorRefIds)(ModulePaths.dataCtorTypeName)(
              for {
                owner <- moduleEmitter.convertDataCtorOwner(ctor.owner).provide(this)
                sig <- ctor.signature
                convSig <- moduleEmitter.convertDataCtorSignature(sig).provide(this)
              } yield module.DataConstructorReference(owner, convSig)
            )

          override def getFunctionIdNum[TPayloadSpec[_, _]](func: ArFunc[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(func)(funcFromCurrentModule)(_.id)(functionIds, functionRefIds)(ModulePaths.funcTypeName)(
              for {
                owner <- moduleEmitter.convertFuncOwner(func.owner).provide(this)
                sig <- func.signature
                convSig <- moduleEmitter.convertFunctionSignature(sig).provide(this)
              } yield module.FunctionReference(owner, convSig)
            )

          override def getMethodIdNum[TPayloadSpec[_, _]](method: ArMethod[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(method)(methodFromCurrentModule)(_.id)(methodIds, methodRefIds)(ModulePaths.methodTypeName)(
              for {
                owner <- moduleEmitter.convertMethodOwner(method.owner).provide(this)
                sig <- method.signatureUnsubstituted
                convSig <- moduleEmitter.convertMethodSignature(sig).provide(this)
              } yield module.MethodReference(owner, moduleEmitter.convertMethodName(method.name), convSig)
            )

          override def getClassCtorIdNum[TPayloadSpec[_, _]](ctor: ClassConstructor[context.type, TPayloadSpec]): Comp[Int] =
            getDefOrRefId(ctor)(classCtorFromCurrentModule)(_.id)(classCtorIds, classCtorRefIds)(ModulePaths.classCtorTypeName)(
              for {
                ownerClassIdNum <- getClassIdNum(ctor.ownerClass)
                sig <- ctor.signatureUnsubstituted
                convSig <- moduleEmitter.convertClassCtorSignature(sig).provide(this)
              } yield module.ClassConstructorReference(ownerClassIdNum, convSig)
            )

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
