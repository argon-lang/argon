package dev.argon.armodule.loader

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.evidence.Is
import dev.argon.compiler.{core, _}
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.compiler.loaders.StandardTypeLoaders
import dev.argon.compiler.lookup.ModuleLookup
import dev.argon.module.GlobalDeclarationElement
import dev.argon.util._
import dev.argon.{module => ArgonModule}
import scalapb.GeneratedMessage
import zio.{IO, ZIO}
import zio.stream.{Stream, ZStream}
import zio.interop.catz.core._
import cats.implicits._
import ModuleCreatorCommon._
import dev.argon.compiler.output.ArgonModuleSerialized

import scala.collection.immutable.{Map, Vector}

private[loader] abstract class ModuleCreatorCommon[TPayloadSpec[_, _]: PayloadSpecInfo] extends ModuleCreatorBase[TPayloadSpec] {

  import context.{typeSystem, signatureContext}, typeSystem.{ context => _, _ }, signatureContext.{ context => _, TTypeWrapper => _, typeWrapperInstances => _, _ }

  val source: ArgonModuleSerialized
  val currentModuleDescriptor: ModuleId
  val referenceList: ArgonModule.ModuleReferencesList
  val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]
  val payloadLoader: PayloadLoader[_ >: context.type, TPayloadSpec]

  val moduleCache: ValueCacheManaged[CompilationError, ArModule[context.type, TPayloadSpec]]
  val traitCache: MemoCacheStore[CompilationError, Int, TraitLoadResult[context.type, TPayloadSpec]]
  val classCache: MemoCacheStore[CompilationError, Int, ClassLoadResult[context.type, TPayloadSpec]]
  val dataCtorCache: MemoCacheStore[CompilationError, Int, DataCtorLoadResult[context.type, TPayloadSpec]]
  val functionCache: MemoCacheStore[CompilationError, Int, FunctionLoadResult[context.type, TPayloadSpec]]
  val methodCache: MemoCacheStore[CompilationError, Int, MethodLoadResult[context.type, TPayloadSpec]]
  val classCtorCache: MemoCacheStore[CompilationError, Int, ClassCtorLoadResult[context.type, TPayloadSpec]]
  val localVariableIdCache: MemoCacheStore[CompilationError, Int, LocalVariableId]

  private def invalidModuleFormatError: CompilationError =
    DiagnosticError.ModuleFormatInvalid(DiagnosticSource.ReferencedModule(currentModuleDescriptor))


  private def parseNamespacePath(ns: ArgonModule.Namespace): NamespacePath =
    NamespacePath(ns.path)

  private object ValidGlobalName {
    def unapply(globalName: ArgonModule.GlobalName): Option[GlobalName] =
      globalName.globalName match {
        case ArgonModule.GlobalName.GlobalName.NormalName(name) => Some(GlobalName.Normal(name))
        case ArgonModule.GlobalName.GlobalName.Operator(op) => Some(GlobalName.Operator(op))
        case ArgonModule.GlobalName.GlobalName.Unnamed(_) => Some(GlobalName.Unnamed)
        case ArgonModule.GlobalName.GlobalName.Empty => None
      }
  }

  private object ParsedAccessModifier {
    def unapply(accessModifier: ArgonModule.AccessModifier): Option[AccessModifier] =
      accessModifier match {
        case ArgonModule.AccessModifier.Unrecognized(_) => None
        case ArgonModule.AccessModifier.Public => Some(AccessModifier.Public)
        case ArgonModule.AccessModifier.Internal => Some(AccessModifier.Internal)
        case ArgonModule.AccessModifier.Protected => Some(AccessModifier.Protected)
        case ArgonModule.AccessModifier.ProtectedInternal => Some(AccessModifier.ProtectedInternal)
        case ArgonModule.AccessModifier.Private => Some(AccessModifier.Private)
        case ArgonModule.AccessModifier.PrivateInternal => Some(AccessModifier.PrivateInternal)
      }
  }

  private def parseMutability(mutability: ArgonModule.Mutability): Comp[Mutability] =
    mutability match {
      case ArgonModule.Mutability.Mutable => IO.succeed(Mutability.Mutable)
      case ArgonModule.Mutability.NonMutable => IO.succeed(Mutability.NonMutable)
      case ArgonModule.Mutability.Unrecognized(_) => Compilation.forErrors(invalidModuleFormatError)
    }


  private lazy val refModuleMap: Map[Int, ModuleLoadResult[context.type]] =
    referenceList.references.zipWithIndex
      .map { case (modRef, i) =>
        val moduleLoadRes: ModuleLoadResult[context.type] =
          referencedModules
            .find { _.id.name.contains(modRef.name) }
          match {
            case Some(referencedModule) => ModuleReference(referencedModule)
            case None => ModuleNotFound(modRef)
          }

        i + 1 -> moduleLoadRes
      }
      .toMap

  private def lookupNamespaceBinding[T, TPS[_, _]]
  (refModule: ArModule[context.type, TPS])
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding.NonNamespace[context.type, TPS] => Comp[Option[T]])
  : Comp[Option[T]] =
    refModule.lookupNamespaceBindings(namespace, name)(f).take(2).runCollect.map { values =>
      if(values.size === 1) values.headOption
      else None
    }

  private def getModule(id: Option[Int]): Comp[ModuleObjectLoadResult[ArModule[context.type, TPayloadSpec], Nothing, ArModule[context.type, ReferencePayloadSpecifier]]] =
    id match {
      case Some(id) =>
        refModuleMap.get(id) match {
          case Some(ModuleReference(module)) =>
            IO.succeed(ModuleObjectReference(module))

          case _ =>
            Compilation.forErrors(DiagnosticError.ModuleObjectNotFound(DiagnosticError.ModuleObjectReferencedModule, id, DiagnosticSource.ReferencedModule(currentModuleDescriptor)))
        }

      case None => this.module.map(ModuleObjectDefinition.apply)
    }

  private def parseTraitOwner(desc: ArgonModule.TraitOwner): Comp[ModuleObjectLoadResult[TraitOwner[context.type, TPayloadSpec], TraitOwner.ByNamespace[context.type, TPayloadSpec], TraitOwner[context.type, ReferencePayloadSpecifier]]] =
    desc.owner match {
      case ArgonModule.TraitOwner.Owner.InNamespace(
        ArgonModule.ByNamespaceOwner(
          moduleId,
          ns,
          ValidGlobalName(name)
        )
      ) =>
        getModule(moduleId).map {
          case ModuleObjectReference(module) => ModuleObjectReference(TraitOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectDefinition(module) => ModuleObjectGlobalDefinition(TraitOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectGlobalDefinition(value) => value
        }

      case _ =>
        Compilation.forErrors(invalidModuleFormatError)
    }

  private def parseClassOwner(desc: ArgonModule.ClassOwner): Comp[ModuleObjectLoadResult[ClassOwner[context.type, TPayloadSpec], ClassOwner.ByNamespace[context.type, TPayloadSpec], ClassOwner[context.type, ReferencePayloadSpecifier]]] =
    desc.owner match {
      case ArgonModule.ClassOwner.Owner.InNamespace(
        ArgonModule.ByNamespaceOwner(
          moduleId,
          ns,
          ValidGlobalName(name)
        )
      ) =>
        getModule(moduleId).map {
          case ModuleObjectReference(module) => ModuleObjectReference(ClassOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectDefinition(module) => ModuleObjectGlobalDefinition(ClassOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectGlobalDefinition(value) => value
        }

      case _ =>
        Compilation.forErrors(invalidModuleFormatError)
    }

  private def parseDataCtorOwner(desc: ArgonModule.DataConstructorOwner): Comp[ModuleObjectLoadResult[DataConstructorOwner[context.type, TPayloadSpec], DataConstructorOwner.ByNamespace[context.type, TPayloadSpec], DataConstructorOwner[context.type, ReferencePayloadSpecifier]]] =
    desc.owner match {
      case ArgonModule.DataConstructorOwner.Owner.InNamespace(
        ArgonModule.ByNamespaceOwner(
          moduleId,
          ns,
          ValidGlobalName(name)
        )
      ) =>
        getModule(moduleId).map {
          case ModuleObjectReference(module) => ModuleObjectReference(DataConstructorOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectDefinition(module) => ModuleObjectGlobalDefinition(DataConstructorOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectGlobalDefinition(value) => value
        }

      case _ =>
        Compilation.forErrors(invalidModuleFormatError)
    }

  private def parseFunctionOwner(desc: ArgonModule.FunctionOwner): Comp[ModuleObjectLoadResult[FunctionOwner[context.type, TPayloadSpec], FunctionOwner.ByNamespace[context.type, TPayloadSpec], FunctionOwner[context.type, ReferencePayloadSpecifier]]] =
    desc.owner match {
      case ArgonModule.FunctionOwner.Owner.InNamespace(
        ArgonModule.ByNamespaceOwner(
          moduleId,
          ns,
          ValidGlobalName(name)
        )
      ) =>
        getModule(moduleId).map {
          case ModuleObjectReference(module) => ModuleObjectReference(FunctionOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectDefinition(module) => ModuleObjectGlobalDefinition(FunctionOwner.ByNamespace(module, parseNamespacePath(ns), name))
          case ModuleObjectGlobalDefinition(value) => value
        }

      case _ =>
        Compilation.forErrors(invalidModuleFormatError)
    }

  private def parseMemberName(memberName: ArgonModule.MethodName): Option[MethodName] =
    memberName.name match {

      case ArgonModule.MethodName.Name.Empty => None
      case ArgonModule.MethodName.Name.SpecialName(ArgonModule.SpecialMethodName.Unrecognized(_)) => None

      case ArgonModule.MethodName.Name.Normal(name) =>
        Some(MemberName.Normal(name))

      case ArgonModule.MethodName.Name.Mutator(name) =>
        Some(MemberName.Mutator(name))

      case ArgonModule.MethodName.Name.SpecialName(ArgonModule.SpecialMethodName.Unnamed) =>
        Some(MemberName.Unnamed)

      case ArgonModule.MethodName.Name.SpecialName(ArgonModule.SpecialMethodName.Call) =>
        Some(MemberName.Call)

    }

  private def parseMethodOwner(owner: ArgonModule.MethodOwner): Comp[ModuleObjectLoadResult[MethodOwner[context.type, TPayloadSpec], Nothing, MethodOwner[context.type, ReferencePayloadSpecifier]]] =
    owner.owner match {
      case ArgonModule.MethodOwner.Owner.Empty => Compilation.forErrors(invalidModuleFormatError)
      case ArgonModule.MethodOwner.Owner.ByClass(classId) =>
        getArClass(classId).map {
          case ModuleObjectReference(value) => ModuleObjectReference(MethodOwner.ByClass(value))
          case ModuleObjectDefinition(value) => ModuleObjectDefinition(MethodOwner.ByClass(value))
          case ModuleObjectGlobalDefinition(value) => ModuleObjectDefinition(MethodOwner.ByClass(value))
        }

      case ArgonModule.MethodOwner.Owner.ByClassObject(classId) =>
        getArClass(classId).map {
          case ModuleObjectReference(value) => ModuleObjectReference(MethodOwner.ByClassObject(value))
          case ModuleObjectDefinition(value) => ModuleObjectDefinition(MethodOwner.ByClassObject(value))
          case ModuleObjectGlobalDefinition(value) => ModuleObjectDefinition(MethodOwner.ByClassObject(value))
        }

      case ArgonModule.MethodOwner.Owner.ByTrait(classId) =>
        getTrait(classId).map {
          case ModuleObjectReference(value) => ModuleObjectReference(MethodOwner.ByTrait(value))
          case ModuleObjectDefinition(value) => ModuleObjectDefinition(MethodOwner.ByTrait(value))
          case ModuleObjectGlobalDefinition(value) => ModuleObjectDefinition(MethodOwner.ByTrait(value))
        }

      case ArgonModule.MethodOwner.Owner.ByTraitObject(classId) =>
        getTrait(classId).map {
          case ModuleObjectReference(value) => ModuleObjectReference(MethodOwner.ByTraitObject(value))
          case ModuleObjectDefinition(value) => ModuleObjectDefinition(MethodOwner.ByTraitObject(value))
          case ModuleObjectGlobalDefinition(value) => ModuleObjectDefinition(MethodOwner.ByTraitObject(value))
        }

      case ArgonModule.MethodOwner.Owner.ByDataConstructor(ctorId) =>
        getDataCtor(ctorId).map {
          case ModuleObjectReference(value) => ModuleObjectReference(MethodOwner.ByDataCtor(value))
          case ModuleObjectDefinition(value) => ModuleObjectDefinition(MethodOwner.ByDataCtor(value))
          case ModuleObjectGlobalDefinition(value) => ModuleObjectDefinition(MethodOwner.ByDataCtor(value))
        }
    }

  private def parseErasedSigType(t: ArgonModule.SigType): Comp[ErasedSignature.SigType[context.type]] =
    t.sigType match {
      case ArgonModule.SigType.SigType._Empty(_) => IO.succeed(ErasedSignature.BlankType())
      case ArgonModule.SigType.SigType.ClassType(ArgonModule.SigTypeClass(classId, typeArguments)) =>
        for {
          arClass <- findClass(classId)
          args <- typeArguments.traverse(parseErasedSigType(_))
        } yield ErasedSignature.ClassType(arClass, args)

      case ArgonModule.SigType.SigType.TraitType(ArgonModule.SigTypeTrait(traitId, typeArguments)) =>
        for {
          arTrait <- findTrait(traitId)
          args <- typeArguments.traverse(parseErasedSigType(_))
        } yield ErasedSignature.TraitType(arTrait, args)

      case ArgonModule.SigType.SigType.DataCtorType(ArgonModule.SigTypeDataConstructor(ctorId, typeArguments)) =>
        for {
          ctor <- findDataConstructor(ctorId)
          args <- typeArguments.traverse(parseErasedSigType(_))
        } yield ErasedSignature.DataConstructorType(ctor, args)

      case ArgonModule.SigType.SigType.TupleType(ArgonModule.SigTypeTuple(elements)) =>
        for {
          elems <- elements.traverse(parseErasedSigType(_))
          nonEmptyElems <- Compilation.requireSome(NonEmptyList.fromList(elems.toList))(invalidModuleFormatError)
        } yield ErasedSignature.TupleType(nonEmptyElems)


      case ArgonModule.SigType.SigType.FunctionType(ArgonModule.SigTypeFunction(argumentType, resultType)) =>
        for {
          arg <- parseErasedSigType(argumentType)
          res <- parseErasedSigType(resultType)
        } yield ErasedSignature.FunctionType(arg, res)

      case ArgonModule.SigType.SigType.Empty => Compilation.forErrors(invalidModuleFormatError)
    }

  private def parseErasedSigParamOnly(sig: ArgonModule.ErasedSignatureParameterOnly): Comp[ErasedSignature.ParameterOnlySignature[context.type]] =
    sig.parameterTypes.traverse(parseErasedSigType(_)).map(ErasedSignature.ParameterOnlySignature.apply)

  private def parseErasedSig(sig: ArgonModule.ErasedSignature): Comp[ErasedSignature[context.type]] = {

    def impl(paramTypes: Vector[ArgonModule.SigType]): Comp[ErasedSignature[context.type]] =
      paramTypes match {
        case VectorUnCons(VectorUnCons.NonEmpty(head, tail)) =>
          for {
            paramType <- parseErasedSigType(head)
            rest <- impl(tail)
          } yield ErasedSignature.Parameter(paramType, rest)

        case VectorUnCons(VectorUnCons.Empty) =>
          parseErasedSigType(sig.resultType).map(ErasedSignature.Result.apply)
      }

    impl(sig.parameterTypes)
  }


  private def convertFileId(id: ArgonModule.FileID): FileID =
    FileID(id.id)

  trait ObjectDefinitionLoader[TDef, TOwner <: AnyRef, TDefResult] {
    def apply[TOwner2 <: TOwner](id: Int, definition: TDef, objOwner: TOwner2): Comp[TDefResult { val owner: objOwner.type }]
  }

  private def handleModuleObjectLoading[
    TRef <: GeneratedMessage,
    TDef <: GeneratedMessage,
    TValueOwner,
    TResultOwner[_ <: Context with Singleton, _[_, _]] <: AnyRef,
    TGlobalOwner[TC <: Context with Singleton, TPS[_, _]] <: TResultOwner[TC, TPS],
    TRefResult,
    TDefResult,
  ](
     cache: MemoCacheStore[CompilationError, Int, ModuleObjectLoadResult[TDefResult, TDefResult { val owner: TGlobalOwner[context.type, TPayloadSpec] }, TRefResult]]
   )(
     moduleObjectType: DiagnosticError.ModuleObjectType,
   )(
       getReference: (ArgonModuleSerialized, Int) => Comp[TRef],
       getDefinition: (ArgonModuleSerialized, Int) => Comp[TDef],
   )(
     refOwnerLens: TRef => TValueOwner,
     defOwnerLens: TDef => TValueOwner,
   )(
     parseDescriptor: TValueOwner => Comp[ModuleObjectLoadResult[TResultOwner[context.type, TPayloadSpec], TGlobalOwner[context.type, TPayloadSpec], TResultOwner[context.type, ReferencePayloadSpecifier]]]
   )(
     referenceHandler: TRef => TResultOwner[context.type, ReferencePayloadSpecifier] => Comp[Option[TRefResult]],
     definitionHandler: ObjectDefinitionLoader[TDef, TResultOwner[context.type, TPayloadSpec], TDefResult],
   ): MemoCache[Any, CompilationError, Int, ModuleObjectLoadResult[TDefResult, TDefResult { val owner: TGlobalOwner[context.type, TPayloadSpec] }, TRefResult]] =
    cache.usingCreate { id =>
      if(id < 0) {
        getReference(source, -id).flatMap { refValue =>
          parseDescriptor(refOwnerLens(refValue)).flatMap {
            case ModuleObjectReference(owner) =>
              referenceHandler(refValue)(owner).flatMap {
                case Some(refResult) => IO.succeed(ModuleObjectReference(refResult))
                case None => Compilation.forErrors(DiagnosticError.ModuleObjectNotFound(
                  moduleObjectType, id, DiagnosticSource.ReferencedModule(currentModuleDescriptor)
                ))
              }

            case ModuleObjectDefinition(_) | ModuleObjectGlobalDefinition(_) =>
              Compilation.forErrors(invalidModuleFormatError)
          }
        }
      }
      else {
        getDefinition(source, id).flatMap { defValue =>
          parseDescriptor(defOwnerLens(defValue)).flatMap {
            case ModuleObjectReference(_) => Compilation.forErrors(invalidModuleFormatError)

            case ModuleObjectDefinition(owner) =>
              definitionHandler(id, defValue, owner).map(ModuleObjectDefinition.apply)

            case ModuleObjectGlobalDefinition(owner2) =>
              definitionHandler[owner2.type](id, defValue, owner2).map(ModuleObjectGlobalDefinition.apply)
          }
        }
      }
    }

  private def getMethodMembers(methods: Vector[ArgonModule.MethodMember]): Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
    methods
      .traverse { methodMember =>
        for {
          methodLoad <- getMethod(methodMember.id)
        } yield (methodLoad, methodMember.accessModifier)
      }
      .map { methodLoads =>
        methodLoads.collect {
          case (ModuleObjectDefinition(method), ParsedAccessModifier(accessModifier)) =>
            MethodBinding(accessModifier, method)
        }
      }

  private def lookupTrait[TPS[_, _]]: ArgonModule.TraitReference => TraitOwner[context.type, TPS] => Comp[Option[ArTrait[context.type, TPS]]] = traitRef => {
    case TraitOwner.ByNamespace(module, namespace, name) =>
      parseErasedSigParamOnly(traitRef.signature).flatMap { sig =>
        lookupNamespaceBinding(module)(namespace, name)(ModuleLookup.lookupGlobalTrait(context)(sig))
      }
  }

  private def getTrait(id: Int): Comp[TraitLoadResult[context.type, TPayloadSpec]] =
    handleModuleObjectLoading[
        ArgonModule.TraitReference,
        ArgonModule.TraitDefinition,
        ArgonModule.TraitOwner,
        TraitOwner,
        TraitOwner.ByNamespace,
        ArTrait[context.type, ReferencePayloadSpecifier],
        ArTrait[context.type, TPayloadSpec]
      ](
        traitCache
      )(
        DiagnosticError.ModuleObjectTrait
      )(
        getReference = _.getTraitRef(_),
        getDefinition = _.getTraitDef(_),
      )(
        refOwnerLens = _.owner,
        defOwnerLens = _.owner,
      )(
        parseDescriptor = parseTraitOwner(_)
      )(
        referenceHandler = lookupTrait,
        definitionHandler = new ObjectDefinitionLoader[ArgonModule.TraitDefinition, TraitOwner[context.type, TPayloadSpec], ArTrait[context.type, TPayloadSpec]] {
          override def apply[TOwner2 <: TraitOwner[context.type, TPayloadSpec]](id: Int, definition: ArgonModule.TraitDefinition, traitOwner: TOwner2): Comp[ArTrait[context.type, TPayloadSpec] { val owner: traitOwner.type }] = for {
            uniqId <- UniqueIdentifier.make
          } yield new ArTrait[context.type, TPayloadSpec] {
            override val context: ModuleCreatorCommon.this.context.type = ModuleCreatorCommon.this.context
            override val contextProof: context.type Is context.type = Is.refl


            override val id: TraitId = TraitId(uniqId)
            override val owner: traitOwner.type = traitOwner
            override val fileId: FileID = convertFileId(definition.fileId)

            override val isSealed: Boolean = definition.isSealed.getOrElse(false)

            override lazy val signature: Comp[Signature[ArTrait.ResultInfo, _ <: Nat]] =
              definition.signature match {
                case ArgonModule.TraitSignature(parameters, baseTraits) =>
                  for {
                    baseTraitsResolved <- baseTraits.traverse(resolveTraitType(_))
                    parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(ParameterVariableOwner.ByTrait(AbsRef(this)))(index)(param) }

                    result = SignatureResult[ArTrait.ResultInfo](
                      ArTrait.ResultInfo(BaseTypeInfoTrait(baseTraitsResolved).pure[Comp])
                    )

                  } yield parametersResolved.foldRight[Signature[ArTrait.ResultInfo, _ <: Nat]](result) {
                    case (param, prevSig: Signature[ArTrait.ResultInfo, len]) =>
                      SignatureParameters[ArTrait.ResultInfo, len](param, prevSig)
                  }
              }

            override lazy val methods: Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
              getMethodMembers(definition.methods)

            override val staticMethods: Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
              getMethodMembers(definition.staticMethods)
          }
        }
      ).get(id)

  private def lookupClass[TPS[_, _]]: ArgonModule.ClassReference => ClassOwner[context.type, TPS] => Comp[Option[ArClass[context.type, TPS]]] = classRef => {
    case ClassOwner.ByNamespace(module, namespace, name) =>
      parseErasedSigParamOnly(classRef.signature).flatMap { sig =>
        lookupNamespaceBinding(module)(namespace, name)(ModuleLookup.lookupGlobalClass(context)(sig))
      }
  }

  private def getArClass(id: Int): Comp[ClassLoadResult[context.type, TPayloadSpec]] =
    handleModuleObjectLoading[
        ArgonModule.ClassReference,
        ArgonModule.ClassDefinition,
        ArgonModule.ClassOwner,
        ClassOwner,
        ClassOwner.ByNamespace,
        ArClass[context.type, ReferencePayloadSpecifier],
        ArClass[context.type, TPayloadSpec]
      ](
        classCache,
      )(
        DiagnosticError.ModuleObjectClass
      )(
        getReference = _.getClassRef(_),
        getDefinition = _.getClassDef(_),
      )(
        refOwnerLens = _.owner,
        defOwnerLens = _.owner,
      )(
        parseDescriptor = parseClassOwner(_)
      )(
        referenceHandler =  lookupClass,
        definitionHandler = new ObjectDefinitionLoader[ArgonModule.ClassDefinition, ClassOwner[context.type, TPayloadSpec], ArClass[context.type, TPayloadSpec]] {
          override def apply[TOwner2 <: ClassOwner[context.type, TPayloadSpec]](id: Int, definition: ArgonModule.ClassDefinition, classOwner: TOwner2): Comp[ArClass[context.type, TPayloadSpec] { val owner: classOwner.type }] = for {
            uniqId <- UniqueIdentifier.make
          } yield new ArClass[context.type, TPayloadSpec] {
            override val context: ModuleCreatorCommon.this.context.type = ModuleCreatorCommon.this.context
            override val contextProof: context.type Is context.type = Is.refl


            override val id: ClassId = ClassId(uniqId)
            override val owner: classOwner.type = classOwner
            override val fileId: FileID = convertFileId(definition.fileId)
            override val classMessageSource: DiagnosticSource = DiagnosticSource.ReferencedModule(currentModuleDescriptor)

            override val isSealed: Boolean = definition.isSealed.getOrElse(false)
            override val isOpen: Boolean = definition.isOpen.getOrElse(false)
            override val isAbstract: Boolean = definition.isAbstract.getOrElse(false)

            override lazy val signature: Comp[Signature[ArClass.ResultInfo, _ <: Nat]] =
              definition.signature match {
                case ArgonModule.ClassSignature(parameters, baseClass, baseTraits) =>
                  for {
                    baseClassResolved <- baseClass.traverse(resolveClassType(_))
                    baseTraitsResolved <- baseTraits.traverse(resolveTraitType(_))
                    parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(ParameterVariableOwner.ByClass(AbsRef(this)))(index)(param) }

                    result = SignatureResult[ArClass.ResultInfo](
                      ArClass.ResultInfo(BaseTypeInfoClass(baseClassResolved, baseTraitsResolved).pure[Comp])
                    )

                  } yield parametersResolved.foldRight[Signature[ArClass.ResultInfo, _ <: Nat]](result) {
                    case (param, prevSig: Signature[ArClass.ResultInfo, len]) =>
                      SignatureParameters[ArClass.ResultInfo, len](param, prevSig)
                  }
              }

            override val fields: Comp[Vector[FieldVariable[context.type, Id]]] =
              definition.fields.traverse { field =>
                for {
                  fieldType <- resolveExpr(field.fieldType)
                  mutability <- parseMutability(field.mutability)
                } yield FieldVariable[context.type, Id](
                  FieldVariableOwner(AbsRef(this)),
                  VariableName.Normal(field.name),
                  mutability,
                  fieldType
                )
              }

            override lazy val methods: Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
              getMethodMembers(definition.methods)

            override lazy val staticMethods: Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
              getMethodMembers(definition.staticMethods)

            override val classConstructors: Comp[Vector[ClassConstructorBinding[context.type, TPayloadSpec]]] =
              definition.constructors
                .traverse { classCtor =>
                  getClassCtor(classCtor.id).map { (_, classCtor.accessModifier) }
                }
                .map { ctorLoads =>
                  ctorLoads.collect {
                    case (ModuleObjectDefinition(ctor), ParsedAccessModifier(accessModifier)) =>
                      ClassConstructorBinding(accessModifier, ctor)
                  }
                }

          }
        }
      ).get(id)

  private def getDataCtor(id: Int): Comp[DataCtorLoadResult[context.type, TPayloadSpec]] =
    handleModuleObjectLoading[
        ArgonModule.DataConstructorReference,
        ArgonModule.DataConstructorDefinition,
        ArgonModule.DataConstructorOwner,
        DataConstructorOwner,
        core.DataConstructorOwner.ByNamespace,
        DataConstructor[context.type, ReferencePayloadSpecifier],
        DataConstructor[context.type, TPayloadSpec]
      ](
        dataCtorCache
      )(
        DiagnosticError.ModuleObjectDataConstructor
      )(
        getReference = _.getDataConstructorRef(_),
        getDefinition = _.getDataConstructorDef(_),
      )(
        refOwnerLens = _.owner,
        defOwnerLens = _.owner,
      )(
        parseDescriptor = parseDataCtorOwner(_)
      )(
        referenceHandler = ctorRef => {
          case DataConstructorOwner.ByNamespace(module, namespace, name) =>
            parseErasedSigParamOnly(ctorRef.signature).flatMap { sig =>
              lookupNamespaceBinding(module)(namespace, name)(ModuleLookup.lookupGlobalDataConstructor(context)(sig))
            }
        },
        definitionHandler = new ObjectDefinitionLoader[ArgonModule.DataConstructorDefinition, DataConstructorOwner[context.type, TPayloadSpec], DataConstructor[context.type, TPayloadSpec]] {
          override def apply[TOwner2 <: DataConstructorOwner[context.type, TPayloadSpec]](id: Int, definition: ArgonModule.DataConstructorDefinition, ctorOwner: TOwner2): Comp[DataConstructor[context.type, TPayloadSpec] { val owner: ctorOwner.type }] = ???
        }
      ).get(id)

  private def getFunction(id: Int): Comp[FunctionLoadResult[context.type, TPayloadSpec]] =
    handleModuleObjectLoading[
        ArgonModule.FunctionReference,
        ArgonModule.FunctionDefinition,
        ArgonModule.FunctionOwner,
        FunctionOwner,
        FunctionOwner.ByNamespace,
        ArFunc[context.type, ReferencePayloadSpecifier],
        ArFunc[context.type, TPayloadSpec]
      ](
        functionCache
      )(
        DiagnosticError.ModuleObjectFunction
      )(
        getReference = _.getFunctionRef(_),
        getDefinition = _.getFunctionDef(_),
      )(
        refOwnerLens = _.owner,
        defOwnerLens = _.owner,
      )(
        parseDescriptor = parseFunctionOwner(_)
      )(
        referenceHandler = funcRef => {
          case FunctionOwner.ByNamespace(module, namespace, name) =>
            parseErasedSig(funcRef.signature).flatMap { sig =>
              lookupNamespaceBinding(module)(namespace, name)(ModuleLookup.lookupGlobalFunction(context)(sig))
            }
        },
        definitionHandler = new ObjectDefinitionLoader[ArgonModule.FunctionDefinition, FunctionOwner[context.type, TPayloadSpec], ArFunc[context.type, TPayloadSpec]] {
          override def apply[TOwner2 <: FunctionOwner[context.type, TPayloadSpec]](id: Int, definition: ArgonModule.FunctionDefinition, functionOwner: TOwner2): Comp[ArFunc[context.type, TPayloadSpec] { val owner: functionOwner.type }] = for {
            uniqId <- UniqueIdentifier.make
          } yield new ArFunc[context.type, TPayloadSpec] {
            override val context: ModuleCreatorCommon.this.context.type = ModuleCreatorCommon.this.context

            override val id: FunctionId = FunctionId(uniqId)
            override val owner: functionOwner.type = functionOwner
            override val fileId: FileID = convertFileId(definition.fileId)
            override val effectInfo: EffectInfo = EffectInfo(
              isPure = definition.effects.isPure,
            )
            override val signature: Comp[Signature[FunctionResultInfo, _ <: Nat]] =
              definition.signature match {
                case ArgonModule.FunctionSignature(parameters, returnType) =>
                  for {
                    returnTypeResolved <- resolveExpr(returnType)
                    parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(ParameterVariableOwner.ByFunction(AbsRef(this)))(index)(param) }

                    result = SignatureResult[FunctionResultInfo](
                      FunctionResultInfo[context.type, Id](returnTypeResolved)
                    )

                  } yield parametersResolved.foldRight[Signature[FunctionResultInfo, _ <: Nat]](result) {
                    case (param, prevSig: Signature[FunctionResultInfo, len]) =>
                      SignatureParameters[FunctionResultInfo, len](param, prevSig)
                  }
              }

            override val payload: TPayloadSpec[Comp[context.TFunctionImplementation], Unit] =
              payloadLoader.createFunctionPayload(context)
          }
        }
      ).get(id)

  private lazy val getMethod: Int => Comp[MethodLoadResult[context.type, TPayloadSpec]] =
    handleModuleObjectLoading[
        ArgonModule.MethodReference,
        ArgonModule.MethodDefinition,
        ArgonModule.MethodOwner,
        MethodOwner,
        Nothing,
        ArMethod[context.type, ReferencePayloadSpecifier],
        ArMethod[context.type, TPayloadSpec]
      ](
        methodCache
      )(
        DiagnosticError.ModuleObjectMethod
      )(
        getReference = _.getMethodRef(_),
        getDefinition = _.getMethodDef(_),
      )(
        refOwnerLens = _.owner,
        defOwnerLens = _.owner,
      )(
        parseDescriptor = parseMethodOwner(_)
      )(
        referenceHandler = methodRef => methodOwner => for {
          methods <- methodOwner match {
            case MethodOwner.ByClass(ownerClass) => ownerClass.methods
            case MethodOwner.ByClassObject(ownerClass) => ownerClass.staticMethods
            case MethodOwner.ByTrait(ownerTrait) => ownerTrait.methods
            case MethodOwner.ByTraitObject(ownerTrait) => ownerTrait.staticMethods
            case MethodOwner.ByDataCtor(dataCtor) => dataCtor.methods
          }

          methodName <- Compilation.requireSome(parseMemberName(methodRef.name))(invalidModuleFormatError)
          methodSig <- parseErasedSig(methodRef.signature)

          method <- methods
            .map { _.method }
            .findM { method =>
              if(method.name === methodName)
                method.signatureUnsubstituted.map { sig => methodSig === ErasedSignature.fromSignature(context)(sig) }
              else
                IO.succeed(false)
            }

        } yield method,
        definitionHandler = new ObjectDefinitionLoader[ArgonModule.MethodDefinition, MethodOwner[context.type, TPayloadSpec], ArMethod[context.type, TPayloadSpec]] {
          override def apply[TOwner2 <: MethodOwner[context.type, TPayloadSpec]](id: Int, definition: ArgonModule.MethodDefinition, methodOwner: TOwner2): Comp[ArMethod[context.type, TPayloadSpec] { val owner: methodOwner.type }] =
            for {
              uniqId <- UniqueIdentifier.make
              methodName <- IO.fromEither(
                parseMemberName(definition.name)
                  .toRight { invalidModuleFormatError }
              )
            } yield new ArMethod[context.type, TPayloadSpec] {
              override val context: ModuleCreatorCommon.this.context.type = ModuleCreatorCommon.this.context
              override val contextProof: context.type Is context.type = Is.refl

              override val id: MethodId = MethodId(uniqId)
              override val name: MethodName = methodName
              override val fileId: FileID = convertFileId(definition.fileId)
              override val effectInfo: EffectInfo = EffectInfo(
                isPure = definition.effects.isPure,
              )

              override val isVirtual: Boolean = definition.isVirtual.getOrElse(false)
              override val isAbstract: Boolean = definition.isAbstract.getOrElse(false)
              override val isImplicitOverride: Boolean = definition.isImplicitOverride.getOrElse(false)
              override val isFinal: Boolean = definition.isFinal.getOrElse(false)

              override val owner: methodOwner.type = methodOwner

              override val signatureUnsubstituted: Comp[Signature[FunctionResultInfo, _ <: Nat]] =
                definition.signature match {
                  case ArgonModule.MethodSignature(parameters, returnType) =>
                    for {
                      returnTypeResolved <- resolveExpr(returnType)
                      parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(ParameterVariableOwner.ByMethod(AbsRef(this)))(index)(param) }

                      result = SignatureResult[FunctionResultInfo](
                        FunctionResultInfo[context.type, Id](returnTypeResolved)
                      )

                    } yield parametersResolved.foldRight[Signature[FunctionResultInfo, _ <: Nat]](result) {
                      case (param, prevSig: Signature[FunctionResultInfo, len]) =>
                        SignatureParameters[FunctionResultInfo, len](param, prevSig)
                    }
                }

              override val payload: TPayloadSpec[Comp[context.TMethodImplementation], Unit] =
                payloadLoader.createMethodPayload(context)
            }
        }
      ).toFunction

  private lazy val getClassCtor: Int => Comp[ClassCtorLoadResult[context.type, TPayloadSpec]] =
    handleModuleObjectLoading[
        ArgonModule.ClassConstructorReference,
        ArgonModule.ClassConstructorDefinition,
        Int,
        ClassConstructorOwner,
        Nothing,
        ClassConstructor[context.type, ReferencePayloadSpecifier],
        ClassConstructor[context.type, TPayloadSpec]
      ](
        classCtorCache
      )(
        DiagnosticError.ModuleObjectClassConstructor
      )(
        getReference = _.getClassConstructorRef(_),
        getDefinition = _.getClassConstructorDef(_),
      )(
        refOwnerLens = _.ownerClass,
        defOwnerLens = _.ownerClass,
      )(
        parseDescriptor = classId => getArClass(classId).map {
          case ModuleObjectReference(value) => ModuleObjectReference(ClassConstructorOwner(value))
          case ModuleObjectDefinition(value) => ModuleObjectDefinition(ClassConstructorOwner(value))
          case ModuleObjectGlobalDefinition(value) => ModuleObjectDefinition(ClassConstructorOwner(value))
        }
      )(
        referenceHandler = _ => ctorOwner =>
          ctorOwner.ownerClass.classConstructors.map { ctors =>
            ctors
              .headOption
              .map { _.ctor }
          },
        definitionHandler = new ObjectDefinitionLoader[ArgonModule.ClassConstructorDefinition, ClassConstructorOwner[context.type, TPayloadSpec], ClassConstructor[context.type, TPayloadSpec]] {
          override def apply[TOwner2 <: ClassConstructorOwner[context.type, TPayloadSpec]](id: Int, definition: ArgonModule.ClassConstructorDefinition, ctorOwner: TOwner2): Comp[ClassConstructor[context.type, TPayloadSpec] { val owner: ctorOwner.type }] =
            for {
              uniqId <- UniqueIdentifier.make
            } yield new ClassConstructor[context.type, TPayloadSpec] {
              override val context: ModuleCreatorCommon.this.context.type = ModuleCreatorCommon.this.context


              override val id: ClassConstructorId = ClassConstructorId(uniqId)
              override val owner: ctorOwner.type = ctorOwner
              override val fileId: FileID = convertFileId(definition.fileId)
              override val effectInfo: EffectInfo = EffectInfo(
                isPure = definition.effects.isPure,
              )

              override lazy val signatureUnsubstituted: Comp[Signature[ClassConstructor.ResultInfo, _ <: Nat]] =
                definition.signature match {
                  case ArgonModule.ClassConstructorSignature(parameters) =>
                    for {
                      parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(ParameterVariableOwner.ByClassConstructor(AbsRef(this)))(index)(param) }

                      result = SignatureResult[ClassConstructor.ResultInfo](
                        ClassConstructor.ResultInfo()
                      )

                    } yield parametersResolved.foldRight[Signature[ClassConstructor.ResultInfo, _ <: Nat]](result) {
                      case (param, prevSig: Signature[ClassConstructor.ResultInfo, len]) =>
                        SignatureParameters[ClassConstructor.ResultInfo, len](param, prevSig)
                    }
                }

              override lazy val payload: TPayloadSpec[Comp[context.TClassConstructorImplementation], Unit] =
                payloadLoader.createClassConstructorPayload(context)

            }
        }
      ).toFunction

  trait GlobalDeclarationHelper[TDecl] {
    def getId(decl: TDecl): Int
    def getAccessModifier(decl: TDecl): ArgonModule.AccessModifier
    def getName(decl: TDecl): ArgonModule.GlobalName

    type ErasedSig
    def getErasedSig(decl: TDecl): Comp[ErasedSig]
  }

  object GlobalDeclarationTypeHelper extends GlobalDeclarationHelper[ArgonModule.GlobalDeclarationType] {

    override def getId(decl: ArgonModule.GlobalDeclarationType): Int = decl.id
    override def getAccessModifier(decl: ArgonModule.GlobalDeclarationType): ArgonModule.AccessModifier = decl.accessModifier

    override def getName(decl: ArgonModule.GlobalDeclarationType): ArgonModule.GlobalName = decl.name

    override type ErasedSig = ErasedSignature.ParameterOnlySignature[context.type]

    override def getErasedSig(decl: ArgonModule.GlobalDeclarationType): Comp[ErasedSignature.ParameterOnlySignature[context.type]] =
      parseErasedSigParamOnly(decl.sig)
  }

  object GlobalDeclarationFunctionHelper extends GlobalDeclarationHelper[ArgonModule.GlobalDeclarationFunction] {

    override def getId(decl: ArgonModule.GlobalDeclarationFunction): Int = decl.id
    override def getAccessModifier(decl: ArgonModule.GlobalDeclarationFunction): ArgonModule.AccessModifier = decl.accessModifier

    override def getName(decl: ArgonModule.GlobalDeclarationFunction): ArgonModule.GlobalName = decl.name

    override type ErasedSig = ErasedSignature[context.type]

    override def getErasedSig(decl: ArgonModule.GlobalDeclarationFunction): Comp[ErasedSignature[context.type]] =
      parseErasedSig(decl.sig)
  }

  private def convertNamespaceElement[TDecl, TElem]
  (
    declaration: TDecl
  )(
    declarationHelper: GlobalDeclarationHelper[TDecl]
  )(
    loadElement: Int => Comp[ModuleObjectLoadResult[_, TElem, _]],
  )(
    createBinding: (GlobalName, AccessModifierGlobal, Option[declarationHelper.ErasedSig], Comp[TElem]) => GlobalBinding.NonNamespace[context.type, TPayloadSpec]
  ): Comp[GlobalBinding.NonNamespace[context.type, TPayloadSpec]] =
    for {
      accessModifier <- declarationHelper.getAccessModifier(declaration) match {
        case ParsedAccessModifier(accessModifier: AccessModifierGlobal) => IO.succeed(accessModifier)
        case _ => Compilation.forErrors(invalidModuleFormatError)
      }

      elem <- loadElement(declarationHelper.getId(declaration)).flatMap {
        case ModuleObjectGlobalDefinition(elem) => IO.succeed(elem)
        case ModuleObjectDefinition(_) | ModuleObjectReference(_) =>
          Compilation.forErrors(DiagnosticError.InvalidGlobal(DiagnosticSource.ReferencedModule(currentModuleDescriptor)))
      }.memoize

      name <- declarationHelper.getName(declaration) match {
        case ValidGlobalName(name) => IO.succeed(name)
        case _ => Compilation.forErrors(invalidModuleFormatError)
      }

      sig <- declarationHelper.getErasedSig(declaration)


    } yield createBinding(name, accessModifier, Some(sig), elem)


  private def convertNamespaceElements[TDecl, TElem](id: Int): CompStream[GlobalBinding.NonNamespace[context.type, TPayloadSpec]] =
    source.namespaceElements(id)
      .mapM {
        case GlobalDeclarationElement.TraitElement(traitElement) =>
          convertNamespaceElement(traitElement)(GlobalDeclarationTypeHelper)(getTrait(_))(GlobalBinding.GlobalTrait(_, _, _, _))

        case GlobalDeclarationElement.ClassElement(classElement) =>
          convertNamespaceElement(classElement)(GlobalDeclarationTypeHelper)(getArClass(_))(GlobalBinding.GlobalClass(_, _, _, _))

        case GlobalDeclarationElement.DataConstructorElement(dataConstructorElement) =>
          convertNamespaceElement(dataConstructorElement)(GlobalDeclarationTypeHelper)(getDataCtor(_))(GlobalBinding.GlobalDataConstructor(_, _, _, _))

        case GlobalDeclarationElement.FunctionElement(functionElement) =>
          convertNamespaceElement(functionElement)(GlobalDeclarationFunctionHelper)(getFunction(_))(GlobalBinding.GlobalFunction(_, _, _, _))
      }





  def findTrait(traitId: Int): Comp[AbsRef[context.type, ArTrait]] =
    getTrait(traitId).map {
      case ModuleObjectReference(arTrait) => AbsRef(arTrait)
      case ModuleObjectDefinition(arTrait) => AbsRef(arTrait)
      case ModuleObjectGlobalDefinition(arTrait) => AbsRef(arTrait)
    }


  def findTraitDef(traitId: Int): Comp[ArTrait[context.type, TPayloadSpec]] =
    getTrait(traitId).flatMap {
      case ModuleObjectDefinition(arTrait) => arTrait.pure[Comp]
      case ModuleObjectGlobalDefinition(arTrait) => arTrait.pure[Comp]
      case ModuleObjectReference(_) =>
        Compilation.forErrors(
          DiagnosticError.ModuleObjectMustBeDefinition(DiagnosticError.ModuleObjectTrait, traitId, DiagnosticSource.ReferencedModule(currentModuleDescriptor))
        )
    }

  def findClass(classId: Int): Comp[AbsRef[context.type, ArClass]] =
    getArClass(classId).map {
      case ModuleObjectReference(arClass) => AbsRef(arClass)
      case ModuleObjectDefinition(arClass) => AbsRef(arClass)
      case ModuleObjectGlobalDefinition(arClass) => AbsRef(arClass)
    }


  def findClassDef(classId: Int): Comp[ArClass[context.type, TPayloadSpec]] =
    getArClass(classId).flatMap {
      case ModuleObjectDefinition(arClass) => arClass.pure[Comp]
      case ModuleObjectGlobalDefinition(arClass) => arClass.pure[Comp]

      case ModuleObjectReference(_) =>
        Compilation.forErrors(
          DiagnosticError.ModuleObjectMustBeDefinition(DiagnosticError.ModuleObjectClass, classId, DiagnosticSource.ReferencedModule(currentModuleDescriptor))
        )
    }


  def findDataConstructor(ctorId: Int): Comp[AbsRef[context.type, DataConstructor]] =
    getDataCtor(ctorId).map {
      case ModuleObjectReference(ctor) => AbsRef(ctor)
      case ModuleObjectDefinition(ctor) => AbsRef(ctor)
      case ModuleObjectGlobalDefinition(ctor) => AbsRef(ctor)
    }


  def findDataConstructorDef(ctorId: Int): Comp[DataConstructor[context.type, TPayloadSpec]] =
    getDataCtor(ctorId).flatMap {
      case ModuleObjectDefinition(ctor) => ctor.pure[Comp]
      case ModuleObjectGlobalDefinition(ctor) => ctor.pure[Comp]

      case ModuleObjectReference(_) =>
        Compilation.forErrors(
          DiagnosticError.ModuleObjectMustBeDefinition(DiagnosticError.ModuleObjectDataConstructor, ctorId, DiagnosticSource.ReferencedModule(currentModuleDescriptor))
        )
    }

  def findClassConstructor(ctorId: Int): Comp[AbsRef[context.type, ClassConstructor]] =
    getClassCtor(ctorId).map {
      case ModuleObjectReference(ctor) => AbsRef(ctor)
      case ModuleObjectDefinition(ctor) => AbsRef(ctor)
      case ModuleObjectGlobalDefinition(ctor) => AbsRef(ctor)
    }

  def findFunction(funcId: Int): Comp[AbsRef[context.type, ArFunc]] =
    getFunction(funcId).map {
      case ModuleObjectReference(func) => AbsRef(func)
      case ModuleObjectDefinition(func) => AbsRef(func)
      case ModuleObjectGlobalDefinition(func) => AbsRef(func)
    }

  def findMethod(methodId: Int): Comp[AbsRef[context.type, ArMethod]] =
    getMethod(methodId).map {
      case ModuleObjectReference(method) => AbsRef(method)
      case ModuleObjectDefinition(method) => AbsRef(method)
      case ModuleObjectGlobalDefinition(method) => AbsRef(method)
    }

  def resolveSignatureTypeArgs[TResult[TContext2 <: Context with Singleton, Wrap[+_]], Len <: Nat, T]
  (sig: Signature[TResult, Len], args: NList[Len, ArgonModule.Expression], convArgs: Vector[WrapExpr])
  (f: (Vector[WrapExpr], TResult[context.type, Id]) => T)
  : Comp[T] =
    sig.visit(new SignatureVisitor[TResult, Len, Comp[T]] {

      override def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenIsSuccRest: Len Is Succ[RestLen]): Comp[T] = {
        val argsAsCons = args.asCons
        resolveExpr(argsAsCons.head).flatMap { argType =>
          val nextSig = sigParams.next(argType)
          resolveSignatureTypeArgs(nextSig, argsAsCons.tail, convArgs :+ argType)(f)
        }
      }

      override def visitResult(sigResult: SignatureResult[TResult])(implicit lenZero: Len Is Zero): Comp[T] =
        f(convArgs, sigResult.result).pure[Comp]
    })

  trait SignatureGetter[TResult[_ <: Context with Singleton, Wrap[+_]], TOwner[_ <: Context with Singleton, _[_, _]]] {
    def apply(owner: AbsRef[context.type, TOwner]): Comp[Signature[TResult, _ <: Nat]]
  }

  def resolveUsingSignature[TResult[_ <: Context with Singleton, Wrap[+_]], TOwner[_ <: Context with Singleton, _[_, _]], T]
  (sigOwner: Comp[AbsRef[context.type, TOwner]])
  (sigGetter: SignatureGetter[TResult, TOwner])
  (args: Vector[ArgonModule.Expression])
  (f: AbsRef[context.type, TOwner] => (Vector[WrapExpr], TResult[context.type, Id]) => T)
  : Comp[T] =
    sigOwner.flatMap { owner =>
      sigGetter(owner).flatMap {
        case sig: Signature[TResult, len] =>
          NList.sized[len, ArgonModule.Expression](args)((sig : Signature[TResult, len]).parameterCountToInt) match {
            case Some(sizedArgs) =>
              resolveSignatureTypeArgs[TResult, len, T](sig, sizedArgs, Vector.empty)(f(owner))

            case None => Compilation.forErrors(invalidModuleFormatError)
          }
      }
    }

  def resolveTraitType(traitType: ArgonModule.TraitType): Comp[TTraitType] = {
    val signatureGetter: SignatureGetter[ArTrait.ResultInfo, ArTrait] = owner => owner.value.signature
    resolveUsingSignature(findTrait(traitType.traitId))(signatureGetter)(traitType.typeArguments) {
      arTrait => (args, _) => TraitType[context.type, TTypeWrapper](arTrait, args)
    }
  }

  def resolveClassType(classType: ArgonModule.ClassType): Comp[TClassType] = {
    val signatureGetter: SignatureGetter[ArClass.ResultInfo, ArClass] = owner => owner.value.signature
    resolveUsingSignature(findClass(classType.classId))(signatureGetter)(classType.typeArguments) {
      arClass => (args, _) => ClassType[context.type, TTypeWrapper](arClass, args)
    }
  }

  def resolveDataConstructorType(dataConstructorType: ArgonModule.DataConstructorType): Comp[TDataConstructorType] = {
    val signatureGetter: SignatureGetter[DataConstructor.ResultInfo, DataConstructor] = owner => owner.value.signature
    resolveUsingSignature(findDataConstructor(dataConstructorType.constructorId))(signatureGetter)(dataConstructorType.typeArguments) {
      dataCtor => (args, result) => DataConstructorType[context.type, TTypeWrapper](dataCtor, args, result.instanceType)
    }
  }

  def resolveBigInt(i: ArgonModule.BigInt): Comp[BigInt] =
    i.intType match {
      case ArgonModule.BigInt.IntType.Signed32(value) => IO.succeed(BigInt(value))
      case ArgonModule.BigInt.IntType.Signed64(value) => IO.succeed(BigInt(value))
      case ArgonModule.BigInt.IntType.Bigint(value) => IO.succeed(BigInt(value.toByteArray()))
      case ArgonModule.BigInt.IntType.Empty => Compilation.forErrors(invalidModuleFormatError)
    }

  def unifyLoadResult[TElem[_ <: Context with Singleton, _[_, _]]](loadResult: ModuleObjectLoadResult[TElem[context.type, TPayloadSpec], TElem[context.type, TPayloadSpec], TElem[context.type, ReferencePayloadSpecifier]]): AbsRef[context.type, TElem] =
    loadResult match {
      case ModuleObjectReference(value) => AbsRef(value)
      case ModuleObjectDefinition(value) => AbsRef(value)
      case ModuleObjectGlobalDefinition(value) => AbsRef(value)
    }

  def parseVariableName(variableName: ArgonModule.VariableName): Comp[VariableName] =
    variableName.name match {
      case ArgonModule.VariableName.Name.Normal(value) => IO.succeed(VariableName.Normal(value))
      case ArgonModule.VariableName.Name.Unnamed(_) => IO.succeed(VariableName.Unnamed)
      case ArgonModule.VariableName.Name.Empty => Compilation.forErrors(invalidModuleFormatError)
    }

  def resolveLocalVariable(variable: ArgonModule.LocalVariable): Comp[LocalVariable[context.type, TTypeWrapper]] = for {
    varId <- localVariableIdCache.usingCreate { _ => UniqueIdentifier.make.map(LocalVariableId(_)) }.get(variable.id)
    varOwner <- variable.owner.owner match {
      case ArgonModule.LocalVariableOwner.Owner.ByClass(classId) => getArClass(classId).map(unifyLoadResult(_)).map(LocalVariableOwner.ByClass(_))
      case ArgonModule.LocalVariableOwner.Owner.ByTrait(traitId) => getTrait(traitId).map(unifyLoadResult(_)).map(LocalVariableOwner.ByTrait(_))
      case ArgonModule.LocalVariableOwner.Owner.ByDataConstructor(ctorId) => getDataCtor(ctorId).map(unifyLoadResult(_)).map(LocalVariableOwner.ByDataConstructor(_))
      case ArgonModule.LocalVariableOwner.Owner.ByFunction(funcId) => getFunction(funcId).map(unifyLoadResult(_)).map(LocalVariableOwner.ByFunction(_))
      case ArgonModule.LocalVariableOwner.Owner.ByMethod(methodId) => getMethod(methodId).map(unifyLoadResult(_)).map(LocalVariableOwner.ByMethod(_))
      case ArgonModule.LocalVariableOwner.Owner.ByClassConstructor(ctorId) => getClassCtor(ctorId).map(unifyLoadResult(_)).map(LocalVariableOwner.ByClassConstructor(_))
      case ArgonModule.LocalVariableOwner.Owner.Empty => Compilation.forErrors(invalidModuleFormatError)
    }
    varName <- parseVariableName(variable.name)
    mutability <- parseMutability(variable.mutability)
    varType <- resolveExpr(variable.varType)
  } yield LocalVariable[context.type, TTypeWrapper](varId, varOwner, varName, mutability, isErased = variable.isErased.getOrElse(false), varType)

  def resolveVariable(variable: ArgonModule.Variable): Comp[Variable[context.type, TTypeWrapper]] =
    variable.variableType match {
      case ArgonModule.Variable.VariableType.Local(localVariable) => resolveLocalVariable(localVariable)
      case ArgonModule.Variable.VariableType.Field(fieldVariable) =>
        for {
          arClass <- getArClass(fieldVariable.ownerClass).map(unifyLoadResult(_))
          varName <- parseVariableName(fieldVariable.name).flatMap {
            case varName: VariableName.Normal => IO.succeed(varName)
            case VariableName.Unnamed => Compilation.forErrors(invalidModuleFormatError)
          }
          mutability <- parseMutability(fieldVariable.mutability)
          varType <- resolveExpr(fieldVariable.varType)
        } yield FieldVariable[context.type, TTypeWrapper](FieldVariableOwner(arClass), varName, mutability, varType)

      case ArgonModule.Variable.VariableType.Param(parameterVariable) =>
        for {
          varOwner <- parameterVariable.owner.owner match {
            case ArgonModule.ParameterVariableOwner.Owner.ByClass(classId) => getArClass(classId).map(unifyLoadResult(_)).map(ParameterVariableOwner.ByClass(_))
            case ArgonModule.ParameterVariableOwner.Owner.ByTrait(traitId) => getTrait(traitId).map(unifyLoadResult(_)).map(ParameterVariableOwner.ByTrait(_))
            case ArgonModule.ParameterVariableOwner.Owner.ByDataConstructor(ctorId) => getDataCtor(ctorId).map(unifyLoadResult(_)).map(ParameterVariableOwner.ByDataConstructor(_))
            case ArgonModule.ParameterVariableOwner.Owner.ByFunction(funcId) => getFunction(funcId).map(unifyLoadResult(_)).map(ParameterVariableOwner.ByFunction(_))
            case ArgonModule.ParameterVariableOwner.Owner.ByMethod(methodId) => getMethod(methodId).map(unifyLoadResult(_)).map(ParameterVariableOwner.ByMethod(_))
            case ArgonModule.ParameterVariableOwner.Owner.ByClassConstructor(ctorId) => getClassCtor(ctorId).map(unifyLoadResult(_)).map(ParameterVariableOwner.ByClassConstructor(_))
            case ArgonModule.ParameterVariableOwner.Owner.Empty => Compilation.forErrors(invalidModuleFormatError)
          }
          varName <- parseVariableName(parameterVariable.name)
          mutability <- parseMutability(parameterVariable.mutability)
          varType <- resolveExpr(parameterVariable.varType)
        } yield ParameterVariable[context.type, TTypeWrapper](varOwner, parameterVariable.index, varName, mutability, isErased = parameterVariable.isErased.getOrElse(false), varType)

      case ArgonModule.Variable.VariableType.Empty => Compilation.forErrors(invalidModuleFormatError)
    }

  def resolveUniverse(universe: ArgonModule.UniverseExpr): Comp[UniverseExpr] =
    universe.exprType match {
      case ArgonModule.UniverseExpr.ExprType.Empty => Compilation.forErrors(invalidModuleFormatError)
      case ArgonModule.UniverseExpr.ExprType.Fixed(value) => resolveBigInt(value).map(FixedUniverse.apply)
      case ArgonModule.UniverseExpr.ExprType.AbstractUniverse(_) => IO.succeed(AbstractUniverse())
      case ArgonModule.UniverseExpr.ExprType.LargestUniverse(inner) =>
        for {
          a <- resolveUniverse(inner.a)
          b <- resolveUniverse(inner.b)
        } yield LargestUniverse(a, b)

      case ArgonModule.UniverseExpr.ExprType.NextLargestUniverse(inner) =>
        resolveUniverse(inner).map(NextLargestUniverse.apply)

      case ArgonModule.UniverseExpr.ExprType.PreviousUniverse(inner) =>
        resolveUniverse(inner).map(PreviousUniverse.apply)
    }

  def resolveExpr(t: ArgonModule.Expression): Comp[TType] = {
    def requireArgsLength(len: Int): Comp[Unit] =
      Compilation.require(t.args.size === len)(invalidModuleFormatError)

    def sizedArgs[N <: Nat: Nat.ToInt]: Comp[NList[N, ArgonModule.Expression]] =
      IO.fromEither(NList.sized(t.args).toRight(invalidModuleFormatError))

    t.exprType match {
      case ArgonModule.Expression.ExprType.TraitType(traitType) => resolveTraitType(ArgonModule.TraitType(traitType, t.args))
      case ArgonModule.Expression.ExprType.ClassType(classType) => resolveClassType(ArgonModule.ClassType(classType, t.args))
      case ArgonModule.Expression.ExprType.DataConstructorType(dataConstructorType) => resolveDataConstructorType(ArgonModule.DataConstructorType(dataConstructorType, t.args))
      case ArgonModule.Expression.ExprType.TypeOfType(_) =>
        for {
          args <- sizedArgs[Succ[Zero]]
          inner <- resolveExpr(args.head)
        } yield TypeOfType[context.type, TTypeWrapper](inner)

      case ArgonModule.Expression.ExprType.TypeN(typeN) =>
        for {
          _ <- requireArgsLength(0)
          univ <- resolveUniverse(typeN.universe)
          subtypeConstraint <- ZIO.foreach(typeN.subtypeConstraint)(resolveExpr(_))
          supertypeConstraint <- ZIO.foreach(typeN.supertypeConstraint)(resolveExpr(_))
        } yield TypeN[context.type, TTypeWrapper](univ, subtypeConstraint, supertypeConstraint)

      case ArgonModule.Expression.ExprType.FunctionType(_) =>
        for {
          args <- sizedArgs[Succ[Succ[Zero]]]
          argType <- resolveExpr(args.head)
          resultType <- resolveExpr(args.tail.head)
        } yield FunctionType[context.type, TTypeWrapper](argType, resultType)

      case ArgonModule.Expression.ExprType.UnionType(_) =>
        for {
          args <- sizedArgs[Succ[Succ[Zero]]]
          a <- resolveExpr(args.head)
          b <- resolveExpr(args.tail.head)
        } yield UnionType[context.type, TTypeWrapper](a, b)

      case ArgonModule.Expression.ExprType.IntersectionType(_) =>
        for {
          args <- sizedArgs[Succ[Succ[Zero]]]
          a <- resolveExpr(args.head)
          b <- resolveExpr(args.tail.head)
        } yield IntersectionType[context.type, TTypeWrapper](a, b)

      case ArgonModule.Expression.ExprType.ExistentialType(variable) =>
        for {
          localVar <- resolveLocalVariable(variable)
          args <- sizedArgs[Succ[Zero]]
          inner <- resolveExpr(args.head)
        } yield ExistentialType[context.type, TTypeWrapper](localVar, inner)

      case ArgonModule.Expression.ExprType.ClassConstructorCall(id) =>
        for {
          ctor <- findClassConstructor(id)
          args <- ZIO.foreach(t.args)(resolveExpr(_))
          classAndArgs <- args match {
            case (classType: ClassType[context.type, TTypeWrapper]) +: ctorArgs =>
              IO.succeed((classType, ctorArgs))

            case _ => Compilation.forErrors(invalidModuleFormatError)
          }
          (classType, ctorArgs) = classAndArgs
        } yield ClassConstructorCall(classType, ctor, ctorArgs.toVector)

      case ArgonModule.Expression.ExprType.DataConstructorCall(_) =>
        for {
          args <- ZIO.foreach(t.args)(resolveExpr(_))
          dataCtorArgs <- args match {
            case (dataCtor: DataConstructorType[context.type, TTypeWrapper]) +: ctorArgs =>
              IO.succeed((dataCtor, ctorArgs))

            case _ => Compilation.forErrors(invalidModuleFormatError)
          }
          (dataCtor, ctorArgs) = dataCtorArgs
        } yield DataConstructorCall(dataCtor, ctorArgs.toVector)

      case ArgonModule.Expression.ExprType.EnsureExecuted(_) =>
        for {
          args <- sizedArgs[Succ[Succ[Zero]]]
          body <- resolveExpr(args.head)
          ensuring <- resolveExpr(args.tail.head)
        } yield EnsureExecuted[context.type, TTypeWrapper](body, ensuring)

      case ArgonModule.Expression.ExprType.FunctionCall(id) =>
        val signatureGetter: SignatureGetter[FunctionResultInfo, ArFunc] = owner => owner.value.signature
        resolveUsingSignature(findFunction(id))(signatureGetter)(t.args) {
          func => (args, resultInfo) => FunctionCall[context.type, TTypeWrapper](func, args, resultInfo.returnType)
        }

      case ArgonModule.Expression.ExprType.FunctionObjectCall(_) =>
        for {
          args <- sizedArgs[Succ[Succ[Succ[Zero]]]]
          func <- resolveExpr(args.head)
          arg <- resolveExpr(args.tail.head)
          returnType <- resolveExpr(args.tail.tail.head)
        } yield FunctionObjectCall[context.type, TTypeWrapper](func, arg, returnType)

      case ArgonModule.Expression.ExprType.IfElse(_) =>
        for {
          args <- sizedArgs[Succ[Succ[Succ[Zero]]]]
          cond <- resolveExpr(args.head)
          ifBody <- resolveExpr(args.tail.head)
          elseBody <- resolveExpr(args.tail.tail.head)
        } yield IfElse[context.type, TTypeWrapper](cond, ifBody, elseBody)

      case ArgonModule.Expression.ExprType.LetBinding(localVariable) =>
        for {
          variable <- resolveLocalVariable(localVariable)
          args <- sizedArgs[Succ[Succ[Zero]]]
          value <- resolveExpr(args.head)
          next <- resolveExpr(args.tail.head)
        } yield LetBinding[context.type, TTypeWrapper](variable, value, next)

      case ArgonModule.Expression.ExprType.LoadConstantBool(b) =>
        for {
          args <- sizedArgs[Succ[Zero]]
          boolType <- resolveExpr(args.head)
        } yield LoadConstantBool[context.type, TTypeWrapper](b, boolType)

      case ArgonModule.Expression.ExprType.LoadConstantInt(i) =>
        for {
          args <- sizedArgs[Succ[Zero]]
          intType <- resolveExpr(args.head)
          value <- resolveBigInt(i)
        } yield LoadConstantInt[context.type, TTypeWrapper](value, intType)

      case ArgonModule.Expression.ExprType.LoadConstantString(s) =>
        for {
          args <- sizedArgs[Succ[Zero]]
          strType <- resolveExpr(args.head)
        } yield LoadConstantString[context.type, TTypeWrapper](s, strType)

      case ArgonModule.Expression.ExprType.LoadLambda(variable) =>
        for {
          args <- sizedArgs[Succ[Zero]]
          convVariable <- resolveLocalVariable(variable)
          body <- resolveExpr(args.head)
        } yield LoadLambda[context.type, TTypeWrapper](convVariable, body)


      case ArgonModule.Expression.ExprType.LoadTuple(tupleMetadata) =>
        for {
          _ <- Compilation.require(tupleMetadata.elements.size === t.args.size)(invalidModuleFormatError)
          elems <- ZIO.foreach(tupleMetadata.elements zip t.args) {
            case (ArgonModule.TupleElement(), arg) =>
              resolveExpr(arg).map(TupleElement[context.type, TTypeWrapper](_))
          }
          elemsNonEmpty <-
            IO.fromEither(
              NonEmptyList.fromList(elems.toList)
                .toRight { invalidModuleFormatError }
            )

        } yield LoadTuple[context.type, TTypeWrapper](elemsNonEmpty)

      case ArgonModule.Expression.ExprType.LoadTupleElement(index) =>
        for {
          args <- sizedArgs[Succ[Succ[Zero]]]
          tuple <- resolveExpr(args.head)
          elemType <- resolveExpr(args.tail.head)
        } yield LoadTupleElement[context.type, TTypeWrapper](tuple, elemType, index)

      case ArgonModule.Expression.ExprType.LoadUnit(_) =>
        for {
          args <- sizedArgs[Succ[Zero]]
          unitType <- resolveExpr(args.head)
        } yield LoadUnit[context.type, TTypeWrapper](unitType)

      case ArgonModule.Expression.ExprType.LoadVariable(variable) =>
        for {
          v <- resolveVariable(variable)
        } yield LoadVariable[context.type, TTypeWrapper](v)

      case ArgonModule.Expression.ExprType.MethodCall(id) =>
        t.args match {
          case instanceExpr +: instanceType +: args =>
            resolveExpr(instanceExpr).flatMap { instance =>
              resolveExpr(instanceType).flatMap {
                case instanceType: TypeWithMethods[context.type, TTypeWrapper] =>
                  resolveUsingSignature(findMethod(id))(_.value.signature(context.signatureContext)(instanceType))(args) {
                    method => (args, resultInfo) => MethodCall[context.type, TTypeWrapper](method, instance, instanceType, args, resultInfo.returnType)
                  }

                case _ => Compilation.forErrors(invalidModuleFormatError)
              }
            }

          case _ => Compilation.forErrors(invalidModuleFormatError)
        }


      case ArgonModule.Expression.ExprType.PatternMatch(_) => ???


      case ArgonModule.Expression.ExprType.Sequence(_) =>
        def convertSequence(rest: NonEmptyVector[ArgonModule.Expression]): Comp[WrapExpr] =
          rest match {
            case NonEmptyVector(head, next +: tail) =>
              for {
                convHead <- resolveExpr(head)
                convTail <- convertSequence(NonEmptyVector(next, tail))
              } yield Sequence[context.type, TTypeWrapper](convHead, convTail)

            case NonEmptyVector(head, Vector()) =>
              resolveExpr(head)

          }

        NonEmptyVector.fromVector(t.args) match {
          case Some(value) => convertSequence(value)
          case None => Compilation.forErrors(invalidModuleFormatError)
        }

      case ArgonModule.Expression.ExprType.StoreVariable(variable) =>
        for {
          v <- resolveVariable(variable)
          args <- sizedArgs[Succ[Succ[Zero]]]
          exprType <- resolveExpr(args.head)
          value <- resolveExpr(args.tail.head)
        } yield StoreVariable[context.type, TTypeWrapper](v, exprType, value)


      case ArgonModule.Expression.ExprType.Empty => Compilation.forErrors(invalidModuleFormatError)
    }
  }

  def resolveParameter(owner: ParameterVariableOwner[context.type])(index: Int)(parameter: ArgonModule.Parameter): Comp[Parameter[context.type, Id]] = {
    val parameterStyle =
      parameter.style match {
        case Some(ArgonModule.ParameterStyle.Normal) | None => ParameterStyle.Normal
        case Some(ArgonModule.ParameterStyle.Inferrable) => ParameterStyle.Inferrable
        case Some(ArgonModule.ParameterStyle.Unrecognized(_)) => ???
      }

    parameter.elements
      .traverse { case ArgonModule.ParameterElement(name, paramType) =>
        val varName = name.map(VariableName.Normal.apply).getOrElse(VariableName.Unnamed)

        resolveExpr(paramType)
          .map { t => (t, varName) }
      }
      .flatMap {
        case VectorUnCons(VectorUnCons.Empty) =>
          for {
            currentModule <- module
            unitType <- StandardTypeLoaders.loadUnitType(context)(
              DiagnosticSource.ReferencedModule(currentModuleDescriptor)
            )(currentModule)(referencedModules)

          } yield Parameter[context.type, Id](
            parameterStyle,
            ParameterVariable[context.type, Id](owner, index, VariableName.Unnamed, Mutability.NonMutable, isErased = parameter.isErased.getOrElse(false), unitType),
            Vector()
          )

        case VectorUnCons(VectorUnCons.NonEmpty((t, varName), VectorUnCons(VectorUnCons.Empty))) =>
          Parameter(
            parameterStyle,
            ParameterVariable[context.type, Id](owner, index, varName, Mutability.NonMutable, isErased = parameter.isErased.getOrElse(false), t),
            Vector()
          ).pure[Comp]

        case elems @ VectorUnCons(VectorUnCons.NonEmpty(head, tail)) =>

          val paramType = context.typeSystem.fromSimpleType(LoadTuple(
            NonEmptyList(head, tail.toList).map { case (t, _) => TupleElement[context.type, Id](t) }
          ))

          val paramVar = ParameterVariable[context.type, Id](owner, index, VariableName.Unnamed, Mutability.NonMutable, isErased = parameter.isErased.getOrElse(false), paramType)
          val paramElems = elems.zipWithIndex.map { case ((t, name), elemIndex) => ParameterElement[context.type, Id](paramVar, name, t, elemIndex) }

          Parameter(parameterStyle, paramVar, paramElems).pure[Comp]
      }
  }

  override lazy val module: Comp[ArModule[context.type, TPayloadSpec]] =
    moduleCache.get(
      for {
        namespaceStream <- StreamMemo.make(
          source.namespaces.map { decl => (parseNamespacePath(decl.ns), decl.id) }
        )

        bindingStreamCache <- MemoCacheManagedStore.make[CompilationError, NamespacePath, Stream[CompilationError, GlobalBinding.NonNamespace[context.type, TPayloadSpec]]]
      } yield new ArModule[context.type, TPayloadSpec] {
        override val context: ModuleCreatorCommon.this.context.type = ModuleCreatorCommon.this.context
        override val id: ModuleId = currentModuleDescriptor

        override val namespaces: CompStream[NamespacePath] =
          namespaceStream.map { case (ns, _) => ns }

        override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] =
          refModuleMap.values.toVector.collect {
            case ModuleReference(moduleRef) => moduleRef
          }

        override def getNamespace(ns: NamespacePath): CompStream[GlobalBinding.NonNamespace[context.type, TPayloadSpec]] = {
          ZStream.unwrap(
            bindingStreamCache.usingCreate { ns =>
              StreamMemo.make(
                namespaceStream
                  .filter { case (ns2, _) => ns2 === ns }
                  .take(1)
                  .flatMap { case (_, id) =>
                    convertNamespaceElements(id)
                  }
              )
            }.get(ns)
          )
        }
      }
    )


}

object ModuleCreatorCommon {

  sealed trait ModuleLoadResult[TContext <: Context with Singleton]
  final case class ModuleReference[TContext <: Context with Singleton](module: ArModule[TContext, ReferencePayloadSpecifier]) extends ModuleLoadResult[TContext]
  final case class ModuleNotFound[TContext <: Context with Singleton](moduleRef: ArgonModule.ModuleReference) extends ModuleLoadResult[TContext]

  sealed trait ModuleObjectLoadResult[+TDefinition, +TGlobalDef <: TDefinition, +TReference]
  final case class ModuleObjectReference[+TDefinition, +TGlobalDef <: TDefinition, +TReference](value: TReference) extends ModuleObjectLoadResult[TDefinition, TGlobalDef, TReference]
  final case class ModuleObjectDefinition[+TDefinition, +TGlobalDef <: TDefinition, +TReference](value: TDefinition) extends ModuleObjectLoadResult[TDefinition, TGlobalDef, TReference]
  final case class ModuleObjectGlobalDefinition[+TDefinition, +TGlobalDef <: TDefinition, +TReference](value: TGlobalDef) extends ModuleObjectLoadResult[TDefinition, TGlobalDef, TReference]


  type TraitLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArTrait[TContext, PayloadSpec], ArTrait.InNamespace[TContext, PayloadSpec], ArTrait[TContext, ReferencePayloadSpecifier]]
  type ClassLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArClass[TContext, PayloadSpec], ArClass.InNamespace[TContext, PayloadSpec], ArClass[TContext, ReferencePayloadSpecifier]]
  type DataCtorLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[DataConstructor[TContext, PayloadSpec], DataConstructor.InNamespace[TContext, PayloadSpec], DataConstructor[TContext, ReferencePayloadSpecifier]]
  type FunctionLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArFunc[TContext, PayloadSpec], ArFunc.InNamespace[TContext, PayloadSpec], ArFunc[TContext, ReferencePayloadSpecifier]]
  type MethodLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArMethod[TContext, PayloadSpec], ArMethod[TContext, PayloadSpec] { val owner: Nothing }, ArMethod[TContext, ReferencePayloadSpecifier]]
  type ClassCtorLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ClassConstructor[TContext, PayloadSpec], ClassConstructor[TContext, PayloadSpec] { val owner: Nothing }, ClassConstructor[TContext, ReferencePayloadSpecifier]]
}
