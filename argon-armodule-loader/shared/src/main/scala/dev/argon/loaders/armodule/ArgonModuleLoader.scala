package dev.argon.loaders.armodule

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup._
import dev.argon.compiler.loaders.{ModuleLoad, ModuleLoader, ModuleMetadata, NamespaceBuilder, ResourceIndicator, StandardTypeLoaders}
import dev.argon.compiler.types._
import dev.argon.{module => ArgonModule}
import dev.argon.util._
import cats.{Id => _, _}
import cats.evidence.{===, Is}
import cats.data.NonEmptyList
import cats.implicits._
import dev.argon.backend.{ResourceAccess, ResourceReader}
import dev.argon.compiler.expr._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.module.Metadata
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import shapeless.ops.nat.{LT, Pred, ToInt}
import shapeless.{Nat, Sized, Succ, _0, Id}
import shapeless.syntax.sized._
import zio.{IO, Managed, ZIO, ZManaged}
import zio.interop.catz._

import scala.collection.immutable.{Map, Vector}

object ArgonModuleLoader {

  def apply[I <: ResourceIndicator, TContext <: Context.WithRes[I]](res: ResourceReader.Service[I])(implicit referencePayloadLoader: PayloadLoader[TContext, ReferencePayloadSpecifier]): ModuleLoad.Service[I, TContext] = new ModuleLoad.Service[I, TContext] {

    override def loadResource(id: I): Managed[ErrorList, Option[ModuleMetadata[TContext]]] =
      id.extension match {
        case "armodule" =>
          res.getZipReader(id).mapM { zip =>
            val entry = res.zipEntryStream(zip, ModulePaths.metadata)
            res.deserializeProtocolBuffer(ArgonModule.Metadata)(entry)
              .map { metadata =>
                Some(ArModuleMetadata(zip, metadata))
              }
          }

        case _ => ZManaged.succeed(None)
      }

    private final case class ArModuleMetadata(zip: res.ZipReader, metadata: Metadata) extends ModuleMetadata[TContext] {
      override val descriptor: ModuleDescriptor = ModuleDescriptor(metadata.name)
      override val referencedModules: Vector[ModuleDescriptor] =
        metadata.references.map {
          case ArgonModule.ModuleReference(name, _) => ModuleDescriptor(name)
        }

      override def loadReference(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): Comp[ArModule[context.type, ReferencePayloadSpecifier]] =
        loadModule[ReferencePayloadSpecifier](context)(zip)(metadata)(referencedModules)(referencePayloadLoader)
    }

    private def loadModule[TPayloadSpec[_, _]]
    (context: TContext)
    (zipFile: res.ZipReader)
    (metadata: ArgonModule.Metadata)
    (referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]])
    (payloadLoader: PayloadLoader[TContext, TPayloadSpec])
    : Comp[ArModule[context.type, TPayloadSpec]] = {

      val context2: context.type = context
      import context.{typeSystem, signatureContext}, typeSystem.{ context => _, _ }, signatureContext.{ context => _, TTypeWrapper => _, typeWrapperInstances => _, _ }

      val currentModuleDescriptor = ModuleDescriptor(metadata.name)


      trait ModuleCreator {
        val module: Comp[ArModule[context.type, TPayloadSpec]]
      }

      for {

        _ <- Compilation.require(
          metadata.formatVersion > 0 && metadata.formatVersion <= ModuleFormatVersion.currentVersion
        )(
          CompilationError.UnsupportedModuleFormatVersion(metadata.formatVersion, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
        )

        moduleCache <- ValueCache.make[ErrorList, ArModule[context.type, TPayloadSpec]]
        traitCache <- MemoCacheStore.make[ErrorList, Int, TraitLoadResult[context.type, TPayloadSpec]]
        classCache <- MemoCacheStore.make[ErrorList, Int, ClassLoadResult[context.type, TPayloadSpec]]
        dataCtorCache <- MemoCacheStore.make[ErrorList, Int, DataCtorLoadResult[context.type, TPayloadSpec]]
        functionCache <- MemoCacheStore.make[ErrorList, Int, FunctionLoadResult[context.type, TPayloadSpec]]
        methodCache <- MemoCacheStore.make[ErrorList, Int, MethodLoadResult[context.type, TPayloadSpec]]
        classCtorCache <- MemoCacheStore.make[ErrorList, Int, ClassCtorLoadResult[context.type, TPayloadSpec]]

        module <- new ModuleCreator {


          private def parseNamespacePath(ns: ArgonModule.Namespace): NamespacePath =
            NamespacePath(ns.path)

          private object ValidGlobalName {
            def unapply(globalName: ArgonModule.GlobalName): Option[GlobalName] =
              globalName.globalName match {
                case ArgonModule.GlobalName.GlobalName.NormalName(name) => Some(GlobalName.Normal(name))
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

          private object ParsedGlobalAccessModifier {
            def unapply(accessModifier: ArgonModule.AccessModifier): Option[AccessModifierGlobal] =
              accessModifier match {
                case ParsedAccessModifier(accessModifier: AccessModifierGlobal) => Some(accessModifier)
                case _ => None
              }
          }



          private lazy val refModuleMap: Map[Int, ModuleLoadResult[context.type]] =
            metadata.references.zipWithIndex
              .map { case (modRef, i) =>
                val moduleLoadRes: ModuleLoadResult[context.type] =
                  referencedModules
                    .find { _.descriptor.name.contains(modRef.name) }
                  match {
                    case Some(referencedModule) => ModuleReference(referencedModule)
                    case None => ModuleNotFound(modRef)
                  }

                i + 1 -> moduleLoadRes
              }
              .toMap

          private def lookupNamespaceValue[T]
          (refModule: ArModule[context.type, ReferencePayloadSpecifier])
          (namespace: NamespacePath, name: GlobalName)
          (f: PartialFunction[GlobalBinding[context.type, ReferencePayloadSpecifier], T])
          : Comp[Option[T]] =
            ModuleLookup.lookupNamespaceValues(context)(refModule)(namespace, name)(f).map {
              case Vector(single) => Some(single)
              case Vector() => None
              case _ => ???
            }

          private def parseTraitDescriptor(id: Int)(module: ModuleDescriptor)(desc: ArgonModule.TraitDescriptor): Option[Either[TraitDescriptor, TraitDescriptor.InNamespace]] =
            desc.descriptor match {
              case ArgonModule.TraitDescriptor.Descriptor.InNamespace(
                ArgonModule.InNamespaceDescriptor(
                  ns,
                  ValidGlobalName(name),
                  _
                )
              ) =>
                Some(Right(TraitDescriptor.InNamespace(module, id, parseNamespacePath(ns), name)))

              case ArgonModule.TraitDescriptor.Descriptor.Empty =>
                None
            }

          private def parseClassDescriptor(id: Int)(module: ModuleDescriptor)(desc: ArgonModule.ClassDescriptor): Option[Either[ClassDescriptor, ClassDescriptor.InNamespace]] =
            desc.descriptor match {
              case ArgonModule.ClassDescriptor.Descriptor.InNamespace(
                ArgonModule.InNamespaceDescriptor(
                  ns,
                  ValidGlobalName(name),
                  _
                )
              ) =>
                Some(Right(ClassDescriptor.InNamespace(module, id, parseNamespacePath(ns), name)))

              case ArgonModule.ClassDescriptor.Descriptor.Empty =>
                None
            }

          private def parseDataCtorDescriptor(id: Int)(module: ModuleDescriptor)(desc: ArgonModule.DataConstructorDescriptor): Option[Either[DataConstructorDescriptor, DataConstructorDescriptor.InNamespace]] =
            desc.descriptor match {
              case ArgonModule.DataConstructorDescriptor.Descriptor.InNamespace(
                ArgonModule.InNamespaceDescriptor(
                  ns,
                  ValidGlobalName(name),
                  _
                )
              ) =>
                Some(Right(DataConstructorDescriptor.InNamespace(module, id, parseNamespacePath(ns), name)))

              case ArgonModule.DataConstructorDescriptor.Descriptor.Empty =>
                None
            }

          private def parseFunctionDescriptor(id: Int)(module: ModuleDescriptor)(desc: ArgonModule.FunctionDescriptor): Option[Either[FuncDescriptor, FuncDescriptor.InNamespace]] =
            desc.descriptor match {
              case ArgonModule.FunctionDescriptor.Descriptor.InNamespace(
                ArgonModule.InNamespaceDescriptor(
                  ns,
                  ValidGlobalName(name),
                  _
                )
              ) =>
                Some(Right(FuncDescriptor.InNamespace(module, id, parseNamespacePath(ns), name)))

              case ArgonModule.FunctionDescriptor.Descriptor.Empty =>
                None
            }

          private def parseMemberName(memberName: ArgonModule.MethodDescriptor.MemberName): Option[MethodName] =
            memberName match {

              case ArgonModule.MethodDescriptor.MemberName.Empty => None
              case ArgonModule.MethodDescriptor.MemberName.SpecialMethodName(ArgonModule.SpecialMethodName.Unrecognized(_)) => None

              case ArgonModule.MethodDescriptor.MemberName.Name(name) =>
                Some(MemberName.Normal(name))

              case ArgonModule.MethodDescriptor.MemberName.Mutator(name) =>
                Some(MemberName.Mutator(name))

              case ArgonModule.MethodDescriptor.MemberName.SpecialMethodName(ArgonModule.SpecialMethodName.Unnamed) =>
                Some(MemberName.Unnamed)

              case ArgonModule.MethodDescriptor.MemberName.SpecialMethodName(ArgonModule.SpecialMethodName.Call) =>
                Some(MemberName.Call)

            }

          private def parseMethodDescriptor(module: ModuleDescriptor)(desc: ArgonModule.MethodDescriptor): Option[Either[MethodDescriptor, Nothing]] =
            desc match {
              case ArgonModule.MethodDescriptor(index, memberName, instanceTypeId, ownerDesc, _) =>
                for {
                  memberNameValue <- parseMemberName(memberName)
                  ownerDescValue <- ownerDesc match {
                    case ArgonModule.MethodDescriptor.InstanceType.Empty => None
                    case ArgonModule.MethodDescriptor.InstanceType.TraitDescriptor(traitDesc) => parseTraitDescriptor(instanceTypeId)(module)(traitDesc).map(_.merge)
                    case ArgonModule.MethodDescriptor.InstanceType.ClassDescriptor(classDesc) => parseClassDescriptor(instanceTypeId)(module)(classDesc).map(_.merge)
                    case ArgonModule.MethodDescriptor.InstanceType.TraitObjectDescriptor(traitDesc) => parseTraitDescriptor(instanceTypeId)(module)(traitDesc).map(_.merge).map(TraitObjectDescriptor.apply)
                    case ArgonModule.MethodDescriptor.InstanceType.ClassObjectDescriptor(classDesc) => parseClassDescriptor(instanceTypeId)(module)(classDesc).map(_.merge).map(ClassObjectDescriptor.apply)
                    case ArgonModule.MethodDescriptor.InstanceType.DataConstructorDescriptor(dataCtorDesc) => parseDataCtorDescriptor(instanceTypeId)(module)(dataCtorDesc).map(_.merge)
                  }
                } yield Left(MethodDescriptor(ownerDescValue, 0, memberNameValue))
            }

          private def parseClassCtorDescriptor(module: ModuleDescriptor)(desc: ArgonModule.ClassConstructorDescriptor): Option[Either[ClassConstructorDescriptor, Nothing]] =
            desc match {
              case ArgonModule.ClassConstructorDescriptor(ownerClass, index, instanceClassId, _) =>
                for {
                  ownerDescValue <- parseClassDescriptor(instanceClassId)(module)(ownerClass).map(_.merge)
                } yield Left(ClassConstructorDescriptor(ownerDescValue, index))
            }

          private def convertFileId(id: ArgonModule.FileID): FileID =
            FileID(id.id)

          trait ObjectDefinitionLoader[TDef, TDescriptor <: AnyRef, TDefResult] {
            def apply(id: Int, definition: TDef, desc: TDescriptor): Comp[TDefResult { val descriptor: desc.type }]
          }

          private def handleModuleObjectLoading
          [
            TRef <: GeneratedMessage,
            TDef <: GeneratedMessage,
            TValueDescriptor,
            TResultDescriptor <: AnyRef,
            TGlobalDescriptor <: TResultDescriptor,
            TRefResult,
            TDefResult,
          ](
           cache: MemoCacheStore[ErrorList, Int, ModuleObjectLoadResult[TDefResult, TDefResult { val descriptor: TGlobalDescriptor }, TRefResult]]
          )(
            moduleObjectType: CompilationError.ModuleObjectType,
          )(
            refPathFunction: Int => String,
            defPathFunction: Int => String,
            refCompanion: GeneratedMessageCompanion[TRef],
            defCompanion: GeneratedMessageCompanion[TDef],
          )(
            refModuleIdLens: TRef => Int,
            refDescriptorLens: TRef => TValueDescriptor,
            defDescriptorLens: TDef => TValueDescriptor,
          )(
            parseDescriptor: ModuleDescriptor => TValueDescriptor => Option[Either[TResultDescriptor, TGlobalDescriptor]]
          )(
            referenceHandler: ArModule[context.type, ReferencePayloadSpecifier] => TResultDescriptor => Comp[Option[TRefResult]],
            definitionHandler: ObjectDefinitionLoader[TDef, TResultDescriptor, TDefResult],
          ): MemoCache[Any, ErrorList, Int, ModuleObjectLoadResult[TDefResult, TDefResult { val descriptor: TGlobalDescriptor }, TRefResult]] =
            cache.usingCreate { id =>

              val zip = zipFile

              if(id < 0) {
                val entry = res.zipEntryStream(zip, refPathFunction(id.abs))
                res.deserializeProtocolBuffer(refCompanion)(entry)
                  .flatMap { refValue =>
                    refModuleMap.get(refModuleIdLens(refValue)) match {
                      case Some(ModuleReference(moduleRef)) =>
                        parseDescriptor(moduleRef.descriptor)(refDescriptorLens(refValue)) match {
                          case Some(descriptor) =>
                            referenceHandler(moduleRef)(descriptor.merge).flatMap {
                              case Some(refResult) => IO.succeed(ModuleObjectReference(refResult))
                              case None => Compilation.forErrors(CompilationError.ModuleObjectNotFound(
                                moduleObjectType, id, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                              ))
                            }

                          case None =>
                            Compilation.forErrors(CompilationError.ModuleObjectInvalidDescriptor(
                              moduleObjectType, id, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                            ))
                        }

                      case _ =>
                        Compilation.forErrors(CompilationError.ModuleObjectModuleNotLoaded(
                          moduleObjectType, id, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                        ))
                    }
                  }
              }
              else {
                val entry = res.zipEntryStream(zip, defPathFunction(id))
                res.deserializeProtocolBuffer(defCompanion)(entry)
                  .flatMap { defValue =>
                    parseDescriptor(currentModuleDescriptor)(defDescriptorLens(defValue)) match {
                      case Some(Left(descriptor)) =>
                        definitionHandler(id, defValue, descriptor).map(ModuleObjectDefinition.apply)

                      case Some(Right(descriptor)) =>
                        definitionHandler(id, defValue, descriptor).map(ModuleObjectGlobalDefinition.apply)

                      case None =>
                        Compilation.forErrors(CompilationError.ModuleObjectInvalidDescriptor(
                          moduleObjectType, id, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                        ))
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
                    MethodBinding(method.descriptor.name, method.descriptor.index, accessModifier, method)
                }
              }

          private def lookupTrait(module: ArModule[context.type, ReferencePayloadSpecifier]): TraitDescriptor => Comp[Option[ArTrait[context.type, ReferencePayloadSpecifier]]] = {
            case TraitDescriptor.InNamespace(_, _, namespace, name) =>
              lookupNamespaceValue(module)(namespace, name) {
                case GlobalBinding.GlobalTrait(_, _, arTrait) => arTrait
              }
          }

          private def getTrait(id: Int): Comp[TraitLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading
            [
              ArgonModule.TraitReference,
              ArgonModule.TraitDefinition,
              ArgonModule.TraitDescriptor,
              TraitDescriptor,
              TraitDescriptor.InNamespace,
              ArTrait[context.type, ReferencePayloadSpecifier],
              ArTrait[context.type, TPayloadSpec]
            ](
              traitCache
            )(
              CompilationError.ModuleObjectTrait
            )(
              refPathFunction = ModulePaths.traitRef,
              defPathFunction = ModulePaths.traitDef,
              refCompanion = ArgonModule.TraitReference,
              defCompanion = ArgonModule.TraitDefinition,
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseTraitDescriptor(id)
            )(
              referenceHandler = lookupTrait(_),
              definitionHandler = new ObjectDefinitionLoader[ArgonModule.TraitDefinition, TraitDescriptor, ArTrait[context.type, TPayloadSpec]] {
                override def apply(id: Int, definition: ArgonModule.TraitDefinition, desc: TraitDescriptor): Comp[ArTrait[context.type, TPayloadSpec] { val descriptor: desc.type }] =
                  new ArTrait[context.type, TPayloadSpec] {
                    override val context: context2.type = context2
                    override val contextProof: context.type Is context.type = Is.refl

                    override val descriptor: desc.type = desc
                    override val fileId: FileID = convertFileId(definition.fileId)

                    override val isSealed: Boolean = definition.isSealed.getOrElse(false)

                    override lazy val signature: Comp[Signature[ArTrait.ResultInfo, _ <: Nat]] =
                      definition.signature match {
                        case ArgonModule.TraitSignature(parameters, baseTraits, _) =>
                          for {
                            baseTraitsResolved <- baseTraits.traverse(resolveTraitType(_))
                            parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

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

                    override lazy val payload: TPayloadSpec[Unit, context.TTraitMetadata] = payloadLoader.createTraitPayload(context)
                  }.pure[Comp]
              }
            ).get(id)

          private def lookupClass(module: ArModule[context.type, ReferencePayloadSpecifier]): ClassDescriptor => Comp[Option[ArClass[context.type, ReferencePayloadSpecifier]]] = {
            case ClassDescriptor.InNamespace(_, _, namespace, name) =>
              lookupNamespaceValue(module)(namespace, name)(ModuleLookup.lookupGlobalClass)
          }

          private def getArClass(id: Int): Comp[ClassLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading
            [
              ArgonModule.ClassReference,
              ArgonModule.ClassDefinition,
              ArgonModule.ClassDescriptor,
              ClassDescriptor,
              ClassDescriptor.InNamespace,
              ArClass[context.type, ReferencePayloadSpecifier],
              ArClass[context.type, TPayloadSpec]
            ](
              classCache,
            )(
              CompilationError.ModuleObjectClass
            )(
              refPathFunction = ModulePaths.classRef,
              defPathFunction = ModulePaths.classDef,
              refCompanion = ArgonModule.ClassReference,
              defCompanion = ArgonModule.ClassDefinition,
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseClassDescriptor(id)
            )(
              referenceHandler = lookupClass(_),
              definitionHandler = new ObjectDefinitionLoader[ArgonModule.ClassDefinition, ClassDescriptor, ArClass[context.type, TPayloadSpec]] {
                override def apply(id: Int, definition: ArgonModule.ClassDefinition, desc: ClassDescriptor): Comp[ArClass[context.type, TPayloadSpec] { val descriptor: desc.type }] =
                  new ArClass[context.type, TPayloadSpec] {
                    override val context: context2.type = context2
                    override val contextProof: context.type Is context.type = Is.refl

                    override val descriptor: desc.type = desc
                    override val fileId: FileID = convertFileId(definition.fileId)
                    override val classMessageSource: CompilationMessageSource = CompilationMessageSource.ReferencedModule(currentModuleDescriptor)

                    override val isSealed: Boolean = definition.isSealed.getOrElse(false)
                    override val isOpen: Boolean = definition.isOpen.getOrElse(false)
                    override val isAbstract: Boolean = definition.isAbstract.getOrElse(false)

                    override lazy val signature: Comp[Signature[ArClass.ResultInfo, _ <: Nat]] =
                      definition.signature match {
                        case ArgonModule.ClassSignature(parameters, baseClass, baseTraits, _) =>
                          for {
                            baseClassResolved <- baseClass.traverse(resolveClassType(_))
                            baseTraitsResolved <- baseTraits.traverse(resolveTraitType(_))
                            parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

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
                          fieldType <- resolveType(field.fieldType)
                        } yield FieldVariable[context.type, Id](
                          FieldDescriptor(descriptor, field.name),
                          AbsRef(this),
                          VariableName.Normal(field.name),
                          Mutability.fromIsMutable(field.isMutable),
                          fieldType
                        )
                      }

                    override lazy val methods: Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
                      getMethodMembers(definition.methods)

                    override lazy val staticMethods: Comp[Vector[MethodBinding[context.type, TPayloadSpec]]] =
                      getMethodMembers(definition.staticMethods)

                    override lazy val payload: TPayloadSpec[Unit, context.TClassMetadata] = payloadLoader.createClassPayload(context)

                    override val classConstructors: Comp[Vector[ClassConstructorBinding[context.type, TPayloadSpec]]] =
                      definition.constructors
                        .traverse { classCtor =>
                          getClassCtor(classCtor.id).map { (_, classCtor.accessModifier) }
                        }
                        .map { ctorLoads =>
                          ctorLoads.collect {
                            case (ModuleObjectDefinition(ctor), ParsedAccessModifier(accessModifier)) =>
                              ClassConstructorBinding(ctor.descriptor.index, accessModifier, ctor)
                          }
                        }

                  }.pure[Comp]
              }
            ).get(id)

          private def getDataCtor(id: Int): Comp[DataCtorLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading
            [
              ArgonModule.DataConstructorReference,
              ArgonModule.DataConstructorDefinition,
              ArgonModule.DataConstructorDescriptor,
              DataConstructorDescriptor,
              DataConstructorDescriptor.InNamespace,
              DataConstructor[context.type, ReferencePayloadSpecifier],
              DataConstructor[context.type, TPayloadSpec]
            ](
              dataCtorCache
            )(
              CompilationError.ModuleObjectDataConstructor
            )(
              refPathFunction = ModulePaths.dataCtorRef,
              defPathFunction = ModulePaths.dataCtorDef,
              refCompanion = ArgonModule.DataConstructorReference,
              defCompanion = ArgonModule.DataConstructorDefinition,
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseDataCtorDescriptor(id)
            )(
              referenceHandler = moduleRef => {
                case DataConstructorDescriptor.InNamespace(_, _, namespace, name) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) => dataCtor
                  }
              },
              definitionHandler = new ObjectDefinitionLoader[ArgonModule.DataConstructorDefinition, DataConstructorDescriptor, DataConstructor[context.type, TPayloadSpec]] {
                override def apply(id: Int, definition: ArgonModule.DataConstructorDefinition, desc: DataConstructorDescriptor): Comp[DataConstructor[context.type, TPayloadSpec] { val descriptor: desc.type }] = ???
              }
            ).get(id)

          private def getFunction(id: Int): Comp[FunctionLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading
            [
              ArgonModule.FunctionReference,
              ArgonModule.FunctionDefinition,
              ArgonModule.FunctionDescriptor,
              FuncDescriptor,
              FuncDescriptor.InNamespace,
              ArFunc[context.type, ReferencePayloadSpecifier],
              ArFunc[context.type, TPayloadSpec]
            ](
              functionCache
            )(
              CompilationError.ModuleObjectFunction
            )(
              refPathFunction = ModulePaths.funcRef,
              defPathFunction = ModulePaths.funcDef,
              refCompanion = ArgonModule.FunctionReference,
              defCompanion = ArgonModule.FunctionDefinition,
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseFunctionDescriptor(id)
            )(
              referenceHandler = moduleRef => {
                case FuncDescriptor.InNamespace(_, _, namespace, name) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case GlobalBinding.GlobalFunction(_, _, func) => func
                  }
              },
              definitionHandler = new ObjectDefinitionLoader[ArgonModule.FunctionDefinition, FuncDescriptor, ArFunc[context.type, TPayloadSpec]] {
                override def apply(id: Int, definition: ArgonModule.FunctionDefinition, desc: FuncDescriptor): Comp[ArFunc[context.type, TPayloadSpec] { val descriptor: desc.type }] =
                  new ArFunc[context.type, TPayloadSpec] {
                    override val context: context2.type = context2
                    override val descriptor: desc.type = desc
                    override val fileId: FileID = convertFileId(definition.fileId)
                    override val effectInfo: EffectInfo = EffectInfo(
                      isPure = definition.effects.isPure,
                    )
                    override val signature: Comp[Signature[FunctionResultInfo, _ <: Nat]] =
                      definition.signature match {
                        case ArgonModule.FunctionSignature(parameters, returnType, _) =>
                          for {
                            returnTypeResolved <- resolveType(returnType)
                            parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                            result = SignatureResult[FunctionResultInfo](
                              FunctionResultInfo[context.type, Id](returnTypeResolved)
                            )

                          } yield parametersResolved.foldRight[Signature[FunctionResultInfo, _ <: Nat]](result) {
                            case (param, prevSig: Signature[FunctionResultInfo, len]) =>
                              SignatureParameters[FunctionResultInfo, len](param, prevSig)
                          }
                      }

                    override val payload: TPayloadSpec[Comp[context.TFunctionImplementation], context.TFunctionMetadata] =
                      payloadLoader.createFunctionPayload(context)
                  }.pure[Comp]
              }
            ).get(id)

          private lazy val getMethod: Int => Comp[MethodLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading
            [
              ArgonModule.MethodReference,
              ArgonModule.MethodDefinition,
              ArgonModule.MethodDescriptor,
              MethodDescriptor,
              Nothing,
              ArMethod[context.type, ReferencePayloadSpecifier],
              ArMethod[context.type, TPayloadSpec]
            ](
              methodCache
            )(
              CompilationError.ModuleObjectMethod
            )(
              refPathFunction = ModulePaths.methodRef,
              defPathFunction = ModulePaths.methodDef,
              refCompanion = ArgonModule.MethodReference,
              defCompanion = ArgonModule.MethodDefinition,
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseMethodDescriptor
            )(
              referenceHandler = moduleRef => {
                case methodDesc @ MethodDescriptor(traitDescriptor: TraitDescriptor, _, _) =>
                  lookupTrait(moduleRef)(traitDescriptor).flatMap { arTraitOpt =>
                    arTraitOpt.flatTraverse[Comp, ArMethod[context.type, ReferencePayloadSpecifier]] { arTrait =>
                      arTrait.methods.map { methods =>
                        methods
                          .find { binding =>
                            binding.method.descriptor === methodDesc
                          }
                          .map { _.method }
                      }
                    }
                  }
                case methodDesc @ MethodDescriptor(TraitObjectDescriptor(traitDescriptor), _, _) =>
                  lookupTrait(moduleRef)(traitDescriptor).flatMap { arTraitOpt =>
                    arTraitOpt.flatTraverse[Comp, ArMethod[context.type, ReferencePayloadSpecifier]] { arTrait =>
                      arTrait.staticMethods.map { methods =>
                        methods
                          .find { binding =>
                            binding.method.descriptor === methodDesc
                          }
                          .map { _.method }
                      }
                    }
                  }

                case methodDesc @ MethodDescriptor(classDescriptor : ClassDescriptor, _, _) =>
                  lookupClass(moduleRef)(classDescriptor).flatMap { arClassOpt =>
                    arClassOpt.flatTraverse[Comp, ArMethod[context.type, ReferencePayloadSpecifier]] { arClass =>
                      arClass.methods.map { methods =>
                        methods
                          .find { binding =>
                            binding.method.descriptor === methodDesc
                          }
                          .map { _.method }
                      }
                    }
                  }

                case methodDesc @ MethodDescriptor(ClassObjectDescriptor(classDescriptor), _, _) =>
                  lookupClass(moduleRef)(classDescriptor).flatMap { arClassOpt =>
                    arClassOpt.flatTraverse[Comp, ArMethod[context.type, ReferencePayloadSpecifier]] { arClass =>
                      arClass.staticMethods.map { methods =>
                        methods
                          .find { binding =>
                            binding.method.descriptor === methodDesc
                          }
                          .map { _.method }
                      }
                    }
                  }

                case methodDesc @ MethodDescriptor(DataConstructorDescriptor.InNamespace(_, _, namespace, name), _, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) => dataCtor
                  }
                    .flatMap {
                      _.flatTraverse[Comp, ArMethod[context.type, ReferencePayloadSpecifier]] { dataCtor =>
                        dataCtor.methods.map { methods =>
                          methods
                            .find { binding =>
                              binding.method.descriptor === methodDesc
                            }
                            .map { _.method }
                        }
                      }
                    }
              },
              definitionHandler = new ObjectDefinitionLoader[ArgonModule.MethodDefinition, MethodDescriptor, ArMethod[context.type, TPayloadSpec]] {
                override def apply(id: Int, definition: ArgonModule.MethodDefinition, desc: MethodDescriptor): Comp[ArMethod[context.type, TPayloadSpec] { val descriptor: desc.type }] =
                  for {
                    methodOwner <- definition.methodOwner match {
                      case ArgonModule.MethodDefinition.MethodOwner.OwnerClassId(ownerId) => findClassDef(ownerId).map(ArMethod.ClassOwner.apply)
                      case ArgonModule.MethodDefinition.MethodOwner.OwnerClassObjectId(ownerId) => findClassDef(ownerId).map(ArMethod.ClassObjectOwner.apply)
                      case ArgonModule.MethodDefinition.MethodOwner.OwnerTraitId(ownerId) => findTraitDef(ownerId).map(ArMethod.TraitOwner.apply)
                      case ArgonModule.MethodDefinition.MethodOwner.OwnerTraitObjectId(ownerId) => findTraitDef(ownerId).map(ArMethod.TraitObjectOwner.apply)
                      case ArgonModule.MethodDefinition.MethodOwner.OwnerConstructorId(ownerId) => findDataConstructorDef(ownerId).map(ArMethod.DataCtorOwner.apply)
                      case ArgonModule.MethodDefinition.MethodOwner.Empty => Compilation.forErrors(
                        CompilationError.MethodMustHaveOwner(
                          CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                        )
                      )
                    }
                  } yield new ArMethod[context.type, TPayloadSpec] {
                    override val context: context2.type = context2
                    override val contextProof: context.type Is context.type = Is.refl
                    override val descriptor: desc.type = desc
                    override val fileId: FileID = convertFileId(definition.fileId)
                    override val effectInfo: EffectInfo = EffectInfo(
                      isPure = definition.effects.isPure,
                    )

                    override val isVirtual: Boolean = definition.isVirtual.getOrElse(false)
                    override val isAbstract: Boolean = definition.isAbstract.getOrElse(false)
                    override val isImplicitOverride: Boolean = definition.isImplicitOverride.getOrElse(false)
                    override val isFinal: Boolean = definition.isFinal.getOrElse(false)

                    override val owner: ArMethod.Owner[context.type, TPayloadSpec] = methodOwner

                    override val signatureUnsubstituted: Comp[Signature[FunctionResultInfo, _ <: Nat]] =
                      definition.signature match {
                        case ArgonModule.MethodSignature(parameters, returnType, _) =>
                          for {
                            returnTypeResolved <- resolveType(returnType)
                            parametersResolved <- parameters.zipWithIndex.toVector.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                            result = SignatureResult[FunctionResultInfo](
                              FunctionResultInfo[context.type, Id](returnTypeResolved)
                            )

                          } yield parametersResolved.foldRight[Signature[FunctionResultInfo, _ <: Nat]](result) {
                            case (param, prevSig: Signature[FunctionResultInfo, len]) =>
                              SignatureParameters[FunctionResultInfo, len](param, prevSig)
                          }
                      }

                    override val payload: TPayloadSpec[Comp[context.TMethodImplementation], context.TMethodMetadata] =
                      payloadLoader.createMethodPayload(context)
                  }
              }
            ).toFunction

          private lazy val getClassCtor: Int => Comp[ClassCtorLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading
              [
                ArgonModule.ClassConstructorReference,
                ArgonModule.ClassConstructorDefinition,
                ArgonModule.ClassConstructorDescriptor,
                ClassConstructorDescriptor,
                Nothing,
                ClassConstructor[context.type, ReferencePayloadSpecifier],
                ClassConstructor[context.type, TPayloadSpec]
              ](
                classCtorCache
              )(
                CompilationError.ModuleObjectClassConstructor
              )(
                refPathFunction = ModulePaths.classCtorRef,
                defPathFunction = ModulePaths.classCtorDef,
                refCompanion = ArgonModule.ClassConstructorReference,
                defCompanion = ArgonModule.ClassConstructorDefinition,
              )(
                refModuleIdLens = _.moduleId,
                refDescriptorLens = _.descriptor,
                defDescriptorLens = _.descriptor,
              )(
                parseDescriptor = parseClassCtorDescriptor
              )(
                referenceHandler = moduleRef => {
                  case classCtorDesc @ ClassConstructorDescriptor(classDescriptor, _) =>
                    lookupClass(moduleRef)(classDescriptor).flatMap { arClassOpt =>
                      arClassOpt.flatTraverse[Comp, ClassConstructor[context.type, ReferencePayloadSpecifier]] { arClass =>
                        arClass.classConstructors.map { ctors =>
                          ctors
                            .find { binding =>
                              binding.ctor.descriptor === classCtorDesc
                            }
                            .map { _.ctor }
                        }
                      }
                    }
                },
                definitionHandler = new ObjectDefinitionLoader[ArgonModule.ClassConstructorDefinition, ClassConstructorDescriptor, ClassConstructor[context.type, TPayloadSpec]] {
                  override def apply(id: Int, definition: ArgonModule.ClassConstructorDefinition, desc: ClassConstructorDescriptor): Comp[ClassConstructor[context.type, TPayloadSpec] { val descriptor: desc.type }] =
                    for {
                      instanceClass <- findClassDef(definition.descriptor.instanceClassId)
                    } yield new ClassConstructor[context.type, TPayloadSpec] {
                      override val context: context2.type = context2
                      override val descriptor: desc.type = desc
                      override val fileId: FileID = convertFileId(definition.fileId)
                      override val effectInfo: EffectInfo = EffectInfo(
                        isPure = definition.effects.isPure,
                      )

                      override val ownerClass: ArClass[context.type, TPayloadSpec] = instanceClass

                      override lazy val signatureUnsubstituted: Comp[Signature[ClassConstructor.ResultInfo, _ <: Nat]] =
                        definition.signature match {
                          case ArgonModule.ClassConstructorSignature(parameters, _) =>
                            for {
                              parametersResolved <- parameters.zipWithIndex.traverse { case (param, index) => resolveParameter(descriptor)(index)(param) }

                              result = SignatureResult[ClassConstructor.ResultInfo](
                                ClassConstructor.ResultInfo()
                              )

                            } yield parametersResolved.foldRight[Signature[ClassConstructor.ResultInfo, _ <: Nat]](result) {
                              case (param, prevSig: Signature[ClassConstructor.ResultInfo, len]) =>
                                SignatureParameters[ClassConstructor.ResultInfo, len](param, prevSig)
                            }
                        }

                      override lazy val payload: TPayloadSpec[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] =
                        payloadLoader.createClassConstructorPayload(context)

                    }
                }
              ).toFunction


          private def convertNamespaceElement[TElem]
          (
            id: Int,
            accessModifier: AccessModifierGlobal,
            descriptor: ArgonModule.InNamespaceDescriptor
          )(
            loadElement: Int => Comp[ModuleObjectLoadResult[_, TElem, _]],
            createBinding: (GlobalName, AccessModifierGlobal, TElem) => GlobalBinding[context.type, TPayloadSpec]
          ): Comp[ModuleElement[context.type, TPayloadSpec]] =
            descriptor match {
              case ArgonModule.InNamespaceDescriptor(ns, ValidGlobalName(name), _) =>
                loadElement(id).flatMap {
                  case ModuleObjectGlobalDefinition(elem) =>
                    val nsPath = parseNamespacePath(ns)
                    ModuleElement(nsPath, createBinding(name, accessModifier, elem)).pure[Comp]

                  case ModuleObjectDefinition(_) | ModuleObjectReference(_) =>
                    Compilation.forErrors(CompilationError.InvalidGlobal(CompilationMessageSource.ReferencedModule(currentModuleDescriptor)))
                }
            }

          private lazy val globalNamespaceComp: Comp[Namespace[context.type, TPayloadSpec]] =
            metadata.globals
              .traverse {
                case ArgonModule.GlobalDeclaration(ArgonModule.GlobalDeclaration.Descriptor.TraitDescriptor(descriptor), id, ParsedGlobalAccessModifier(accessModifier), _) =>
                  convertNamespaceElement(id, accessModifier, descriptor)(getTrait(_), GlobalBinding.GlobalTrait.apply)

                case ArgonModule.GlobalDeclaration(ArgonModule.GlobalDeclaration.Descriptor.ClassDescriptor(descriptor), id, ParsedGlobalAccessModifier(accessModifier), _) =>
                  convertNamespaceElement(id, accessModifier, descriptor)(getArClass(_), GlobalBinding.GlobalClass.apply)

                case ArgonModule.GlobalDeclaration(ArgonModule.GlobalDeclaration.Descriptor.DataConstructorDescriptor(descriptor), id, ParsedGlobalAccessModifier(accessModifier), _) =>
                  convertNamespaceElement(id, accessModifier, descriptor)(getDataCtor(_), GlobalBinding.GlobalDataConstructor.apply)

                case ArgonModule.GlobalDeclaration(ArgonModule.GlobalDeclaration.Descriptor.FunctionDescriptor(descriptor), id, ParsedGlobalAccessModifier(accessModifier), _) =>
                  convertNamespaceElement(id, accessModifier, descriptor)(getFunction(_), GlobalBinding.GlobalFunction.apply)

                case ArgonModule.GlobalDeclaration(id, _, _, _) =>
                  Compilation.forErrors(CompilationError.InvalidGlobal(CompilationMessageSource.ReferencedModule(currentModuleDescriptor)))
              }
              .map(NamespaceBuilder.createNamespace)


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
                  CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
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
                  CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )
            }


          def findDataConstructorDef(ctorId: Int): Comp[DataConstructor[context.type, TPayloadSpec]] =
            getDataCtor(ctorId).flatMap {
              case ModuleObjectDefinition(ctor) => ctor.pure[Comp]
              case ModuleObjectGlobalDefinition(ctor) => ctor.pure[Comp]

              case ModuleObjectReference(_) =>
                Compilation.forErrors(
                  CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectDataConstructor, ctorId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )
            }

          def resolveSignatureTypeArgs[TResult[TContext2 <: Context with Singleton, Wrap[+_]], Len <: Nat, T]
          (sig: Signature[TResult, Len], args: Sized[Vector[ArgonModule.TypeArg], Len], convArgs: Vector[TTypeArgument])
          (f: (Vector[TTypeArgument], TResult[context.type, Id]) => T)
          : Comp[T] =
            sig.visit(new SignatureVisitor[TResult, Len, Comp[T]] {

              override def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): Comp[T] = {
                args.head match {
                  case ArgonModule.TypeArg(ArgonModule.TypeArg.TypeInfo.Type(head), _) =>
                    resolveType(head).flatMap { argType =>
                      val nextSig = sigParams.next(argType)
                      resolveSignatureTypeArgs(nextSig, args.tail, convArgs :+ TypeArgument.Expr[context.type, Id](argType))(f)
                    }

                  case ArgonModule.TypeArg(ArgonModule.TypeArg.TypeInfo.Wildcard(ArgonModule.Wildcard(_)), _) =>
                    ??? //resolveSignatureTypeArgs(sigParams.nextUnsubstituted, tail, convArgs :+ TypeArgument.Wildcard)(f)

                  case ArgonModule.TypeArg(ArgonModule.TypeArg.TypeInfo.Empty, _) => ???
                }
              }

              override def visitResult(sigResult: SignatureResult[TResult])(implicit lenEq: Len === _0): Comp[T] =
                f(convArgs, sigResult.result).pure[Comp]
            })

          def resolveTraitType(traitType: ArgonModule.TraitType): Comp[TTraitType] =
            findTrait(traitType.traitId).flatMap { arTrait =>
              arTrait.value.signature.flatMap {
                case sig: Signature[ArTrait.ResultInfo, len] =>
                  traitType.typeArguments.sized((sig : Signature[ArTrait.ResultInfo, len]).parameterCountToInt) match {
                    case Some(typeArgs) =>
                      resolveSignatureTypeArgs[ArTrait.ResultInfo, len, TTraitType](sig, typeArgs, Vector.empty) {
                        (args, result) => TraitType(arTrait, args)
                      }

                    case None => ???
                  }
              }
            }

          def resolveClassType(classType: ArgonModule.ClassType): Comp[TClassType] =
            findClass(classType.classId).flatMap { arClass =>
              arClass.value.signature.flatMap {
                case sig: Signature[ArClass.ResultInfo, len] =>
                  classType.typeArguments.sized((sig : Signature[ArClass.ResultInfo, len]).parameterCountToInt) match {
                    case Some(typeArgs) =>
                      resolveSignatureTypeArgs[ArClass.ResultInfo, len, TClassType](sig, typeArgs, Vector.empty) {
                        (args, result) => ClassType(arClass, args)
                      }


                    case None => ???
                  }              }
            }

          def resolveType(t: ArgonModule.Type): Comp[TType] =
            t.typeInfo match {
              case ArgonModule.Type.TypeInfo.TraitType(traitType) => resolveTraitType(traitType)
              case ArgonModule.Type.TypeInfo.ClassType(classType) => resolveClassType(classType)
              case ArgonModule.Type.TypeInfo.DataConstructorType(_) => ???
              case ArgonModule.Type.TypeInfo.Empty => ???
            }

          def resolveParameter(ownerDescriptor: ParameterOwnerDescriptor)(index: Int)(parameter: ArgonModule.Parameter): Comp[Parameter[context.type, Id]] = {
            val parameterStyle =
              parameter.style match {
                case Some(ArgonModule.ParameterStyle.Normal) | None => ParameterStyle.Normal
                case Some(ArgonModule.ParameterStyle.Inferrable) => ParameterStyle.Inferrable
                case Some(ArgonModule.ParameterStyle.Unrecognized(_)) => ???
              }

            parameter.elements
              .traverse { case ArgonModule.ParameterElement(name, paramType, _) =>
                val varName = name.map(VariableName.Normal).getOrElse(VariableName.Unnamed)

                resolveType(paramType)
                  .map { t => (t, varName) }
              }
              .flatMap {
                case Vector() =>
                  for {
                    currentModule <- module
                    unitType <- StandardTypeLoaders.loadUnitType(context)(
                      CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                    )(currentModule)(referencedModules)

                  } yield Parameter[context.type, Id](
                    parameterStyle,
                    ParameterVariable[context.type, Id](ParameterDescriptor(ownerDescriptor, index), VariableName.Unnamed, Mutability.NonMutable, unitType),
                    Vector()
                  )

                case Vector((t, varName)) =>
                  Parameter(
                    parameterStyle,
                    ParameterVariable[context.type, Id](ParameterDescriptor(ownerDescriptor, index), varName, Mutability.NonMutable, t),
                    Vector()
                  ).pure[Comp]

                case elems @ head +: tail =>

                  val paramType = context.typeSystem.fromSimpleType(LoadTuple(
                    NonEmptyList(head, tail.toList).map { case (t, _) => TupleElement[context.type, Id](t) }
                  ))

                  val paramVar = ParameterVariable[context.type, Id](ParameterDescriptor(ownerDescriptor, index), VariableName.Unnamed, Mutability.NonMutable, paramType)
                  val paramElems = elems.zipWithIndex.map { case ((t, name), elemIndex) => ParameterElement[context.type, Id](paramVar, name, t, elemIndex) }

                  Parameter(parameterStyle, paramVar, paramElems).pure[Comp]
              }
          }

          override lazy val module: Comp[ArModule[context.type, TPayloadSpec]] =
            moduleCache.get(
              for {
                globalNamespaceCache <- ValueCache.make[ErrorList, Namespace[context.type, TPayloadSpec]]
              } yield new ArModule[context.type, TPayloadSpec] {
                override val context: context2.type = context2
                override val descriptor: ModuleDescriptor = currentModuleDescriptor
                override lazy val globalNamespace: Comp[Namespace[context.type, TPayloadSpec]] = globalNamespaceCache.get(globalNamespaceComp)
                override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] =
                  refModuleMap.values.toVector.collect {
                    case ModuleReference(moduleRef) => moduleRef
                  }
              }
            )

        }.module

      } yield module
    }

  }




  trait PayloadLoader[TContext <: Context, TPayloadSpec[_, _]] {

    def createClassPayload(context: TContext): TPayloadSpec[Unit, context.TClassMetadata]
    def createTraitPayload(context: TContext): TPayloadSpec[Unit, context.TTraitMetadata]
    def createDataConstructorPayload(context: TContext): TPayloadSpec[Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata]
    def createFunctionPayload(context: TContext): TPayloadSpec[Comp[context.TFunctionImplementation], context.TFunctionMetadata]
    def createMethodPayload(context: TContext): TPayloadSpec[Comp[context.TMethodImplementation], context.TMethodMetadata]
    def createClassConstructorPayload(context: TContext): TPayloadSpec[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata]

  }

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
  type MethodLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArMethod[TContext, PayloadSpec], ArMethod[TContext, PayloadSpec] { val descriptor: Nothing }, ArMethod[TContext, ReferencePayloadSpecifier]]
  type ClassCtorLoadResult[TContext <: Context with Singleton, PayloadSpec[_, _]] = ModuleObjectLoadResult[ClassConstructor[TContext, PayloadSpec], ClassConstructor[TContext, PayloadSpec] { val descriptor: Nothing }, ClassConstructor[TContext, ReferencePayloadSpecifier]]

}
