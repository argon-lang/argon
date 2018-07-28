package com.mi3software.argon.compiler

import java.io.File
import java.util.Locale

import com.mi3software.argon.{module => ArgonModule}
import scalaz.{Lens => _, _}
import Scalaz._
import com.mi3software.argon.compiler.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.util._
import com.mi3software.argon.Compilation
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TSimpleFileTransport
import scalaz.effect.IO

import scala.collection.immutable._
import scala.collection.{ Seq => QSeq }

object ArgonModuleLoader extends ModuleLoader {

  override type ModuleData = ArgonModule.Module
  override def loadFile(file: File): IO[Option[ArgonModule.Module]] =
    IO { file.getName.toLowerCase(Locale.ENGLISH) }.flatMap { ext =>
      if(ext.endsWith(".armodule"))
        IO {
          val trans = new TSimpleFileTransport(file.getPath)
          val prot = new TBinaryProtocol(trans)

          Some(ArgonModule.Module.decode(prot))
        }
      else
        IO(None : Option[ArgonModule.Module])
    }

  override def dataDescriptor(data: ArgonModule.Module): Option[ModuleDescriptor] =
    Some(ModuleDescriptor(data.name))

  override def dataReferencedModules(data: ArgonModule.Module): Vector[ModuleDescriptor] =
    data.referencedModules.map {
      case ArgonModule.ModuleReference(name) => ModuleDescriptor(name)
    }.toVector


  override def loadModuleReference
  (context: Context)
  (data: ArgonModule.Module)
  (referencedModules: Vector[ArModule[context.type]])
  : context.Comp[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]] =
    loadModule(context)(data)(referencedModules)(referencePayloadLoader)

  private val currentFormatVersion = 1

  private trait ModuleCreator[TContext <: Context, Comp[_], TPayloadSpec[_, _]] {
    val module: Comp[ArModuleWithPayload[TContext, TPayloadSpec]]
  }

  def loadModule[TPayloadSpec[_, _]]
  (context: Context)
  (binModule: ArgonModule.Module)
  (referencedModules: Vector[ArModule[context.type]])
  (payloadLoader: PayloadLoader[TPayloadSpec])
  : context.Comp[ArModuleWithPayload[context.type, TPayloadSpec]] = {

    type CurrentScopeTypes = context.ScopeTypesWithPayload[TPayloadSpec]
    val context2: context.type = context

    def impl[Comp[+_] : Compilation](implicit compEv: LeibnizK[Comp, context.Comp]): Comp[ArModuleWithPayload[context.type, TPayloadSpec]] = {
      val currentModuleDescriptor = ModuleDescriptor(binModule.name)

      for {

        _ <-
          if(binModule.formatVersion === 0 || binModule.formatVersion > currentFormatVersion)
            implicitly[Compilation[Comp]].forErrors(CompilationError.UnsupportedModuleFormatVersion(binModule.formatVersion, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)))
          else
            implicitly[Monad[Comp]].point(())

        module <- new ModuleCreator[context.type, Comp, TPayloadSpec] {


          private def parseNamespacePath(ns: ArgonModule.Namespace): NamespacePath =
            Option(ns) match {
              case Some(ArgonModule.Namespace(parts)) => NamespacePath(parts.toVector)
              case None => NamespacePath.empty
            }

          private object ValidGlobalName {
            def unapply(globalName: ArgonModule.GlobalName): Option[GlobalName] =
              globalName match {
                case ArgonModule.GlobalName.NormalName(name) => Some(GlobalName.Normal(name))
                case ArgonModule.GlobalName.Unnamed(ArgonModule.UnnamedGlobalName(fileID, index)) =>
                  Some(GlobalName.Unnamed(FileID(fileID), index))

                case ArgonModule.GlobalName.UnknownUnionField(_) => None
              }
          }

          private object ParsedAccessModifier {
            def unapply(accessModifier: ArgonModule.AccessModifier): Option[AccessModifier] =
              accessModifier match {
                case ArgonModule.AccessModifier.Invalid | ArgonModule.AccessModifier.EnumUnknownAccessModifier(_) => None
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
            binModule.referencedModules.zipWithIndex
              .map { case (modRef, i) =>
                val moduleLoadRes: ModuleLoadResult[context.type] =
                  referencedModules
                    .find { _.descriptor.name.contains(modRef.name) }
                  match {
                    case Some(referencedModule) => ModuleReference(referencedModule)
                    case None => ModuleNotFound(modRef)
                  }

                i -> moduleLoadRes
              }(collection.breakOut)

          private def lookupNamespaceValue[T]
          (refModule: ArModule[context.type])
          (namespace: NamespacePath, name: GlobalName)
          (f: PartialFunction[ScopeValue[context.ContextScopeTypes], T])
          : Option[T] = {
            def impl(namespaceParts: Vector[String])(namespaceValues: Namespace[ScopeValue[context.ContextScopeTypes]]): Option[T] =
              namespaceParts match {
                case head +: tail =>
                  namespaceValues.bindings.find(_.name === GlobalName.Normal(head)) match {
                    case Some(binding) =>
                      binding.namespaceElement match {
                        case NamespaceScopeValue(nestedNS) => impl(tail)(nestedNS)
                        case _ => None
                      }

                    case None => None
                  }

                case Vector() =>
                  namespaceValues.bindings
                    .find { _.name === name }
                    .map { _.namespaceElement }
                    .collect(f)
              }

            impl(namespace.ns)(refModule.globalNamespace)
          }

          private def parseTraitDescriptor(module: ModuleDescriptor)(desc: ArgonModule.TraitDescriptor): Option[TraitDescriptor] =
            desc match {
              case ArgonModule.TraitDescriptor.InNamespace(
                ArgonModule.TraitDescriptorInNamespace(
                  ArgonModule.FileSpec(fileID, fileName),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              ) =>
                Some(TraitDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case ArgonModule.TraitDescriptor.UnknownUnionField(_) =>
                None
            }

          private def parseClassDescriptor(module: ModuleDescriptor)(desc: ArgonModule.ClassDescriptor): Option[ClassDescriptor] =
            desc match {
              case ArgonModule.ClassDescriptor.InNamespace(
                ArgonModule.ClassDescriptorInNamespace(
                  ArgonModule.FileSpec(fileID, fileName),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              ) =>
                Some(ClassDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case ArgonModule.ClassDescriptor.MetaClass(
                ArgonModule.ClassDescriptorMetaClass(ownerClassDescriptor)
              ) =>
                parseClassDescriptor(module)(ownerClassDescriptor).map(ClassDescriptor.MetaClass.apply)

              case ArgonModule.ClassDescriptor.TraitMetaClass(
                ArgonModule.ClassDescriptorTraitMetaClass(ownerTraitDescriptor)
              ) =>
                parseTraitDescriptor(module)(ownerTraitDescriptor).map(ClassDescriptor.TraitMetaClass.apply)

              case ArgonModule.ClassDescriptor.UnknownUnionField(_) =>
                None
            }

          private def parseDataCtorDescriptor(module: ModuleDescriptor)(desc: ArgonModule.DataConstructorDescriptor): Option[DataConstructorDescriptor] =
            desc match {
              case ArgonModule.DataConstructorDescriptor.InNamespace(
                ArgonModule.DataConstructorDecscriptorInNamespace(
                  ArgonModule.FileSpec(fileID, fileName),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              ) =>
                Some(DataConstructorDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case ArgonModule.DataConstructorDescriptor.UnknownUnionField(_) =>
                None
            }

          private def parseFunctionDescriptor(module: ModuleDescriptor)(desc: ArgonModule.FunctionDescriptor): Option[FuncDescriptor] =
            desc match {
              case ArgonModule.FunctionDescriptor.InNamespace(
                ArgonModule.FunctionDescriptorInNamespace(
                  ArgonModule.FileSpec(fileID, fileName),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              ) =>
                Some(FuncDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case ArgonModule.FunctionDescriptor.UnknownUnionField(_) =>
                None
            }

          private def parseMemberName(memberName: ArgonModule.MemberName): Option[MemberName] =
            memberName match {

              case ArgonModule.MemberName.UnknownUnionField(_) => None

              case ArgonModule.MemberName.Name(name) =>
                Some(MemberName.Normal(name))

              case ArgonModule.MemberName.UnnamedMemberIndex(index) =>
                Some(MemberName.Unnamed(index))

              case ArgonModule.MemberName.SpecialMemberName(ArgonModule.SpecialMemberName.EnumUnknownSpecialMemberName(_)) => None

              case ArgonModule.MemberName.SpecialMemberName(ArgonModule.SpecialMemberName.Call) =>
                Some(MemberName.Call)

              case ArgonModule.MemberName.SpecialMemberName(ArgonModule.SpecialMemberName.New) =>
                Some(MemberName.New)

            }

          private def parseMethodDescriptor(module: ModuleDescriptor)(desc: ArgonModule.MethodDescriptor): Option[MethodDescriptor] =
            desc match {
              case ArgonModule.MethodDescriptor(memberName, ParsedGlobalAccessModifier(accessModifier), ownerDesc) =>
                for {
                  memberNameValue <- parseMemberName(memberName)
                  ownerDescValue <- ownerDesc match {
                    case ArgonModule.ClassLikeDescriptor.UnknownUnionField(_) => None
                    case ArgonModule.ClassLikeDescriptor.TraitDescriptor(traitDesc) => parseTraitDescriptor(module)(traitDesc)
                    case ArgonModule.ClassLikeDescriptor.ClassDescriptor(classDesc) => parseClassDescriptor(module)(classDesc)
                  }
                } yield MethodDescriptor(ownerDescValue, memberNameValue, accessModifier)
            }

          private trait ModuleObjectRefDef[TValue, TRef, TDef] {
            def fromModuleObject(value: TValue): Option[TRef \/ TDef]
          }

          private def handleModuleObjectLoading
          [TValue, TRef, TDef, TValueDescriptor, TResultDescriptor, TRefResult, TDefResult]
          (
            moduleObjectType: CompilationError.ModuleObjectType,
          )(
            valueLens: ArgonModule.Module => QSeq[TValue]
          )(
            refDef: ModuleObjectRefDef[TValue, TRef, TDef]
          )(
            refModuleIdLens: TRef => Int,
            refDescriptorLens: TRef => TValueDescriptor,
            defDescriptorLens: TDef => TValueDescriptor,
          )(
            parseDescriptor: ModuleDescriptor => TValueDescriptor => Option[TResultDescriptor]
          )(
            referenceHandler: ArModule[context.type] => TResultDescriptor => Comp[Option[TRefResult]],
            definitionHandler: Int => TDef => TResultDescriptor => TDefResult
          ): Comp[Map[Int, ModuleObjectLoadResult[TRefResult, TDefResult]]] =
            valueLens(binModule).toVector.zipWithIndex.traverse { case (moduleObject, i) =>
              ((refDef.fromModuleObject(moduleObject) match {
                case Some(-\/(refValue)) =>
                  refModuleMap.get(refModuleIdLens(refValue)) match {
                    case Some(ModuleReference(moduleRef)) =>
                      parseDescriptor(moduleRef.descriptor)(refDescriptorLens(refValue)) match {
                        case Some(descriptor) =>
                          referenceHandler(moduleRef)(descriptor).flatMap {
                            case Some(refResult) => ModuleObjectReference(refResult).point[Comp]
                            case None => Compilation[Comp].forErrors(CompilationError.ModuleObjectNotFound(
                              moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                            ))
                          }

                        case None =>
                          Compilation[Comp].forErrors(CompilationError.ModuleObjectInvalidDescriptor(
                            moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                          ))
                      }

                    case _ =>
                      Compilation[Comp].forErrors(CompilationError.ModuleObjectModuleNotLoaded(
                        moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                      ))
                  }

                case Some(\/-(defValue)) =>
                  parseDescriptor(currentModuleDescriptor)(defDescriptorLens(defValue)) match {
                    case Some(descriptor) =>
                      ModuleObjectDefinition(definitionHandler(i)(defValue)(descriptor)).point[Comp]

                    case None =>
                      Compilation[Comp].forErrors(CompilationError.ModuleObjectInvalidDescriptor(
                        moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                      ))
                  }

                case None =>
                  Compilation[Comp].forErrors(CompilationError.ModuleObjectUndefined(
                    moduleObjectType, i, CompilationMessageSource.ReferencedModule(currentModuleDescriptor)
                  ))
              }) : Comp[ModuleObjectLoadResult[TRefResult, TDefResult]])
                .map { i -> _ }
            }.map { elems => Map(elems: _*) }

          private def lookupTrait(module: ArModule[context.type]): TraitDescriptor => Comp[Option[ArTrait[context.type]]] = {
            case TraitDescriptor.InNamespace(_, namespace, name, _) =>
              lookupNamespaceValue(module)(namespace, name) {
                case TraitScopeValue(arTrait) => arTrait
              }.point[Comp]
          }

          private lazy val traitMap: Comp[Map[Int, TraitLoadResult[context.type, TPayloadSpec]]] =
            handleModuleObjectLoading(
              CompilationError.ModuleObjectTrait
            )(
              valueLens = _.traits
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Trait, ArgonModule.TraitReference, ArgonModule.TraitDefinition] {
                override def fromModuleObject(value: ArgonModule.Trait): Option[ArgonModule.TraitReference \/ ArgonModule.TraitDefinition] =
                  value match {
                    case ArgonModule.Trait.TraitRef(traitRef) => Some(-\/(traitRef))
                    case ArgonModule.Trait.TraitDef(traitDef) => Some(\/-(traitDef))
                    case ArgonModule.Trait.UnknownUnionField(_) => None
                  }
              }
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseTraitDescriptor
            )(
              referenceHandler = lookupTrait(_),
              definitionHandler = traitId => traitValue => traitDescriptor => new ArTraitWithPayload[context.type, TPayloadSpec] {
                override val context: context2.type = context2
                override val contextProof: Leibniz[context.type, context.type, context.type, context.type] = Leibniz.refl

                override val descriptor: TraitDescriptor = traitDescriptor

                override val isSealed: Boolean = traitValue.isSealed

                override lazy val signature: context.Comp[Signature[context.typeSystem.type, ArTrait.ResultInfo]] =
                  traitValue.signature match {
                    case ArgonModule.TraitSignature(parameters, baseTraits) =>
                      compEv(
                        for {
                          baseTraitsResolved <- baseTraits.toVector.traverse(resolveTraitType(_))
                          parametersResolved <- parameters.toVector.traverse(resolveParameter(_))

                          result = SignatureResult[context.typeSystem.type, ArTrait.ResultInfo](
                            ArTrait.ResultInfo(BaseTypeInfoTrait(baseTraitsResolved))
                          )

                        } yield parametersResolved.foldRight[Signature[context.typeSystem.type, ArTrait.ResultInfo]](result)(SignatureParameters[context.typeSystem.type, ArTrait.ResultInfo])
                      )
                  }

                override lazy val methods: context.Comp[Vector[ArMethodWithPayload[context.type, TPayloadSpec]]] =
                  compEv(
                    methodMap.map { methods =>
                      methods.collect {
                        case (_, ModuleObjectDefinition(method)) if (method.descriptor.typeDescriptor match {
                          case ownerDesc: TraitDescriptor => ownerDesc === descriptor
                          case _ => false
                        }) =>
                          method
                      }.toVector
                    }
                  )

                override lazy val metaType: context.Comp[MetaClass[ArClassWithPayload[context.type, TPayloadSpec]]] =
                  compEv(findClassDef(traitValue.metaClassSpecifier.metaClassId).map(MetaClass.apply))

                override lazy val payload: TPayloadSpec[Unit, context.TTraitMetadata] = payloadLoader.createTraitPayload(context)
              }
            )

          private def lookupClass(module: ArModule[context.type]): ClassDescriptor => Comp[Option[ArClass[context.type]]] = {
            case ClassDescriptor.InNamespace(_, namespace, name, _) =>
              lookupNamespaceValue(module)(namespace, name) {
                case ClassScopeValue(arClass) => arClass
              }.point[Comp]

            case ClassDescriptor.MetaClass(ownerClass) =>
              lookupClass(module)(ownerClass)
                .flatMap { _.traverse[Comp, ArClass[context.type]] { resolvedOwner =>
                  compEv.flip(resolvedOwner.metaType).map { _.metaClass }
                } }

            case ClassDescriptor.TraitMetaClass(ownerTrait) =>
              lookupTrait(module)(ownerTrait)
                .flatMap { _.traverse[Comp, ArClass[context.type]] { resolvedOwner =>
                  compEv.flip(resolvedOwner.metaType).map { _.metaClass }
                } }
          }

          private lazy val classMap: Comp[Map[Int, ClassLoadResult[context.type, TPayloadSpec]]] =
            handleModuleObjectLoading(
              CompilationError.ModuleObjectClass
            )(
              valueLens = _.classes
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Class, ArgonModule.ClassReference, ArgonModule.ClassDefinition] {
                override def fromModuleObject(value: ArgonModule.Class): Option[ArgonModule.ClassReference \/ ArgonModule.ClassDefinition] =
                  value match {
                    case ArgonModule.Class.ClassRef(classRef) => Some(-\/(classRef))
                    case ArgonModule.Class.ClassDef(classDef) => Some(\/-(classDef))
                    case ArgonModule.Class.UnknownUnionField(_) => None
                  }
              }
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseClassDescriptor
            )(
              referenceHandler = lookupClass(_),
              definitionHandler = classId => classValue => classDescriptor => new ArClassWithPayload[context.type, TPayloadSpec] {
                override val context: context2.type = context2
                override val contextProof: Leibniz[context.type, context.type, context.type, context.type] = Leibniz.refl

                override val descriptor: ClassDescriptor = classDescriptor

                override val isSealed: Boolean = classValue.isSealed
                override val isOpen: Boolean = classValue.isOpen
                override val isAbstract: Boolean = classValue.isAbstract

                override lazy val signature: context.Comp[Signature[context.typeSystem.type, ArClass.ResultInfo]] =
                  classValue.signature match {
                    case ArgonModule.ClassSignature(parameters, baseClass, baseTraits) =>
                      compEv(
                        for {
                          baseClassResolved <- baseClass.traverse(resolveClassType(_))
                          baseTraitsResolved <- baseTraits.toVector.traverse(resolveTraitType(_))
                          parametersResolved <- parameters.toVector.traverse(resolveParameter(_))

                          result = SignatureResult[context.typeSystem.type, ArClass.ResultInfo](
                            ArClass.ResultInfo(BaseTypeInfoClass(baseClassResolved, baseTraitsResolved))
                          )

                        } yield parametersResolved.foldRight[Signature[context.typeSystem.type, ArClass.ResultInfo]](result)(SignatureParameters[context.typeSystem.type, ArClass.ResultInfo])
                      )
                  }

                override lazy val methods: context.Comp[Vector[ArMethodWithPayload[context.type, TPayloadSpec]]] =
                  compEv(
                    methodMap.map { methods =>
                      methods.collect {
                        case (_, ModuleObjectDefinition(method)) if (method.descriptor.typeDescriptor match {
                          case ownerDesc: ClassDescriptor => ownerDesc === descriptor
                          case _ => false
                        }) =>
                          method
                      }.toVector
                    }
                  )

                override lazy val metaType: context.Comp[MetaClass[ArClassWithPayload[context.type, TPayloadSpec]]] =
                  compEv(findClassDef(classValue.metaClassSpecifier.metaClassId).map(MetaClass.apply))

                override lazy val payload: TPayloadSpec[Unit, context.TClassMetadata] = payloadLoader.createClassPayload(context)

                override val classConstructors: context.Comp[Vector[ClassConstructorWithPayload[context.type, TPayloadSpec]]] =
                  context.compCompilationInstance.point(Vector.empty)

              }
            )

          private lazy val dataCtorMap: Comp[Map[Int, DataCtorLoadResult[context.type, TPayloadSpec]]] =
            handleModuleObjectLoading(
              CompilationError.ModuleObjectDataConstructor
            )(
              valueLens = _.dataConstructors
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.DataConstructor, ArgonModule.DataConstructorReference, ArgonModule.DataConstructorDefinition] {
                override def fromModuleObject(value: ArgonModule.DataConstructor): Option[ArgonModule.DataConstructorReference \/ ArgonModule.DataConstructorDefinition] =
                  value match {
                    case ArgonModule.DataConstructor.DataCtorRef(dataCtorRef) => Some(-\/(dataCtorRef))
                    case ArgonModule.DataConstructor.DataCtorDef(dataCtorDef) => Some(\/-(dataCtorDef))
                    case ArgonModule.DataConstructor.UnknownUnionField(_) => None
                  }
              }
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseDataCtorDescriptor
            )(
              referenceHandler = moduleRef => {
                case DataConstructorDescriptor.InNamespace(_, namespace, name, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case DataConstructorScopeValue(dataCtor) => dataCtor
                  }.point[Comp]
              },
              definitionHandler = _ => ???
            )

          private lazy val functionMap: Comp[Map[Int, FunctionLoadResult[context.type, TPayloadSpec]]] =
            handleModuleObjectLoading(
              CompilationError.ModuleObjectFunction
            )(
              valueLens = _.functions
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Function, ArgonModule.FunctionReference, ArgonModule.FunctionDefinition] {
                override def fromModuleObject(value: ArgonModule.Function): Option[ArgonModule.FunctionReference \/ ArgonModule.FunctionDefinition] =
                  value match {
                    case ArgonModule.Function.FuncRef(funcRef) => Some(-\/(funcRef))
                    case ArgonModule.Function.FuncDef(funcDef) => Some(\/-(funcDef))
                    case ArgonModule.Function.UnknownUnionField(_) => None
                  }
              }
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseFunctionDescriptor
            )(
              referenceHandler = moduleRef => {
                case FuncDescriptor.InNamespace(_, namespace, name, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case FunctionScopeValue(func) => func
                  }.point[Comp]
              },
              definitionHandler = _ => ???
            )

          private lazy val methodMap: Comp[Map[Int, MethodLoadResult[context.type, TPayloadSpec]]] =
            handleModuleObjectLoading(
              CompilationError.ModuleObjectMethod
            )(
              valueLens = _.methods
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Method, ArgonModule.MethodReference, ArgonModule.MethodDefinition] {
                override def fromModuleObject(value: ArgonModule.Method): Option[ArgonModule.MethodReference \/ ArgonModule.MethodDefinition] =
                  value match {
                    case ArgonModule.Method.MethodRef(methodRef) => Some(-\/(methodRef))
                    case ArgonModule.Method.MethodDef(methodDef) => Some(\/-(methodDef))
                    case ArgonModule.Method.UnknownUnionField(_) => None
                  }
              }
            )(
              refModuleIdLens = _.moduleId,
              refDescriptorLens = _.descriptor,
              defDescriptorLens = _.descriptor,
            )(
              parseDescriptor = parseMethodDescriptor
            )(
              referenceHandler = moduleRef => {
                case methodDesc @ MethodDescriptor(traitDescriptor: TraitDescriptor, _, _) =>
                  lookupTrait(moduleRef)(traitDescriptor).flatMap { arClassOpt =>
                    arClassOpt.traverseM[Comp, ArMethod[context.type]] { arClass =>
                      compEv.flip(arClass.methods).map { methods =>
                        methods.find { method =>
                          method.descriptor === methodDesc
                        }
                      }
                    }
                  }

                case methodDesc @ MethodDescriptor(classDescriptor : ClassDescriptor, _, _) =>
                  lookupClass(moduleRef)(classDescriptor).flatMap { arClassOpt =>
                    arClassOpt.traverseM[Comp, ArMethod[context.type]] { arClass =>
                      compEv.flip(arClass.methods).map { methods =>
                        methods.find { method =>
                          method.descriptor === methodDesc
                        }
                      }
                    }
                  }

                case methodDesc @ MethodDescriptor(DataConstructorDescriptor.InNamespace(_, namespace, name, _), _, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case DataConstructorScopeValue(dataCtor) => dataCtor
                  }.traverseM[Comp, ArMethod[context.type]] { dataCtor =>
                    compEv.flip(dataCtor.methods).map { methods =>
                      methods.find { method =>
                        method.descriptor === methodDesc
                      }
                    }
                  }
              },
              definitionHandler = _ => ???
            )

          private def createNamespaceElements[TRef, TDef, TResult]
          (
            elementMap: Comp[Map[Int, ModuleObjectLoadResult[TRef, TDef]]]
          )(
            f: TDef => Option[ModuleElement[ScopeValue[CurrentScopeTypes]]]
          ): Comp[Vector[ModuleElement[ScopeValue[CurrentScopeTypes]]]] =
            elementMap.map {
              _.toVector.flatMap {
                case (_, ModuleObjectDefinition(element)) => f(element).toList.toVector
                case (_, ModuleObjectReference(_)) => Vector.empty
              }
            }

          private def combineNamespaceElements(elements: Comp[Vector[ModuleElement[ScopeValue[CurrentScopeTypes]]]]*): Comp[Vector[ModuleElement[ScopeValue[CurrentScopeTypes]]]] =
            elements.toVector.sequence.map { _.flatten }

          private lazy val globalNamespaceComp: Comp[Namespace[ScopeValue[CurrentScopeTypes]]] =
            combineNamespaceElements(
              createNamespaceElements(traitMap) { arTrait =>
                arTrait.descriptor match {
                  case TraitDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    Some(ModuleElement(namespace, NamespaceBinding(name, accessModifier, TraitScopeValue[CurrentScopeTypes](arTrait))))
                }
              },
              createNamespaceElements(classMap) { arClass =>
                arClass.descriptor match {
                  case ClassDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    Some(ModuleElement(namespace, NamespaceBinding(name, accessModifier, ClassScopeValue[CurrentScopeTypes](arClass))))

                  case ClassDescriptor.MetaClass(_) | ClassDescriptor.TraitMetaClass(_) =>
                    None
                }
              },
              createNamespaceElements(dataCtorMap) { dataCtor =>
                dataCtor.descriptor match {
                  case DataConstructorDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    Some(ModuleElement(namespace, NamespaceBinding(name, accessModifier, DataConstructorScopeValue[CurrentScopeTypes](dataCtor))))
                }
              },
              createNamespaceElements(functionMap) { func =>
                func.descriptor match {
                  case FuncDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    Some(ModuleElement(namespace, NamespaceBinding(name, accessModifier, FunctionScopeValue[CurrentScopeTypes](func))))
                }
              },
            ).map(NamespaceBuilder.createNamespace)


          def findTrait(traitId: Int): Comp[ArTrait[context.type]] =
            traitMap.flatMap { _.get(traitId) match {
              case Some(ModuleObjectReference(arTrait)) => arTrait.point[Comp]
              case Some(ModuleObjectDefinition(arTrait)) => arTrait.point[Comp]
              case None =>
                implicitly[Compilation[Comp]].forErrors(
                  CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )

            } }


          def findTraitDef(traitId: Int): Comp[ArTraitWithPayload[context.type, TPayloadSpec]] =
            traitMap.flatMap { _.get(traitId) match {
              case Some(ModuleObjectDefinition(arTrait)) => arTrait.point[Comp]


              case Some(ModuleObjectReference(_)) =>
                implicitly[Compilation[Comp]].forErrors(
                  CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )

              case None =>
                implicitly[Compilation[Comp]].forErrors(
                  CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectTrait, traitId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )

            } }


          def findClass(classId: Int): Comp[ArClass[context.type]] =
            classMap.flatMap { _.get(classId) match {
              case Some(ModuleObjectReference(arClass)) => arClass.point[Comp]
              case Some(ModuleObjectDefinition(arClass)) => arClass.point[Comp]
              case None =>
                implicitly[Compilation[Comp]].forErrors(
                  CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )

            } }


          def findClassDef(classId: Int): Comp[ArClassWithPayload[context.type, TPayloadSpec]] =
            classMap.flatMap { _.get(classId) match {
              case Some(ModuleObjectDefinition(arClass)) => arClass.point[Comp]


              case Some(ModuleObjectReference(_)) =>
                implicitly[Compilation[Comp]].forErrors(
                  CompilationError.ModuleObjectMustBeDefinition(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )

              case None =>
                implicitly[Compilation[Comp]].forErrors(
                  CompilationError.ModuleObjectInvalidId(CompilationError.ModuleObjectClass, classId, CompilationMessageSource.ReferencedModule(currentModuleDescriptor))
                )

            } }

          def resolveTraitType(traitType: ArgonModule.TraitType): Comp[TraitType[context.typeSystem.type]] = ???
          def resolveClassType(classType: ArgonModule.ClassType): Comp[ClassType[context.typeSystem.type]] = ???

          def resolveParameter(parameter: ArgonModule.Parameter): Comp[Parameter[context.typeSystem.type]] = ???

          override lazy val module: Comp[ArModuleWithPayload[context.type, TPayloadSpec]] =
            for {
              globalNamespaceCompValue <- globalNamespaceComp
            } yield new ArModuleWithPayload[context.type, TPayloadSpec] {
              override val context: context2.type = context2
              override val descriptor: ModuleDescriptor = currentModuleDescriptor
              override lazy val globalNamespace: Namespace[ScopeValue[CurrentScopeTypes]] = globalNamespaceCompValue
              override val referencedModules: Vector[ArModule[context.type]] =
                refModuleMap.values.collect {
                  case ModuleReference(moduleRef) => moduleRef
                }(collection.breakOut)
            }

        }.module

      } yield module
    }

    impl[context.Comp](context.compCompilationInstance, LeibnizK.refl)
  }

  private trait PayloadLoader[TPayloadSpec[_, _]] {

    def createClassPayload(context: Context): TPayloadSpec[Unit, context.TClassMetadata]
    def createTraitPayload(context: Context): TPayloadSpec[Unit, context.TTraitMetadata]
    def createDataConstructorPayload(context: Context): TPayloadSpec[context.Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata]
    def createFunctionPayload(context: Context): TPayloadSpec[context.Comp[context.TFunctionImplementation], context.TFunctionMetadata]
    def createMethodPayload(context: Context): TPayloadSpec[context.Comp[context.TMethodImplementation], context.TMethodMetadata]
    def createClassConstructorPayload(context: Context): TPayloadSpec[context.Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata]

  }

  private val referencePayloadLoader: PayloadLoader[PayloadSpecifiers.ReferencePayloadSpecifier] =
    new PayloadLoader[PayloadSpecifiers.ReferencePayloadSpecifier] {

      override def createClassPayload(context: Context): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ???

      override def createTraitPayload(context: Context): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ???

      override def createDataConstructorPayload(context: Context): ReferencePayloadSpecifier[context.Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ???

      override def createFunctionPayload(context: Context): ReferencePayloadSpecifier[context.Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ???

      override def createMethodPayload(context: Context): ReferencePayloadSpecifier[context.Comp[context.TMethodImplementation], context.TMethodMetadata] = ???

      override def createClassConstructorPayload(context: Context): ReferencePayloadSpecifier[context.Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ???
    }

  sealed trait ModuleLoadResult[TContext <: Context]
  final case class ModuleReference[TContext <: Context](module: ArModule[TContext]) extends ModuleLoadResult[TContext]
  final case class ModuleNotFound[TContext <: Context](moduleRef: ArgonModule.ModuleReference) extends ModuleLoadResult[TContext]

  sealed trait ModuleObjectLoadResult[+TReference, +TDefinition]
  final case class ModuleObjectReference[+TReference, +TDefinition](value: TReference) extends ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectDefinition[+TReference, +TDefinition](value: TDefinition) extends ModuleObjectLoadResult[TReference, TDefinition]

  type TraitLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArTrait[TContext], ArTraitWithPayload[TContext, PayloadSpec]]
  type ClassLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArClass[TContext], ArClassWithPayload[TContext, PayloadSpec]]
  type DataCtorLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[DataConstructor[TContext], DataConstructorWithPayload[TContext, PayloadSpec]]
  type FunctionLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArFunc[TContext], ArFuncWithPayload[TContext, PayloadSpec]]
  type MethodLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArMethod[TContext], ArMethodWithPayload[TContext, PayloadSpec]]

}
