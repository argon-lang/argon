package com.mi3software.argon.compiler

import java.io.File
import java.util.Locale

import com.mi3software.argon.module.ArgonModule
import scalaz.{Lens => _, _}
import Scalaz._
import com.mi3software.argon.util._
import scalapb.json4s.JsonFormat
import scalaz.effect.IO
import shapeless._

import scala.collection.immutable._

object ArgonModuleLoader extends ModuleLoader {

  override type ModuleData = ArgonModule.Module
  override def loadFile(file: File): IO[Option[ArgonModule.Module]] =
    IO { file.getName.toLowerCase(Locale.ENGLISH) }.flatMap { ext =>
      if(ext.endsWith(".armodule.json"))
        FileOperations.readAllText(file)
          .map(JsonFormat.fromJsonString[ArgonModule.Module])
          .map(Some.apply)
      else
        IO(None : Option[ArgonModule.Module])
    }

  override def dataDescriptor(data: ArgonModule.Module): Option[ModuleDescriptor] =
    data.name.map(ModuleDescriptor.apply)

  override def dataReferencedModules(data: ArgonModule.Module): Vector[ModuleDescriptor] =
    data.referencedModules.collect {
      case ArgonModule.ModuleReference(Some(name)) => ModuleDescriptor(name)
    }


  override def loadModuleReference
  (context: Context)
  (data: ArgonModule.Module)
  (referencedModules: Vector[ArModule[context.type]])
  : context.Comp[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]] =
    loadModule(context)(data)(referencedModules)

  private val currentFormatVersion = 1

  private trait ModuleCreator[TContext <: Context, Comp[_], TPayloadSpec[_, _]] {
    val module: Comp[ArModuleWithPayload[TContext, TPayloadSpec]]
  }

  def loadModule[TPayloadSpec[_, _]]
  (context: Context)
  (pbModule: ArgonModule.Module)
  (referencedModules: Vector[ArModule[context.type]])
  : context.Comp[ArModuleWithPayload[context.type, TPayloadSpec]] = {

    type CurrentScopeTypes = context.ScopeTypesWithPayload[TPayloadSpec]
    val context2: context.type = context

    def impl[Comp[_] : Monad : Compilation](implicit compEv: LeibnizK[Comp, context.Comp]): Comp[ArModuleWithPayload[context.type, TPayloadSpec]] =
      for {

        desc <-
          pbModule.name match {
            case None =>
              val desc = ModuleDescriptor("unknown-module")
              implicitly[Compilation[Comp]].forErrors(desc, CompilationError.MissingModuleName(CompilationMessageSource.ReferencedModule(desc)))

            case Some(name) =>
              implicitly[Monad[Comp]].point(ModuleDescriptor(name))
          }

        _ <-
          if(pbModule.formatVersion === 0 || pbModule.formatVersion > currentFormatVersion)
            implicitly[Compilation[Comp]].forErrors((), CompilationError.UnsupportedModuleFormatVersion(pbModule.formatVersion, CompilationMessageSource.ReferencedModule(desc)))
          else
            implicitly[Monad[Comp]].point(())

        module <- new ModuleCreator[context.type, Comp, TPayloadSpec] {


          private def parseNamespacePath(ns: Option[ArgonModule.Namespace]): NamespacePath =
            ns match {
              case Some(ArgonModule.Namespace(parts)) => NamespacePath(parts.toVector)
              case None => NamespacePath.empty
            }

          private object ValidGlobalName {
            def unapply(globalName: Option[ArgonModule.GlobalName]): Option[GlobalName] =
              globalName.collect {
                case ArgonModule.GlobalName(ArgonModule.GlobalName.GlobalNameType.NormalName(name)) => GlobalName.Normal(name)
                case ArgonModule.GlobalName(ArgonModule.GlobalName.GlobalNameType.Unnamed(ArgonModule.UnnamedGlobalName(Some(fileID), Some(index)))) =>
                  GlobalName.Unnamed(FileID(fileID), index)
              }
          }

          private object ParsedAccessModifier {
            def unapply(accessModifier: ArgonModule.AccessModifier): Option[AccessModifier] =
              accessModifier match {
                case ArgonModule.AccessModifier.Invalid | ArgonModule.AccessModifier.Unrecognized(_) => None
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
            pbModule.referencedModules.zipWithIndex
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

          private def parseTraitDescriptor(module: ModuleDescriptor)(desc: Option[ArgonModule.TraitDescriptor]): Option[TraitDescriptor] =
            desc.flatMap {
              case ArgonModule.TraitDescriptor(ArgonModule.TraitDescriptor.TraitDescType.InNamespace(
                ArgonModule.TraitDescriptorInNamespace(
                  Some(ArgonModule.FileSpec(Some(fileID), Some(fileName))),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              )) =>
                Some(TraitDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case _ =>
                None
            }

          private def parseClassDescriptor(module: ModuleDescriptor)(desc: Option[ArgonModule.ClassDescriptor]): Option[ClassDescriptor] =
            desc.flatMap {
              case ArgonModule.ClassDescriptor(ArgonModule.ClassDescriptor.ClassDescType.InNamespace(
                ArgonModule.ClassDescriptorInNamespace(
                  Some(ArgonModule.FileSpec(Some(fileID), Some(fileName))),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              )) =>
                Some(ClassDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case ArgonModule.ClassDescriptor(ArgonModule.ClassDescriptor.ClassDescType.MetaClass(
              ArgonModule.ClassDescriptorMetaClass(Some(ownerClassDescriptor))
              )) =>
                ???

              case ArgonModule.ClassDescriptor(ArgonModule.ClassDescriptor.ClassDescType.TraitMetaClass(
              ArgonModule.ClassDescriptorTraitMetaClass(Some(ownerTraitDescriptor))
              )) =>
                ???

              case _ =>
                None
            }

          private def parseDataCtorDescriptor(module: ModuleDescriptor)(desc: Option[ArgonModule.DataConstructorDescriptor]): Option[DataConstructorDescriptor] =
            desc.flatMap {
              case ArgonModule.DataConstructorDescriptor(ArgonModule.DataConstructorDescriptor.DataConstructorDescType.InNamespace(
                ArgonModule.DataConstructorDecscriptorInNamespace(
                  Some(ArgonModule.FileSpec(Some(fileID), Some(fileName))),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              )) =>
                Some(DataConstructorDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case _ =>
                None
            }

          private def parseFunctionDescriptor(module: ModuleDescriptor)(desc: Option[ArgonModule.FunctionDescriptor]): Option[FuncDescriptor] =
            desc.flatMap {
              case ArgonModule.FunctionDescriptor(ArgonModule.FunctionDescriptor.FunctionDescType.InNamespace(
                ArgonModule.FunctionDescriptorInNamespace(
                  Some(ArgonModule.FileSpec(Some(fileID), Some(fileName))),
                  ns,
                  ValidGlobalName(name),
                  ParsedGlobalAccessModifier(accessModifier)
                )
              )) =>
                Some(FuncDescriptor.InNamespace(module, parseNamespacePath(ns), name, accessModifier))

              case _ =>
                None
            }

          private trait ModuleObjectRefDef[TValue, TRef, TDef] {
            def fromModuleObject(value: TValue): Option[TRef \/ TDef]
          }

          private def handleModuleObjectLoading
          [TValue, TRef, TDef, TValueDescriptor, TResultDescriptor, TRefResult, TDefResult]
          (
            valueLens: Lens[ArgonModule.Module, Vector[TValue]]
          )(
            refDef: ModuleObjectRefDef[TValue, TRef, TDef]
          )(
            refModuleIdLens: Lens[TRef, Int],
            refDescriptorLens: Lens[TRef, TValueDescriptor],
            defDescriptorLens: Lens[TDef, TValueDescriptor]
          )(
            parseDescriptor: ModuleDescriptor => TValueDescriptor => Option[TResultDescriptor]
          )(
            referenceHandler: ArModule[context.type] => TResultDescriptor => Option[TRefResult],
            definitionHandler: TResultDescriptor => TDefResult
          ): Map[Int, ModuleObjectLoadResult[TRefResult, TDefResult]] =
            valueLens.get(pbModule).zipWithIndex.map { case (moduleObject, i) =>
              i -> ((refDef.fromModuleObject(moduleObject) match {
                case Some(-\/(refValue)) =>
                  refModuleMap.get(refModuleIdLens.get(refValue)) match {
                    case Some(ModuleReference(moduleRef)) =>
                      parseDescriptor(moduleRef.descriptor)(refDescriptorLens.get(refValue)) match {
                        case Some(descriptor) =>
                          referenceHandler(moduleRef)(descriptor) match {
                            case Some(refResult) => ModuleObjectReference(refResult)
                            case None => ModuleObjectNotFound()
                          }

                        case None =>
                          ModuleObjectInvalidDescriptor()
                      }

                    case _ =>
                      ModuleObjectModuleNotLoaded()
                  }

                case Some(\/-(defValue)) =>
                  parseDescriptor(desc)(defDescriptorLens.get(defValue)) match {
                    case Some(descriptor) =>
                      ModuleObjectDefinition(definitionHandler(descriptor))

                    case None =>
                      ModuleObjectInvalidDescriptor()
                  }

                case None =>
                  ModuleObjectUndefined()
              }) : ModuleObjectLoadResult[TRefResult, TDefResult])
            }(collection.breakOut)


          private lazy val traitMap: Map[Int, TraitLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading(
              valueLens = lens[ArgonModule.Module] >> 'traits
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Trait, ArgonModule.TraitReference, ArgonModule.TraitDefinition] {
                override def fromModuleObject(value: ArgonModule.Trait): Option[ArgonModule.TraitReference \/ ArgonModule.TraitDefinition] =
                  value.traitValue match {
                    case ArgonModule.Trait.TraitValue.TraitRef(traitRef) => Some(-\/(traitRef))
                    case ArgonModule.Trait.TraitValue.TraitDef(traitDef) => Some(\/-(traitDef))
                    case _ => None
                  }
              }
            )(
              refModuleIdLens = lens[ArgonModule.TraitReference] >> 'moduleId,
              refDescriptorLens = lens[ArgonModule.TraitReference] >> 'descriptor,
              defDescriptorLens = lens[ArgonModule.TraitDefinition] >> 'descriptor,
            )(
              parseDescriptor = parseTraitDescriptor
            )(
              referenceHandler = moduleRef => {
                case TraitDescriptor.InNamespace(_, namespace, name, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case TraitScopeValue(arTrait) => arTrait
                  }
              },
              definitionHandler = _ => ???
            )

          private lazy val classMap: Map[Int, ClassLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading(
              valueLens = lens[ArgonModule.Module] >> 'classes
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Class, ArgonModule.ClassReference, ArgonModule.ClassDefinition] {
                override def fromModuleObject(value: ArgonModule.Class): Option[ArgonModule.ClassReference \/ ArgonModule.ClassDefinition] =
                  value.classValue match {
                    case ArgonModule.Class.ClassValue.ClassRef(classRef) => Some(-\/(classRef))
                    case ArgonModule.Class.ClassValue.ClassDef(classDef) => Some(\/-(classDef))
                    case _ => None
                  }
              }
            )(
              refModuleIdLens = lens[ArgonModule.ClassReference] >> 'moduleId,
              refDescriptorLens = lens[ArgonModule.ClassReference] >> 'descriptor,
              defDescriptorLens = lens[ArgonModule.ClassDefinition] >> 'descriptor,
            )(
              parseDescriptor = parseClassDescriptor
            )(
              referenceHandler = moduleRef => {
                case ClassDescriptor.InNamespace(_, namespace, name, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case ClassScopeValue(arClass) => arClass
                  }
              },
              definitionHandler = _ => ???
            )

          private lazy val dataCtorMap: Map[Int, DataCtorLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading(
              valueLens = lens[ArgonModule.Module] >> 'dataConstructors
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.DataConstructor, ArgonModule.DataConstructorReference, ArgonModule.DataConstructorDefinition] {
                override def fromModuleObject(value: ArgonModule.DataConstructor): Option[ArgonModule.DataConstructorReference \/ ArgonModule.DataConstructorDefinition] =
                  value.dataConstructorValue match {
                    case ArgonModule.DataConstructor.DataConstructorValue.DataCtorRef(dataCtorRef) => Some(-\/(dataCtorRef))
                    case ArgonModule.DataConstructor.DataConstructorValue.DataCtorDef(dataCtorDef) => Some(\/-(dataCtorDef))
                    case _ => None
                  }
              }
            )(
              refModuleIdLens = lens[ArgonModule.DataConstructorReference] >> 'moduleId,
              refDescriptorLens = lens[ArgonModule.DataConstructorReference] >> 'descriptor,
              defDescriptorLens = lens[ArgonModule.DataConstructorDefinition] >> 'descriptor,
            )(
              parseDescriptor = parseDataCtorDescriptor
            )(
              referenceHandler = moduleRef => {
                case DataConstructorDescriptor.InNamespace(_, namespace, name, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case DataConstructorScopeValue(dataCtor) => dataCtor
                  }
              },
              definitionHandler = _ => ???
            )

          private lazy val functionMap: Map[Int, FunctionLoadResult[context.type, TPayloadSpec]] =
            handleModuleObjectLoading(
              valueLens = lens[ArgonModule.Module] >> 'functions
            )(
              refDef = new ModuleObjectRefDef[ArgonModule.Function, ArgonModule.FunctionReference, ArgonModule.FunctionDefinition] {
                override def fromModuleObject(value: ArgonModule.Function): Option[ArgonModule.FunctionReference \/ ArgonModule.FunctionDefinition] =
                  value.functionType match {
                    case ArgonModule.Function.FunctionType.FuncRef(funcRef) => Some(-\/(funcRef))
                    case ArgonModule.Function.FunctionType.FuncDef(funcDef) => Some(\/-(funcDef))
                    case _ => None
                  }
              }
            )(
              refModuleIdLens = lens[ArgonModule.FunctionReference] >> 'moduleId,
              refDescriptorLens = lens[ArgonModule.FunctionReference] >> 'descriptor,
              defDescriptorLens = lens[ArgonModule.FunctionDefinition] >> 'descriptor,
            )(
              parseDescriptor = parseFunctionDescriptor
            )(
              referenceHandler = moduleRef => {
                case FuncDescriptor.InNamespace(_, namespace, name, _) =>
                  lookupNamespaceValue(moduleRef)(namespace, name) {
                    case FunctionScopeValue(arTrait) => arTrait
                  }
              },
              definitionHandler = _ => ???
            )

          private def createNamespaceElements[TRef, TDef, TResult]
          (
            elementMap: Map[Int, ModuleObjectLoadResult[TRef, TDef]]
          )(
            moduleObjectType: CompilationError.ModuleObjectType
          )(
            f: TDef => ModuleElement[ScopeValue[CurrentScopeTypes]]
          ): Comp[Vector[ModuleElement[ScopeValue[CurrentScopeTypes]]]] =
            elementMap.toVector.traverseM[Comp, ModuleElement[ScopeValue[CurrentScopeTypes]]] {
              case (_, ModuleObjectDefinition(element)) => Vector(f(element)).point[Comp]
              case (_, ModuleObjectReference(_)) => Vector.empty.point[Comp]
              case (id, ModuleObjectInvalidDescriptor()) =>
                implicitly[Compilation[Comp]].forErrors(Vector.empty, CompilationError.ModuleObjectInvalidDescriptor(moduleObjectType, id, CompilationMessageSource.ReferencedModule(desc)))
              case (id, ModuleObjectModuleNotLoaded()) =>
                implicitly[Compilation[Comp]].forErrors(Vector.empty, CompilationError.ModuleObjectModuleNotLoaded(moduleObjectType, id, CompilationMessageSource.ReferencedModule(desc)))
              case (id, ModuleObjectNotFound()) =>
                implicitly[Compilation[Comp]].forErrors(Vector.empty, CompilationError.ModuleObjectNotFound(moduleObjectType, id, CompilationMessageSource.ReferencedModule(desc)))
              case (id, ModuleObjectUndefined()) =>
                implicitly[Compilation[Comp]].forErrors(Vector.empty, CompilationError.ModuleObjectUndefined(moduleObjectType, id, CompilationMessageSource.ReferencedModule(desc)))
            }

          private def combineNamespaceElements(elements: Comp[Vector[ModuleElement[ScopeValue[CurrentScopeTypes]]]]*): Comp[Vector[ModuleElement[ScopeValue[CurrentScopeTypes]]]] =
            elements.toVector.sequence.map { _.flatten }

          private lazy val globalNamespaceComp: Comp[Namespace[ScopeValue[CurrentScopeTypes]]] =
            combineNamespaceElements(
              createNamespaceElements(traitMap)(CompilationError.ModuleObjectTrait) { arTrait =>
                arTrait.descriptor match {
                  case TraitDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    ModuleElement(namespace, NamespaceBinding(name, accessModifier, TraitScopeValue[CurrentScopeTypes](arTrait)))
                }
              },
              createNamespaceElements(classMap)(CompilationError.ModuleObjectClass) { arClass =>
                arClass.descriptor match {
                  case ClassDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    ModuleElement(namespace, NamespaceBinding(name, accessModifier, ClassScopeValue[CurrentScopeTypes](arClass)))
                }
              },
              createNamespaceElements(dataCtorMap)(CompilationError.ModuleObjectDataConstructor) { dataCtor =>
                dataCtor.descriptor match {
                  case DataConstructorDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    ModuleElement(namespace, NamespaceBinding(name, accessModifier, DataConstructorScopeValue[CurrentScopeTypes](dataCtor)))
                }
              },
              createNamespaceElements(functionMap)(CompilationError.ModuleObjectFunction) { func =>
                func.descriptor match {
                  case FuncDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    ModuleElement(namespace, NamespaceBinding(name, accessModifier, FunctionScopeValue[CurrentScopeTypes](func)))
                }
              },
            ).map(NamespaceBuilder.createNamespace)

          override lazy val module: Comp[ArModuleWithPayload[context.type, TPayloadSpec]] =
            for {
              globalNamespaceCompValue <- globalNamespaceComp
            } yield new ArModuleWithPayload[context.type, TPayloadSpec] {
              override val context: context2.type = context2
              override val descriptor: ModuleDescriptor = desc
              override lazy val globalNamespace: Namespace[ScopeValue[CurrentScopeTypes]] = globalNamespaceCompValue
            }

        }.module

      } yield module

    impl[context.Comp](context.compMonadInstance, context.compCompilationInstance, LeibnizK.refl)
  }

  sealed trait ModuleLoadResult[TContext <: Context]
  final case class ModuleReference[TContext <: Context](module: ArModule[TContext]) extends ModuleLoadResult[TContext]
  final case class ModuleNotFound[TContext <: Context](moduleRef: ArgonModule.ModuleReference) extends ModuleLoadResult[TContext]

  sealed trait ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectReference[TReference, TDefinition](value: TReference) extends ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectDefinition[TReference, TDefinition](value: TDefinition) extends ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectUndefined[TReference, TDefinition]() extends ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectInvalidDescriptor[TReference, TDefinition]() extends ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectModuleNotLoaded[TReference, TDefinition]() extends ModuleObjectLoadResult[TReference, TDefinition]
  final case class ModuleObjectNotFound[TReference, TDefinition]() extends ModuleObjectLoadResult[TReference, TDefinition]

  type TraitLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArTrait[TContext], ArTraitWithPayload[TContext, PayloadSpec]]
  type ClassLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArClass[TContext], ArClassWithPayload[TContext, PayloadSpec]]
  type DataCtorLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[DataConstructor[TContext], DataConstructorWithPayload[TContext, PayloadSpec]]
  type FunctionLoadResult[TContext <: Context, PayloadSpec[_, _]] = ModuleObjectLoadResult[ArFunc[TContext], ArFuncWithPayload[TContext, PayloadSpec]]

}
