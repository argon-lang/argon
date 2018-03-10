package com.mi3software.argon.compiler

import com.mi3software.argon.module.ArgonModule
import scalaz._
import Scalaz._
import com.mi3software.argon.util.{Compilation, LeibnizK}

import scala.collection.immutable._

object ArgonModuleLoader {

  private val currentFormatVersion = 1

  def loadModuleReference
  (context: Context)
  (pbModule: ArgonModule.Module)
  : context.Comp[ArModuleReference[context.type]] = {

    import context.ReferenceScopeTypes
    val context2: context.type = context

    def impl[Comp[_] : Monad : Compilation](implicit compEv: LeibnizK[Comp, context.Comp]): Comp[ArModuleReference[context.type]] =
      for {
        desc <-
          if(pbModule.name === "") {
            val desc = ModuleDescriptor("unknown-module")
            implicitly[Compilation[Comp]].forErrors(desc, CompilationError.MissingModuleName(CompilationMessageSource.ReferencedModule(desc)))
          }
          else
            implicitly[Monad[Comp]].point(ModuleDescriptor(pbModule.name))

        _ <-
          if(pbModule.formatVersion === 0 || pbModule.formatVersion > currentFormatVersion)
            implicitly[Compilation[Comp]].forErrors((), CompilationError.UnsupportedModuleFormatVersion(pbModule.formatVersion, CompilationMessageSource.ReferencedModule(desc)))
          else
            implicitly[Monad[Comp]].point(())


      } yield new ArModuleReference[context.type] {
        override val context: context2.type = context2
        override val descriptor: ModuleDescriptor = desc

        private lazy val refModuleMap: Map[Int, UnloadedModule \/ ArModule[context.type]] =
          pbModule.referencedModules.zipWithIndex
            .map { case (modRef, i) =>
              i -> context.referencedModules
                .find { _.descriptor.name === modRef.name }
                .toRightDisjunction { UnloadedModule(modRef) }
            }(collection.breakOut)

        private def lookupNamespaceValue[T]
        (moduleId: Int)
        (namespace: Option[ArgonModule.Namespace], name: String)
        (f: PartialFunction[ScopeValue[context.ContextScopeTypes], T])
        : Option[T] = {
          val namespaceParts = namespace
            .map { ns => ns.parts.toVector }
            .getOrElse { Vector.empty }

          def impl(namespaceParts: Vector[String])(namespaceValues: Namespace[ScopeValue[context.ContextScopeTypes]]): Option[T] =
            namespaceParts match {
              case head +: tail =>
                namespaceValues.bindings.find(_.name === head) match {
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

          refModuleMap.get(moduleId) match {
            case None | Some(-\/(_)) => None
            case Some(\/-(module)) => impl(namespaceParts)(module.globalNamespace)
          }
        }

        private lazy val traitMap: Map[Int, TraitLoadResult[context.type]] =
          pbModule.traits.zipWithIndex
            .map { case (traitInfo, i) =>
              i -> ((traitInfo.traitValue match {
                case ArgonModule.Trait.TraitValue.Empty => TraitUnloaded(i, None)
                case ArgonModule.Trait.TraitValue.TraitRef(traitRef @ ArgonModule.TraitReference(moduleId, namespace, name)) =>
                  lookupNamespaceValue(moduleId)(namespace, name) {
                    case TraitScopeValue(arTrait) => TraitLoaded(arTrait)
                  }
                    .getOrElse { TraitUnloaded(i, Some(traitRef)) }

                case ArgonModule.Trait.TraitValue.TraitDef(traitDef) =>
                  ???

              }) : TraitLoadResult[context.type])
            }(collection.breakOut)

        private lazy val classMap: Map[Int, ClassLoadResult[context.type]] =
          pbModule.classes.zipWithIndex
            .map { case (classInfo, i) =>
              i -> ((classInfo.classValue match {
                case ArgonModule.Class.ClassValue.Empty => ClassUnloaded(i, None)
                case ArgonModule.Class.ClassValue.ClassRef(classRef @ ArgonModule.ClassReference(moduleId, namespace, name)) =>
                  lookupNamespaceValue(moduleId)(namespace, name) {
                    case ClassScopeValue(arClass) => ClassLoaded(arClass)
                  }
                    .getOrElse { ClassUnloaded(i, Some(classRef)) }

                case ArgonModule.Class.ClassValue.ClassDef(classDef) =>
                  ???

              }) : ClassLoadResult[context.type])
            }(collection.breakOut)

        private lazy val dataCtorMap: Map[Int, DataCtorLoadResult[context.type]] =
          pbModule.dataConstructors.zipWithIndex
            .map { case (dataCtorInfo, i) =>
              i -> ((dataCtorInfo.dataConstructorValue match {
                case ArgonModule.DataConstructor.DataConstructorValue.Empty => DataCtorUnloaded(i, None)
                case ArgonModule.DataConstructor.DataConstructorValue.DataCtorRef(dataCtorRef @ ArgonModule.DataConstructorReference(moduleId, namespace, name)) =>
                  lookupNamespaceValue(moduleId)(namespace, name) {
                    case DataConstructorScopeValue(dataConstructor) => DataCtorLoaded(dataConstructor)
                  }
                    .getOrElse { DataCtorUnloaded(i, Some(dataCtorRef)) }

                case ArgonModule.DataConstructor.DataConstructorValue.DataCtorDef(traitDef) =>
                  ???

              }) : DataCtorLoadResult[context.type])
            }(collection.breakOut)

        override lazy val globalNamespace: Namespace[ScopeValue[ReferenceScopeTypes]] =
          NamespaceBuilder.createNamespace(
            traitMap.flatMap {
              case (_, TraitDefinition(arTrait)) =>
                arTrait.descriptor match {
                  case TraitDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                    Vector(ModuleElement(namespace, NamespaceBinding(name, accessModifier, TraitScopeValue[ReferenceScopeTypes](arTrait))))
                }

              case (_, _) => Vector.empty
            }.toVector ++
              classMap.flatMap {
                case (_, ClassDefinition(arClass)) =>
                  arClass.descriptor match {
                    case ClassDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                      Vector(ModuleElement(namespace, NamespaceBinding(name, accessModifier, ClassScopeValue[ReferenceScopeTypes](arClass))))
                  }

                case (_, _) => Vector.empty
              }.toVector ++
              dataCtorMap.flatMap {
                case (_, DataCtorDefinition(dataCtor)) =>
                  dataCtor.descriptor match {
                    case DataConstructorDescriptor.InNamespace(_, namespace, name, accessModifier) =>
                      Vector(ModuleElement(namespace, NamespaceBinding(name, accessModifier, DataConstructorScopeValue[ReferenceScopeTypes](dataCtor))))
                  }

                case (_, _) => Vector.empty
              }.toVector
          )
      }

    impl[context.Comp](context.compMonadInstance, context.compCompilationInstance, LeibnizK.refl)
  }

  final case class UnloadedModule(moduleRef: ArgonModule.ModuleReference)

  sealed trait TraitLoadResult[TContext <: Context]
  final case class TraitLoaded[TContext <: Context](arTrait: ArTrait[TContext]) extends TraitLoadResult[TContext]
  final case class TraitUnloaded[TContext <: Context](traitId: Int, traitRef: Option[ArgonModule.TraitReference]) extends TraitLoadResult[TContext]
  final case class TraitDefinition[TContext <: Context](arTrait: ArTraitReference[TContext]) extends TraitLoadResult[TContext]

  sealed trait ClassLoadResult[TContext <: Context]
  final case class ClassLoaded[TContext <: Context](arClass: ArClass[TContext]) extends ClassLoadResult[TContext]
  final case class ClassUnloaded[TContext <: Context](classId: Int, classRef: Option[ArgonModule.ClassReference]) extends ClassLoadResult[TContext]
  final case class ClassDefinition[TContext <: Context](arClass: ArClassReference[TContext]) extends ClassLoadResult[TContext]

  sealed trait DataCtorLoadResult[TContext <: Context]
  final case class DataCtorLoaded[TContext <: Context](dataCtor: DataConstructor[TContext]) extends DataCtorLoadResult[TContext]
  final case class DataCtorUnloaded[TContext <: Context](dataCtorId: Int, dataCtorRef: Option[ArgonModule.DataConstructorReference]) extends DataCtorLoadResult[TContext]
  final case class DataCtorDefinition[TContext <: Context](dataCtor: DataConstructorReference[TContext]) extends DataCtorLoadResult[TContext]

}
