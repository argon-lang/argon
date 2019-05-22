package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup.{LookupNames, ModuleLookup}
import dev.argon.util.NamespacePath
import cats._
import cats.implicits._

object StandardTypeLoaders {

  private val arCore = ModuleDescriptor(LookupNames.argonCoreLib)

  def loadUnitType[TPayloadSpec[_, _]]
  (context: Context)
  (messageSource: => CompilationMessageSource)
  (currentModule: ArModule[context.type, TPayloadSpec])
  (referencedModules: Vector[ArModule[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
  : context.Comp[context.typeSystem.TType] =
    context.compCompilationInstance.flatMap(
      context.compCompilationInstance.flatMap(
        if(currentModule.descriptor === arCore)
          context.compCompilationInstance.map(
            ModuleLookup.lookupNamespaceValue(context)(currentModule)(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))(ModuleLookup.lookupGlobalClass)
          ) { _.map(AbsRef.apply) }
        else
          context.compCompilationInstance.map(
            ModuleLookup.lookupValue(context)(referencedModules)(arCore)(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))(ModuleLookup.lookupGlobalClass)
          )  { _.map(AbsRef.apply) }
      ) { unitClassOpt =>
        context.compCompilationInstance.requireSome(unitClassOpt)(
          CompilationError.NamespaceElementNotFound(arCore, NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"), messageSource)
        )
      }
    ) { unitClass =>
      context.compCompilationInstance.map(unitClass.value.signature) { sig =>
        context.typeSystem.fromSimpleType(context.typeSystem.ClassType(unitClass, Vector(), sig.unsubstitutedResult.baseTypes))
      }
    }



}
