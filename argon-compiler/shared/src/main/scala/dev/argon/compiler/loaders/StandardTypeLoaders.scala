package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup.{LookupNames, ModuleLookup}
import dev.argon.util.NamespacePath
import cats.implicits._
import dev.argon.compiler.expr.ArExpr.ClassType

object StandardTypeLoaders {

  private val arCore = ModuleId(LookupNames.argonCoreLib)

  def loadUnitType[TPayloadSpec[_, _]: PayloadSpecInfo]
  (context: Context)
  (messageSource: => DiagnosticSource)
  (currentModule: ArModule[context.type, TPayloadSpec])
  (referencedModules: Vector[ArModule[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
  : Comp[context.typeSystem.TType] = {

    val noArgs = ErasedSignature.ParameterOnlySignature[context.type](Vector())

    {
      if(currentModule.id === arCore)
        currentModule.lookupNamespaceBindings(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))(ModuleLookup.lookupGlobalClass(context)(noArgs))
            .map(AbsRef(_))
      else
        ModuleLookup.lookupValues(context)(referencedModules)(arCore)(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))(ModuleLookup.lookupGlobalClass(context)(noArgs))
          .map(AbsRef(_))

    }
      .take(2)
      .runCollect
      .flatMap { foundClasses =>
        val unitClassOpt =
          if(foundClasses.size === 1) foundClasses.headOption
          else None

        Compilation.requireSome(unitClassOpt)(
          DiagnosticError.NamespaceElementNotFound(arCore, NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"), messageSource)
        )
      }
      .map { unitClass =>
        context.typeSystem.fromSimpleType(ClassType(unitClass, Vector()))
      }
  }


}
