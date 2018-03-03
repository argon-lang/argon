package com.mi3software.argon.compiler

import com.mi3software.argon.module.ArgonModule
import scalaz._
import Scalaz._
import com.mi3software.argon.util.{Compilation, LeibnizK}

object ArgonModuleLoader {

  private val currentFormatVersion = 1

  def loadModuleReference[Comp[_] : Monad : Compilation]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type]])
  (pbModule: ArgonModule.Module)
  (implicit compEv: LeibnizK[Comp, context.Comp])
  : Comp[ArModuleReference[context.type]] = {

    import context.{ Comp => _, _ }
    val context2: context.type = context

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
      override lazy val globalNamespace: context.Comp[Namespace[ScopeValue[ReferenceScopeTypes]]] = ??? : context.Comp[Namespace[ScopeValue[ReferenceScopeTypes]]]
    }
  }

}
