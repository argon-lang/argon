package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.module.*
import dev.argon.io.*
import dev.argon.plugin.ImporterC
import zio.*
import dev.argon.util.{*, given}

object SourceTube {

  def make
    (
      context: Context,
      tubeImporter: TubeImporter & HasContext[context.type],
      tubeName: TubeName,
      opts: context.Options,
      modules: Map[ModulePath, ArgonSourceCodeResource[context.Env, context.Error]],
    )
    : UIO[ArTubeC & HasContext[context.type]] =
    val context2: context.type = context
    val tubeName2 = tubeName
    for {
      loadModule <-
        ZIO.memoize[CompEnv, CompError, (ArTubeC & HasContext[context.type] & HasImplementation[true], ModulePath), ArModuleC & HasContext[context.type] & HasImplementation[true]] { args =>
          val (tube, path) = args
          val moduleName = ModuleName(tubeName, path)
          modules.get(path) match {
            case Some(module) => SourceModule.make(context, tubeImporter, tube, moduleName, module)
            case None => ZIO.fail(DiagnosticError.UnknownModuleException(moduleName))
          }
        }
    } yield new ArTubeC {
      override type IsImplementation = true

      override val context: context2.type = context2
      override val tubeName: TubeName = tubeName2

      override val options: context.Options = opts

      override def module(path: ModulePath): Comp[ArModule & HasImplementation[true]] = loadModule((this, path))
      override lazy val modulePaths: Set[ModulePath] = modules.keySet

      override def asDeclaration: Option[this.type & HasImplementation[true]] = Some(this)
    }
  end make

}
