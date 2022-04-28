package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.module.*
import dev.argon.io.*
import zio.*
import dev.argon.util.{*, given}

object SourceTube {

  def make
    (
      context: Context,
      tubeName: TubeName,
      importer: ImporterC with HasContext[context.type],
      modules: Map[ModulePath, SourceCodeResource],
    )
    : UIO[ArTubeC with HasContext[context.type]] =
    val context2: context.type = context
    val tubeName2 = tubeName
    for {
      loadModule <-
        ZIO.memoize[CompEnv, CompError, (ArTubeC with HasContext[context.type], ModulePath), ArModuleC with HasContext[context.type]] { args =>
          val (tube, path) = args
          val moduleName = ModuleName(tubeName, path)
          modules.get(path) match {
            case Some(module) => SourceModule.make(context, tube, moduleName, importer, module)
            case None => IO.fail(DiagnosticError.UnknownModuleException(moduleName))
          }
        }
    } yield new ArTubeC {
      override val context: context2.type = context2
      override val tubeName: TubeName = tubeName2

      override def module(path: ModulePath): Comp[ArModule] = loadModule((this, path))
    }
  end make

}
