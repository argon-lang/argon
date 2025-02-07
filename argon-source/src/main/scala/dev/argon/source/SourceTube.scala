package dev.argon.source

import cats.data.{NonEmptySeq, OptionT}
import dev.argon.ast.{IdentifierExpr, ModulePatternMapping, ModulePatternSegment}
import dev.argon.compiler.{ArModuleC, ArTubeC, CompilerError, Context, ErrorLog, HasContext, ModulePath, TubeImporter, TubeName}
import dev.argon.io.{DirectoryEntry, DirectoryResource}
import dev.argon.util.{*, given}
import zio.ZIO
import zio.stream.ZStream
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.util.Locale

object SourceTube {
  def make
  (ctx: Context { type Error >: SourceError })
  (tubeOptions: SourceCodeTubeOptions[ctx.Error])
  (using TubeImporter & HasContext[ctx.type])
  : ZIO[ctx.Env, ctx.Error, ArTubeC & HasContext[ctx.type]] =
    for
      moduleMap <- buildModuleMap(ctx)(tubeOptions.name)(
        ZStream.fromIterable(tubeOptions.sources).flatMap(getSourceCode(ctx)(Seq.empty))
      )

    yield new ArTubeC {
      override val context: ctx.type = ctx

      override val name: TubeName = tubeOptions.name

      override val referencedTubes: Set[TubeName] = tubeOptions.referencedTubes

      override val modules: Map[ModulePath, ArModule] =
        moduleMap
    }
  end make



  private def getSourceCode
  (context: Context)
  (path: Seq[String])
  (resource: DirectoryResource[context.Error, ArgonSourceCodeResource])
  : ZStream[context.Env, context.Error, (ModulePath, ArgonSourceCodeResource[context.Error])] =
    resource.contents.flatMap {
      case DirectoryEntry.Subdirectory(name, resource) =>
        getSourceCode(context)(path :+ URLDecoder.decode(name, StandardCharsets.UTF_8).nn)(resource)
      case DirectoryEntry.File(name, resource) if name.toUpperCase(Locale.ROOT).nn.endsWith(".ARGON") =>
        val nameNoExt = name.substring(0, name.length - 6).nn

        val fullPath =
          if nameNoExt == "index" then
            path
          else if nameNoExt.matches("_+index") then
            path :+ nameNoExt.substring(1).nn
          else
            path :+ URLDecoder.decode(nameNoExt, StandardCharsets.UTF_8).nn

        ZStream(ModulePath(fullPath) -> resource)


      case DirectoryEntry.File(_, _) => ZStream.empty
    }

  private def buildModuleMap
  (context: Context { type Error >: SourceError })
  (tubeName: TubeName)
  (stream: ZStream[context.Env, context.Error, (ModulePath, ArgonSourceCodeResource[context.Error])])
  (using TubeImporter & HasContext[context.type])
  : context.Comp[Map[ModulePath, ArModuleC & HasContext[context.type]]] =
    stream.runFoldZIO(Map.empty[ModulePath, ArModuleC & HasContext[context.type]]) {
      case (modules, (path, sourceCode)) =>
        if modules.contains(path) then
          ErrorLog.logError(CompilerError.DuplicateModuleDefinition(path)).as(modules)
        else
          SourceModule.make(context)(tubeName, path)(sourceCode)
            .map { module => modules.updated(path, module) }
    }

}

