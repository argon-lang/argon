package dev.argon.plugins.source

import cats.data.{NonEmptySeq, OptionT}
import dev.argon.ast.{IdentifierExpr, ModulePatternMapping, ModulePatternSegment}
import dev.argon.compiler.{ArModuleC, ArTubeC, CompilerError, Context, ErrorLog, HasContext, ModulePath, TubeImporter, TubeName}
import dev.argon.io.{DirectoryEntry, DirectoryResource}
import dev.argon.util.{*, given}
import zio.ZIO
import zio.stream.ZStream
import cats.*
import cats.implicits.given
import dev.argon.plugin.PlatformPluginSet
import zio.interop.catz.core.given

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.util.Locale
import dev.argon.plugin.PluginError

object SourceTube {
  def make[E >: PluginError]
  (platforms: PlatformPluginSet[E])
  (ctx: platforms.ContextOnlyIncluding)
  (tubeOptions: SourceCodeTubeOptions[ctx.Error, platforms.PlatformOptions])
  (tubeImporter: TubeImporter & HasContext[ctx.type])
  : ZIO[ctx.Env, ctx.Error, ArTubeC & HasContext[ctx.type]] =
    val tubeName = TubeName(NonEmptySeq.of(tubeOptions.name.head, tubeOptions.name.tail*))
    for
      moduleMap <- buildModuleMap(platforms)(ctx)(tubeName)(tubeImporter)(tubeOptions.platforms)(
        ZStream.fromIterable(tubeOptions.sources).flatMap(getSourceCode(ctx)(Seq.empty))
      )

    yield new ArTubeC {
      override val context: ctx.type = ctx

      override val name: TubeName = tubeName

      override val modules: Map[ModulePath, ArModule] =
        moduleMap
    }
  end make



  private def getSourceCode[E >: PluginError]
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

  private def buildModuleMap[E >: PluginError]
  (platforms: PlatformPluginSet[E])
  (context: platforms.ContextOnlyIncluding)
  (tubeName: TubeName)
  (tubeImporter: TubeImporter & HasContext[context.type])
  (platformOptions: platforms.PlatformOptions)
  (stream: ZStream[context.Env, context.Error, (ModulePath, ArgonSourceCodeResource[context.Error])])
  : context.Comp[Map[ModulePath, ArModuleC & HasContext[context.type]]] =
    stream.runFoldZIO(Map.empty[ModulePath, ArModuleC & HasContext[context.type]]) {
      case (modules, (path, sourceCode)) =>
        if modules.contains(path) then
          ErrorLog.logError(CompilerError.DuplicateModuleDefinition(path)).as(modules)
        else
          SourceModule.make(platforms)(context)(tubeName, path)(tubeImporter)(sourceCode, platformOptions)
            .map { module => modules.updated(path, module) }
    }

}

