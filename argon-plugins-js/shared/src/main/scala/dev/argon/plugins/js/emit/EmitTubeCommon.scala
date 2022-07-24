package dev.argon.plugins.js.emit

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.io.{DirectoryEntry, DirectoryResource}
import dev.argon.plugins.js.*
import zio.*
import zio.stream.*

private[emit] trait EmitTubeCommon extends UsingContext {
  override val context: Context { type Error >: JSPluginError }
  val tube: ArTube
  val options: JSOptions[context.Env, context.Error]

  protected def getModuleFileName(tube: ArTube)(path: ModulePath): ModuleFile =
    if path.ids.isEmpty then
      def attemptIndex(i: Int): String =
        val indexPath = s"index.${i}"
        if tube.modulePaths.contains(ModulePath(Seq(indexPath))) then
          attemptIndex(i + 1)
        else
          indexPath

      val name =
        if tube.modulePaths.contains(ModulePath(Seq("index"))) then
          attemptIndex(0)
        else
          "index"

      ModuleFile(Seq(), name, path)
    else
      def escapeSegment(seg: String): String =
        seg.replace("%", "%25").replace("/", "%2F")

      val escaped = path.ids.map(escapeSegment)
      ModuleFile(escaped.init, escaped.last, path)
    end if
  end getModuleFileName

}