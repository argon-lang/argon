package dev.argon.plugins.js.emit

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.io.{DirectoryEntry, DirectoryResource}
import dev.argon.plugin.PluginContextAdapter
import dev.argon.plugins.js.*
import zio.*
import zio.stream.*

private[emit] trait EmitTubeCommon extends UsingContext {
  override val context: Context { type Error >: JSPluginError }
  val tube: ArTube
  val options: JSOptions[context.Env, context.Error]
  val adapter: PluginContextAdapter.Aux[context.type, JSPlugin.type]


  protected val vtableBuilder: VTableBuilder[context.type]

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
        seg.replace("%", "%25").nn.replace("/", "%2F").nn

      val escaped = path.ids.map(escapeSegment)
      ModuleFile(escaped.init, escaped.last, path)
    end if
  end getModuleFileName

}
