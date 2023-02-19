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
  val plugin: JSPlugin[context.Env, context.Error]
  val adapter: PluginContextAdapter.Aux[context.type, plugin.type]


  private def escapeSegment(seg: String): String =
    seg.replace("%", "%25").nn.replace("/", "%2F").nn

  protected def getModuleFileName(path: ModulePath): ModuleFile =
    path.ids match {
      case Seq() => ModuleFile(Seq(), "index", path)
      case Seq(segment) if segment.startsWith("_") =>
        ModuleFile(Seq(), "_" + escapeSegment(segment), path)

      case Seq("index") =>
        ModuleFile(Seq(), "index", path)

      case _ =>
        val escaped = path.ids.map(escapeSegment)
        ModuleFile(escaped.init, escaped.last, path)
    }

}
