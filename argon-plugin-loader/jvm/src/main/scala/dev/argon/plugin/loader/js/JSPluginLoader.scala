package dev.argon.plugin.loader.js

import dev.argon.io.PathLike
import dev.argon.plugin.tube.InvalidTube
import dev.argon.plugin.{Plugin, PluginLoader}
import org.graalvm.polyglot.{HostAccess, Value, Context as JSContext}
import zio.*

import scala.xml

class JSPluginLoader[R, E >: InvalidTube] extends PluginLoader[R, E] {
  override type PluginConfig = JSPluginConfig
  override val configTagName: String = "JavaScript"
  override def decodeConfig(path: PathLike, elem: xml.Elem): Either[String, JSPluginConfig] =
    for
      module <- (elem \  "ModulePath").collectFirst { case modPath: xml.Elem => PathLike.join(path, modPath.text) }.toRight("ModulePath not specified")
      exportName <- (elem \ "ExportName").collectFirst { case e: xml.Elem => e.text }.toRight("ExportName not specified")
    yield JSPluginConfig(module, exportName)

  override def load(config: JSPluginConfig): ZIO[R & Scope, E, Plugin[R, E]] =
    for
      given JSContext <- ZIO.fromAutoCloseable(ZIO.succeed(
        JSContext.newBuilder("js").nn
          .allowHostAccess(HostAccess.ALL).nn
          .allowIO(true).nn
          .option("engine.WarnInterpreterOnly", false.toString).nn
          .build().nn
      ))

      given Runtime[R] <- ZIO.runtime[R]

      wrapperContext = WrapperContext[R, E]

      factory <- JSPromiseUtil.fromPromiseJS(
        ValueDecoder[JSPromise[Value]].decode(
          summon[JSContext].eval("js", "async (path, name) => (await import(path))[name]").nn
            .execute(config.module.toString, config.exportName).nn
          )
      ).orDie

      operations = new wrapperContext.Operations()

    yield wrapperContext.createPlugin(factory)(operations)

}
