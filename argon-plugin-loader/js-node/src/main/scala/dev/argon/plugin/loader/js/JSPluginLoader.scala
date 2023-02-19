package dev.argon.plugin.loader.js

import dev.argon.io.PathLike
import dev.argon.plugin.{Plugin, PluginLoader, jsapi}
import zio.*
import dev.argon.plugin.jsapi.{PluginConsumer, PluginFactory, PluginOperations}
import dev.argon.plugin.tube.InvalidTube

import scala.xml

import scalajs.js

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
      obj <- ZIO.fromPromiseJS(js.`import`[scalajs.js.Object](config.module.toString)).orDie

      given Runtime[R] <- ZIO.runtime[R]

      wrapperContext = WrapperContext[R, E]

      factory = obj.asInstanceOf[js.Dynamic].selectDynamic(config.exportName).asInstanceOf[PluginFactory]

    yield factory.create(new js.Object with PluginOperations {}, new js.Object with PluginConsumer[Plugin[R, E]] {
      override def consume[Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation]
      (plugin: jsapi.Plugin[Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation])
      : Plugin[R, E] =
        wrapperContext.UnwrapPlugin(plugin)
    })
}
