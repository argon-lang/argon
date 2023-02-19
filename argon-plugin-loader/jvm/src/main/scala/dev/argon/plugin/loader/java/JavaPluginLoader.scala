package dev.argon.plugin.loader.java

import dev.argon.plugin.{Plugin, PluginLoader}
import zio.*

import java.net.URLClassLoader
import java.nio.file.Path
import scala.util.chaining.*
import scala.xml
import dev.argon.plugin.api as japi
import dev.argon.plugin.tube.InvalidTube

final class JavaPluginLoader[R, E >: JavaPluginLoadException | InvalidTube] extends PluginLoader[R, E] {
  override type PluginConfig = JavaPluginConfig
  override val configTagName: String = "Java"
  override def decodeConfig(path: Path, elem: xml.Elem): Either[String, JavaPluginConfig] =
    for
      factoryClass <- (elem \ "PluginFactoryClass").collectFirst { case e: xml.Elem => e.text }.toRight("PluginFactoryClass not specified")
      modules <- (elem \ "Module").collect { case modElem: xml.Elem => path.resolve(modElem.text).nn }
        .pipe(Some.apply)
        .filter(_.nonEmpty)
        .toRight("No module specified")
    yield JavaPluginConfig(factoryClass, modules)

  override def load(config: JavaPluginConfig): ZIO[R & Scope, E, Plugin[R, E]] =
    for
      loader <- ZIO.succeedBlocking {
        val urls = config.modules.map(_.toUri.nn.toURL).toArray
        new URLClassLoader(urls)
      }

      factoryClass <- ZIO.attemptBlocking {
        loader.loadClass(config.pluginFactoryClass).nn
      }
        .refineToOrDie[ClassNotFoundException]
        .mapError(PluginFactoryNotFoundException(config.pluginFactoryClass, _))

      factory <- ZIO.attemptBlocking {
        factoryClass.getDeclaredConstructor().nn.newInstance() match {
          case factory: japi.PluginFactory => Some(factory)
          case _ => None
        }
      }
        .refineToOrDie[CouldNotConstructPluginFactoryException]
        .mapError(PluginFactoryNotFoundException(config.pluginFactoryClass, _))
        .flatMap(ZIO.fromOption(_).mapError(_ => InvalidPluginFactoryException()))

      given Runtime[R] <- ZIO.runtime[R]

      wrapperContext = WrapperContext[R, E]
      plugin <- ZIO.succeed { factory.create[wrapperContext.WrappedError](wrapperContext.Operations()) }
    yield wrapperContext.UnwrapPlugin(plugin)
}
