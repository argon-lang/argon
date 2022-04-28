package dev.argon.plugin

import zio.*
import zio.stream.*
import dev.argon.compiler.CompError

import java.io.IOException
import java.nio.file.{Path, Files}
import scala.jdk.CollectionConverters.*
import java.net.URLClassLoader
import org.apache.commons.io.FilenameUtils
import java.util.ServiceLoader
import dev.argon.plugin.{api => japi}

trait PluginLoaderObjectPlatformSpecific {
  
  def live: ZLayer[System, Nothing, PluginLoader[CompError]] =
    ZLayer(
      for {
        system <- ZIO.service[System]
      } yield new PluginLoader[CompError] {
        override def load: IO[IOException, Seq[Plugin[CompError]]] =
          system.env("ARGON_PLUGINS_DIRECTORY").orDie.flatMap {
            case None => IO.succeed(Seq.empty)
            case Some(pluginsDirStr) =>
              IO.runtime.flatMap { runtime =>
                given Runtime[Any] = runtime
                
                IO.attemptBlockingInterrupt {
                  val pluginsDir = Path.of(pluginsDirStr)
                  Files.list(pluginsDir)
                    .iterator
                    .asScala
                    .filter(Files.isDirectory(_))
                    .flatMap(loadPluginFromDir)
                    .toSeq
                }.refineToOrDie[IOException]                
              }
          }

        private def loadPluginFromDir(dir: Path)(using Runtime[Any]): Seq[Plugin[CompError]] =
          loadJavaPlugin(dir)

        private def loadJavaPlugin(dir: Path)(using Runtime[Any]): Seq[Plugin[CompError]] =
          val jarDir = dir.resolve("java")
          val jars = Files.list(jarDir)
            .iterator
            .asScala
            .filter(jarPath => FilenameUtils.getExtension(jarPath.toString) == "jar")
            .map(_.toUri.toURL)
            .toArray

          val classLoader = new URLClassLoader(jars, ClassLoader.getSystemClassLoader)
          ServiceLoader.load(classOf[japi.PluginFactory], classLoader)
            .iterator
            .asScala
            .map { pluginFactory =>
              val plugin = pluginFactory.createPlugin[JavaCompErrorWrapper]
              new JavaPlugin(plugin)
            }
            .toSeq
        end loadJavaPlugin
      }
    )
    

}
