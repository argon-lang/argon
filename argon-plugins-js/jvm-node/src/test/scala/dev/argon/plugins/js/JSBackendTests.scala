package dev.argon.plugins.js

import dev.argon.compiler.Context
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.io.{DirectoryEntry, DirectoryResource, TextResource}
import dev.argon.plugin.Plugin
import dev.argon.plugin.test.*
import dev.argon.util.*
import zio.*
import zio.json.ast.Json
import zio.stm.*
import zio.stream.*
import zio.test.TestEnvironment

import java.io.FileNotFoundException

object JSBackendTests extends ExecutionTests[JSPluginError] {
  override val pluginName: String = "js"
  override val plugin: JSPlugin.type = JSPlugin
  override val testOptions: JSOptions[Environment, E] =
    JSOptions(
      extern = None,
      modules = JSModuleOptionsMap(Map.empty),
      tubes = JSTubeOptionsMap(Map(
        TubeName(NonEmptyList("Argon", "Core")) -> JSTubeOptions(
          import_path = "@tubes/Argon.Core",
        ),
      )),
    )

  override def executeTest
  (
    tube: plugin.Output[Environment, E],
    libraries: Map[TubeName, plugin.Output[Environment, E]],
  )
  : ZIO[Environment, E, String] =
    for
      fileSystem <- TMap.empty[String, String].commit
      _ <- addArgonRuntime(fileSystem)
      _ <- ZIO.foreachDiscard(libraries)(addLibraryOutput(fileSystem))
      _ <- addLibraryOutput(fileSystem)(testTubeName, tube)
      fileSystem2 <- fileSystem.toMap.commit
      output <- JSOutputExecutor.run(fileSystem2).mapError(TestError.ExecutionError.apply)
    yield output

  private def addLibraryOutput(fileSystem: TMap[String, String])(tubeName: TubeName, output: plugin.Output[Environment, E]): ZIO[Environment, E, Unit] =
    val tubeNameStr = tubeName.name.toList.mkString(".")
    val packageDir = "/argon/node_modules/@tubes/" + tubeNameStr
    val libDir = packageDir + "/lib"
    for
      _ <- addDirectoryResource(fileSystem, libDir, output.`package`)
      fsEntries <- fileSystem.keys.commit
      exports = Json.Obj(
        fsEntries
          .filter { name => name.startsWith(libDir + "/") }
          .map { name =>
            val subPath = name.substring(libDir.length + 1)
            if subPath == "index.js" then
              (".", Json.Str("./lib/" + subPath))
            else if subPath.endsWith("/index.js") then
              ("./" + subPath.stripSuffix("/index.js"), Json.Str("./lib/" + subPath))
            else
              ("./" + subPath.stripSuffix(".js"), Json.Str("./lib/" + subPath))
          }*
      )
      packageJson = Json.Obj(
        "name" -> Json.Str("@tubes/" + tubeNameStr),
        "exports" -> exports,
      )
      _ <- fileSystem.put(packageDir + "/package.json", packageJson.toString).commit
    yield ()

  private def addDirectoryResource(fileSystem: TMap[String, String], baseDir: String, dir: DirectoryResource[Environment, E, TextResource]): ZIO[Environment, E, Unit] =
    dir.contents.foreach {
      case DirectoryEntry.Subdirectory(name, resource) =>
        addDirectoryResource(fileSystem, s"$baseDir/$name", resource)

      case DirectoryEntry.File(name, resource) =>
        for
          text <- resource.asText.run(ZSink.mkString)
          _ <- fileSystem.put(s"$baseDir/$name", text).commit
        yield ()
    }

  private val argonRuntimeLib =
    ReadFileCompileTime.readDirectory("runtime/js", (isDir, name) => name != "node_modules")

  private def addArgonRuntime(fileSystem: TMap[String, String]): ZIO[Environment, E, Unit] =
    val dir = new DirectoryResource[Environment, E, TextResource] {
      override def contents: ZStream[TestEnvironment, JSBackendTests.E, DirectoryEntry[TestEnvironment, JSBackendTests.E, TextResource]] =
        ZStream.succeed(argonRuntimeLib.toDirectoryEntry("/argon/node_modules/@argon-lang/runtime", "runtime"))

      override def fileName: Option[String] = Some("/argon/node_modules/@argon-lang")
    }

    addDirectoryResource(fileSystem, "/argon/node_modules/@argon-lang", dir)
  end addArgonRuntime


}
