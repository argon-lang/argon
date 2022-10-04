package dev.argon.platform

import dev.argon.compiler.DiagnosticError
import dev.argon.io.JsonResource.DecodeError
import dev.argon.parser.SyntaxError
import dev.argon.plugin.Plugin
import dev.argon.plugins.source.SourcePlugin
import dev.argon.build.BuildError
import dev.argon.plugins.js.{JSPlugin, JSPluginError}
import zio.*

import java.io.IOException
import java.nio.charset.CharacterCodingException
import typings.node.processMod.global.process as NodeProcess

abstract class PlatformApp extends ZIOApp {
  final override type Environment = PathUtil
  type Error = SyntaxError | DiagnosticError | IOException | CharacterCodingException | DecodeError | BuildError | JSPluginError

  final override def environmentTag: EnvironmentTag[Environment] = summon[EnvironmentTag[Environment]]

  final override def bootstrap: ZLayer[ZIOAppArgs & Scope, Any, Environment] =
    ZLayer.succeed(PlatformPathUtil())

  final override def run: ZIO[Environment & ZIOAppArgs, Any, Any] =
    runApp
      .provideSomeLayer[Environment & ZIOAppArgs](NodeSystemLayer.live)
      .provideSomeLayer[Environment & ZIOAppArgs](ZLayer.succeed(ZIOAppArgs(Chunk.fromIterable(NodeProcess.argv))))
      .onExit {
        case Exit.Success(exitCode) => ZIO.succeed { NodeProcess.exitCode = exitCode.code }
        case Exit.Failure(_) => ZIO.succeed { NodeProcess.exitCode = 1 }
      }

  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode]

  def plugins: Map[String, Plugin[Environment, Error]] =
    Map(
      "source" -> SourcePlugin,
      "js" -> JSPlugin,
    )

}
