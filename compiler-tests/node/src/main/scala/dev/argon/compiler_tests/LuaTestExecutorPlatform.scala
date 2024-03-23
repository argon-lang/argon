package dev.argon.compiler_tests

import dev.argon.util.{*, given}
import dev.argon.plugin.{PluginEnv, PluginError}
import zio.*
import zio.stream.*
import dev.argon.io.jstypes.node.{NodeFileSystem, NodePath}
import dev.argon.platform.NodeOS
import java.io.IOException
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters.*

trait LuaTestExecutorPlatform {
  protected def executeLua(fs: Map[String, String]): ZIO[PluginEnv, PluginError, TestExecutionResult] =
    ZIO.scoped(
      for
        tempDir <- ZIO.acquireRelease(
          ZIO.fromPromiseJS(
            NodeFileSystem.mkdtemp(NodePath.join(NodeOS.tmpdir(), "argon-lua-"))
          ).refineToOrDie[IOException]
        )(
          tempDir => ZIO.fromPromiseJS(
            NodeFileSystem.rm(tempDir, new NodeFileSystem.RMOptions {
              override val recursive: js.UndefOr[Boolean] = true
            })
          ).orDie
        )

        _ <- ZIO.foreachDiscard(fs) { (name, content) =>
          ZIO.fromPromiseJS(
            NodeFileSystem.writeFile(NodePath.join(tempDir, name), content)
          ).refineToOrDie[IOException]
        }

        res <- runProcess(
          "lua",
          Seq(NodePath.join(tempDir, "main.lua")),
          workingDir = tempDir,
        ).either
      yield res match {
        case Left(value) => TestExecutionResult.ExecutionErrors(value)
        case Right(output) => TestExecutionResult.Output(output)
      }
    )


  private def runProcess(command: String, args: Seq[String], workingDir: String): Task[String] =
    ZIO.async[Any, Throwable, String] { register =>
      NodeChildProcess.execFile(command, args.toJSArray, new NodeChildProcess.ExecFileOptions {
        override val cwd: js.UndefOr[String] = workingDir
      }, (error, stdout, stderr) => {
        register(error.toOption.fold(ZIO.succeed(stdout))(e => ZIO.fail(JavaScriptException(e))))
      })
    }

  
}
