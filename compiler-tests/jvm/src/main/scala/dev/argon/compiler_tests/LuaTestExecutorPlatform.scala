package dev.argon.compiler_tests

import dev.argon.util.{*, given}
import dev.argon.plugins.lua.LuaEmitter
import dev.argon.plugins.lua.LuaOutput
import dev.argon.plugin.{PluginEnv, PluginError, PluginCompatibleContext}
import zio.*
import dev.argon.plugins.lua.LuaOutputOptions
import dev.argon.esexpr.Dictionary
import dev.argon.plugin.TubeEmitter
import scala.reflect.TypeTest
import dev.argon.esexpr.ESExpr
import dev.argon.plugins.lua.AST
import dev.argon.compiler.TubeName
import scala.util.Using
import scala.collection.mutable
import zio.stream.ZSink
import scala.jdk.CollectionConverters.*
import java.io.StringWriter
import java.io.PrintWriter
import java.nio.file.Files
import org.apache.commons.io.FileUtils
import java.lang.ProcessBuilder.Redirect
import java.io.ByteArrayOutputStream
import org.apache.commons.io.IOUtils
import java.nio.charset.StandardCharsets

trait LuaTestExecutorPlatform {
  protected def executeLua(fs: Map[String, String]): ZIO[PluginEnv, PluginError, TestExecutionResult] =
    ZIO.attemptBlocking {
        val tempDir = Files.createTempDirectory("argon-lua-").nn
        try {
          for (name, content) <- fs do
            Files.writeString(tempDir.resolve(name), content)
          end for

          val pb = ProcessBuilder("lua", tempDir.resolve("main.lua").nn.toString())
          pb.directory(tempDir.toFile())
          pb.redirectError(Redirect.PIPE)
          pb.redirectOutput(Redirect.PIPE)
          val process = pb.start().nn

          val output = new ByteArrayOutputStream()

          val stdoutThread = Thread.startVirtualThread(() => {
            IOUtils.copy(process.getInputStream().nn, output)
          }).nn

          val stderrThread = Thread.startVirtualThread(() => {
            IOUtils.copy(process.getErrorStream().nn, output)
          }).nn

          stdoutThread.join()
          stderrThread.join()

          val outputStr = output.toString(StandardCharsets.UTF_8)

          val exitCode = process.waitFor()
          if exitCode != 0 then
            throw new RuntimeException(s"Lua exited with code $exitCode\n$outputStr")
          end if

          TestExecutionResult.Output(outputStr)
        }
        finally FileUtils.deleteDirectory(tempDir.toFile())

      }.catchAll { ex =>
        ex.printStackTrace()
        ZIO.succeed(TestExecutionResult.ExecutionErrors(ex))
      }

  
}
