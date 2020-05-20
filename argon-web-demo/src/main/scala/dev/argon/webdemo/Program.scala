package dev.argon.webdemo

import java.io.{PrintWriter, StringWriter}
import java.net.URI

import cats.Id
import cats.data.NonEmptyList
import dev.argon.backend.ResourceAccess
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode, JSModuleExtractorImpl}
import dev.argon.build.{BuildEnvironment, BuildProcess, InputFileInfo, Pipeline}
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.CompilationError
import dev.argon.compiler.options.CompilerOptions
import dev.argon.io.Path
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.parser.SourceAST
import dev.argon.platform.PlatformApp
import dev.argon.compiler.options.FileList
import dev.argon.stream.builder.{Source, ZStreamSource}
import dev.argon.util.{FileID, FileSpec}
import zio._
import zio.stream._
import org.scalajs.dom

object Program extends PlatformApp {


  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "dev.argon.warts.ZioEffect"))
  override def runApp(args: List[String]): URIO[ZEnv with FileIOLite, Int] = (
    for {
      queue <- Queue.bounded[DemoCommand](1000)
      state <- RefM.make[ExecutionStatus](ExecutionStatus.NotRun)

      textElem <- IO.effectTotal { dom.document.getElementById("source-editor").asInstanceOf[dom.html.TextArea] }
      runButton <- IO.effectTotal { dom.document.getElementById("run-button").asInstanceOf[dom.html.Button] }
      outputElem <- IO.effectTotal { dom.document.getElementById("source-editor").asInstanceOf[dom.html.TextArea] }

      _ <- IO.effectTotal {
        runButton.onclick = _ =>
          unsafeRunAsync_(queue.offer(DemoCommand.Compile(textElem.value)))
      }

      _ <- ZStream.fromQueue(queue).foreach {
        case DemoCommand.Compile(code) =>
          state.update { oldState =>
            val cancelOld = oldState match {
              case ExecutionStatus.NotRun | ExecutionStatus.CompileFailed(_) | ExecutionStatus.Failed(_) | ExecutionStatus.Completed(_) =>
                IO.succeed(())

              case ExecutionStatus.Running(task) =>
                task.interrupt.unit
            }

            cancelOld
              .flatMap { _ => queue.offer(DemoCommand.ClearOutput) }
              .flatMap { _ => runCode(queue)(code).fork }
              .map(ExecutionStatus.Running.apply)
          }

        case DemoCommand.CompileFailureEvent(errors) =>
          IO.effectTotal {
            outputElem.value += errors.map(_.toString + "\n").toList.mkString
          }.andThen(state.set(ExecutionStatus.CompileFailed(errors)))

        case DemoCommand.ExecutionFailedEvent(error) =>
          IO.effectTotal {
            val writer = new StringWriter()
            error.printStackTrace(new PrintWriter(writer))
            val _ = writer.append('\n')
            outputElem.value += writer.toString
          }.andThen(state.set(ExecutionStatus.Failed(error)))

        case DemoCommand.ExecutionCompleteEvent =>
          for {
            output <- IO.effectTotal { outputElem.value }
            _ <- state.set(ExecutionStatus.Completed(output))
            _ <- IO.effectTotal { outputElem.value += "Done\n" }
          } yield ()

        case DemoCommand.ClearOutput =>
          IO.effectTotal { outputElem.value = "" }

        case DemoCommand.AppendOutput(output) =>
          IO.effectTotal { outputElem.value += output }

      }
    } yield 0
  ).provideLayer(HttpResourceReader.live)

  private def references =
    new FileList[WebDemoResourceIndicator](
      List("Argon.Core")
        .map { name => UriResourceIndicator(s"libraries/$name.armodule") }
    )

  type CIO[+A] = ZIO[ResourceReader[WebDemoResourceIndicator], NonEmptyList[CompilationError], A]
  type UCIO[+A] = ZIO[ResourceReader[WebDemoResourceIndicator], Nothing, A]

  private def runCode(queue: Queue[DemoCommand])(code: String): UCIO[Unit] =
    compileCode(code)
      .foldM(
        failure = errors => queue.offer(DemoCommand.CompileFailureEvent(errors)).unit,
        success = compiledCode =>
          executeJS(s => queue.offer(DemoCommand.AppendOutput(s)).unit)(compiledCode).foldM(
            failure = error => queue.offer(DemoCommand.ExecutionFailedEvent(error)).unit,
            success = _ => queue.offer(DemoCommand.ExecutionCompleteEvent).unit
          )
      )

  private def compileCode(code: String): CIO[String] = {
    val inputFiles = List(
      LocalResourceIndicator("test.argon", code),
    )

    BuildProcess.compile(
      JSBackend(JSModuleExtractorImpl)
    )(
      CompilerOptions[Id, WebDemoResourceIndicator](
        moduleName = "Test",
        inputFiles = new FileList(inputFiles),
        references = references,
      ),
      JSBackendOptions[Id, WebDemoResourceIndicator](
        extern = new FileList[WebDemoResourceIndicator](List.empty),
        inject = JSInjectCode[Id, WebDemoResourceIndicator](
          before = None,
          after = None,
        )
      ),
    ).use { output =>
      output.textStream
        .fold("") { (a, b) => a + b }
    }
  }

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private def executeJS(onOutput: String => UIO[Unit])(compiledCode: String): Task[Unit] =
    ZIO.runtime[Any].flatMap { runtime =>
      ZManaged.make(IO.effect {
        val api = new SandboxApi(str => runtime.unsafeRun(onOutput(str)))
        WebSandbox.create(api)
      })(sandbox => IO.effectTotal { sandbox.destroy() })
        .use { sandbox =>
          ???
        }
    }

}
