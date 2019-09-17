package dev.argon.webdemo

import cats.Id
import cats.data.NonEmptyList
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.build.{BuildEnvironment, BuildProcess, InputFileInfo, Pipeline}
import dev.argon.compiler.{CompilationError, CompilerOptions, IOCompilation}
import dev.argon.io.{FileIO, Path}
import dev.argon.parser.SourceAST
import dev.argon.stream.builder.{Source, ZStreamSource}
import dev.argon.util.{FileID, FileSpec}
import zio._
import zio.stream.ZStream
import org.scalajs.dom

object Program extends App {

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  override def run(args: List[String]): URIO[Environment, Int] = (
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
          ???

        case DemoCommand.ExecutionFailedEvent(error) =>
          ???

        case DemoCommand.ExecutionCompleteEvent =>
          ???

        case DemoCommand.ClearOutput =>
          IO.effectTotal { outputElem.value = "" }

        case DemoCommand.AppendOutput(output) =>
          IO.effectTotal { outputElem.value += output }

      }
    } yield 0
  )

  private def references = Vector(DummyFileSystem.argonCoreFileName)

  type CIO[+A] = ZIO[BuildEnvironment, NonEmptyList[CompilationError], A]

  private def runCode(queue: Queue[DemoCommand])(code: String): UIO[Unit] =
    compileCode(code)
      .foldM(
        failure = errors => queue.offer(DemoCommand.CompileFailureEvent(errors)).unit,
        success = compiledCode =>
          executeJS(s => queue.offer(DemoCommand.AppendOutput(s)).unit)(compiledCode).foldM(
            failure = error => queue.offer(DemoCommand.ExecutionFailedEvent(error)).unit,
            success = _ => queue.offer(DemoCommand.ExecutionCompleteEvent).unit
          )
      )
      .provideSomeM(DummyFileSystem.create)

  private def compileCode(code: String): CIO[String] =
    IOCompilation.compilationInstance[BuildEnvironment].flatMap { implicit ioComp =>
      ZIO.access[FileIO] { env => IOCompilation.fileSystemResourceAccessFactory[BuildEnvironment](env.fileIO) }
        .flatMap { implicit resFactory =>

          val inputFiles: Source[CIO, InputFileInfo[CIO], Unit] =
            ZStreamSource[BuildEnvironment, NonEmptyList[CompilationError], InputFileInfo[CIO]](ZStream(
              InputFileInfo[CIO](FileSpec(FileID(0), "test.argon"), ZStreamSource(ZStream.fromIterable(code))),
            ))

          BuildProcess.parseInput[CIO](inputFiles)
            .foldLeftM(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
            .flatMap {
              case (parsedInput, _) =>
                BuildProcess.compile[ZIO[BuildEnvironment, NonEmptyList[CompilationError], +*], Path, String](
                  JSBackend
                )(
                  parsedInput,
                  references,
                  CompilerOptions[Id](
                    moduleName = "Test"
                  ),
                  JSBackendOptions[Id, Path](
                    outputFile = new Path("test.js"),
                    extern = Map.empty,
                    inject = JSInjectCode[Id](
                      before = None,
                      after = None,
                    )
                  ),
                ) { output =>
                  output.textStream
                    .foldLeftM("") { (a, b) => IO.succeed(a + b) }
                    .map { case (str, _) => str }
                }
            }
        }
    }

  private def executeJS(onOutput: String => UIO[Unit])(compiledCode: String): UIO[Unit] = ???

}
