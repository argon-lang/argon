package dev.argon.compiler_tests.executors

import dev.argon.compiler.TubeName
import dev.argon.backend.platforms.js.JSBackend
import zio.*
import zio.stream.*
import dev.argon.compiler_tests.TestExecutor

trait JSExecutor extends TestExecutor {
  override val backend: JSBackend
  
  final case class JSProgram(files: Map[String, String])
  type TestProgram = JSProgram

  override def toTestProgram[E](program: backend.JSOutput[E]): IO[E, TestProgram] =
    for
      files <- program.sourceCode
        .contents
        .mapZIO { entry =>
          for
            contents <- entry.resource.asText.run(ZSink.mkString)
          yield (entry.dirs :+ entry.fileName).mkString("/") -> contents
        }
        .runCollect
    yield JSProgram(
      files = files.toMap
    )

}
