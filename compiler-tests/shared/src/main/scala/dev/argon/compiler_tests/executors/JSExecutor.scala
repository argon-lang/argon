package dev.argon.compiler_tests.executors

import dev.argon.backend.backends.js.JSBackend
import dev.argon.compiler.TubeName
import zio.*
import zio.stream.*
import dev.argon.compiler_tests.*

trait JSExecutor extends TestExecutor {
  override val backend: JSBackend[TestError]
  
  final case class JSProgram(files: Map[String, String])
  type TestProgram = JSProgram

  override def toTestProgram(program: backend.JSOutput): IO[TestError, TestProgram] =
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
