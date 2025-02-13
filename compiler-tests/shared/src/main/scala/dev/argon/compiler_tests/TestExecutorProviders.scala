package dev.argon.compiler_tests

import dev.argon.compiler_tests.TestExecutorProvider.ExecutorFactory
import dev.argon.backend.platforms
import dev.argon.compiler.CompilerError
import dev.argon.compiler.Context
import zio.*
import zio.stream.*
import dev.argon.compiler.TubeName


object TestExecutorProviders {
  
  def provider: TestExecutorProvider =
    TestExecutorProvider(
      ExecutorFactory[platforms.js.JSBackend] { b =>
        new TestExecutor {
          override val backend: b.type = b
          
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


          override def run(program: TestProgram, libraries: Map[TubeName, TestProgram]): Task[String] = ???
        }
      }
    )

}
