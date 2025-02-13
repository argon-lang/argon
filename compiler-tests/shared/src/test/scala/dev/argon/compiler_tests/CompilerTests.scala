package dev.argon.compiler_tests

import zio.*
import zio.stream.*
import zio.test.{TestExecutor as _, *}
import zio.test.Assertion.*
import dev.argon.compiler.ErrorLog
import java.io.IOException
import fs2.data.xml.XmlException
import esexpr.{ESExpr, ESExprCodec}
import dev.argon.build.*
import dev.argon.compiler_tests.TestCase.ExpectedResult
import dev.argon.compiler.*
import cats.data.NonEmptySeq
import dev.argon.util.async.ZIOErrorUtil
import esexpr.parser.ESExprTextReader

import dev.argon.util.{*, given}
import dev.argon.io.PathUtil
import java.io.StringWriter
import java.io.PrintWriter
import dev.argon.io.*
import dev.argon.backend.{Backend, Backends}
import dev.argon.vm.resource.VmIrResource
import dev.argon.tube.resource.TubeResourceContext
import dev.argon.source.ArgonSourceCodeResource

object CompilerTests extends ZIOSpecDefault {

  type Env = Any
  type Error = TestError

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("Compiler Tests")(
      buildBackendTests.provideSomeLayer[Env](ArgonLibraryProvider.live)
    )

  private def buildBackendTests: Spec[Env & ArgonLibraryProvider, Error] =
    Spec.multiple(
      Chunk.fromIterable(
        Backends.allBackends.map { backend =>
          buildTestCases(backend)
        }
      )
    )


  private def loadTestCases: ZIO[Env, Error, Map[Seq[String], TestCase]] =
    PathUtil.directoryResource(PathLike.fromString("testcases"))
      .upcast[DirectoryResource[TestError, BinaryResource]]
      .decode[TestCaseResource]
      .contents
      .mapZIO { entry =>
        for
          testCase <- entry.resource.asTestCase
        yield (entry.dirs :+ entry.fileName) -> testCase
      }
      .runCollect
      .map(_.toMap)

  

  private def buildTestCases(backend: Backend): Spec[Env & ArgonLibraryProvider, Error] =
    Spec.scoped(
      (for
        testCases <- loadTestCases
        
        options <- ZIO.fromEither(
          TestCaseBackendOptions.provider.getOptionsForBackend(backend)
            .toRight { TestException(s"Could not get test options for backend ${backend.name}") }
        )
        
        executor <- ZIO.fromEither(
          TestExecutorProviders.provider.getExecutorForBackend(backend)
            .toRight { TestException(s"Could not get executor for backend ${backend.name}") }
        )

        outputProvider <- BackendLibraryOutputProvider.make(backend, executor)
      yield {
        def compileTestCase(testCase: TestCase): ZIO[Env & ArgonLibraryProvider, Error, Either[Seq[CompilerError], executor.TestProgram]] =
          ZIO.scoped(
            (
              for
                vmIrResource <- toVmIr(testCase)
                libIr <- ZIO.foreach(testCase.libraries.toSeq) { libName =>
                  ZIO.serviceWith[ArgonLibraryProvider](_.getIrLibrary(libName))
                    .map(libName -> _)
                } 
                output <- backend.codegen(options, vmIrResource, libIr.toMap)
                testProgram <- executor.toTestProgram(output)
                errors <- ZIO.serviceWithZIO[LogReporter](_.getErrors)
              yield (
                if errors.isEmpty then
                  Right(testProgram)
                else
                  Left(errors)
              )
            ).provideSomeLayer[Scope & Env & ArgonLibraryProvider](LogReporter.live)
          )

        def executeTestProgram(testCase: TestCase, program: executor.TestProgram): ZIO[Env & ArgonLibraryProvider, Error, TestResult] =
          for
            libs <- ZIO.foreach(testCase.libraries.toSeq) { libName =>
                outputProvider.getLibraryOutput(libName)
                  .map(libName -> _)
            }

            res <- executor.run(program, libs.toMap).fold(
              failure = ex => TestResult.ExecutionError(ex.getMessage()),
              success = TestResult.Success.apply,
            )
          yield res


        def runTestCase(testCase: TestCase): ZIO[Env & ArgonLibraryProvider, Error, TestResult] =
          compileTestCase(testCase).flatMap {
            case Left(errors) =>
              ZIO.succeed(TestResult.CompileError(errors))

            case Right(program) =>
              executeTestProgram(testCase, program)
          }


        def createTest(testCase: TestCase): Spec[Env & ArgonLibraryProvider, Error] =
          test(testCase.name) {
            for
              res <- runTestCase(testCase)
            yield testCase.expectedResult match {
              case ExpectedResult.ExpectedOutput(text) =>
                assert(res)(outputMatches(text))

              case _ => ???
            }
          }


        def groupTestCases(testCases: Map[Seq[String], TestCase]): Seq[Spec[Env & ArgonLibraryProvider, Error]] =
            testCases
              .groupBy {
                case (h +: _ +: _, _) => Some(h)
                case _ => None
              }
              .toSeq
              .flatMap {
                case (Some(k), subdir) =>
                  Seq(suite(k)(groupTestCases(subdir.map { (k, v) => k.drop(1) -> v })*))

                case (None, testCases) =>
                  testCases.values
                    .map(createTest)
              }



        Spec.multiple(Chunk.fromIterable(groupTestCases(testCases)))
      }).mapError(TestFailure.fail)
    )

  private def toVmIr(testCase: TestCase): ZIO[Scope & Env & ErrorLog & ArgonLibraryProvider, Error, VmIrResource[Error]] =
    val ctx = Context.Impl[Env & ErrorLog, Error]
    
    for
      tubeResContext <- TubeResourceContext.make(ctx)

      libProvider <- ZIO.service[ArgonLibraryProvider]

      compile = new Compile {
        override val context: ctx.type = ctx

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def tubeName: TubeName = TubeName("Argon", "TestCase")
        override def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource] =
          testCase.toDirectoryResource
            .upcast[DirectoryResource[context.Error, BinaryResource]]
            .decode[ArgonSourceCodeResource]

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          testCase.libraries.toSeq.map { libName =>
            libProvider.getLibrary(libName)
              .decode[TubeResource]
          }
      }

      compileOutput <- compile.compile()

      genIr = new GenerateIR {
        override val context: ctx.type = ctx

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def inputTube(using TubeImporter & HasContext[ctx.type]): TubeResource[context.Error] =
          compileOutput.tube

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          testCase.libraries.toSeq.map { libName =>
            libProvider.getLibrary(libName)
              .decode[TubeResource]
          }
      }

      irOutput <- genIr.compile()

    yield irOutput.tube
  end toVmIr




  private def outputMatches(expected: String): Assertion[TestResult] =
    Assertion.assertion("Output matches") { actual =>
      def normalize(s: String): String =
        s.trim().nn.split("\\n").nn.map(_.nn.trim().nn).mkString("\n")

      actual match {
        case TestResult.Success(actual) =>
          normalize(expected) == normalize(actual)

        case _ => false
      }
    }

  private enum TestResult {
    case Success(output: String)
    case ExecutionError(message: String)
    case CompileError(errors: Seq[dev.argon.compiler.CompilerError])
  }

      

  private final case class FullProgram[CompiledProgram](libraries: Map[TubeName, CompiledProgram], program: CompiledProgram)

  private def testCaseToSourcesMap(testCase: TestCase): Map[String, String] =
    testCase.sources.map { is =>
      ("Test/src/" + is.name) -> is.content
    }.toMap

}
