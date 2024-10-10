package dev.argon.compiler_tests

import zio.*
import zio.stream.*
import zio.test.{TestExecutor as _, *}
import zio.test.Assertion.*
import dev.argon.compiler.ErrorLog
import dev.argon.plugin.{PluginError, PluginCompatibleContext}
import dev.argon.util.xml.XmlParser
import java.io.IOException
import fs2.data.xml.XmlException
import esexpr.{ESExpr, ESExprCodec}
import dev.argon.util.xml.XmlDocumentCountException
import dev.argon.plugin.PluginContext
import dev.argon.plugin.PluginSet
import dev.argon.plugin.PlatformPluginSet
import dev.argon.plugin.PluginLoader
import dev.argon.build.Compile
import dev.argon.build.BuildError
import dev.argon.plugin.TubeLoaderName
import dev.argon.build.TubeImporterImpl
import dev.argon.build.TubeOptions
import dev.argon.build.TubeLoaderOptions
import dev.argon.compiler_tests.TestCase.ExpectedResult
import dev.argon.compiler.TubeName
import dev.argon.io.ResourceReader
import cats.data.NonEmptySeq
import dev.argon.util.async.ZIOErrorUtil
import esexpr.parser.ESExprTextReader
import dev.argon.build.BuildConfigESExprParseError

object CompilerTests extends ZIOSpecDefault {

  type Env = Any
  type Error = PluginError | IOException | BuildError

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("Compiler Tests")(buildPluginTests)

  private def buildPluginTests: Spec[Env, Error] =
    Spec.multiple(
      Chunk.fromIterable(executors.map { exec =>
        suite(exec.pluginId + " plugin")(
          buildTestCases(exec)
        )
      })
    )
  
  private def buildTestCases[Emitter[Ctx <: PluginCompatibleContext]](exec: TestExecutor): Spec[Env, Error] =
    def groupTestCases(testCases: Map[Seq[String], TestCase]): Seq[Spec[Env, Error]] =
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
                .map(createTest(exec))
          }

    Spec.scoped(
      ZIO.foreach(testCases) { (path, testCase) =>
        for
          doc <- XmlParser.parse(testCase).orDie
          tc = TestCase.fromXml(doc.docElem)
        yield path.split("/").nn.toSeq.map(_.nn) -> tc
      }.map(tcm => Spec.multiple(Chunk.fromIterable(groupTestCases(tcm))))
    )
  end buildTestCases


  private def outputMatches(expected: String): Assertion[TestExecutionResult] =
    Assertion.assertion("Output matches") { actual =>
      def normalize(s: String): String =
        s.trim().nn.split("\\n").nn.map(_.nn.trim().nn).mkString("\n")

      actual match {
        case TestExecutionResult.Output(actual) =>
          normalize(expected) == normalize(actual)

        case _ => false
      }
    }

  private def createTest(exec: TestExecutor)(testCase: TestCase): Spec[Env, Error] =
    test(testCase.name) {
      for
        res <- runTestCompile(exec)(testCase)
      yield testCase.expectedResult match {
        case ExpectedResult.ExpectedOutput(text) =>
          assert(res)(outputMatches(text))

        case _ => ???
      }
    }

  private def runTestCompile(exec: TestExecutor)(testCase: TestCase): ZIO[Env, Error, TestExecutionResult] =
    ZIO.scoped[Env](
      ZIOErrorUtil.catchSplit[Env & Scope, TestExecutionResult, Error, TestExecutionResult, TestExecutionResult](
        compileProgram(exec)(testCase)
          .exit
          .flatMap { res =>
            ZIO.serviceWithZIO[Compile.LogReporter](_.getErrors)
              .flatMap { errors =>
                if errors.isEmpty then ZIO.done(res)
                else ZIO.fail(TestExecutionResult.CompileErrors(errors))
              }
          }
          .flatMap(p => exec.execute(p.libraries, p.program))
          .provideSome[Env & Scope](Compile.LogReporter.live)
      )(ZIO.succeed(_))
    )
      

  private final case class FullProgram[CompiledProgram](libraries: Map[TubeName, CompiledProgram], program: CompiledProgram)

  private def compileProgram(exec: TestExecutor)(testCase: TestCase): ZIO[Env & ErrorLog & Scope, Error, FullProgram[exec.CompiledProgram]] =
    ZIO.scoped(
      Compile.createContext[Env, Error](Seq(exec.pluginId, "source"))
        .flatMap { context =>
          val emitter = context.plugins.emitter[context.type].extract[exec.Emitter[context.type]].get
          val sourceLoader = context.plugins.tubeLoaders[context.type](TubeLoaderName("source", "argon-sources"))
          val sourceReader: ResourceReader = MapReader(libraries ++ testCaseToSourcesMap(testCase))

          val outputOptions = exec.outputOptions[context.Error, context.type](emitter)

          TubeImporterImpl(context)
            .flatMap { tubeImporter =>
              def loadTube(tubeName: TubeName): ZIO[Env & ErrorLog & Scope, Error, exec.CompiledProgram] =
                for
                  platformOptions <- ZIO.foreach(libraries.get(tubeName.encode + "/build.esx")) { buildEsx =>
                    ZStream(buildEsx).via(
                      ESExprTextReader.read(Some(tubeName.encode + "/build.esx"))
                        .mapError(BuildConfigESExprParseError.apply)
                    ).runHead.map { buildEsx =>
                      def getKey(e: ESExpr, s: String): Option[ESExpr] =
                        e match {
                          case ESExpr.Constructor(_, args, kwargs) => kwargs.get(s)
                          case _ => None
                        }

                      for
                        buildEsx <- buildEsx
                        tubeOptions <- getKey(buildEsx.value, "tube")
                        loaderOptions <- getKey(tubeOptions, "options")
                        platforms <- getKey(loaderOptions, "platforms")
                        platformOptions <- getKey(platforms, exec.pluginId)
                      yield platformOptions
                    }
                  }

                  tube <- tubeImporter.loadTube(identity)(TubeOptions(
                    loader = TubeLoaderOptions("source", "argon-sources"),
                    options = ESExpr.Constructor("source-options", Seq(), Map(
                      "name" -> summon[ESExprCodec[dev.argon.tube.TubeName]].encode(
                        dev.argon.tube.TubeName(tubeName.parts.head, tubeName.parts.tail)
                      ),
                      "sources" -> summon[ESExprCodec[Seq[String]]].encode(Seq(tubeName.encode + "/src")),
                      "platforms" -> ESExpr.Constructor("dict", Seq(), Map(
                        exec.pluginId -> platformOptions.flatten.getOrElse(exec.options),
                      )),
                    ))
                  )).provideSomeLayer(ZLayer.succeed(sourceReader))
                  tubeOutput <- emitter.emitTube(context)(tube)(outputOptions)
                  tubeProgram <- exec.programState(context)(emitter)(tubeOutput)
                yield tubeProgram

              for
                argonCoreProgram <- loadTube(TubeName(NonEmptySeq.of("Argon", "Core")))
                testProgram <- loadTube(TubeName(NonEmptySeq.of("Test")))
              yield FullProgram(
                libraries = Map(
                  TubeName(NonEmptySeq.of("Argon", "Core")) -> argonCoreProgram,
                ),
                program = testProgram,
              )
            }
        }
    )

  private def testCaseToSourcesMap(testCase: TestCase): Map[String, String] =
    testCase.sources.map { is =>
      ("Test/src/" + is.name) -> is.content
    }.toMap

}
