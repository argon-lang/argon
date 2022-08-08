package dev.argon.plugin.test

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.io.*
import dev.argon.options.OptionDecoder
import dev.argon.plugin.test.TestCase.ExpectedResult
import dev.argon.plugin.{Plugin, PluginContextAdapter}
import dev.argon.plugins.source.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.Toml
import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*
import zio.json.*
import zio.json.internal.RetractReader

import java.io.{FileNotFoundException, IOException}
import java.nio.charset.CharacterCodingException
import java.nio.file.NotDirectoryException

abstract class ExecutionTests[E0] extends CompilerTestsBase {
  type E = E0 | SourceError | TestError

  val pluginName: String
  val plugin: Plugin[Environment, E]
  val testOptions: plugin.Options[Environment, E]



  def executeTest
  (
    tube: plugin.Output[Environment, E],
    libraries: Map[TubeName, plugin.Output[Environment, E]],
  )
  : ZIO[Environment, E, String]

  type PluginContext = Context {
    type Env = Environment
    type Error = E
    type Options = plugin.Options[Environment, E]
    type ExternMethodImplementation = plugin.ExternalMethodImplementation
    type ExternFunctionImplementation = plugin.ExternalFunctionImplementation
  }

  override def suiteName: String = s"Execution Tests ($pluginName)"


  val testTubeName = TubeName(NonEmptyList("Test"))

  private final class PluginContextImpl(runtime: Runtime[Environment & Scope], tubeOptions: Map[TubeName, SourceLibOptions[Environment, E, plugin.Options[Environment, E]]]) extends Context {
    override type Env = Environment
    override type Error = E

    override type Options = plugin.Options[Environment, E]
    override type ExternMethodImplementation = plugin.ExternalMethodImplementation
    override type ExternFunctionImplementation = plugin.ExternalFunctionImplementation

    override def getExternMethodImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternMethodImplementation] =
      plugin.loadExternMethod(options)(id).some

    override def getExternFunctionImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternFunctionImplementation] =
      plugin.loadExternFunction(options)(id).some


    private val tubes: Exit[Nothing, Fiber[Error, Map[TubeName, ArTubeC with HasContext[this.type]]]] =
      Unsafe.unsafe {
        runtime.unsafe.run(
          ZIO.foreach(tubeOptions) { (name, libOptions) =>
            for
              tube <- SourceTubeLoader.load(this)(libOptions)
            yield name -> tube
          }.fork
        )
      }

    override def getTube(tubeName: TubeName): Comp[ArTubeC with HasContext[this.type]] =
      ZIO.done(tubes)
        .flatMap(_.join)
        .flatMap { tubes =>
          ZIO.fromEither(tubes.get(tubeName).toRight(DiagnosticError.UnknownTube(tubeName)))
        }
  }

  private val librariesDir: CompileTimeFileSystem =
    ReadFileCompileTime.readDirectory("libraries", (isDir, name) => name != "node_modules" && name != "bin")

  private val libraries: IO[E, Map[TubeName, SourceLibOptions[Environment, E, plugin.Options[Environment, E]]]] =
    given OptionDecoder[E, plugin.Options[Environment, E]] with
      override def decode(value: Toml): ZIO[ResourceFactory, String, plugin.Options[Environment, E]] =
        val pluginValue = value match {
          case Toml.Table(table) => table.get(pluginName).getOrElse(Toml.Table.empty)
          case _ => Toml.Table.empty
        }

        plugin.optionDecoder[E].decode(pluginValue)
      end decode
    end given

    ZIO.foreach(librariesDir match {
      case CompileTimeFileSystem.Directory(entries) => entries
      case CompileTimeFileSystem.File(_) => Map.empty
    }) {
      case (name, entry) =>
        val dir = entry.toDirectoryEntry("libraries/" + name, name)

        val resourceFactory = new ResourceFactory {
          private def subPath(name: String): ZIO[Any, IOException, DirectoryEntry[Any, IOException, BinaryResource]] =
            name.split("/").toSeq.foldLeftM(dir) {
              case (DirectoryEntry.Subdirectory(_, subEntries), subPath) =>
                subEntries.contents.find { _.name == subPath }.runHead.flatMap { result =>
                  ZIO.fromEither(result.toRight(new FileNotFoundException(name)))
                }

              case (DirectoryEntry.File(_, _), _) =>
                ZIO.fail(new FileNotFoundException(name))
            }

          override def directoryResource(name: String): DirectoryResource[Any, IOException, BinaryResource] =
            new DirectoryResource[Any, IOException, BinaryResource] with Resource.WithoutFileName {
              override def contents: ZStream[Any, IOException, DirectoryEntry[Any, IOException, BinaryResource]] =
                ZStream.unwrap(
                  subPath(name).map {
                    case DirectoryEntry.Subdirectory(_, resource) => resource.contents
                    case DirectoryEntry.File(_, _) => ZStream.fail(NotDirectoryException(name))
                  }
                )
            }


          override def binaryResource(name: String): BinaryResource[Any, IOException] =
            new BinaryResource[Any, IOException] with Resource.WithoutFileName {
              override def asBytes: ZStream[Any, IOException, Byte] =
                ZStream.unwrap(
                  subPath(name).map {
                    case DirectoryEntry.Subdirectory(_, _) => ZStream.fail(IOException("Cannot read directory as file"))
                    case DirectoryEntry.File(_, resource) => resource.asBytes
                  }
                )
            }
        }

        (
          for
            buildJson <- entry match {
              case CompileTimeFileSystem.Directory(entries) =>
                entries.get("build.json") match {
                  case Some(CompileTimeFileSystem.File(buildJson)) => ZIO.succeed(buildJson)
                  case _ => ???
                }

              case CompileTimeFileSystem.File(_) => ???
            }

            buildToml <- ZIO.fromEither(
              summon[JsonDecoder[Toml]].mapOrFail {
                case Toml.Table(map) =>
                  map.get("tube") match {
                    case Some(Toml.Table(map)) =>
                      map.get("options").toRight("Missing options")

                    case _ => Left("Missing tube")
                  }

                case _ => Left("Expected object")
              }.decodeJson(buildJson)
            )

            options <- SourceTubeLoader.libOptionDecoder[E, plugin.Options[Environment, E]]
              .decode(buildToml)
              .provide(ZLayer.succeed(resourceFactory))

          yield TubeName(NonEmptyList.fromList(name.split("\\.").toList).get) -> options
        ).mapError(TestError.ErrorLoadingBuildConfig.apply)

    }

  private def inputSourcesToDirectory(sources: Map[String, String]): DirectoryResource[Any, SourceError, ArgonSourceCodeResource] =
    def impl(prevPath: Seq[String])(sources: UStream[(NonEmptyList[String], String)]): DirectoryResource[Any, SourceError, ArgonSourceCodeResource] =
      new DirectoryResource[Any, SourceError, ArgonSourceCodeResource] {

        override def contents: ZStream[Any, SourceError, DirectoryEntry[Any, SourceError, ArgonSourceCodeResource]] =
          ZStream.unwrapScoped(
            sources.partitionEither {
              case (NonEmptyList(head, head2 :: tail), contents) => ZIO.succeed(Left((head, head2, tail, contents)))
              case (NonEmptyList(name, Nil), contents) => ZIO.succeed(Right((name, contents)))
            }.map { (dirEntries, fileEntries) =>
              val subDirs =
                dirEntries
                  .groupBy {
                    case (head, head2, tail, contents) => ZIO.succeed((head, (NonEmptyList.cons(head2, tail), contents)))
                  } { (dirName, entries) =>
                    ZStream.succeed(DirectoryEntry.Subdirectory(dirName, impl(prevPath :+ dirName)(entries)))
                  }

              val files = fileEntries.map { (name, contents) =>
                val textResource = new TextResource[Any, SourceError] with TextResource.Impl[Any, SourceError] {
                  override def asText: ZStream[Any, SourceError, String] =
                    ZStream.succeed(contents)

                  override def fileName: Option[String] =
                    Some(prevPath.mkString("/") + "/" + name)
                }

                val codeResource = summon[BinaryResourceDecoder[ArgonSourceCodeResource, Any, SourceError]].decode(textResource)

                DirectoryEntry.File(name, codeResource)
              }

              subDirs.merge(files)
            }
          )




        override def fileName: Option[String] = Some(prevPath.mkString("/"))
      }

    impl(Seq("test"))(ZStream.fromIterable(sources.map { (k, v) => (NonEmptyList.fromList(k.split("/").toList).get, v) }))
  end inputSourcesToDirectory

  private val defaultTubeSpec: ArgonTubeSpecResource[Environment, E] =
    summon[BinaryResourceDecoder[ArgonTubeSpecResource, Environment, E]].decode(
      TextResource.fromString(
        """
          |** as path => "#{path}/index.argon"
          |** as path / * as name => "#{path}/#{name}.argon"
          |""".stripMargin)
    )

  override protected[test] def createTest(testCase: TestCase): Spec[Environment with Scope, Any] =
    test(testCase.name) {
      for
        libs <- libraries

        runtime <- ZIO.runtime[Environment & Scope]

        context <- ZIO.succeed {
          new PluginContextImpl(runtime, libs + (testTubeName -> SourceLibOptions(
            name = NonEmptyList("Test"),
            spec = defaultTubeSpec,
            sources = inputSourcesToDirectory(testCase.inputSources),
            plugin = testOptions,
          )))
        }

        libTubes <- ZIO.foreach(libs.keySet.toSeq) { libName =>
          getTubeOutput(context)(libName).map { libName -> _ }
        }.map { _.toMap }

        testTube <- getTubeOutput(context)(testTubeName)
        output <- executeTest(testTube, libTubes).tapDefect { defect =>
          ZIO.succeed {
            defect.defects.foreach(_.printStackTrace())
          }
        }
      yield checkExpectedOutput(testCase.expectedResult, output)
    }


  private def getTubeOutput(ctx: PluginContext)(tubeName: TubeName): ctx.Comp[plugin.Output[Environment, E]] =
    val adapter: PluginContextAdapter.Aux[ctx.type, plugin.type] = new PluginContextAdapter {
      override val context: ctx.type = ctx
      override val plugin: ExecutionTests.this.plugin.type = ExecutionTests.this.plugin

      override def extractExternMethodImplementation(impl: plugin.ExternalMethodImplementation): plugin.ExternalMethodImplementation =
        impl

      override def extractExternFunctionImplementation(impl: plugin.ExternalFunctionImplementation): plugin.ExternalFunctionImplementation =
        impl
    }

    for
      tube <- ctx.getTube(tubeName)
      output <- plugin.emitTube(ctx)(adapter)(tube.options)(tube.asDeclaration.get)
    yield output
  end getTubeOutput



  private def checkExpectedOutput(expectedResult: TestCase.ExpectedResult, output: String): TestResult =
    expectedResult match
      case ExpectedResult.Output(text) => assertTrue(normalizeOutput(output) == normalizeOutput(text))
    end match

  private def normalizeOutput(s: String): String =
    s.split('\n').map(_.trim).filter(_.nonEmpty).mkString("\n")
}