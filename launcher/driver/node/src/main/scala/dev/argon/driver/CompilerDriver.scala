package dev.argon.driver

import dev.argon.backend.metadata.BackendMetadata
import dev.argon.backend.scalaApi.{BinaryWriter, DirectoryEntry, DirectoryWriter, ScopedResource}
import dev.argon.backend.{Backend, BackendException, BackendFactory, BackendProvider}
import dev.argon.backend.{ScalaApiBackendLoader, scalaApi, sjs}
import dev.argon.driver.scalaApi.command.DriverCommand
import dev.argon.driver.{scalaApi as dScalaApi, sjs as dsjs}
import dev.argon.util.async.{ErrorWrapper, JSPromiseUtil}
import esexpr.Dictionary
import zio.*
import nobleidl.core.{ErrorType, JSAdapter}

import java.io.IOException
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.JavaScriptException

@JSExportTopLevel("compilerDriver")
object CompilerDriver extends dsjs.CompilerDriver {
  override def parseMetadata(metadata: String): sjs.metadata.BackendMetadata =
    toml.Toml.parse(metadata)
      .flatMap { metadata =>
        summon[toml.Codec[BackendMetadata]](metadata, defaults = Map.empty, index = 0)
      }
      .map(BackendMetadata.toApi andThen scalaApi.metadata.BackendMetadata.jsAdapter().toJS)
      .left.map { (_, message) => throw js.JavaScriptException(js.Error(message)) }
      .toTry
      .get

  override def parseCommandLineArguments(backends: js.Array[sjs.metadata.BackendMetadata], arguments: js.Array[String]): dsjs.command.DriverCommand[String, String, String, String] =
    val args = arguments.toSeq

    val parser = CompilerDriverOptions.command(
      backends
        .view
        .map(scalaApi.metadata.BackendMetadata.jsAdapter().fromJS andThen BackendMetadata.fromApi)
        .toSeq
    )

    val parsed = parser.parse(args, Map.empty)
    val driverCommand = CompilerDriverOptions.toDriverCommand(args, parsed)

    dScalaApi.command.DriverCommand.jsAdapter(JSAdapter.identity, JSAdapter.identity, JSAdapter.identity, JSAdapter.identity)
      .toJS(driverCommand)
  end parseCommandLineArguments

  override def runCommand(options: dsjs.CompilerDriverOptions): js.Promise[Int] =
    given Runtime[Any] = Runtime.default

    val backendFactories = options.backendFactories
      .view
      .map { factory =>
        new BackendFactory {
          override val metadata: BackendMetadata =
            BackendMetadata.fromApi(
              scalaApi.metadata.BackendMetadata.jsAdapter().fromJS(factory.metadata)
            )

          override def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
            loadJSApiBackend(metadata.backend.name)(factory)
        }
      }
      .toSeq



    JSPromiseUtil.runEffectToPromiseRaw(
      CompilerDriverImpl.runCommand(
          DriverCommandConverters.convertDriverCommand(
          dScalaApi.command.DriverCommand.jsAdapter(
              scalaApi.BinaryResource.jsAdapter(JSAdapter.identity[js.Error]),
              scalaApi.DirectoryResource.jsAdapter(JSAdapter.identity[js.Error]),
              scalaApi.BinaryResourceSink.jsAdapter(JSAdapter.identity[js.Error]),
              scalaApi.DirectoryResourceSink.jsAdapter(JSAdapter.identity[js.Error]),
          )
            .fromJS(options.command)
        )
      )
        .provideLayer(BackendProvider.liveFromFactories(backendFactories))
        .mapErrorCause[Throwable] { cause =>
          cause.failureOption match {
            case Some(ex: Throwable) => Cause.fail(ex)
            case Some(_) => Cause.fail(FiberFailure(cause))
            case None => cause.stripFailures
          }
        }
        .map { _.code }
    )
  end runCommand


  private def jsBackendToScala[E >: BackendException | IOException, Outs](using ew: ErrorWrapper[E], rt: Runtime[Any])(backendName: String)(backend: sjs.Backend[ew.EX, Outs]): UIO[Backend[E]] =
    val adapter = scalaApi.Backend.jsAdapter[ew.EX, ew.EX, Outs, Outs](
      JSAdapter.identity,
      JSAdapter.identity,
    )

    val backendScala = adapter.fromJS(backend)

    ScalaApiBackendLoader.loadScalaApiBackend(
      backendName
    )(
      backendScala
    )

  private def loadJSApiBackend[E >: BackendException | IOException](backendName: String)(factory: sjs.BackendFactory): ZIO[Scope, E, Backend[E]] =
    for
      given Runtime[Any] <- ZIO.runtime[Any]
      backend <- ZIO.suspendSucceed {
        val errorContext = ErrorWrapper.Context[E]()
        import errorContext.given

        val jsBackend = createBackend(factory)
        jsBackendToScala(backendName)(jsBackend)
      }
    yield backend

  private def createBackend[E >: BackendException | IOException](factory: sjs.BackendFactory)(using ew: ErrorWrapper[E]): sjs.Backend[ew.EX, ?] =
    import ew.given
    val errorType = ErrorType.toJSErrorChecker(summon[ErrorType[ew.EX]])

    val hostOperations = new sjs.HostOperations[ew.EX] {}

    factory.create(errorType, hostOperations, backend => backend)
  end createBackend


  type JSErrorDriverCommand = dScalaApi.command.DriverCommand[
    scalaApi.BinaryResource[js.Error],
    scalaApi.DirectoryResource[js.Error],
    scalaApi.BinaryResourceSink[js.Error],
    scalaApi.DirectoryResourceSink[js.Error],
  ]



  private object DriverCommandConverters {
    def convertDriverCommand(command: JSErrorDriverCommand): CompilerDriverImpl.LiveDriverCommand =
      command match {
        case DriverCommand.HelpCommand(isError, arguments) =>
          DriverCommand.HelpCommand(isError, arguments)

        case DriverCommand.VersionCommand() =>
          DriverCommand.VersionCommand()

        case DriverCommand.ListBackendsCommand() =>
          DriverCommand.ListBackendsCommand()

        case DriverCommand.Rpc() =>
          DriverCommand.Rpc()

        case DriverCommand.CompileCommand(tubeName, inputDir, outputFile, referencedTubes, supportedPlatforms, platformOptions) =>
          DriverCommand.CompileCommand(
            tubeName,
            convertDirectoryRes(inputDir),
            convertBinaryResSink(outputFile),
            referencedTubes.map(convertBinaryRes),
            supportedPlatforms,
            convertDict(convertDict(convertOptionValue))(platformOptions),
          )

        case DriverCommand.GenIrCommand(inputFile, outputFile, referencedTubes, platform) =>
          DriverCommand.GenIrCommand(
            convertBinaryRes(inputFile),
            convertBinaryResSink(outputFile),
            referencedTubes.map(convertBinaryRes),
            platform
          )

        case DriverCommand.CodegenCommand(backend, inputFile, referencedTubes, platformOptions, platformOutputOptions) =>
          DriverCommand.CodegenCommand(
            backend,
            convertBinaryRes(inputFile),
            referencedTubes.map(convertBinaryRes),
            convertDict(convertOptionValue)(platformOptions),
            convertDict(convertOutputValue)(platformOutputOptions)
          )
      }

    private def convertOptionValue
    (value: dScalaApi.command.CompilerDriverOptionValue[scalaApi.BinaryResource[js.Error], scalaApi.DirectoryResource[js.Error]])
    : dScalaApi.command.CompilerDriverOptionValue[scalaApi.BinaryResource[IOException], scalaApi.DirectoryResource[IOException]] =
      value match {
        case dScalaApi.command.CompilerDriverOptionValue.Single(value) =>
          dScalaApi.command.CompilerDriverOptionValue.Single(convertOptionValueAtom(value))

        case dScalaApi.command.CompilerDriverOptionValue.Many(head, tail) =>
          dScalaApi.command.CompilerDriverOptionValue.Many(
            convertOptionValueAtom(head),
            tail.map(convertOptionValueAtom),
          )
      }


    private def convertOptionValueAtom
    (value: dScalaApi.command.CompilerDriverOptionValueAtom[scalaApi.BinaryResource[js.Error], scalaApi.DirectoryResource[js.Error]])
    : dScalaApi.command.CompilerDriverOptionValueAtom[scalaApi.BinaryResource[IOException], scalaApi.DirectoryResource[IOException]] =
      value match {
        case dScalaApi.command.CompilerDriverOptionValueAtom.String(s) =>
          dScalaApi.command.CompilerDriverOptionValueAtom.String(s)

        case dScalaApi.command.CompilerDriverOptionValueAtom.Bool(b) =>
          dScalaApi.command.CompilerDriverOptionValueAtom.Bool(b)

        case dScalaApi.command.CompilerDriverOptionValueAtom.File(f) =>
          dScalaApi.command.CompilerDriverOptionValueAtom.File(convertBinaryRes(f))

        case dScalaApi.command.CompilerDriverOptionValueAtom.Directory(d) =>
          dScalaApi.command.CompilerDriverOptionValueAtom.Directory(convertDirectoryRes(d))
      }

    private def convertOutputValue
    (value: dScalaApi.command.CompilerDriverOutput[scalaApi.BinaryResourceSink[js.Error], scalaApi.DirectoryResourceSink[js.Error]])
    : dScalaApi.command.CompilerDriverOutput[scalaApi.BinaryResourceSink[IOException], scalaApi.DirectoryResourceSink[IOException]] =
      value match {
        case dScalaApi.command.CompilerDriverOutput.File(f) =>
          dScalaApi.command.CompilerDriverOutput.File(convertBinaryResSink(f))

        case dScalaApi.command.CompilerDriverOutput.Directory(d) =>
          dScalaApi.command.CompilerDriverOutput.Directory(convertDirectoryResSink(d))
      }



    private def convertBinaryRes(res: scalaApi.BinaryResource[js.Error]): scalaApi.BinaryResource[IOException] =
      new scalaApi.BinaryResource[IOException] {
        override val fileName: Option[String] =
          res.fileName

        override def asBytes: stream.Stream[IOException, Byte] =
          res.asBytes.mapErrorCause(convertJSError)
      }

    private def convertDirectoryRes(res: scalaApi.DirectoryResource[js.Error]): scalaApi.DirectoryResource[IOException] =
      new scalaApi.DirectoryResource[IOException] {
        override def contents(): UIO[ScopedResource[IOException, scalaApi.Stream[IOException, DirectoryEntry[IOException]]]] =
          res.contents().map(convertScopedResource(convertStream(convertDirectoryEntry)))
      }

    private def convertBinaryResSink(res: scalaApi.BinaryResourceSink[js.Error]): scalaApi.BinaryResourceSink[IOException] =
      new scalaApi.BinaryResourceSink[IOException] {
        override def sink(): UIO[ScopedResource[IOException, BinaryWriter[IOException]]] =
          res.sink().map(convertScopedResource(convertBinaryWriter))
      }

    private def convertDirectoryResSink(res: scalaApi.DirectoryResourceSink[js.Error]): scalaApi.DirectoryResourceSink[IOException] =
      new scalaApi.DirectoryResourceSink[IOException] {
        override def sink(): UIO[ScopedResource[IOException, DirectoryWriter[IOException]]] =
          res.sink().map(convertScopedResource(convertDirectoryWriter))
      }

    private def convertJSError(c: Cause[js.Error]): Cause[IOException] =
      c.flatMap { e =>
        e.asInstanceOf[js.Dictionary[Matchable]].get("code")
          .flatMap { code =>
            if js.typeOf(code) == "string" then
              Some(code.asInstanceOf[String])
            else
              None
          }
          .flatMap(mapErrorByCode(e).lift)
          .map(Cause.fail(_))
          .getOrElse { Cause.die(new JavaScriptException(e)) }
      }

    private def mapErrorByCode(error: js.Error): PartialFunction[String, IOException] = {
      case "ENOENT" => new IOException(error.message)
      case "EACCES" => new IOException(error.message)
      case "EISDIR" => new IOException(error.message)
      case "ENOTDIR" => new IOException(error.message)
      case "EBUSY" => new IOException(error.message)
      case "EEXIST" => new IOException(error.message)
      case "EMFILE" => new IOException(error.message)
      case "ENOSPC" => new IOException(error.message)
      case "EROFS" => new IOException(error.message)
      case "EINVAL" => new IOException(error.message)
      case "EPERM" => new IOException(error.message)
      case "ETIMEDOUT" => new IOException(error.message)
    }

    private def convertDirectoryEntry(entry: scalaApi.DirectoryEntry[js.Error]): scalaApi.DirectoryEntry[IOException] =
      scalaApi.DirectoryEntry[IOException](
        entry.dirs,
        entry.fileName,
        convertBinaryRes(entry.resource),
      )

    private def convertScopedResource[A, B](convA: A => B)(res: scalaApi.ScopedResource[js.Error, A]): scalaApi.ScopedResource[IOException, B] =
      new scalaApi.ScopedResource[IOException, B] {
        override def get(): IO[IOException, B] =
          res.get().map(convA).mapErrorCause(convertJSError)

        override def close(): UIO[Unit] =
          res.close()
      }

    private def convertStream[A, B](convA: A => B)(stream: scalaApi.Stream[js.Error, A]): scalaApi.Stream[IOException, B] =
      new scalaApi.Stream[IOException, B] {
        override def next(): IO[IOException, Seq[B]] =
          stream.next().map(_.map(convA)).mapErrorCause(convertJSError)
      }

    private def convertBinaryWriter(writer: scalaApi.BinaryWriter[js.Error]): scalaApi.BinaryWriter[IOException] =
      new scalaApi.BinaryWriter[IOException] {
        override def write(bytes: Chunk[Byte]): IO[IOException, Unit] =
          writer.write(bytes).mapErrorCause(convertJSError)
      }

    private def convertDirectoryWriter(writer: scalaApi.DirectoryWriter[js.Error]): scalaApi.DirectoryWriter[IOException] =
      new scalaApi.DirectoryWriter[IOException] {
        override def write(dirs: Seq[String], fileName: String): UIO[ScopedResource[IOException, BinaryWriter[IOException]]] =
          writer.write(dirs, fileName).map(convertScopedResource(convertBinaryWriter))
      }


    private def convertDict[A, B](convA: A => B)(dict: Dictionary[A]): Dictionary[B] =
      Dictionary(dict.dict.view.mapValues(convA).toMap)
  }

}
