package dev.argon

import dev.argon.build.{BackendProvider, BackendProviderImpl, BuildProcess}
import cats._
import cats.implicits._
import dev.argon.backend.Backend
import dev.argon.compiler.CompilationError
import zio.{ZEnv, BuildInfo => _, _}
import zio.console._
import dev.argon.compiler.options.{CompilerInput, CompilerOptionID, CompilerOptions, CompilerOutput, GeneralOutputOptionID, GeneralOutputOptions}
import dev.argon.options.{OptionDecodeResult, OptionID, OptionInfo, Options, OptionsHandler, SingleFile}
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.platform._

object Program extends PlatformApp {

  override def runApp(args: List[String]): ZIO[ZEnv with FileIO with ZipRead, Nothing, ExitCode] =
    baseCommand(args)
      .provideSomeLayer[ZEnv with FileIO with ZipRead](BackendProviderImpl.live)
      .as(ExitCode.success)
      .catchAll(IO.succeed(_))

  private def errorMessage(message: String): ZIO[Console, ExitCode, Nothing] =
    putStrLn(message).orDie *> IO.fail(ExitCode.failure)

  private def baseCommand(args: List[String]): ZIO[ZEnv with FileIO with ZipRead with BackendProvider, ExitCode, Unit] =
    args match {
      case "build" :: backendName :: tail =>
        ZIO.access[BackendProvider](_.get.findBackend(backendName)).flatMap {
          case Some(backend) =>
            buildCommand(backend)(
              CompilerOptions.handler.empty[Id],
              backend.backendOptions.empty[Id],
              GeneralOutputOptions.handler.empty[Lambda[X => SingleFile]],
              backend.outputOptions.empty[Lambda[X => SingleFile]],
            )(tail)
          case None =>
            errorMessage("Could not find backend: " + backendName)
        }

      case Nil =>
        errorMessage("No command specified")

      case cmdName :: _ =>
        errorMessage(s"Unknown command: $cmdName")
    }

  object CLISwitch {
    def unapply(arg: String): Option[String] =
      if(arg.startsWith("--")) Some(arg.stripPrefix("--"))
      else None
  }

  private def buildCommand
  (backend: Backend)
  (
    compilerOptions: Options[Option, CompilerOptionID],
    backendOptions: Options[Option, backend.BackendOptionID],
    generalOutputOptions: Options[Lambda[X => Option[SingleFile]], GeneralOutputOptionID],
    backendOutputOptions: Options[Lambda[X => Option[SingleFile]], backend.OutputOptionID],
  )
  (args: List[String]): ZIO[ZEnv with FileIO with ZipRead, ExitCode, Unit] =
    args match {
      case CLISwitch(name) :: value :: t =>

        def compOpt =
          parseOption[CompilerOptionID, Id](name, value, CompilerOptions.handler, compilerOptions)
            .flatMap { opts =>
              buildCommand(backend)(opts, backendOptions, generalOutputOptions, backendOutputOptions)(t).asSomeError
            }

        def backendOpts =
          parseOption[backend.BackendOptionID, Id](name, value, backend.backendOptions, backendOptions)
            .flatMap { opts =>
              buildCommand(backend)(compilerOptions, opts, generalOutputOptions, backendOutputOptions)(t).asSomeError
            }

        def genOutOpt =
          parseOption[GeneralOutputOptionID, Lambda[X => SingleFile]](name, value, GeneralOutputOptions.handler, generalOutputOptions)
            .flatMap { opts =>
              buildCommand(backend)(compilerOptions, backendOptions, opts, backendOutputOptions)(t).asSomeError
            }

        def backendOutOpt =
          parseOption[backend.OutputOptionID, Lambda[X => SingleFile]](name, value, backend.outputOptions, backendOutputOptions)
            .flatMap { opts =>
              buildCommand(backend)(compilerOptions, backendOptions, generalOutputOptions, opts)(t).asSomeError
            }

        compOpt
          .orElseOptional(backendOpts)
          .orElseOptional(genOutOpt)
          .orElseOptional(backendOutOpt)
          .flatMapError {
            case Some(error) => IO.succeed(error)
            case None => errorMessage("Unknown option: " + name).flip
          }


      case CLISwitch(name) :: Nil =>
        errorMessage("Option " + name + "is missing a value")


      case x :: _ => errorMessage("Unexpected option: " + x)

      case Nil =>
        for {
          compilerOptsResolved <- resolveOptions(compilerOptions, CompilerOptions.handler)
          backendOptsResolved <- resolveOptions(backendOptions, backend.backendOptions)

          compilerInput = CompilerInput(compilerOptsResolved, backendOptsResolved)
          compilerOutput = CompilerOutput(generalOutputOptions, backendOutputOptions)
          _ <- BuildProcess.compile(backend)(compilerInput, compilerOutput).catchAllCause(printCompilationErrors)
        } yield ()

    }


  private def findOptionID[O <: OptionID, Decoded[_]]
  (
    name: String,
    optionsHandler: OptionsHandler[O, Decoded],
  ): Option[O] = {
    val info = optionsHandler.optionsToRepr(optionsHandler.info)
    optionsHandler.combineRepr(info, info)(
      new OptionsHandler.CombineFunction[O, OptionInfo, OptionInfo, Lambda[X => Unit], Either[O, *]] {
        override def apply(id: O)(ax: OptionInfo[id.ElementType], bx: OptionInfo[id.ElementType]): Either[O, Unit] =
          if(ax.name === name) Left(id)
          else Right(())
      }
    ).swap.toOption
  }


  private def parseOption[O <: OptionID, Decoded[_]]
  (
    name: String,
    value: String,
    optionsHandler: OptionsHandler[O, Decoded],
    options: Options[Lambda[X => Option[Decoded[X]]], O]
  ): ZIO[Console, Option[ExitCode], Options[Lambda[X => Option[Decoded[X]]], O]] =
    ZIO.fromOption(findOptionID(name, optionsHandler)).flatMap { id =>
      val decoder = optionsHandler.decoder.get(id)
      val decodedRes =
        options.get(id) match {
          case Some(prevValue) => decoder.decodeAdditionalValue(prevValue, value)
          case None => decoder.decodeValue(value)
        }

      decodedRes match {
        case OptionDecodeResult.Result(decodedValue) =>
          IO.succeed(options.set(id)(Some(decodedValue)))

        case OptionDecodeResult.CouldNotDecode =>
          errorMessage("Could not decode value for option " + name).asSomeError

        case OptionDecodeResult.MultipleValuesNotSupported =>
          errorMessage("Multiple values were specified for option " + name).asSomeError
      }
    }


  private def resolveOptions[O <: OptionID, Decoded[_]](options: Options[Option, O], handler: OptionsHandler[O, Decoded]): ZIO[Console, ExitCode, Options[Id, O]] =
    IO.fromEither(handler.inferDefaults(options))
      .flatMapError { id => putStrLn("Missing value for option: " + handler.info.get(id).name).orDie.as(ExitCode.failure) }


  def printMessages(msgs: List[CompilationError]): URIO[Console, Unit] =
    ZIO.foreach_(msgs) { msg =>
      putStrLn(msg.toString).orDie
    }

  private def printCompilationErrors(cause: Cause[CompilationError]): ZIO[Console, ExitCode, Nothing] = {
    val errors = cause.failures
    val remaining = cause.stripFailures

    if(errors.nonEmpty) {
      val errorAction =
        if(remaining.isEmpty) IO.fail(ExitCode.failure)
        else IO.halt(remaining)

      printMessages(errors) *> errorAction
    }
    else {
      IO.halt(remaining)
    }
  }


}
