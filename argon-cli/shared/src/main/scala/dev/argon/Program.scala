package dev.argon

import dev.argon.build.{BackendProvider, BackendProviderImpl, BuildEnvironment, Pipeline}
import cats._
import cats.implicits._
import dev.argon.backend.{Backend, BuildInfo}
import zio.{BuildInfo => _, _}
import zio.console._
import zio.interop.catz.core._
import dev.argon.compiler.options.{CompilerOptions, FileList, OptionsField, OptionsHandler, PrimitiveCodecs, SingleFile}
import dev.argon.io.Path
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.module.PathResourceIndicator
import dev.argon.platform._

object Program extends PlatformApp {



  override def runApp(args: List[String]): ZIO[ZEnv with FileIO[FilePath] with FileIOLite, Nothing, Int] =
    parseArgs(args)
      .provideSomeLayer[ZEnv with FileIO[FilePath] with FileIOLite](BackendProviderImpl.live)
      .flatMap {
        case CommandLineArguments(cmd: BuildCommand) =>
          runCompilation(cmd).as(0)
      }
      .catchAll(IO.succeed(_))


  private def resolveOptions[Options[_[_], _]](options: Options[Option, String], handler: OptionsHandler[Options]): ZIO[Console with FileIO[FilePath], Int, Options[Id, PathResourceIndicator[FilePath]]] =
    IO.fromEither(handler.inferDefaults(options))
      .flatMapError { field => putStrLn("Missing value for option: " + field.name).as(1) }
      .flatMap { options2 =>
        Path.of[FilePath](".")
          .flatMap { currentDir =>
            implicit val fileHandler = PathResourceIndicator.fileHandlerPath(currentDir)
            handler.optionsLoader[String, PathResourceIndicator[FilePath]].loadOptions(options2)
          }
          .flatMapError { ex => putStrLn("Error: " + ex.getMessage).as(1) }
      }

  private def runCompilation(args: BuildCommand): ZIO[BuildEnvironment with Console with FileIO[FilePath] with FileIOLite, Int, Unit] = for {
    resCompilerOpts <- resolveOptions(args.compilerOptions, CompilerOptions.handler)
    resBackendOpts <- resolveOptions(args.backendOptions, args.backend.backendOptions)
    resOutputOpts <- resolveOptions(args.outputOptions, args.backend.outputOptions)
    buildInfo = BuildInfo(args.backend)(resCompilerOpts, resBackendOpts, resOutputOpts)
    _ <- Pipeline.run(buildInfo)
  } yield ()


  private def parseArgs(args: List[String]): ZIO[Console with BackendProvider, Int, CommandLineArguments] =
    parseCommands(args).map { CommandLineArguments(_) }

  private def parseCommands(args: List[String]): ZIO[Console with BackendProvider, Int, ArgonCommand] =
    args match {
      case Nil =>
        putStrLn("No command specified") *> IO.fail(1)

      case "build" :: backendName :: tail =>
        parseBuildCommand(backendName, tail)

      case cmdName :: _ =>
        putStrLn(s"Unknown command: $cmdName") *> IO.fail(1)
    }

  private trait ArgDecoder[A] {
    def decode(prevValue: Option[A], value: String): Either[String, A]
  }

  private def parseBuildCommand(backendName: String, args: List[String]): ZIO[Console with BackendProvider, Int, ArgonCommand] =
    ZIO.access[BackendProvider](_.get.findBackend(backendName))
      .flatMap {
        case Some(b) =>
          implicit val primitiveCodecs: PrimitiveCodecs[ArgDecoder, String] = new PrimitiveCodecs[ArgDecoder, String] {
            override def stringCodec: ArgDecoder[String] = new ArgDecoder[String] {
              override def decode(prevValue: Option[String], value: String): Either[String, String] =
                if(prevValue.isDefined)
                  Left("already has a value")
                else
                  Right(value)
            }

            override def resourceIndicatorCodec: ArgDecoder[String] = stringCodec

            override def fileListCodec[A](codec: ArgDecoder[A]): ArgDecoder[FileList[A]] =
              new ArgDecoder[FileList[A]] {
                override def decode(prevValue: Option[FileList[A]], value: String): Either[String, FileList[A]] =
                  codec.decode(None, value).map { a =>
                    new FileList[A](prevValue.getOrElse { new FileList[A](List.empty) }.files :+ a)
                  }
              }

            override def singleFileCodec[A](codec: ArgDecoder[A]): ArgDecoder[SingleFile[A]] = new ArgDecoder[SingleFile[A]] {
              override def decode(prevValue: Option[SingleFile[A]], value: String): Either[String, SingleFile[A]] =
                if(prevValue.isDefined)
                  Left("already has a value")
                else
                  codec.decode(None, value).map { new SingleFile[A](_) }
            }

            override def optionCodec[A](codec: ArgDecoder[A]): ArgDecoder[Option[A]] = new ArgDecoder[Option[A]] {
              override def decode(prevValue: Option[Option[A]], value: String): Either[String, Option[A]] =
                codec.decode(prevValue.flatten, value).map { Some(_) }
            }

          }

          def generateSwitches(namePrefix: String, fields: Seq[OptionsField[Option, String]]): Map[String, OptionsField[Option, String]] =
            fields
              .map { field => ("--" + namePrefix + field.info.name) -> field }
              .toMap

          def parseBuildArgs(args: List[String], fields: Map[String, OptionsField[Option, String]]): ZIO[Console, Int, Unit] =
            args match {
              case switch :: value :: tail if switch.startsWith("--") =>
                (
                  fields.get(switch) match {
                    case Some(field) =>
                      for {
                        prevValue <- field.fieldRef.get
                        newValue <- IO.fromEither(field.info.codecSelector.codec[ArgDecoder].decode(prevValue, value))
                          .flatMapError { msg =>
                            putStrLn(s"Error parsing value for switch $switch: $msg").as(1)
                          }
                        _ <- field.fieldRef.set(Some(newValue))
                      } yield ()

                    case None =>
                      putStrLn(s"Unknown switch: $switch") *> IO.fail(1)
                  }
                ).flatMap { _ => parseBuildArgs(tail, fields) }

              case switch :: Nil if switch.startsWith("--") =>
                putStrLn(s"Switch $switch missing value.") *> IO.fail(1)

              case Nil => IO.unit
            }

          for {
            (compilerOptsFields, createCompilerOpts) <- CompilerOptions.handler.fields(CompilerOptions.handler.empty[String])
            (backendOptsFields, createBackendOpts) <- b.backendOptions.fields(b.backendOptions.empty[String])
            (outputOptsFields, createOutputOpts) <- b.outputOptions.fields(b.outputOptions.empty[String])
            _ <- parseBuildArgs(args, generateSwitches(b.id + ":", backendOptsFields ++ outputOptsFields) ++ generateSwitches("",  compilerOptsFields))
            compilerOpts <- createCompilerOpts
            backendOpts <- createBackendOpts
            outputOpts <- createOutputOpts
          } yield new BuildCommand {
            override val backend: b.type = b

            override val compilerOptions: CompilerOptions[Option, String] = compilerOpts
            override val backendOptions: backend.BackendOptions[Option, String] = backendOpts
            override val outputOptions: backend.BackendOutputOptions[Option, String] = outputOpts
          }

        case None =>
          putStrLn(s"Unknown backend: $backendName") *> IO.fail(1)
      }



}
