package dev.argon.esexpr.generator

import dev.argon.esexpr.ESExprCodec
import dev.argon.esexpr.parser.ESExprTextReader
import dev.argon.esexpr.schema.Definition
import zio.*
import zio.stream.*
import dev.argon.util.{*, given}

trait GeneratorApp extends ZIOAppDefault {

  type ArgValues
  def getArgValues: PartialFunction[Seq[String], ArgValues]
  def generate(esxFile: String, definitions: Seq[Definition], argValues: ArgValues): ZIO[Scope, Any, Unit]

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
    ZIOAppArgs.getArgs.flatMap {
      case Chunk(esxFile, restArgs*) =>
        getArgValues.lift(restArgs) match {
          case Some(args) =>

            ZStream.fromFileName(esxFile)
              .via(ZPipeline.utf8Decode)
              .pipeThroughChannel(ESExprTextReader.read(Some(esxFile)))
              .runCollect
              .flatMap { exprs =>
                ZIO.fromEither(exprs.map(_.value).traverse(summon[ESExprCodec[Definition]].decode))
              }
              .flatMap { definitions =>
                generate(esxFile, definitions, args)
              }

          case None =>
            Console.printLine("Invalid args") *> exit(ExitCode.failure)
        }

      case _ =>
        Console.printLine("Invalid args") *> exit(ExitCode.failure)
    }
}
