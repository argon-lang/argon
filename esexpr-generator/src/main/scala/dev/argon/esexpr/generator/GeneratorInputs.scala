package dev.argon.esexpr.generator

import dev.argon.esexpr.ESExprCodec
import dev.argon.esexpr.parser.ESExprTextReader
import zio.*
import zio.json.JsonCodec
import zio.stream.*

import java.nio.file.Path

object GeneratorInputs extends ZIOAppDefault {
  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
    ZIOAppArgs.getArgs.flatMap {
      case Chunk(configFile) =>
        ZStream.fromFileName(configFile)
          .via(ZPipeline.utf8Decode)
          .pipeThroughChannel(ESExprTextReader.read(Some(configFile)))
          .mapZIO(expr => ZIO.fromEither(summon[ESExprCodec[GeneratorConfig]].decode(expr.value)))
          .mapZIO { config =>
            ZIO.succeed {
              Path.of(configFile).nn.resolveSibling(config.esxFile).nn.toAbsolutePath().nn.toString
            }
          }
          .runCollect
          .flatMap { paths =>
            val json = summon[JsonCodec[Seq[String]]].encodeJson(paths, None)
            Console.print(json)
          }

      case _ =>
        Console.printLine("Invalid args") *> exit(ExitCode.failure)
    }
}
