package dev.argon.esexpr.generator

import dev.argon.util.{*, given}
import dev.argon.esexpr.ESExprCodec
import dev.argon.esexpr.parser.ESExprTextReader
import dev.argon.esexpr.schema.Definition
import zio.*
import zio.stream.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given

import java.nio.file.Path

object Generator extends ZIOAppDefault {

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
    ZIOAppArgs.getArgs.flatMap {
      case Chunk(configFile, outDir) =>
        val outPath = Path.of(outDir).nn

        ZStream.fromFileName(configFile)
          .via(ZPipeline.utf8Decode >>> ESExprTextReader.read(Some(configFile)))
          .mapZIO(expr => ZIO.fromEither(summon[ESExprCodec[GeneratorConfig]].decode(expr.value)))
          .foreach { config =>
            val esxFile = Path.of(configFile).nn.resolveSibling(config.esxFile).nn
            ZStream.fromPath(esxFile)
              .via(ZPipeline.utf8Decode >>> ESExprTextReader.read(Some(esxFile.toString)))
              .runCollect
              .flatMap { exprs =>
                ZIO.fromEither(exprs.map(_.value).traverse(summon[ESExprCodec[Definition]].decode))
              }
              .flatMap { definitions =>
                config match {
                  case config: GeneratorConfig.Java =>
                    JavaGenerator.generate(outPath, definitions, config)

                  case config: GeneratorConfig.Scala =>
                    ScalaGenerator.generate(outPath, definitions, config)

                  case config: GeneratorConfig.TypeScript =>
                    TypeScriptGenerator.generate(outPath, definitions, config)

                  case config: GeneratorConfig.JavaRunJS =>
                    JavaRunJSGenerator.generate(outPath, definitions, config)

                  case config: GeneratorConfig.ScalaJS =>
                    ScalaJSGenerator.generate(outPath, definitions, config)

                  case config: GeneratorConfig.ScalaRunJava =>
                    ScalaRunJavaGenerator.generate(outPath, definitions, config)

                  case config: GeneratorConfig.ScalaRunSJS =>
                    ScalaRunSJSGenerator.generate(outPath, definitions, config)
                }
              }
          }

      case _ =>
        Console.printLine("Invalid args") *> exit(ExitCode.failure)
    }
}
