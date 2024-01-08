package dev.argon.esexpr.bparser

import dev.argon.esexpr.{ESExpr, ESExprError}
import dev.argon.util.{FileOffset, WithLocation}
import zio.stream.ZPipeline

object ESExprBinaryReader {

  def readWithExternalStringPool(fileName: Option[String])(stringPool: StringPool): ZPipeline[Any, ESExprError, Byte, ESExpr] =
    ZPipeline.fromChannel[Any, ESExprError, Byte, ESExprSP](
        ESExprLexer.tokenize(fileName)
          .pipeToOrFail(ESExprParser.parse(fileName))
    ) >>>
      StringPoolHandler.processExternalStringPool(stringPool)

  def readWithInlineStringPool(fileName: Option[String]): ZPipeline[Any, ESExprError, Byte, ESExpr] =
    ZPipeline.fromChannel[Any, ESExprError, Byte, ESExprSP](
      ESExprLexer.tokenize(fileName)
        .pipeToOrFail(ESExprParser.parse(fileName))
    ) >>>
      StringPoolHandler.processInlineStringPool

  def readWithHybridStringPool(fileName: Option[String])(stringPool: StringPool): ZPipeline[Any, ESExprError, Byte, ESExpr] =
    ZPipeline.fromChannel[Any, ESExprError, Byte, ESExprSP](
      ESExprLexer.tokenize(fileName)
        .pipeToOrFail(ESExprParser.parse(fileName))
    ) >>>
      StringPoolHandler.processHybridStringPool(stringPool)

}
