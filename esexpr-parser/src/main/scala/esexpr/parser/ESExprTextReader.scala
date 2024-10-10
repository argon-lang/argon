package esexpr.parser

import esexpr.ESExpr
import dev.argon.grammar.Characterizer
import dev.argon.util.WithSource
import dev.argon.util.async.ZChannelUtil
import zio.Chunk
import zio.stream.{ZChannel, ZPipeline}

object ESExprTextReader {
  def read(fileName: Option[String]): ZPipeline[Any, ESExprTextParseException, String, WithSource[ESExpr]] =
    ZPipeline.mapChunks[String, Char](c => c.flatMap(s => Chunk.fromArray(s.toCharArray.nn))) >>>
      ZPipeline.fromChannel(
        Characterizer.characterize(fileName)
          .pipeToOrFail(ESExprLexer.lex(fileName))
          .pipeToOrFail(ESExprParser.parse(fileName))
      )

}
