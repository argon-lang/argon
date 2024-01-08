package dev.argon.esexpr.bparser

import dev.argon.esexpr.{ESExpr, ESExprCodec}
import dev.argon.util.*
import zio.*
import zio.stream.*

object StringPoolHandler {

  def resolveSymbols(stringPool: StringPool)(expr: ESExprSP): IO[ESExprBinaryParseException, ESExpr] =
    def getSym(i: BigInt): IO[ESExprBinaryParseException, String] =
      ZIO.fromEither(
        stringPool.symbols.lift(i.bigInteger.intValueExact())
          .toRight { ESExprBinaryParseException("Invalid string pool index") }
      )

    expr match
      case ESExprSP.Constructed(constructor, kwargs, args) =>
        for
          ctorName <- getSym(constructor)
          kwargExprs <- ZIO.foreach(kwargs) { (k, v) =>
            for
              k2 <- getSym(k)
              v2 <- resolveSymbols(stringPool)(v)
            yield k2 -> v2
          }
          argExprs <- ZIO.foreach(args)(resolveSymbols(stringPool))
        yield ESExpr.Constructed(ctorName, kwargExprs, argExprs)

      case ESExprSP.Bool(b) => ZIO.succeed(ESExpr.Bool(b))
      case ESExprSP.Int(n) => ZIO.succeed(ESExpr.Int(n))
      case ESExprSP.Str(s) => ZIO.succeed(ESExpr.Str(s))
      case ESExprSP.StrPooled(s) => getSym(s).map(ESExpr.Str.apply)
      case ESExprSP.Binary(b) => ZIO.succeed(ESExpr.Binary(b))
      case ESExprSP.Float32(f) => ZIO.succeed(ESExpr.Float32(f))
      case ESExprSP.Float64(d) => ZIO.succeed(ESExpr.Float64(d))
      case ESExprSP.Null => ZIO.succeed(ESExpr.Null)
    end match
  end resolveSymbols

  val metaStringPool: StringPool = StringPool("string-pool")


  def processExternalStringPool(stringPool: StringPool): ZPipeline[Any, ESExprBinaryParseException, ESExprSP, ESExpr] =
    ZPipeline.mapZIO(resolveSymbols(stringPool))

  def processInlineStringPool: ZPipeline[Any, ESExprBinaryParseException, ESExprSP, ESExpr] =
    ZPipelineUtil.branchOnHead[Any, ESExprBinaryParseException, ESExprSP, ESExpr] { stringPool =>
      ZPipeline.unwrap(
        resolveSymbols(metaStringPool)(stringPool)
          .flatMap(p => ZIO.fromEither(summon[ESExprCodec[StringPool]].decode(p)).mapError(err => ESExprBinaryParseException(s"Could not parse string pool: $err")))
          .map { stringPool =>
            ZPipeline.mapZIO(resolveSymbols(stringPool))
          }
      )
    }

  def processHybridStringPool(commonStringPool: StringPool): ZPipeline[Any, ESExprBinaryParseException, ESExprSP, ESExpr] =
    ZPipelineUtil.branchOnHead[Any, ESExprBinaryParseException, ESExprSP, ESExpr] { inlineStringPool =>
      ZPipeline.unwrap(
        resolveSymbols(metaStringPool)(inlineStringPool)
          .flatMap(p => ZIO.fromEither(summon[ESExprCodec[StringPool]].decode(p)).mapError(err => ESExprBinaryParseException(s"Could not parse string pool: $err")))
          .map { inlineStringPool =>
            val stringPool = StringPool((commonStringPool.symbols ++ inlineStringPool.symbols)*)
            ZPipeline.mapZIO(resolveSymbols(stringPool))
          }
      )
    }

}
