package dev.argon.plugins.scheme

import dev.argon.plugin.scalaApi.*
import esexpr.Dictionary
import dev.argon.tube.ImportSpecifier
import zio.*
import dev.argon.plugin.scalaApi.options.OptionDecoder
import nobleidl.core.Esexpr
import esexpr.ESExprCodec
import dev.argon.tube.BinaryOperator
import dev.argon.tube.UnaryOperator
import dev.argon.tube.ErasedSignatureType
import dev.argon.tube.BuiltinType

final class SchemePlugin[E <: Throwable] extends PlatformPlugin[SchemeExterns, E, SchemeOptions] {

  override def pluginId(): UIO[String] = ZIO.succeed("scheme")

  override def optionDecoder(): UIO[OptionDecoder[E, SchemeOptions]] =
    ZIO.succeed(SchemeOptions.optionDecoder)

  override def functionReferenceCodec(): UIO[EsexprCodec[ExternFunctionRef[SchemeExterns]]] =
    ZIO.succeed(new EsexprCodec[ExternFunctionRef[SchemeExterns]] {
      override def encode(value: ExternFunctionRef[SchemeExterns]): UIO[Esexpr] =
        value match {
          case value: SchemeExterns.ExternRefFunction =>
            ZIO.succeed(summon[ESExprCodec[SchemeExterns.ExternRefFunction]].encode(value))
        }
        
      override def decode(expr: Esexpr): IO[DecodeError, ExternFunctionRef[SchemeExterns]] =
        ZIO.fromEither(summon[ESExprCodec[SchemeExterns.ExternRefFunction]].decode(expr))
          .mapError(DecodeError.fromCodecError)
    })

  override def functionImplementationCodec(): UIO[EsexprCodec[ExternFunctionImpl[SchemeExterns]]] =
    ZIO.succeed(new EsexprCodec[ExternFunctionImpl[SchemeExterns]] {
      override def encode(value: ExternFunctionImpl[SchemeExterns]): UIO[Esexpr] =
        value match {
          case value: SchemeExterns.ExternFunction =>
            ZIO.succeed(summon[ESExprCodec[SchemeExterns.ExternFunction]].encode(value))
        }
        
      override def decode(expr: Esexpr): IO[DecodeError, ExternFunctionImpl[SchemeExterns]] =
        ZIO.fromEither(summon[ESExprCodec[SchemeExterns.ExternFunction]].decode(expr))
          .mapError(DecodeError.fromCodecError)
    })

  override def recordReferenceCodec(): UIO[EsexprCodec[ExternRecordRef[SchemeExterns]]] =
    ZIO.succeed(new EsexprCodec[ExternRecordRef[SchemeExterns]] {
      override def encode(value: ExternRecordRef[SchemeExterns]): UIO[Esexpr] =
        value match {
          case value: SchemeExterns.ExternRefRecord =>
            ZIO.succeed(summon[ESExprCodec[SchemeExterns.ExternRefRecord]].encode(value))
        }
        
      override def decode(expr: Esexpr): IO[DecodeError, ExternRecordRef[SchemeExterns]] =
        ZIO.fromEither(summon[ESExprCodec[SchemeExterns.ExternRefRecord]].decode(expr))
          .mapError(DecodeError.fromCodecError)
    })


  override def defineFunctionReference(options: SchemeOptions, specifier: ImportSpecifier): IO[E, ExternFunctionRef[SchemeExterns]] =
    ZIO.succeed(
      SchemeExterns.ExternRefFunction(
        TubeExportEncoding.buildTubeLibName(specifier.tube),
        TubeExportEncoding.buildTubeExportId(specifier.modulePath, specifier.name, specifier.sig),
      )
    )

  override def loadExternFunction(options: SchemeOptions, id: String): IO[E, Option[ExternFunctionImpl[SchemeExterns]]] =
    ZIO.succeed(
      options.externs.dict.get(id)
        .map { si => SchemeExterns.ExternFunction(si.library, si.name) }
    )

  override def defineRecordReference(options: SchemeOptions, specifier: ImportSpecifier): IO[E, ExternRecordRef[SchemeExterns]] =
    ZIO.succeed(
      SchemeExterns.ExternRefRecord(
        TubeExportEncoding.buildTubeLibName(specifier.tube),
        TubeExportEncoding.buildTubeExportId(specifier.modulePath, specifier.name, specifier.sig),
      )
    )

  override def emitter(): UIO[Option[TubeEmitterFactory[SchemeExterns, E]]] = ???

  override def tubeLoaders(): UIO[Dictionary[TubeLoader[SchemeExterns]]] = ???

  
}
