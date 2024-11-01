package dev.argon.plugin

import dev.argon.compiler.*
import esexpr.ESExprCodec.{DecodeError, ErrorPath}
import esexpr.{Dictionary, ESExpr, ESExprCodec, ESExprTag}
import dev.argon.io.{BinaryResource, FileSystemResource, ResourceFactory}
import dev.argon.options.{OptionDecoder, OutputHandler, OutputInfo}
import zio.*
import dev.argon.io.ResourceReader

object PluginSetUtil {

  trait PartialOptionDecoder[A] {
    def decode(kwargs: Map[String, ESExpr]): ZIO[ResourceReader, DecodeError, A]

    final def toDecoder: OptionDecoder[A] =
      new OptionDecoder[A] {
        override def decode(expr: ESExpr): ZIO[ResourceReader, DecodeError, A] =
          ZIO.fromEither(summon[ESExprCodec[Dictionary[ESExpr]]].decode(expr))
            .flatMap { dict =>
              PartialOptionDecoder.this.decode(dict.dict)
            }
      }
  }

  object PartialOptionDecoder {
    given [A](using partialDec: PartialOptionDecoder[A]): OptionDecoder[A] = partialDec.toDecoder
  }

  private[plugin] final class PartialOptionDecoderEmpty extends PartialOptionDecoder[Unit] {
    override def decode(kwargs: Map[String, ESExpr]): IO[DecodeError, Unit] =
      ZIO.unit
  }

  private[plugin] final class PartialOptionDecoderSingleton[A](pluginId: String)(using aDecoder: OptionDecoder[A]) extends PartialOptionDecoder[A] {
    override def decode(kwargs: Map[String, ESExpr]): ZIO[ResourceReader, DecodeError, A] =
      ZIO.fromEither(kwargs.get(pluginId).toRight(DecodeError(s"Missing value for plugin $pluginId", ErrorPath.Constructor("dict"))))
        .flatMap(value =>
          aDecoder.decode(value)
            .mapError(error => DecodeError(error.message, ErrorPath.Keyword("dict", pluginId, error.path)))
        )
  }

  private[plugin] final class PartialOptionDecoderUnion[A, B](using aDecoder: PartialOptionDecoder[A], bDecoder: PartialOptionDecoder[B]) extends PartialOptionDecoder[(A, B)] {
    override def decode(kwargs: Map[String, ESExpr]): ZIO[ResourceReader, DecodeError, (A, B)] =
      for
        aValue <- aDecoder.decode(kwargs)
        bValue <- bDecoder.decode(kwargs)
      yield (aValue, bValue)
  }

  private[plugin] final class OutputHandlerEmpty[E] extends OutputHandler[E, Unit] {
    override lazy val outputs: UIO[Map[Seq[String], OutputInfo[E, Unit]]] = ZIO.succeed(Map.empty)
  }

  private[plugin] final class OutputHandlerSingleton[E, A](pluginId: String, inner: OutputHandler[E, A]) extends OutputHandler[E, A] {
    override def outputs: UIO[Map[Seq[String], OutputInfo[E, A]]] =
      inner.outputs.map(_.map((k, v) => (pluginId +: k) -> v))
  }

  private[plugin] final class OutputHandlerUnion[E, A, B](using aHandler: OutputHandler[E, A], bHandler: OutputHandler[E, B]) extends OutputHandler[E, (A, B)] {
    override lazy val outputs: UIO[Map[Seq[String], OutputInfo[E, (A, B)]]] =
      for
        aOptions <- aHandler.outputs
        bOptions <- bHandler.outputs
      yield aOptions.view.mapValues { oi =>
        new OutputInfo[E, (A, B)] {
          override def getValue(options: (A, B)): UIO[FileSystemResource[E, BinaryResource]] =
            oi.getValue(options._1)
        }
      }.toMap ++
        bOptions.view.mapValues { oi =>
          new OutputInfo[E, (A, B)] {
            override def getValue(options: (A, B)): UIO[FileSystemResource[E, BinaryResource]] =
              oi.getValue(options._2)
          }
        }.toMap
  }

  private[plugin] trait PartialESExprCodec[A] {
    def encode(value: ZEnvironment[A]): UIO[Map[String, ESExpr]]
    def decode(kwargs: Map[String, ESExpr]): IO[DecodeError, ZEnvironment[A]]

    def toCodec: ESExprCodecAsync[ZEnvironment[A]] =
      new ESExprCodecAsync[ZEnvironment[A]] {
        override def encode(value: ZEnvironment[A]): UIO[ESExpr] =
          for
            dict <- PartialESExprCodec.this.encode(value)
          yield summon[ESExprCodec[Dictionary[ESExpr]]].encode(Dictionary(dict))

        override def decode(expr: ESExpr): IO[DecodeError, ZEnvironment[A]] =
          ZIO.fromEither(summon[ESExprCodec[Dictionary[ESExpr]]].decode(expr))
            .flatMap { dict =>
              PartialESExprCodec.this.decode(dict.dict)
            }
      }
  }

  private[plugin] final class PartialESExprCodecEmpty extends PartialESExprCodec[Any] {
    override def encode(value: ZEnvironment[Any]): UIO[Map[String, ESExpr]] =
      ZIO.succeed(Map.empty)

    override def decode(kwargs: Map[String, ESExpr]): IO[DecodeError, ZEnvironment[Any]] =
      ZIO.succeed(ZEnvironment.empty)
  }

  private[plugin] final class PartialESExprCodecSingleton[A: ESExprCodecAsync: Tag](pluginId: String) extends PartialESExprCodec[A] {
    override def encode(value: ZEnvironment[A]): UIO[Map[String, ESExpr]] =
      for
        encodedValue <- summon[ESExprCodecAsync[A]].encode(value.get[A])
      yield Map(pluginId -> encodedValue)

    override def decode(kwargs: Map[String, ESExpr]): IO[DecodeError, ZEnvironment[A]] =
      for
        encodedValue <- ZIO.fromEither(kwargs.get(pluginId).toRight(DecodeError(s"Missing value for plugin $pluginId", ErrorPath.Constructor("dict"))))
        value <- summon[ESExprCodecAsync[A]].decode(encodedValue)
          .mapError(error => DecodeError(error.message, ErrorPath.Keyword("dict", pluginId, error.path)))
      yield ZEnvironment(value)
  }

  private[plugin] final class PartialESExprCodecUnion[A: PartialESExprCodec, B: PartialESExprCodec: EnvironmentTag] extends PartialESExprCodec[A & B] {
    override def encode(value: ZEnvironment[A & B]): UIO[Map[String, ESExpr]] =
      for
        aMap <- summon[PartialESExprCodec[A]].encode(value)
        bMap <- summon[PartialESExprCodec[B]].encode(value)
      yield aMap ++ bMap

    override def decode(kwargs: Map[String, ESExpr]): IO[DecodeError, ZEnvironment[A & B]] =
      for
        aEnv <- summon[PartialESExprCodec[A]].decode(kwargs)
        bEnv <- summon[PartialESExprCodec[B]].decode(kwargs)
      yield aEnv ++ bEnv
  }
  
  

  



}
