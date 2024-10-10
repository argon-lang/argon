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
    override lazy val options: Map[Seq[String], OutputInfo[E, Unit]] = Map.empty
  }

  private[plugin] final class OutputHandlerSingleton[E, A](pluginId: String, inner: OutputHandler[E, A]) extends OutputHandler[E, A] {
    override lazy val options: Map[Seq[String], OutputInfo[E, A]] =
      inner.options.map((k, v) => (pluginId +: k) -> v)
  }

  private[plugin] final class OutputHandlerUnion[E, A, B](using aHandler: OutputHandler[E, A], bHandler: OutputHandler[E, B]) extends OutputHandler[E, (A, B)] {
    override lazy val options: Map[Seq[String], OutputInfo[E, (A, B)]] =
      aHandler.options.view.mapValues { oi =>
        new OutputInfo[E, (A, B)] {
          override def getValue(options: (A, B)): FileSystemResource[E, BinaryResource] =
            oi.getValue(options._1)
        }
      }.toMap ++
        bHandler.options.view.mapValues { oi =>
          new OutputInfo[E, (A, B)] {
            override def getValue(options: (A, B)): FileSystemResource[E, BinaryResource] =
              oi.getValue(options._2)
          }
        }.toMap
  }

  private[plugin] trait PartialESExprCodec[A] {
    def encode(value: ZEnvironment[A]): Map[String, ESExpr]
    def decode(kwargs: Map[String, ESExpr]): Either[DecodeError, ZEnvironment[A]]

    def toCodec: ESExprCodec[ZEnvironment[A]] =
      new ESExprCodec[ZEnvironment[A]] {
        override lazy val tags: Set[ESExprTag] = summon[ESExprCodec[Dictionary[ESExpr]]].tags

        override def encode(value: ZEnvironment[A]): ESExpr =
          summon[ESExprCodec[Dictionary[ESExpr]]]
            .encode(Dictionary(PartialESExprCodec.this.encode(value)))

        override def decode(expr: ESExpr): Either[DecodeError, ZEnvironment[A]] =
          summon[ESExprCodec[Dictionary[ESExpr]]].decode(expr)
            .flatMap { dict =>
              PartialESExprCodec.this.decode(dict.dict)
            }
      }
  }

  private[plugin] final class PartialESExprCodecEmpty extends PartialESExprCodec[Any] {
    override def encode(value: ZEnvironment[Any]): Map[String, ESExpr] = Map.empty
    override def decode(kwargs: Map[String, ESExpr]): Either[DecodeError, ZEnvironment[Any]] =
      Right(ZEnvironment.empty)
  }

  private[plugin] final class PartialESExprCodecSingleton[A: ESExprCodec: Tag](pluginId: String) extends PartialESExprCodec[A] {
    override def encode(value: ZEnvironment[A]): Map[String, ESExpr] =
      val encodedValue = summon[ESExprCodec[A]].encode(value.get[A])
      Map(pluginId -> encodedValue)
    end encode

    override def decode(kwargs: Map[String, ESExpr]): Either[DecodeError, ZEnvironment[A]] =
      for
        encodedValue <- kwargs.get(pluginId).toRight(DecodeError(s"Missing value for plugin $pluginId", ErrorPath.Constructor("dict")))
        value <- summon[ESExprCodec[A]].decode(encodedValue)
          .left.map(error => DecodeError(error.message, ErrorPath.Keyword("dict", pluginId, error.path)))
      yield ZEnvironment(value)
  }

  private[plugin] final class PartialESExprCodecUnion[A: PartialESExprCodec, B: PartialESExprCodec: EnvironmentTag] extends PartialESExprCodec[A & B] {
    override def encode(value: ZEnvironment[A & B]): Map[String, ESExpr] =
      val aMap = summon[PartialESExprCodec[A]].encode(value)
      val bMap = summon[PartialESExprCodec[B]].encode(value)
      aMap ++ bMap
    end encode

    override def decode(kwargs: Map[String, ESExpr]): Either[DecodeError, ZEnvironment[A & B]] =
      for
        aEnv <- summon[PartialESExprCodec[A]].decode(kwargs)
        bEnv <- summon[PartialESExprCodec[B]].decode(kwargs)
      yield aEnv ++ bEnv
  }
  
  

  



}
