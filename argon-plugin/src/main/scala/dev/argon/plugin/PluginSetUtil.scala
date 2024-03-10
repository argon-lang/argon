package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.esexpr.ESExprCodec.{DecodeError, ProductDecodeError, ProductErrorPath}
import dev.argon.esexpr.{Dictionary, ESExpr, ESExprCodec, ESExprTag}
import dev.argon.io.{BinaryResource, FileSystemResource, ResourceFactory}
import dev.argon.options.{OptionDecoder, OutputHandler, OutputInfo}
import zio.*

object PluginSetUtil {

  trait PartialOptionDecoder[E, A] {
    def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, A]

    final def toDecoder: OptionDecoder[E, A] =
      new OptionDecoder[E, A] {
        override lazy val tags: Set[ESExprTag] = summon[ESExprCodec[Dictionary[ESExpr]]].tags

        override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, A] =
          summon[ESExprCodec[Dictionary[ESExpr]]].decode(value)
            .flatMap { dict =>
              PartialOptionDecoder.this.decode(resFactory)(dict.dict)
                .left.map(_.toDecodeError("dict"))
            }
      }
  }

  object PartialOptionDecoder {
    given [E, A](using partialDec: PartialOptionDecoder[E, A]): OptionDecoder[E, A] = partialDec.toDecoder
  }

  private[plugin] final class PartialOptionDecoderEmpty[E] extends PartialOptionDecoder[E, Unit] {
    override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, Unit] =
      Right(())
  }

  private[plugin] final class PartialOptionDecoderSingleton[E, A](pluginId: String)(using aDecoder: OptionDecoder[E, A]) extends PartialOptionDecoder[E, A] {
    override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, A] =
      kwargs.get(pluginId).toRight(ProductDecodeError(s"Missing value for plugin $pluginId", ProductErrorPath.Current))
        .flatMap(value =>
          aDecoder.decode(resFactory)(value)
            .left.map(error => ProductDecodeError(error.message, ProductErrorPath.Keyword(pluginId, error.path)))
        )
  }

  private[plugin] final class PartialOptionDecoderUnion[E, A, B](using aDecoder: PartialOptionDecoder[E, A], bDecoder: PartialOptionDecoder[E, B]) extends PartialOptionDecoder[E, (A, B)] {
    override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, (A, B)] =
      for
        aValue <- aDecoder.decode(resFactory)(kwargs)
        bValue <- bDecoder.decode(resFactory)(kwargs)
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
    def decode(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, ZEnvironment[A]]

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
                .left.map(_.toDecodeError("dict"))
            }
      }
  }

  private[plugin] final class PartialESExprCodecEmpty extends PartialESExprCodec[Any] {
    override def encode(value: ZEnvironment[Any]): Map[String, ESExpr] = Map.empty
    override def decode(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, ZEnvironment[Any]] =
      Right(ZEnvironment.empty)
  }

  private[plugin] final class PartialESExprCodecSingleton[A: ESExprCodec: Tag](pluginId: String) extends PartialESExprCodec[A] {
    override def encode(value: ZEnvironment[A]): Map[String, ESExpr] =
      val encodedValue = summon[ESExprCodec[A]].encode(value.get[A])
      Map(pluginId -> encodedValue)
    end encode

    override def decode(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, ZEnvironment[A]] =
      for
        encodedValue <- kwargs.get(pluginId).toRight(ProductDecodeError(s"Missing value for plugin $pluginId", ProductErrorPath.Current))
        value <- summon[ESExprCodec[A]].decode(encodedValue)
          .left.map(error => ProductDecodeError(error.message, ProductErrorPath.Keyword(pluginId, error.path)))
      yield ZEnvironment(value)
  }

  private[plugin] final class PartialESExprCodecUnion[A: PartialESExprCodec, B: PartialESExprCodec: EnvironmentTag] extends PartialESExprCodec[A & B] {
    override def encode(value: ZEnvironment[A & B]): Map[String, ESExpr] =
      val aMap = summon[PartialESExprCodec[A]].encode(value)
      val bMap = summon[PartialESExprCodec[B]].encode(value)
      aMap ++ bMap
    end encode

    override def decode(kwargs: Map[String, ESExpr]): Either[ProductDecodeError, ZEnvironment[A & B]] =
      for
        aEnv <- summon[PartialESExprCodec[A]].decode(kwargs)
        bEnv <- summon[PartialESExprCodec[B]].decode(kwargs)
      yield aEnv ++ bEnv
  }
  
  

  



}
