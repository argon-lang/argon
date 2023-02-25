package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.util.toml.{Toml, TomlCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import zio.*

import scala.compiletime.summonInline
import scala.quoted.{Expr, Quotes, Type}

trait OptionDecoder[R, E, A] {
  def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A]
  def defaultValue: Option[A] = None
}

object OptionDecoder {

  inline def derive[R, E, A](using m: Mirror.ProductOf[A]): OptionDecoder[R, E, A] =
    val tupleDecoder = summonInline[TupleOptionDecoder[R, E, m.MirroredElemTypes, m.MirroredElemLabels]]
    new OptionDecoder[R, E, A] {
      override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
        tupleDecoder.decode(resFactory)(value).map(m.fromTuple)
    }
  end derive


  trait TupleOptionDecoder[R, E, T <: Tuple, L <: Tuple] extends OptionDecoder[R, E, T]


  private[options] open class EmptyTupleOptionDecoderBase[R, E] extends TupleOptionDecoder[R, E, EmptyTuple, EmptyTuple] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, EmptyTuple] =
      value match {
        case Toml.Table(_) => Right(EmptyTuple)
        case _ => Left("Expected object")
      }
  }

  given [R, E]: TupleOptionDecoder[R, E, EmptyTuple, EmptyTuple] = EmptyTupleOptionDecoderBase[R, E]


  private[options] open class ConsTupleOptionDecoderBase[R, E, H, T <: Tuple, Name <: String: ValueOf, TNames <: Tuple](using field: OptionFieldDecoder[R, E, H], tail: TupleOptionDecoder[R, E, T, TNames]) extends TupleOptionDecoder[R, E, H *: T, Name *: TNames] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, H *: T] =
      value match {
        case Toml.Table(table) =>
          for
            h <- table.get(summon[ValueOf[Name]].value)
              .fold(field.defaultValue.toRight { "E" })(field.decode(resFactory))
            t <- tail.decode(resFactory)(value)
          yield h *: t

        case _ => Left("Expected object")
      }
  }

  given[R, E, H, T <: Tuple, Name <: String: ValueOf, TNames <: Tuple](using field: OptionFieldDecoder[R, E, H], tail: TupleOptionDecoder[R, E, T, TNames]): TupleOptionDecoder[R, E, H *: T, Name *: TNames] =
    ConsTupleOptionDecoderBase[R, E, H, T, Name, TNames]





  // Toml
  given [R, E, A](using TomlCodec[A]): OptionDecoder[R, E, A] =
    summon[OptionCodec[R, E, A]]

  // Seq
  private[options] open class SeqOptionDecoderBase[R, E, A](using OptionDecoder[R, E, A]) extends OptionDecoder[R, E, Seq[A]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, Seq[A]] =
      value match {
        case Toml.Array(value) =>
          value.traverse(summon[OptionDecoder[R, E, A]].decode(resFactory))

        case _ => Left("Expected array")
      }
  }


  given[R, E, A](using OptionDecoder[R, E, A], NotGiven[TomlCodec[A]]): OptionDecoder[R, E, Seq[A]] =
    SeqOptionDecoderBase()


  // DirectoryResource
  private[options] open class DirectoryResourceOptionDecoderBase[Res[-R2, +E2] <: Resource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]) extends OptionDecoder[R, E, DirectoryResource[R, E, Res]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, DirectoryResource[R, E, Res]] =
      value match {
        case Toml.String(str) =>
          Right((resFactory.directoryResource(str): DirectoryResource[R, E, BinaryResource]).decode[Res])

        case _ => Left("Expected string")
      }
  }

  given directoryResourceDecoder[Res[-R2, +E2] <: Resource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, DirectoryResource[R, E, Res]] =
    DirectoryResourceOptionDecoderBase()


  // BinaryResource
  private[options] open class BinaryResourceOptionDecoderBase[Res[_, _], R, E](using BinaryResourceDecoder[Res, R, E]) extends OptionDecoder[R, E, Res[R, E]] {
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, Res[R, E]] =
      value match {
        case Toml.String(str) =>
          Right(summon[BinaryResourceDecoder[Res, R, E]].decode(resFactory.binaryResource(str)))

        case _ => Left("Expected string")
      }
  }

  given [Res[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, Res[R, E]] =
    BinaryResourceOptionDecoderBase()

}


