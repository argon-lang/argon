package dev.argon.util.toml

import dev.argon.util
import magnolia1.*
import dev.argon.util.{*, given}
import java.time.{OffsetDateTime, LocalDateTime, LocalDate, LocalTime}

trait TomlCodec[A] {
  def encode(a: A): Toml
  def decode(toml: Toml): Either[String, A]
  def skipForField(a: A): Boolean = false
  def defaultValue: Option[A] = None
}

object TomlCodec extends Derivation[TomlCodec] {

  override def join[T](ctx: CaseClass[TomlCodec, T]): TomlCodec[T] =
    new TomlCodec[T] {
      override def encode(a: T): Toml =
        Toml.Table(
          ctx.params
            .iterator
            .filterNot { param => param.typeclass.skipForField(param.deref(a)) }
            .map { param =>
              param.label -> param.typeclass.encode(param.deref(a))
            }
            .toMap
        )

      override def decode(toml: Toml): Either[String, T] =
        toml match {
          case Toml.Table(map) =>
            ctx.constructEither { param =>
              map.get(param.label) match {
                case Some(memberValue) => param.typeclass.decode(memberValue)
                case None =>
                  param.typeclass.defaultValue match {
                    case Some(memberValue) => Right(memberValue)
                    case None => Left(s"Missing key in table: ${param.label}, map: ${map}")
                  }

              }
            }.left.map { _.mkString("\n") }

          case _ =>
            Left("Expected table")
        }
    }

  override def split[T](ctx: SealedTrait[TomlCodec, T]): TomlCodec[T] =
    new TomlCodec[T] {
      override def encode(a: T): Toml =
        ctx.choose(a) { sub => sub.typeclass.encode(sub.cast(a)) }

      override def decode(toml: Toml): Either[String, T] =
        toml match {
          case Toml.Table(map) =>
            for {
              typeName <- map.get("type") match {
                case Some(Toml.String(typeName)) => Right(typeName)
                case _ => Left(s"Could not get type specifier from $map")
              }
              subtype <- ctx.subtypes.find(_.typeInfo.short == typeName).toRight { println(ctx.subtypes.toSeq); s"Could not find specified type: $typeName" }
              value <- subtype.typeclass.decode(toml)
            } yield value

          case _ =>
            Left("Invalid table")
        }
    }



  given [A: TomlCodec]: TomlCodec[Map[String, A]] with
    override def encode(a: Map[String, A]): Toml =
      Toml.Table(a.view.mapValues(summon[TomlCodec[A]].encode).toMap)

    override def decode(toml: Toml): Either[String, Map[String, A]] =
      toml match {
        case Toml.Table(map) =>
          map
            .toSeq
            .traverse { case (name, value) => summon[TomlCodec[A]].decode(value).map(name.->) }
            .map { _.toMap }

        case _ => Left("Expected table")
      }
  end given

  given [A: TomlCodec]: TomlCodec[Seq[A]] with
    override def encode(a: Seq[A]): Toml =
      Toml.Array(a.map(summon[TomlCodec[A]].encode))

    override def decode(toml: Toml): Either[String, Seq[A]] =
      toml match {
        case Toml.Array(value) =>
          value.traverse(summon[TomlCodec[A]].decode)

        case _ => Left("Expected array")
      }
  end given

  given [A: TomlCodec]: TomlCodec[Option[A]] with
    override def encode(a: Option[A]): Toml =
      a.fold(Toml.Table(Map.empty))(summon[TomlCodec[A]].encode)

    override def decode(toml: Toml): Either[String, Option[A]] =
      summon[TomlCodec[A]].decode(toml).map(Some.apply)

    override def skipForField(a: Option[A]): Boolean = a.isEmpty

    override def defaultValue: Option[Option[A]] = Some(None)
  end given

  given TomlCodec[String] with
    override def encode(a: String): Toml =
      Toml.String(a)

    override def decode(toml: Toml): Either[String, String] =
      toml match {
        case Toml.String(value) =>
          Right(value)

        case _ => Left("Expected string")
      }
  end given

  given TomlCodec[BigInt] with
    override def encode(a: BigInt): Toml =
      Toml.Int(a)

    override def decode(toml: Toml): Either[String, BigInt] =
      toml match {
        case Toml.Int(value) =>
          Right(value)

        case _ => Left("Expected int")
      }
  end given

  given TomlCodec[Double] with
    override def encode(a: Double): Toml =
      Toml.Float(a)

    override def decode(toml: Toml): Either[String, Double] =
      toml match {
        case Toml.Float(value) =>
          Right(value)

        case _ => Left("Expected string")
      }
  end given

  given TomlCodec[OffsetDateTime] with
    override def encode(a: OffsetDateTime): Toml =
      Toml.OffsetDateTime(a)

    override def decode(toml: Toml): Either[String, OffsetDateTime] =
      toml match {
        case Toml.OffsetDateTime(value) =>
          Right(value)

        case _ => Left("Expected OffsetDateTime")
      }
  end given

  given TomlCodec[LocalDateTime] with
    override def encode(a: LocalDateTime): Toml =
      Toml.LocalDateTime(a)

    override def decode(toml: Toml): Either[String, LocalDateTime] =
      toml match {
        case Toml.LocalDateTime(value) =>
          Right(value)

        case _ => Left("Expected LocalDateTime")
      }
  end given

  given TomlCodec[LocalDate] with
    override def encode(a: LocalDate): Toml =
      Toml.LocalDate(a)

    override def decode(toml: Toml): Either[String, LocalDate] =
      toml match {
        case Toml.LocalDate(value) =>
          Right(value)

        case _ => Left("Expected LocalDate")
      }
  end given

  given TomlCodec[LocalTime] with
    override def encode(a: LocalTime): Toml =
      Toml.LocalTime(a)

    override def decode(toml: Toml): Either[String, LocalTime] =
      toml match {
        case Toml.LocalTime(value) =>
          Right(value)

        case _ => Left("Expected LocalTime")
      }
  end given

}

