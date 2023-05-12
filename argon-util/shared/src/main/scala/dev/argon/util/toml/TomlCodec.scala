package dev.argon.util.toml

import dev.argon.util
import dev.argon.util.{*, given}
import java.time.{OffsetDateTime, LocalDateTime, LocalDate, LocalTime}
import scala.deriving.Mirror
import scala.compiletime.summonInline

trait TomlCodec[A] {
  def encode(a: A): Toml
  def decode(toml: Toml): Either[String, A]
  def defaultValue: Option[A] = None
}

object TomlCodec {


  inline def derived[A <: Product](using m: Mirror.ProductOf[A]): TomlCodec[A] =
    val tupleCodec = summonInline[TupleTomlCodec[m.MirroredElemTypes, m.MirroredElemLabels]]
    new TomlCodec[A] {
      override def decode(value: Toml): Either[String, A] =
        value match {
          case value @ Toml.Table(_) =>
            tupleCodec.decode(value).map(m.fromTuple)

          case _ => Left("Expected object")
        }

      override def encode(value: A): Toml =
        tupleCodec.encode(Tuple.fromProductTyped(value))
    }
  end derived

  trait TupleTomlCodec[T <: Tuple, L <: Tuple] {
    def encode(a: T): Toml.Table
    def decode(toml: Toml.Table): Either[String, T]
  }

  given TupleTomlCodec[EmptyTuple, EmptyTuple] with
    override def encode(a: EmptyTuple): Toml.Table =
      Toml.Table.empty

    override def decode(toml: Toml.Table): Either[String, EmptyTuple] =
      Right(EmptyTuple)
  end given

  given[H, T <: Tuple, Name <: String : ValueOf, TNames <: Tuple](using field: TomlFieldCodec[H], tail: TupleTomlCodec[T, TNames]): TupleTomlCodec[H *: T, Name *: TNames] with
    override def encode(a: H *: T): Toml.Table =
      val (h *: t) = a
      val table = tail.encode(t)
      field.encode(h).fold(table) { v => Toml.Table(table.map.updated(summon[ValueOf[Name]].value, v)) }
    end encode

    override def decode(toml: Toml.Table): Either[String, H *: T] =
      for
        h <- toml.map.get(summon[ValueOf[Name]].value)
          .fold(field.defaultValue.toRight { "E" })(field.decode)
        t <- tail.decode(toml)
      yield h *: t
  end given


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

  given [A: TomlCodec]: TomlCodec[NonEmptyList[A]] with
    override def encode(a: NonEmptyList[A]): Toml =
      Toml.Array(a.map(summon[TomlCodec[A]].encode).toList)

    override def decode(toml: Toml): Either[String, NonEmptyList[A]] =
      toml match {
        case Toml.Array(head +: tail) =>
          NonEmptyList.cons(head, tail.toList).traverse(summon[TomlCodec[A]].decode)

        case _ => Left("Expected non-empty array")
      }
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

  given TomlCodec[Boolean] with
    override def encode(a: Boolean): Toml =
      Toml.Boolean(a)

    override def decode(toml: Toml): Either[String, Boolean] =
      toml match {
        case Toml.Boolean(value) =>
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

