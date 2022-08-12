package dev.argon.util.toml

import zio.json.*
import zio.json.ast.Json

enum Toml {
  case Table(map: Map[java.lang.String, Toml])
  case Array(value: Seq[Toml])
  case String(value: java.lang.String)
  case Int(value: BigInt)
  case Float(value: Double)
  case Boolean(value: scala.Boolean)
  case OffsetDateTime(value: java.time.OffsetDateTime)
  case LocalDateTime(value: java.time.LocalDateTime)
  case LocalDate(value: java.time.LocalDate)
  case LocalTime(value: java.time.LocalTime)
}

object Toml {
  given TomlCodec[Toml] with
    override def encode(a: Toml): Toml = a
    override def decode(toml: Toml): Either[java.lang.String, Toml] = Right(toml)
  end given

  object Table {
    val empty = Toml.Table(Map.empty)
  }

  given JsonDecoder[Toml] =
    def jsonToToml(json: Json): Toml =
      json match {
        case Json.Obj(fields) => Toml.Table(fields.map { case (name, value) => (name, jsonToToml(value)) }.toMap)
        case Json.Arr(elements) => Toml.Array(elements.map(jsonToToml))
        case Json.Bool(value) => Toml.Boolean(value)
        case Json.Str(value) => Toml.String(value)
        case Json.Num(value) =>
          try {
            Toml.Int(value.toBigIntegerExact.nn)
          }
          catch {
            case _: ArithmeticException => Toml.Float(value.doubleValue())
          }
        case _: Json.Null.type => Toml.Table.empty
      }

    summon[JsonDecoder[Json]].map(jsonToToml)
  end given

}
