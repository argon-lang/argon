package dev.argon.util.toml

enum Toml {
  case Table(map: Map[java.lang.String, Toml])
  case Array(value: Seq[Toml])
  case String(value: java.lang.String)
  case Int(value: BigInt)
  case Float(value: Double)
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
}
