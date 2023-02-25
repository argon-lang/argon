package dev.argon.tube

import dev.argon.util.toml.Toml as AToml
import dev.argon.util.{*, given}

object TomlConverter {
  def encodeToml(toml: AToml): Toml =
    toml match {
      case AToml.Table(map) =>
        val values = map
          .iterator
          .map { (k, v) => TomlKeyValue(k, encodeToml(v)) }
          .toSeq

        Toml(Toml.Value.Table(TomlTable(values)))

      case AToml.Array(value) =>
        Toml(Toml.Value.Array(TomlArray(value.map(encodeToml))))

      case AToml.String(value) =>
        Toml(Toml.Value.StringValue(value))

      case AToml.Int(value) =>
        Toml(Toml.Value.IntValue(value))

      case AToml.Float(value) =>
        Toml(Toml.Value.FloatValue(value))

      case AToml.Boolean(value) =>
        Toml(Toml.Value.BoolValue(value))

      case AToml.OffsetDateTime(value) =>
        Toml(Toml.Value.OffsetDateTime(value))

      case AToml.LocalDateTime(value) =>
        Toml(Toml.Value.LocalDateTime(value))

      case AToml.LocalDate(value) =>
        Toml(Toml.Value.LocalDate(value))

      case AToml.LocalTime(value) =>
        Toml(Toml.Value.LocalTime(value))
    }


  def decodeToml(toml: Toml): AToml =
    toml.value match {
      case Toml.Value.IntValue(i) => AToml.Int(i)
      case Toml.Value.StringValue(s) => AToml.String(s)
      case Toml.Value.FloatValue(d) => AToml.Float(d)
      case Toml.Value.BoolValue(b) => AToml.Boolean(b)
      case Toml.Value.OffsetDateTime(odt) => AToml.OffsetDateTime(odt)
      case Toml.Value.LocalDateTime(dt) => AToml.LocalDateTime(dt)
      case Toml.Value.LocalDate(d) => AToml.LocalDate(d)
      case Toml.Value.LocalTime(t) => AToml.LocalTime(t)
      case Toml.Value.Array(arr) =>
        AToml.Array(arr.elements.map(decodeToml))

      case Toml.Value.Table(table) =>
        AToml.Table(
          table.elements
            .map { kvp => kvp.key -> decodeToml(kvp.value) }
            .toMap
        )

      case _: Toml.Value.Empty.type =>
        throw new IllegalArgumentException("Invalid toml value")
    }
}
