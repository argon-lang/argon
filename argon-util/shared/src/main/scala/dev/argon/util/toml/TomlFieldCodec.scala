package dev.argon.util.toml

trait TomlFieldCodec[A] {
  def encode(a: A): Option[Toml]
  def decode(toml: Toml): Either[String, A]
  def defaultValue: Option[A]
}

object TomlFieldCodec {
  given [A: TomlCodec]: TomlFieldCodec[A] with
    override def encode(a: A): Option[Toml] =
      Some(summon[TomlCodec[A]].encode(a))

    override def decode(toml: Toml): Either[String, A] =
      summon[TomlCodec[A]].decode(toml)

    override def defaultValue: Option[A] =
      summon[TomlCodec[A]].defaultValue
  end given

  given [A: TomlCodec]: TomlFieldCodec[Option[A]] with
    override def encode(a: Option[A]): Option[Toml] =
      a.map(summon[TomlCodec[A]].encode)

    override def decode(toml: Toml): Either[String, Option[A]] =
      summon[TomlCodec[A]].decode(toml).map(Some.apply)

    override def defaultValue: Option[Option[A]] =
      Some(None)
  end given
}
