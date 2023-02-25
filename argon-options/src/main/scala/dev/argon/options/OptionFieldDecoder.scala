package dev.argon.options

import dev.argon.io.ResourceFactory
import dev.argon.util.toml.Toml

trait OptionFieldDecoder[R, E, A] {
  def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A]
  def defaultValue: Option[A]
}

object OptionFieldDecoder {
  given [R, E, A](using decoder: OptionDecoder[R, E, A]): OptionFieldDecoder[R, E, A] with
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, A] =
      decoder.decode(resFactory)(value)

    override def defaultValue: Option[A] = None
  end given

  given [R, E, A](using decoder: OptionDecoder[R, E, A]): OptionFieldDecoder[R, E, Option[A]] with
    override def decode(resFactory: ResourceFactory[R, E])(value: Toml): Either[String, Option[A]] =
      decoder.decode(resFactory)(value).map(Some.apply)
  
    override def defaultValue: Option[Option[A]] = Some(None)
  end given


}
