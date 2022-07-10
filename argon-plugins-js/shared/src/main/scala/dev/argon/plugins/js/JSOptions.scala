package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.*
import java.io.IOException

final case class JSOptions[F[_], E]
(
  header: F[Option[JSProgramResource[E]]],
  footer: F[Option[JSProgramResource[E]]],
  extern: F[Seq[JSProgramResource[E]]],
)

object JSOptions {
  object Header extends OptionInfoResource[[E] =>> Option[JSProgramResource[E]], JSOptions] {
    override val name: String = "header"
    override val description: String = "A file to insert at the header of the generated file."

    override def getValue[F[_], E](options: JSOptions[F, E]): F[Option[JSProgramResource[E]]] =
      options.header

    override def addOptionValue[E >: ResourceDecodeException | IOException](prev: JSOptions[Option, E], value: BinaryResource[E]): Either[OptionsError.ParseError, JSOptions[Option, E]] =
      if prev.header.isDefined then
        Left(OptionsError.MultipleValuesNotSupported(name))
      else
        Right(prev.copy(header = Some(Some(JSProgramResource.decode(TextResource.decode(value))))))
  }

  object Footer extends OptionInfoResource[[E] =>> Option[JSProgramResource[E]], JSOptions] {
    override val name: String = "footer"
    override val description: String = "A file to insert at the footer of the generated file."

    override def getValue[F[_], E](options: JSOptions[F, E]): F[Option[JSProgramResource[E]]] =
      options.footer

    override def addOptionValue[E >: ResourceDecodeException | IOException](prev: JSOptions[Option, E], value: BinaryResource[E]): Either[OptionsError.ParseError, JSOptions[Option, E]] =
      if prev.footer.isDefined then
        Left(OptionsError.MultipleValuesNotSupported(name))
      else
        Right(prev.copy(footer = Some(Some(JSProgramResource.decode(TextResource.decode(value))))))
  }

  object Extern extends OptionInfoResource[[E] =>> Seq[JSProgramResource[E]], JSOptions] {
    override val name: String = "extern"
    override val description: String = "A module with exports to be used as implementations of extern functions."

    override def getValue[F[_], E](options: JSOptions[F, E]): F[Seq[JSProgramResource[E]]] =
      options.extern

    override def addOptionValue[E >: ResourceDecodeException | IOException](prev: JSOptions[Option, E], value: BinaryResource[E]): Either[OptionsError.ParseError, JSOptions[Option, E]] =
      Right(prev.copy(extern = Some(prev.extern.getOrElse(Seq.empty) :+ JSProgramResource.decode(TextResource.decode(value)))))
  }

  object Handler extends OptionHandler[JSOptions] {
    override def options: Set[OptionInfoAny[JSOptions]] =
      Set(Header, Footer, Extern)

    override def empty[E]: JSOptions[Option, E] = JSOptions(
      header = None,
      footer = None,
      extern = None,
    )

    override def build[E](options: JSOptions[Option, E]): Either[OptionsError.MissingOption, JSOptions[Id, E]] =
      Right(JSOptions(
        header = options.header.flatten,
        footer = options.footer.flatten,
        extern = options.extern.getOrElse(Seq.empty)
      ))
  }

}
