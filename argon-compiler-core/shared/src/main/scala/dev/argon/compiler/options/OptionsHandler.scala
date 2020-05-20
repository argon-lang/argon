package dev.argon.compiler.options

import cats.Applicative
import cats.implicits._
import shapeless.Id
import zio.{IO, Ref, UIO}

trait OptionsHandler[Options[_[_], _]] {
  def info[I]: Options[OptionInfo[*, I], I]
  def converter[I]: OptionsConverter[Options[*[_], I]]
  def optionsLoader[IOld, I]: OptionsLoader[Options[Id, IOld], Options[Id, I], IOld, I]

  final def empty[I]: Options[Option, I] = {
    val infoI = info[I]
    converter[I].convert[OptionInfo[*, I], OptionInfo[*, I], Option, Id](infoI, infoI)(new OptionsConverterFunction[OptionInfo[*, I], OptionInfo[*, I], Option, Id] {
      override def apply[X](ax: OptionInfo[X, I], bx: OptionInfo[X, I]): Option[X] = None
    })
  }

  final def inferDefaults[I](options: Options[Option, I]): Either[OptionInfo[_, I], Options[Id, I]] =
    converter[I].convert[Option, OptionInfo[*, I], Id, Either[OptionInfo[_, I], *]](options, info)(
      new OptionsConverterFunction[Option, OptionInfo[*, I], Id, Either[OptionInfo[_, I], *]] {
        override def apply[X](ax: Option[X], bx: OptionInfo[X, I]): Either[OptionInfo[_, I], X] =
          ax.orElse { bx.defaultValueOption }.toRight(bx)
      }
    )

  final def fields[F[_], I](options: Options[F, I]): UIO[(Seq[OptionsField[F, I]], UIO[Options[F, I]])] = {
    type FieldIO[A] = UIO[(Seq[OptionsField[F, I]], UIO[A])]
    implicit val fieldIOAp: Applicative[FieldIO] = new Applicative[FieldIO] {
      override def pure[A](x: A): FieldIO[A] = IO.succeed((Seq.empty, IO.succeed(x)))

      override def ap[A, B](ff: FieldIO[A => B])(fa: FieldIO[A]): FieldIO[B] =
        ff.flatMap {
          case (fieldsA, ioF) =>
            fa.map {
              case (fieldsB, ioA) =>
                (fieldsA ++ fieldsB, ioA.flatMap { a => ioF.map { f => f(a) } })
            }
        }
    }

    converter[I].convert[F, OptionInfo[*, I], F, FieldIO](options, info[I])(new OptionsConverterFunction[F, OptionInfo[*, I], F, FieldIO] {
      override def apply[X](ax: F[X], bx: OptionInfo[X, I]): FieldIO[F[X]] =
        Ref.make(ax).map { refFX =>
          val field = new OptionsField[F, I] {
            override type FieldType = X

            override val info: OptionInfo[X, I] = bx
            override val fieldRef: Ref[F[X]] = refFX
          }

          (Seq(field), refFX.get)
        }
    })
  }

}
