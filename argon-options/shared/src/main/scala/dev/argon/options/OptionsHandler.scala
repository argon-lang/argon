package dev.argon.options

import cats.{Applicative, Id, ~>}
import dev.argon.options.OptionsHandler.{CombineFunction, InferDefaultsFunction}

trait OptionsHandler[O <: OptionID { type Decoded[A] = Dec[A] }, Dec[A]] {
  type OptRepr[A[_]]

  type AsIDType[X] = O { type ElementType = X }
  type AsOptional[A[_], X] = Option[A[X]]
  type AsDecoder[X] = OptionDecoder[Dec[X]]

  def ids: OptRepr[AsIDType]
  def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]]

  def convertRepr[A[_], B[_]](l: OptRepr[A])(f: A ~> B): OptRepr[B] =
    combineRepr(l, l)(new CombineFunction[O, A, A, B, Id] {
      override def apply(id: O)(ax: A[id.ElementType], bx: A[id.ElementType]): Id[B[id.ElementType]] = f(ax)
    })


  def optionsToRepr[A[_]](options: Options[A, O]): OptRepr[A] =
    convertRepr(ids)(new (AsIDType ~> A) {
      override def apply[X](fa: O { type ElementType = X }): A[X] = options.get(fa)
    })

  def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O]

  final def info: Options[OptionInfo, O] = Options.fromFunction(
    new Options.OptionValueFunction[OptionInfo, O] {
      override def apply[E](id: O with TypedOptionID[E]): OptionInfo[E] =
        id.info
    }
  )
  final def decoder: Options[AsDecoder, O] = Options.fromFunction(
    new Options.OptionValueFunction[AsDecoder, O] {
      override def apply[E](id: O with TypedOptionID[E]): OptionDecoder[Dec[E]] =
        id.decoder
    }
  )

  final def empty[A[_]]: Options[AsOptional[A, *], O] = new Options.MapBackedOptionOptions[A, O](Map.empty)

  final def inferDefaults(options: Options[Option, O]): Either[O, Options[Id, O]] =
    combineRepr(optionsToRepr(info), optionsToRepr(options))(
      new InferDefaultsFunction[O, OptionInfo, Option] {
        override def aToOptionInfo[X](a: OptionInfo[X]): OptionInfo[X] = a

        override def bToOption[X](b: Option[X]): Option[X] = b
      }
    ).map(reprToOptions)

}

object OptionsHandler {

  trait CombineFunction[-O <: OptionID, A[_], B[_], C[_], F[_]] {
    def apply(id: O)(ax: A[id.ElementType], bx: B[id.ElementType]): F[C[id.ElementType]]
  }

  trait OptionsFunction[O <: OptionID, F[_]] {
    def apply(id: O): F[id.ElementType]
  }

  class Empty[Decoded[_]] extends OptionsHandler[Nothing, Decoded] {
    override type OptRepr[A[_]] = Unit

    override def ids: Unit = ()

    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: Unit, listb: Unit)(f: OptionsHandler.CombineFunction[Nothing, A, B, C, F]): F[Unit] =
      Applicative[F].pure(())

    override def reprToOptions[A[_]](list: Unit): Options[A, Nothing] =
      Options.fromFunction[A, Nothing](new Options.OptionValueFunction[A, Nothing] {
        override def apply[E](id: Nothing with TypedOptionID[E]): A[E] = id
      })
  }

  // This is only needed to prevent AbstractMethodError
  private abstract class InferDefaultsFunction[O <: OptionID, A[_], B[_]] extends CombineFunction[O, A, B, Id, Either[O, *]] {

    def aToOptionInfo[X](a: A[X]): OptionInfo[X]
    def bToOption[X](b: B[X]): Option[X]

    override def apply(id: O)(ax: A[id.ElementType], bx: B[id.ElementType]): Either[O, Id[id.ElementType]] =
      bToOption(bx).orElse(aToOptionInfo(ax).defaultValueOption).toRight(id)
  }

}
