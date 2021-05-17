package dev.argon.options

import cats.{Applicative, Id, ~>}
import dev.argon.options.OptionsHandler.{CombineFunction, InferDefaultsFunction}
import shapeless.HNil

trait OptionsHandler[O <: OptionID, Decoded[_]] {
  type OptRepr[A[_]]
  def ids: OptRepr[Lambda[X => O { type ElementType = X }]]
  def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]]

  def convertRepr[A[_], B[_]](l: OptRepr[A])(f: A ~> B): OptRepr[B] =
    combineRepr(l, l)(new CombineFunction[O, A, A, B, Id] {
      override def apply(id: O)(ax: A[id.ElementType], bx: A[id.ElementType]): Id[B[id.ElementType]] = f(ax)
    })


  def optionsToRepr[A[_]](options: Options[A, O]): OptRepr[A] =
    convertRepr(ids)(new (Lambda[X => O { type ElementType = X }] ~> A) {
      override def apply[X](fa: O { type ElementType = X }): A[X] = options.get(fa)
    })

  def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O]

  def info: Options[OptionInfo, O]
  def decoder: Options[Lambda[X => OptionDecoder[Decoded[X]]], O]

  final def empty[A[_]]: Options[Lambda[X => Option[A[X]]], O] = new Options.MapBackedOptionOptions[A, O](Map.empty)

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
    override type OptRepr[A[_]] = HNil

    override def ids: HNil = HNil

    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: HNil, listb: HNil)(f: OptionsHandler.CombineFunction[Nothing, A, B, C, F]): F[HNil] =
      Applicative[F].pure(HNil)

    override def reprToOptions[A[_]](list: HNil): Options[A, Nothing] =
      Options.fromFunction[A, Nothing](new Options.OptionValueFunction[A, Nothing] {
        override def apply[E](id: Nothing { type ElementType = E }): A[E] = id
      })

    override def info: Options[OptionInfo, Nothing] = reprToOptions(HNil)

    override def decoder: Options[Lambda[X => OptionDecoder[Decoded[X]]], Nothing] = reprToOptions(HNil)
  }

  // This is only needed to prevent AbstractMethodError
  private abstract class InferDefaultsFunction[O <: OptionID, A[_], B[_]] extends CombineFunction[O, A, B, Id, Either[O, *]] {

    def aToOptionInfo[X](a: A[X]): OptionInfo[X]
    def bToOption[X](b: B[X]): Option[X]

    override def apply(id: O)(ax: A[id.ElementType], bx: B[id.ElementType]): Either[O, Id[id.ElementType]] =
      bToOption(bx).orElse(aToOptionInfo(ax).defaultValueOption).toRight(id)
  }

}
