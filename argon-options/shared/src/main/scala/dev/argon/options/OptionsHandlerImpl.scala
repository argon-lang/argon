package dev.argon.options

import cats.Applicative
import dev.argon.options.OptionsHandler.CombineFunction
import dev.argon.options.OptionsHandlerImpl.OptionIDList
import shapeless.{Id => _, _}

abstract class OptionsHandlerImpl[O <: OptionID { type Decoded[A] = Dec[A] }, Dec[_]](implicit val idList: OptionIDList[O]) extends OptionsHandler[O, Dec] {
  override type OptRepr[A[_]] = idList.OptRepr[A]

  override def ids: OptRepr[Lambda[X => O { type ElementType = X }]] = idList.ids

  override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]] =
    idList.combineRepr(lista, listb)(f)

  override def reprToOptions[A[_]](list: idList.OptRepr[A]): Options[A, O] =
    idList.reprToOptions(list)
}

object OptionsHandlerImpl {

  sealed trait OptionIDList[O <: OptionID] {
    type OptRepr[A[_]]
    def ids: OptRepr[Lambda[X => O { type ElementType = X }]]
    def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]]
    def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O]
  }

  sealed trait OptionIDListPart[O <: OptionID, L <: Coproduct] {
    type OptRepr[A[_]] <: HList
    def ids: OptRepr[Lambda[X => O { type ElementType = X }]]
    def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]]
    def getOptionValue[L2 <: L, A[_]](lookupId: L2, values: OptRepr[A]): A[_]
  }

  sealed trait OptionIDListPartCons[O <: OptionID, H <: O, T <: Coproduct] extends OptionIDListPart[O, H :+: T] {
    val id: H
    def idAsO: O { type ElementType = id.ElementType } = id
  }

  implicit def optionIDListPartCNil[O <: OptionID]: OptionIDListPart[O, CNil] = new OptionIDListPart[O, CNil] {
    override type OptRepr[A[_]] = HNil
    override def ids: HNil = HNil
    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: HNil, listb: HNil)(f: CombineFunction[O, A, B, C, F]): F[HNil] =
      Applicative[F].pure(HNil)

    override def getOptionValue[L2 <: CNil, A[_]](lookupId: L2, values: HNil): A[_] = lookupId.impossible
  }

  implicit def optionIDListPartCons[O <: OptionID, H <: O, T <: Coproduct](implicit headGeneric: Generic.Aux[H, HNil], tailListPart: OptionIDListPart[O, T]): OptionIDListPart[O, H :+: T] = new OptionIDListPartCons[O, H, T] {
    override val id: H = headGeneric.from(HNil)
    override type OptRepr[A[_]] = A[id.ElementType] :: tailListPart.OptRepr[A]
    override def ids: OptRepr[Lambda[X => O { type ElementType = X }]] = idAsO :: tailListPart.ids

    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: A[id.ElementType] :: tailListPart.OptRepr[A], listb: B[id.ElementType] :: tailListPart.OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[C[id.ElementType] :: tailListPart.OptRepr[C]] =
      Applicative[F].map2(
        f(id)(lista.head, listb.head),
        tailListPart.combineRepr(lista.tail, listb.tail)(f)
      ) { (h, t) => h :: t }

    override def getOptionValue[L2 <: H :+: T, A[_]](lookupId: L2, values: A[id.ElementType] :: tailListPart.OptRepr[A]): A[_] =
      lookupId match {
        case Inl(_) => values.head
        case Inr(t) => tailListPart.getOptionValue(t, values.tail)
      }
  }


  implicit def optionIDList[O <: OptionID, L <: Coproduct](implicit listGeneric: Generic.Aux[O, L], listPart: OptionIDListPart[O, L]): OptionIDList[O] = new OptionIDList[O] {
    override type OptRepr[A[_]] = listPart.OptRepr[A]

    override def ids: OptRepr[Lambda[X => O { type ElementType = X }]] = listPart.ids
    override def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]] = listPart.combineRepr(lista, listb)(f)
    override def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O] =
      Options.fromFunction(new Options.OptionValueFunction[A, O] {
        @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
        override def apply[E](id: O { type ElementType = E }): A[E] =
          listPart.getOptionValue(listGeneric.to(id), list).asInstanceOf[A[E]]
      })
  }


}

