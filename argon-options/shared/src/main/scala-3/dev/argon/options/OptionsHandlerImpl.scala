package dev.argon.options

import cats.Applicative
import dev.argon.options.OptionsHandler.CombineFunction
import dev.argon.options.OptionsHandlerImpl.OptionIDList
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

final class OptionsHandlerImpl[O <: OptionID { type Decoded[A] = Dec[A] }, Dec[_]](implicit val idList: OptionIDList[O]) extends OptionsHandler[O, Dec] {
  override type OptRepr[A[_]] = idList.OptRepr[A]

  override def ids: OptRepr[[X] =>> O { type ElementType = X }] = idList.ids

  override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]] =
    idList.combineRepr(lista, listb)(f)

  override def reprToOptions[A[_]](list: idList.OptRepr[A]): Options[A, O] =
    idList.reprToOptions(list)
}

object OptionsHandlerImpl {

  sealed trait OptionIDList[O <: OptionID] {
    type OptRepr[A[_]]
    def ids: OptRepr[[X] =>> O { type ElementType = X }]
    def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]]
    def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O]
  }

  sealed trait OptionIDListPart[O <: OptionID] {
    type OptRepr[+A[_]] <: Tuple
    def ids: OptRepr[[X] =>> O { type ElementType = X }]
    def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]]
    def getOptionValue[A[_]](lookupId: O, values: OptRepr[A]): A[lookupId.ElementType]
  }

  final class OptionIDListPartNil extends OptionIDListPart[Nothing] {
    override type OptRepr[+_[_]] = EmptyTuple
    override def ids: OptRepr[[X] =>> Nothing] = EmptyTuple
    override def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[Nothing, A, B, C, F]): F[OptRepr[C]] =
      Applicative[F].pure(EmptyTuple)

    override def getOptionValue[A[_]](lookupId: OptionID & Nothing, values: OptRepr[A]): A[lookupId.ElementType] =
      lookupId
  }

  final class OptionIDListPartCons[H <: OptionID & Singleton, T <: OptionID](val id: H, val tailListPart: OptionIDListPart[T]) extends OptionIDListPart[H | T] {
    type O = H | T

    override type OptRepr[+A[_]] = A[id.ElementType] *: tailListPart.OptRepr[A]
    override def ids: O { type ElementType = id.ElementType } *: tailListPart.OptRepr[[X] =>> O { type ElementType = X }] =
      id *: tailListPart.ids

    override def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]] = {
      val (aHead *: aTail) = lista
      val (bHead *: bTail) = listb
      Applicative[F].map2(
        f(id : O { type ElementType = id.ElementType })(aHead, bHead),
        tailListPart.combineRepr(aTail, bTail)(f)
      ) { (h, t) => h *: t }
    }

    override def getOptionValue[A[_]](lookupId: O, values: OptRepr[A]): A[lookupId.ElementType] = {
      val (h *: t) = values
      if(lookupId eq id) h.asInstanceOf[A[lookupId.ElementType]]
      else tailListPart.getOptionValue(lookupId.asInstanceOf[T], t).asInstanceOf[A[lookupId.ElementType]]
    }
  }

  type TupleUnion[L <: Tuple] = L match {
    case EmptyTuple => Nothing
    case h *: t => h | TupleUnion[t]
  }

  private transparent inline def optionIDListPart[O <: OptionID, L <: Tuple]: OptionIDListPart[_] =
    inline erasedValue[L] match {
      case _: EmptyTuple => OptionIDListPartNil()
      case _: (h *: t) =>
        new OptionIDListPartCons[h & O & Singleton, TupleUnion[t] & O](summonInline[Mirror.ProductOf[h & O & Singleton]].fromProduct(EmptyTuple), optionIDListPart[O, t & O].asInstanceOf[OptionIDListPart[TupleUnion[t] & O]])
    }


  implicit inline def optionIDList[O <: OptionID](implicit mirror: Mirror.Of[O]): OptionIDList[O] = {
    val listPart: OptionIDListPart[O] = optionIDListPart[O, mirror.MirroredElemTypes].asInstanceOf[OptionIDListPart[O]]



    new OptionIDList[O] {
      override type OptRepr[A[_]] = listPart.OptRepr[A]

      override def ids: OptRepr[[X] =>> O { type ElementType = X }] = listPart.ids
      override def combineRepr[A[_], B[_], C[_], F[_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: CombineFunction[O, A, B, C, F]): F[OptRepr[C]] = listPart.combineRepr(lista, listb)(f)
      override def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O] =
        Options.fromFunction(new Options.OptionValueFunction[A, O] {
          override def apply[E](id: O with TypedOptionID[E]): A[E] =
            listPart.getOptionValue(id, list)
        })
    }
  }

}

