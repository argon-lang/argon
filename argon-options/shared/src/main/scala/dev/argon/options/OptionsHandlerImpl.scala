package dev.argon.options

import dev.argon.util.{_, given}
import dev.argon.options.OptionsHandler.CombineFunction
import dev.argon.options.OptionsHandlerImpl.OptionIDList
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

final class OptionsHandlerImpl[O <: OptionID { type Decoded[A] = Dec[A] }, Dec[_]](implicit val idList: OptionIDList[O])
    extends OptionsHandler[O, Dec] {
  override type OptRepr[A[_]] = idList.OptRepr[A]

  override def ids: OptRepr[[X] =>> O { type ElementType = X }] = idList.ids

  override def combineRepr[A[_], B[_], C[_], F[+_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])
    (f: CombineFunction[O, A, B, C, F])
    : F[OptRepr[C]] = idList.combineRepr(lista, listb)(f)

  override def reprToOptions[A[_]](list: idList.OptRepr[A]): Options[A, O] = idList.reprToOptions(list)
}

object OptionsHandlerImpl {

  sealed trait OptionIDList[O <: OptionID] {
    type OptRepr[A[_]]
    def ids: OptRepr[[X] =>> O { type ElementType = X }]

    def combineRepr[A[_], B[_], C[_], F[+_]: Applicative](lista: OptRepr[A], listb: OptRepr[B])
      (f: CombineFunction[O, A, B, C, F])
      : F[OptRepr[C]]

    def reprToOptions[A[_]](list: OptRepr[A]): Options[A, O]
  }

  sealed trait OptionIDListPart[O <: OptionID] {
    val id: O

    def combineRepr[A[_], B[_], C[_], F[+_]: Applicative](lista: A[O], listb: B[O])(f: CombineFunction[O, A, B, C, F])
      : F[C[O]]

  }

  inline def getOptionID[L <: Tuple]: List[Any] =
    inline erasedValue[L] match {
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[Mirror.ProductOf[h]].fromProduct(EmptyTuple) :: getOptionID[t]
    }

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  inline implicit def optionIDList[O <: OptionID](implicit mirror: Mirror.SumOf[O]): OptionIDList[O] = {
    val optionIds = getOptionID[mirror.MirroredElemTypes]
    new OptionIDList[O] {
      override type OptRepr[A[_]] = List[Any]

      override val ids: List[Any] = optionIds

      override def combineRepr[A[_], B[_], C[_], F[+_]: Applicative](lista: List[Any], listb: List[Any])
        (f: CombineFunction[O, A, B, C, F])
        : F[List[Any]] =
        ids.zip(lista.zip(listb)).traverse { case (id, (a, b)) =>
          val id2 = id.asInstanceOf[O]
          f(id2)(a.asInstanceOf[A[id2.ElementType]], b.asInstanceOf[B[id2.ElementType]]).asInstanceOf[F[Any]]
        }

      override def reprToOptions[A[_]](list: List[Any]): Options[A, O] =
        Options.fromFunction(new Options.OptionValueFunction[A, O] {
          override def apply[E](id: O with TypedOptionID[E]): A[E] = list(mirror.ordinal(id)).asInstanceOf[A[E]]
        })
    }
  }

}
