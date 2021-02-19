package dev.argon.options

import cats.Applicative
import dev.argon.options.OptionsHandler.CombineFunction
import shapeless.{Id => _, _}

object OptionCombineHelper {

  trait HListCombiner[OL, AL, BL, CL, O <: OptionID, A[_], B[_], C[_], F[_]] {
    def combine(ol: OL, al: AL, bl: BL)(f: CombineFunction[O, A, B, C, F])(implicit fApp: Applicative[F]): F[CL]
  }

  implicit def hListCombinerNil[O <: OptionID, A[_], B[_], C[_], F[_]]: HListCombiner[HNil, HNil, HNil, HNil, O, A, B, C, F] = new HListCombiner[HNil, HNil, HNil, HNil, O, A, B, C, F] {
    override def combine(ol: HNil, al: HNil, bl: HNil)(f: CombineFunction[O, A, B, C, F])(implicit fApp: Applicative[F]): F[HNil] =
      fApp.pure(al)
  }

  implicit def hListCombinerCons[O <: OptionID, A[_], B[_], C[_], H, OT <: HList, AT <: HList, BT <: HList, CT <: HList, F[_]]
  (implicit tailCombiner: HListCombiner[OT, AT, BT, CT, O, A, B, C, F])
  : HListCombiner[O { type ElementType = H } :: OT, A[H] :: AT, B[H] :: BT, C[H] :: CT, O, A, B, C, F] =
    new HListCombiner[O { type ElementType = H } :: OT, A[H] :: AT, B[H] :: BT, C[H] :: CT, O, A, B, C, F] {
      override def combine(ol: O { type ElementType = H } :: OT, al: A[H] :: AT, bl: B[H] :: BT)(f: CombineFunction[O, A, B, C, F])(implicit fApp: Applicative[F]): F[C[H] :: CT] =
        fApp.map2(
          f(ol.head)(al.head, bl.head),
          tailCombiner.combine(ol.tail, al.tail, bl.tail)(f)
        ) { (h, t) => h :: t }
    }

  def combineHLists[OL, AL, BL, CL, O <: OptionID, A[_], B[_], C[_], F[_]: Applicative](ol: OL, al: AL, bl: BL)(f: CombineFunction[O, A, B, C, F])(implicit combiner: HListCombiner[OL, AL, BL, CL, O, A, B, C, F]): F[CL] =
    combiner.combine(ol, al, bl)(f)

}
