package dev.argon.compiler.vtable

import dev.argon.compiler.core._
import cats._
import cats.implicits._

sealed trait VTableEntry[TContext <: Context with Singleton] {
  val entrySource: EntrySource[TContext]
}

final case class VTableEntryMethod[TContext <: Context with Singleton](method: AbsRef[TContext, ArMethod], entrySource: EntrySource[TContext]) extends VTableEntry[TContext]
final case class VTableEntryAmbiguous[TContext <: Context with Singleton](methods: Set[AbsRef[TContext, ArMethod]], entrySource: EntrySource[TContext]) extends VTableEntry[TContext]
final case class VTableEntryAbstract[TContext <: Context with Singleton](entrySource: EntrySource[TContext]) extends VTableEntry[TContext]

sealed trait EntrySource[TContext <: Context with Singleton]
final case class EntrySourceClass[TContext <: Context with Singleton](arClass: AbsRef[TContext, ArClass], baseClasses: Vector[AbsRef[TContext, ArClass]], baseTraits: Vector[AbsRef[TContext, ArTrait]]) extends EntrySource[TContext]
final case class EntrySourceTrait[TContext <: Context with Singleton](arTrait: AbsRef[TContext, ArTrait], baseTraits: Vector[AbsRef[TContext, ArTrait]]) extends EntrySource[TContext]
final case class EntrySourceDataCtor[TContext <: Context with Singleton](dataCtor: AbsRef[TContext, DataConstructor], baseTraits: Vector[AbsRef[TContext, ArTrait]]) extends EntrySource[TContext]
final case class EntrySourceMulti[TContext <: Context with Singleton](sourceA: EntrySource[TContext], sourceB: EntrySource[TContext]) extends EntrySource[TContext]

object VTableEntry {

  private def sameSource[TContext <: Context with Singleton](a: EntrySource[TContext], b: EntrySource[TContext]): Boolean =
    (a, b) match {
      case (EntrySourceMulti(_, _), _) | (_, EntrySourceMulti(_, _)) => false

      case (EntrySourceClass(a, _, _), EntrySourceClass(b, _, _)) => a.value.descriptor === b.value.descriptor
      case (EntrySourceClass(_, _, _), _) | (_, EntrySourceClass(_, _, _)) => false

      case (EntrySourceTrait(a, _), EntrySourceTrait(b, _)) => a.value.descriptor === b.value.descriptor
      case (EntrySourceTrait(_, _), _) | (_, EntrySourceTrait(_, _)) => false

      case (EntrySourceDataCtor(a, _), EntrySourceDataCtor(b, _)) => a.value.descriptor === b.value.descriptor
      case (EntrySourceDataCtor(_, _), _) | (_, EntrySourceDataCtor(_, _)) => false
    }

  private def moreSpecificSource[TContext <: Context with Singleton](a: EntrySource[TContext], b: EntrySource[TContext]): Boolean =
    (a, b) match {
      case (EntrySourceMulti(a1, a2), _) => moreSpecificSource(a1, b) || moreSpecificSource(a2, b)
      case (_, EntrySourceMulti(b1, b2)) => moreSpecificSource(a, b1) && moreSpecificSource(a, b2)
      case (EntrySourceTrait(_, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.value.descriptor === arTrait.value.descriptor }
      case (EntrySourceTrait(_, _), _) => false
      case (EntrySourceClass(_, baseClasses, _), EntrySourceClass(arClass, _, _)) => baseClasses.exists { _.value.descriptor === arClass.value.descriptor }
      case (EntrySourceClass(_, _, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.value.descriptor === arTrait.value.descriptor }
      case (EntrySourceClass(_, _, _), _) => false
      case (EntrySourceDataCtor(_, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.value.descriptor === arTrait.value.descriptor }
      case (EntrySourceDataCtor(_, _), _) => false
    }

  implicit def semigroupInstance[TContext <: Context with Singleton]: Semigroup[VTableEntry[TContext]] = new Semigroup[VTableEntry[TContext]] {
    override def combine(x: VTableEntry[TContext], y: VTableEntry[TContext]): VTableEntry[TContext] = (x, y) match {
      case (a, b) if sameSource(a.entrySource, b.entrySource) => a
      case (a, b) if moreSpecificSource(a.entrySource, b.entrySource) => a
      case (a, b) if moreSpecificSource(b.entrySource, a.entrySource) => b
      case (VTableEntryAbstract(sourceA), VTableEntryAbstract(sourceB)) => VTableEntryAbstract(EntrySourceMulti(sourceA, sourceB))
      case (VTableEntryAbstract(_), right) => right
      case (left, VTableEntryAbstract(_)) => left
      case (VTableEntryAmbiguous(a, sourceA), VTableEntryAmbiguous(b, sourceB)) => VTableEntryAmbiguous(a ++ b, EntrySourceMulti(sourceA, sourceB))
      case (VTableEntryAmbiguous(a, sourceA), VTableEntryMethod(b, sourceB)) => VTableEntryAmbiguous(a + b, EntrySourceMulti(sourceA, sourceB))
      case (VTableEntryMethod(a, sourceA), VTableEntryAmbiguous(b, sourceB)) => VTableEntryAmbiguous(b + a, EntrySourceMulti(sourceA, sourceB))
      case (VTableEntryMethod(a, sourceA), VTableEntryMethod(b, sourceB)) => VTableEntryAmbiguous(Set(a, b), EntrySourceMulti(sourceA, sourceB))
    }
  }

}
