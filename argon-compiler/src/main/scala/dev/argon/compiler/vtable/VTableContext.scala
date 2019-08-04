package dev.argon.compiler.vtable

import dev.argon.compiler._
import dev.argon.compiler.core._
import cats._
import cats.implicits._


trait VTableContext {

  val context: Context

  sealed trait VTableEntry {
    val entrySource: EntrySource
  }

  final case class VTableEntryMethod(method: AbsRef[context.type, ArMethod], entrySource: EntrySource) extends VTableEntry
  final case class VTableEntryAmbiguous(methods: Set[AbsRef[context.type, ArMethod]], entrySource: EntrySource) extends VTableEntry
  final case class VTableEntryAbstract(entrySource: EntrySource) extends VTableEntry

  sealed trait EntrySource
  final case class EntrySourceClass(arClass: AbsRef[context.type, ArClass], baseClasses: Vector[AbsRef[context.type, ArClass]], baseTraits: Vector[AbsRef[context.type, ArTrait]]) extends EntrySource
  final case class EntrySourceTrait(arTrait: AbsRef[context.type, ArTrait], baseTraits: Vector[AbsRef[context.type, ArTrait]]) extends EntrySource
  final case class EntrySourceDataCtor(dataCtor: AbsRef[context.type, DataConstructor], baseTraits: Vector[AbsRef[context.type, ArTrait]]) extends EntrySource
  final case class EntrySourceMulti(sourceA: EntrySource, sourceB: EntrySource) extends EntrySource

  object VTableEntry {

    private def sameSource(a: EntrySource, b: EntrySource): Boolean =
      (a, b) match {
        case (EntrySourceMulti(_, _), _) | (_, EntrySourceMulti(_, _)) => false

        case (EntrySourceClass(a, _, _), EntrySourceClass(b, _, _)) => a.value.descriptor === b.value.descriptor
        case (EntrySourceClass(_, _, _), _) | (_, EntrySourceClass(_, _, _)) => false

        case (EntrySourceTrait(a, _), EntrySourceTrait(b, _)) => a.value.descriptor === b.value.descriptor
        case (EntrySourceTrait(_, _), _) | (_, EntrySourceTrait(_, _)) => false

        case (EntrySourceDataCtor(a, _), EntrySourceDataCtor(b, _)) => a.value.descriptor === b.value.descriptor
        case (EntrySourceDataCtor(_, _), _) | (_, EntrySourceDataCtor(_, _)) => false
      }

    private def moreSpecificSource(a: EntrySource, b: EntrySource): Boolean =
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

    implicit def semigroupInstance: Semigroup[VTableEntry] = new Semigroup[VTableEntry] {
      override def combine(x: VTableEntry, y: VTableEntry): VTableEntry = (x, y) match {
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

  final case class VTable
  (
    methodMap: Map[AbsRef[context.type, ArMethod], VTableEntry]
  )

  object VTable {

    implicit def vtableMonoid(implicit entrySemigroup: Semigroup[VTableEntry]): Monoid[VTable] = new Monoid[VTable] {


      override def empty: VTable = VTable(methodMap = Map.empty)

      override def combine(x: VTable, y: VTable): VTable =
        VTable(
          methodMap = x.methodMap |+| y.methodMap
        )
    }

  }
}

object VTableContext {

  type Aux[TContext <: Context with Singleton] = VTableContext {
    val context: TContext
  }

}
