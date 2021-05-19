package dev.argon.compiler.vtable

import dev.argon.compiler.core._
import cats._
import cats.implicits._
import dev.argon.util.Nat


trait VTableContext {

  val context: Context

  type EntrySignature = context.signatureContext.Signature[FunctionResultInfo, _ <: Nat]

  final case class VTableEntry(signature: EntrySignature, entrySource: EntrySource, impl: VTableEntryImpl)

  sealed trait VTableEntryImpl

  final case class VTableEntryMethod(method: AbsRef[context.type, ArMethod]) extends VTableEntryImpl
  final case class VTableEntryAmbiguous(methods: Set[AbsRef[context.type, ArMethod]]) extends VTableEntryImpl
  case object VTableEntryAbstract extends VTableEntryImpl

  sealed trait EntrySource
  final case class EntrySourceClass(arClass: AbsRef[context.type, ArClass], baseClasses: Vector[AbsRef[context.type, ArClass]], baseTraits: Vector[AbsRef[context.type, ArTrait]]) extends EntrySource
  final case class EntrySourceTrait(arTrait: AbsRef[context.type, ArTrait], baseTraits: Vector[AbsRef[context.type, ArTrait]]) extends EntrySource
  final case class EntrySourceDataCtor(dataCtor: AbsRef[context.type, DataConstructor], baseTraits: Vector[AbsRef[context.type, ArTrait]]) extends EntrySource
  final case class EntrySourceMulti(sourceA: EntrySource, sourceB: EntrySource) extends EntrySource

  object VTableEntry {

    private def sameSource(a: EntrySource, b: EntrySource): Boolean =
      (a, b) match {
        case (EntrySourceMulti(_, _), _) | (_, EntrySourceMulti(_, _)) => false

        case (EntrySourceClass(a, _, _), EntrySourceClass(b, _, _)) => a.value.id === b.value.id
        case (EntrySourceClass(_, _, _), _) | (_, EntrySourceClass(_, _, _)) => false

        case (EntrySourceTrait(a, _), EntrySourceTrait(b, _)) => a.value.id === b.value.id
        case (EntrySourceTrait(_, _), _) | (_, EntrySourceTrait(_, _)) => false

        case (EntrySourceDataCtor(a, _), EntrySourceDataCtor(b, _)) => a.value.id === b.value.id
        case (EntrySourceDataCtor(_, _), _) | (_, EntrySourceDataCtor(_, _)) => false
      }

    private def moreSpecificSource(a: EntrySource, b: EntrySource): Boolean =
      (a, b) match {
        case (EntrySourceMulti(a1, a2), _) => moreSpecificSource(a1, b) || moreSpecificSource(a2, b)
        case (_, EntrySourceMulti(b1, b2)) => moreSpecificSource(a, b1) && moreSpecificSource(a, b2)
        case (EntrySourceTrait(_, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.value.id === arTrait.value.id }
        case (EntrySourceTrait(_, _), _) => false
        case (EntrySourceClass(_, baseClasses, _), EntrySourceClass(arClass, _, _)) => baseClasses.exists { _.value.id === arClass.value.id }
        case (EntrySourceClass(_, _, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.value.id === arTrait.value.id }
        case (EntrySourceClass(_, _, _), _) => false
        case (EntrySourceDataCtor(_, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.value.id === arTrait.value.id }
        case (EntrySourceDataCtor(_, _), _) => false
      }

    implicit def semigroupInstance: Semigroup[VTableEntry] = new Semigroup[VTableEntry] {
      override def combine(x: VTableEntry, y: VTableEntry): VTableEntry = (x, y) match {
        case (VTableEntry(_, a, _), VTableEntry(_, b, _)) if sameSource(a, b) => x
        case (VTableEntry(_, a, _), VTableEntry(_, b, _)) if moreSpecificSource(a, b) => x
        case (VTableEntry(_, a, _), VTableEntry(_, b, _)) if moreSpecificSource(b, a) => y
        case (VTableEntry(sigA, sourceA, VTableEntryAbstract), VTableEntry(_, sourceB, VTableEntryAbstract)) =>
          VTableEntry(sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAbstract)

        case (VTableEntry(_, _, VTableEntryAbstract), right) => right
        case (left, VTableEntry(_, _, VTableEntryAbstract)) => left
        case (VTableEntry(sigA, sourceA, VTableEntryAmbiguous(a)), VTableEntry(_, sourceB, VTableEntryAmbiguous(b))) =>
          VTableEntry(sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(a ++ b))

        case (VTableEntry(sigA, sourceA, VTableEntryAmbiguous(a)), VTableEntry(_, sourceB, VTableEntryMethod(b))) =>
          VTableEntry(sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(a + b))

        case (VTableEntry(sigA, sourceA, VTableEntryMethod(a)), VTableEntry(_, sourceB, VTableEntryAmbiguous(b))) =>
          VTableEntry(sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(b + a))

        case (VTableEntry(sigA, sourceA, VTableEntryMethod(a)), VTableEntry(_, sourceB, VTableEntryMethod(b))) =>
          VTableEntry(sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(Set(a, b)))
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
