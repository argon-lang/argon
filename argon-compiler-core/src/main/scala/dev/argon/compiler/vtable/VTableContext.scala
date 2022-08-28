package dev.argon.compiler.vtable

import dev.argon.compiler.*
import dev.argon.compiler.signature.*
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}

trait VTableContext extends UsingContext {

  import context.ExprContext.{WrapExpr, ExprConstructor, FunctionResult}
  import ExprConstructor.MethodCallOwnerType

  type EntrySignature = Signature[WrapExpr, FunctionResult]

  final case class VTableEntry(name: Option[IdentifierExpr], signature: EntrySignature, slotInstanceType: MethodCallOwnerType, entrySource: EntrySource, impl: VTableEntryImpl)

  object VTableEntry {

    private def sameSource(a: EntrySource, b: EntrySource): Boolean =
      (a, b) match {
        case (EntrySourceMulti(_, _), _) | (_, EntrySourceMulti(_, _)) => false

        case (EntrySourceClass(a, _, _), EntrySourceClass(b, _, _)) => a.id == b.id
        case (EntrySourceClass(_, _, _), _) | (_, EntrySourceClass(_, _, _)) => false

        case (EntrySourceTrait(a, _), EntrySourceTrait(b, _)) => a.id == b.id
      }

    private def moreSpecificSource(a: EntrySource, b: EntrySource): Boolean =
      (a, b) match {
        case (EntrySourceMulti(a1, a2), _) => moreSpecificSource(a1, b) || moreSpecificSource(a2, b)
        case (_, EntrySourceMulti(b1, b2)) => moreSpecificSource(a, b1) && moreSpecificSource(a, b2)
        case (EntrySourceTrait(_, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists {
          _.id == arTrait.id
        }
        case (EntrySourceTrait(_, _), _) => false
        case (EntrySourceClass(_, baseClasses, _), EntrySourceClass(arClass, _, _)) => baseClasses.exists {
          _.id == arClass.id
        }
        case (EntrySourceClass(_, _, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists {
          _.id == arTrait.id
        }
      }

    implicit def semigroupInstance: Semigroup[VTableEntry] = new Semigroup[VTableEntry] {
      override def combine(x: VTableEntry, y: VTableEntry): VTableEntry = (x, y) match {
        case (VTableEntry(_, _, _, a, _), VTableEntry(_, _, _, b, _)) if sameSource(a, b) => x
        case (VTableEntry(_, _, _, a, _), VTableEntry(_, _, _, b, _)) if moreSpecificSource(a, b) => x
        case (VTableEntry(_, _, _, a, _), VTableEntry(_, _, _, b, _)) if moreSpecificSource(b, a) => y
        case (VTableEntry(nameA, sigA, slotInstanceTypeA, sourceA, VTableEntryAbstract), VTableEntry(_, _, _, sourceB, VTableEntryAbstract)) =>
          VTableEntry(nameA, sigA, slotInstanceTypeA, EntrySourceMulti(sourceA, sourceB), VTableEntryAbstract)

        case (VTableEntry(_, _, _, _, VTableEntryAbstract), right) => right
        case (left, VTableEntry(_, _, _, _, VTableEntryAbstract)) => left
        case (VTableEntry(nameA, sigA, slotInstanceTypeA, sourceA, VTableEntryAmbiguous(a)), VTableEntry(_, _, _, sourceB, VTableEntryAmbiguous(b))) =>
          VTableEntry(nameA, sigA, slotInstanceTypeA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(a ++ b))

        case (VTableEntry(nameA, sigA, slotInstanceTypeA, sourceA, VTableEntryAmbiguous(a)), VTableEntry(_, _, _, sourceB, VTableEntryMethod(b, _))) =>
          VTableEntry(nameA, sigA, slotInstanceTypeA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(a + b))

        case (VTableEntry(nameA, sigA, slotInstanceTypeA, sourceA, VTableEntryMethod(a, _)), VTableEntry(_, _, _, sourceB, VTableEntryAmbiguous(b))) =>
          VTableEntry(nameA, sigA, slotInstanceTypeA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(b + a))

        case (VTableEntry(nameA, sigA, slotInstanceTypeA, sourceA, VTableEntryMethod(a, _)), VTableEntry(_, _, _, sourceB, VTableEntryMethod(b, _))) =>
          VTableEntry(nameA, sigA, slotInstanceTypeA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(Set(a, b)))
      }
    }

  }


  sealed trait VTableEntryImpl derives CanEqual
  final case class VTableEntryMethod(method: ArMethod, methodInstanceType: MethodCallOwnerType) extends VTableEntryImpl
  final case class VTableEntryAmbiguous(methods: Set[ArMethod]) extends VTableEntryImpl
  case object VTableEntryAbstract extends VTableEntryImpl


  sealed trait EntrySource
  final case class EntrySourceClass(arClass: ArClass, baseClasses: Vector[ArClass], baseTraits: Vector[ArTrait]) extends EntrySource
  final case class EntrySourceTrait(arTrait: ArTrait, baseTraits: Vector[ArTrait]) extends EntrySource
  final case class EntrySourceMulti(sourceA: EntrySource, sourceB: EntrySource) extends EntrySource


  final case class VTable
  (
    methodMap: Map[ArMethod, VTableEntry]
  )

  object VTable {
    implicit def vtableMonoid(implicit entrySemigroup: Semigroup[VTableEntry]): Monoid[VTable] = new Monoid[VTable] {
      override def identity: VTable = VTable(methodMap = Map.empty)

      override def combine(x: VTable, y: VTable): VTable =
        VTable(
          methodMap = summon[Semigroup[Map[ArMethod, VTableEntry]]].combine(x.methodMap, y.methodMap)
        )
    }
  }
}
