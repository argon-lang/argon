package dev.argon.compiler.vtable

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.{ArgonExprContext, ExprUtil, ExprUtilWithHoles, HolesExprContext}
import dev.argon.compiler.signature.*
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}
import zio.*

sealed abstract class VTableBuilder[TContext <: Context](override val context: TContext) extends UsingContext with ExprUtil {
  def fromClass(arClass: ArClass): Comp[VTable]
  def fromTrait(arTrait: ArTrait): Comp[VTable]

  // Get only the changes from the base class
  def diffFromClass(arClass: ArClass): Comp[VTable]

  override val exprContext: context.ExprContext.type = context.ExprContext
  import context.ExprContext.WrapExpr

  type EntrySignature = Signature[WrapExpr, WrapExpr]

  final case class VTableEntry(name: Option[IdentifierExpr], signature: EntrySignature, entrySource: EntrySource, impl: VTableEntryImpl)

  sealed trait VTableEntryImpl derives CanEqual

  final case class VTableEntryMethod(method: ArMethod) extends VTableEntryImpl
  final case class VTableEntryAmbiguous(methods: Set[ArMethod]) extends VTableEntryImpl
  case object VTableEntryAbstract extends VTableEntryImpl

  sealed trait EntrySource
  final case class EntrySourceClass(arClass: ArClass, baseClasses: Vector[ArClass], baseTraits: Vector[ArTrait]) extends EntrySource
  final case class EntrySourceTrait(arTrait: ArTrait, baseTraits: Vector[ArTrait]) extends EntrySource
  final case class EntrySourceMulti(sourceA: EntrySource, sourceB: EntrySource) extends EntrySource

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
        case (EntrySourceTrait(_, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.id == arTrait.id }
        case (EntrySourceTrait(_, _), _) => false
        case (EntrySourceClass(_, baseClasses, _), EntrySourceClass(arClass, _, _)) => baseClasses.exists { _.id == arClass.id }
        case (EntrySourceClass(_, _, baseTraits), EntrySourceTrait(arTrait, _)) => baseTraits.exists { _.id == arTrait.id }
      }

    implicit def semigroupInstance: Semigroup[VTableEntry] = new Semigroup[VTableEntry] {
      override def combine(x: VTableEntry, y: VTableEntry): VTableEntry = (x, y) match {
        case (VTableEntry(_, _, a, _), VTableEntry(_, _, b, _)) if sameSource(a, b) => x
        case (VTableEntry(_, _, a, _), VTableEntry(_, _, b, _)) if moreSpecificSource(a, b) => x
        case (VTableEntry(_, _, a, _), VTableEntry(_, _, b, _)) if moreSpecificSource(b, a) => y
        case (VTableEntry(nameA, sigA, sourceA, VTableEntryAbstract), VTableEntry(_, _, sourceB, VTableEntryAbstract)) =>
          VTableEntry(nameA, sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAbstract)

        case (VTableEntry(_, _, _, VTableEntryAbstract), right) => right
        case (left, VTableEntry(_, _, _, VTableEntryAbstract)) => left
        case (VTableEntry(nameA, sigA, sourceA, VTableEntryAmbiguous(a)), VTableEntry(_, _, sourceB, VTableEntryAmbiguous(b))) =>
          VTableEntry(nameA, sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(a ++ b))

        case (VTableEntry(nameA, sigA, sourceA, VTableEntryAmbiguous(a)), VTableEntry(_, _, sourceB, VTableEntryMethod(b))) =>
          VTableEntry(nameA, sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(a + b))

        case (VTableEntry(nameA, sigA, sourceA, VTableEntryMethod(a)), VTableEntry(_, _, sourceB, VTableEntryAmbiguous(b))) =>
          VTableEntry(nameA, sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(b + a))

        case (VTableEntry(nameA, sigA, sourceA, VTableEntryMethod(a)), VTableEntry(_, _, sourceB, VTableEntryMethod(b))) =>
          VTableEntry(nameA, sigA, EntrySourceMulti(sourceA, sourceB), VTableEntryAmbiguous(Set(a, b)))
      }
    }

  }

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

object VTableBuilder {

  def apply(context: Context): UIO[VTableBuilder[context.type]] = {
    val ctx: context.type = context

    for {
      classVtableCache0 <- MemoCacheStore.make[ctx.Env, ctx.Error, ArClassC with HasContext[ctx.type], Any]
      traitVtableCache0 <- MemoCacheStore.make[ctx.Env, ctx.Error, ArTraitC with HasContext[ctx.type], Any]
    } yield new VTableBuilder[context.type](context) {
      import this.context.ExprContext.{ArExpr, ExprConstructor, WrapExpr, Variable, ParameterVariable}

      private val classVtableCache = classVtableCache0.asInstanceOf[MemoCacheStore[ctx.Env, ctx.Error, ArClass, VTable]]
      private val traitVtableCache = traitVtableCache0.asInstanceOf[MemoCacheStore[ctx.Env, ctx.Error, ArTrait, VTable]]



      type VT = VTable
      type Entry = VTableEntry

      private def signatureMatches[PS[_, _], PSSlot[_, _]](method: ArMethod)(slotSig: EntrySignature): Comp[Boolean] =
        val holesExprContext = new HolesExprContext {
          override val context: ctx.type = ctx
        }

        val exprUtil = new ExprUtilWithHoles {
          override val context: ctx.type = ctx
          override val exprContext: holesExprContext.type = holesExprContext
        }

        val emptyEnv = exprUtil.Env(exprUtil.Scope.empty, model = Map.empty)

        def impl(sig: Signature[holesExprContext.WrapExpr, holesExprContext.WrapExpr], slotSig: Signature[holesExprContext.WrapExpr, holesExprContext.WrapExpr]): Comp[Boolean] =
          (sig, slotSig) match {
            case (aParam @ Signature.Parameter(_, _, _, _, _), bParam @ Signature.Parameter(_, _, _, _, _)) =>
              exprUtil.isSubType(emptyEnv, aParam.paramType, bParam.paramType).map { _.isDefined }
                .flatMap {
                  case true => exprUtil.isSubType(emptyEnv, bParam.paramType, aParam.paramType).map { _.isDefined }
                  case false => ZIO.succeed(false)
                }
                .flatMap {
                  case true => impl(aParam.next, bParam.next)
                  case false => ZIO.succeed(false)
                }

            case (Signature.Result(aResult), Signature.Result(bResult)) =>
              exprUtil.isSubType(emptyEnv, aResult, bResult).map { _.isDefined }

            case _ => ZIO.succeed(false)
          }

        method.signatureUnsubstituted.flatMap { sig =>
          impl(exprUtil.convertSig(exprUtil.functionSigHandler)(sig), exprUtil.convertSig(exprUtil.functionSigHandler)(slotSig))
        }
      end signatureMatches

      private def overrideMethod(methodName: Option[IdentifierExpr], method: ArMethod)(source: EntrySource)(baseTypeVTable: VT): Comp[VT] = {

        val newEntryImpl =
          if(method.isAbstract)
            VTableEntryAbstract
          else
            VTableEntryMethod(method)

        method.signatureUnsubstituted.flatMap { sig =>
          (
            if(method.isImplicitOverride)
              ZIO.filter(
                baseTypeVTable.methodMap
                  .toSeq
                  .filter { case (slotMethod, slotEntry) =>
                    slotMethod.isVirtual &&
                      !slotMethod.isFinal &&
                      methodName.isDefined &&
                      slotEntry.name == methodName
                  }
              ) { case (_, VTableEntry(_, slotSig, _, _)) => signatureMatches(method)(slotSig) }
                .map { slotMethods =>
                  VTable(
                    methodMap = slotMethods
                      .map {
                        case (slotMethod, VTableEntry(slotName, slotSig, _, _)) =>
                          slotMethod -> VTableEntry(slotName, slotSig, source, newEntryImpl)
                      }
                      .toMap[ArMethod, VTableEntry]
                  )
                }
            else
              ZIO.succeed(VTable(
                methodMap = Map.empty
              ))
          ).map { case VTable(methodMap) =>
            VTable(
              methodMap = methodMap + (method -> VTableEntry(methodName, sig, source, newEntryImpl))
            )
          }
        }
      }

      private def addNewMethods(methodGroups: Map[Option[IdentifierExpr], Seq[ArMethod]])(source: EntrySource)(baseTypeVTable: VT): Comp[VT] =
        ZIO.foreach(methodGroups.toSeq) { case (methodName, methods) =>
          ZIO.foreach(methods) { method =>
            overrideMethod(methodName, method)(source)(baseTypeVTable)
          }
        }
          .map { newEntries =>
            summon[Semigroup[VT]].combine(baseTypeVTable, summon[Semigroup[VT]].combineAll(newEntries.flatten))
          }

      private def findAllBaseClasses(baseClass: Option[ArExpr[ExprConstructor.ClassType]]): Comp[Vector[ArClass]] = {

        def addClass(acc: Vector[ArClass], baseClass: ArExpr[ExprConstructor.ClassType]): Comp[Vector[ArClass]] =
          if(acc.exists { _.id == baseClass.constructor.arClass.id })
            ZIO.succeed(acc)
          else
            for {
              sig <- baseClass.constructor.arClass.signature
              baseBaseClass = sig.unsubstitutedResult._2
              classes <- ZIO.foldLeft(baseBaseClass)(acc :+ baseClass.constructor.arClass)(addClass)
            } yield classes

        ZIO.foldLeft(baseClass)(Vector.empty[ArClass])(addClass)
      }

      private def findAllBaseTraits(baseClasses: Vector[ArClass], baseTraits: Seq[ArExpr[ExprConstructor.TraitType]]): Comp[Vector[ArTrait]] = {

        def addTrait(acc: Vector[ArTrait], baseTrait: ArExpr[ExprConstructor.TraitType]): Comp[Vector[ArTrait]] =
          if(acc.exists { _.id == baseTrait.constructor.arTrait.id })
            ZIO.succeed(acc)
          else
            for {
              sig <- baseTrait.constructor.arTrait.signature
              baseBaseTraits = sig.unsubstitutedResult._2
              traits <- ZIO.foldLeft(baseBaseTraits)(acc :+ baseTrait.constructor.arTrait)(addTrait)
            } yield traits

        def addTraitsFromClass(acc: Vector[ArTrait], baseClass: ArClass): Comp[Vector[ArTrait]] =
          for {
            sig <- baseClass.signature
            baseTraits = sig.unsubstitutedResult._3
            traits <- ZIO.foldLeft(baseTraits)(acc)(addTrait)
          } yield traits

        val acc = Vector.empty[ArTrait]
        for {
          acc <- ZIO.foldLeft(baseTraits)(acc)(addTrait)
          acc <- ZIO.foldLeft(baseClasses)(acc)(addTraitsFromClass)
        } yield acc
      }

      private def ensureNonAbstract(vtable: VT, source: DiagnosticSource): Comp[Unit] =
        ZIO.foreachDiscard(vtable.methodMap.values) {
          case VTableEntry(_, _, _, VTableEntryMethod(_)) => ZIO.unit
          case VTableEntry(_, _, _, VTableEntryAbstract) =>
            ZIO.fail(DiagnosticError.AbstractMethodNotImplemented())

          case VTableEntry(_, _, _, VTableEntryAmbiguous(_)) => ???
        }

      private def getSigParameterVars(owner: exprContext.ParameterVariableOwner, sig: Signature[WrapExpr, ?], acc: Seq[Variable]): Seq[Variable] =
        sig match
          case Signature.Parameter(_, isErased, paramName, paramType, next) =>
            getSigParameterVars(owner, next, acc :+ ParameterVariable(owner, acc.size, paramType, isErased, paramName))

          case Signature.Result(_) => acc
        end match

      private def getBaseTraitVTable(bt: ArExpr[ExprConstructor.TraitType]): Comp[VT] =
        for {
          btSig <- bt.constructor.arTrait.signature
          baseTraitVTable <- fromTrait(bt.constructor.arTrait)
          newMap = baseTraitVTable.methodMap.toSeq.map {
            case (key, VTableEntry(entryName, signature, entrySource, impl)) =>
              val subst = getSigParameterVars(bt.constructor.arTrait, btSig, Seq())
                .zip(bt.args)
                .toMap

              val newSig = substituteSignatureMany(subst)(functionSigHandler)(signature)
              key -> VTableEntry(entryName, newSig, entrySource, impl)
          }
        } yield VTable(newMap.toMap)

      override def fromClass(arClass: ArClass): Comp[VT] =
        classVtableCache.usingCreate(arClass) { arClass =>
          for {
            sig <- arClass.signature
            (_, baseClass, baseTraits) = sig.unsubstitutedResult
            baseClassVTable <- baseClass.traverse[Comp, VT] { bc =>
              for {
                bcSig <- bc.constructor.arClass.signature
                baseClassVTable <- fromClass(bc.constructor.arClass)
                newMap = baseClassVTable.methodMap.toSeq.map {
                  case (key, VTableEntry(entryName, signature, entrySource, impl)) =>
                    val subst = getSigParameterVars(bc.constructor.arClass, bcSig, Seq())
                      .zip(bc.args)
                      .toMap

                    val substSig = substituteSignatureMany(subst)(functionSigHandler)(signature)
                    key -> VTableEntry(
                      entryName,
                      substSig,
                      entrySource,
                      impl
                    )
                }
              } yield VTable(newMap.toMap)
            }
            baseTraitVTables <- baseTraits.traverse(getBaseTraitVTable)

            baseTypeOnlyVTable = summon[Semigroup[VT]].combine(
              summon[Semigroup[VT]].combineAll(baseClassVTable),
              summon[Semigroup[VT]].combineAll(baseTraitVTables)
            )

            methods <- arClass.methods
            allBaseClasses <- findAllBaseClasses(baseClass)
            allBaseTraits <- findAllBaseTraits(allBaseClasses, baseTraits)
            source = EntrySourceClass(arClass, allBaseClasses, allBaseTraits)
            newVTable <- addNewMethods(methods)(source)(baseTypeOnlyVTable)

            _ <- ensureNonAbstract(newVTable, arClass.classMessageSource).unless(arClass.isAbstract)

          } yield newVTable
        }

      override def fromTrait(arTrait: ArTrait): Comp[VT] =
        traitVtableCache.usingCreate(arTrait) { arTrait =>
          for {
            sig <- arTrait.signature
            (_, baseTraits) = sig.unsubstitutedResult
            baseTraitVTables <- baseTraits.traverse(getBaseTraitVTable)

            baseTraitOnlyVTable = summon[Semigroup[VT]].combineAll(baseTraitVTables)

            methods <- arTrait.methods
            allBaseTraits <- findAllBaseTraits(Vector.empty, baseTraits)
            source = EntrySourceTrait(arTrait, allBaseTraits)
            newVTable <- addNewMethods(methods)(source)(baseTraitOnlyVTable)

          } yield newVTable
        }

      private def diffVTable(current: VT, base: VT): VT =
        VTable(current.methodMap.filter {
          case (slot, entry) =>
            !base.methodMap.get(slot).forall { baseEntry => entry.impl == baseEntry.impl }
        })

      override def diffFromClass(arClass: ArClass): Comp[VTable] =
        for
          vt <- fromClass(arClass)
          sig <- arClass.signature
          (_, baseClass, _) = sig.unsubstitutedResult
          baseVT <- ZIO.foreach(baseClass) { baseClass => fromClass(baseClass.constructor.arClass) }
        yield baseVT.fold(vt)(diffVTable(vt, _))

    }
  }

}