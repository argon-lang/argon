package dev.argon.compiler.vtable

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.signature.*
import dev.argon.compiler.tube.TubeImporter
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}
import zio.*

sealed abstract class VTableBuilder[TContext <: Context](override val context: TContext)
  extends ExprUtilBase
    with ExprUtilSubstitution
{
  override val exprContext: context.ExprContext.type = context.ExprContext

  import context.VT.{context as _, *}


  def fromClass(arClass: ArClass): Comp[VTable]
  def fromTrait(arTrait: ArTrait): Comp[VTable]

  // Get only the changes from the base class
  def diffFromClass(arClass: ArClass): Comp[VTable]

}

object VTableBuilder {

  def apply(context: Context, tubeImporter2: TubeImporter & HasContext[context.type]): UIO[VTableBuilder[context.type]] = {
    val ctx: context.type = context
    import context.VT.{context as _, *}

    for {
      classVtableCache <- MemoCacheStore.make[ctx.Env, ctx.Error, ArClassC & HasContext[ctx.type], VTable]
      traitVtableCache <- MemoCacheStore.make[ctx.Env, ctx.Error, ArTraitC & HasContext[ctx.type], VTable]
    } yield new VTableBuilder[context.type](context) {
      import this.context.ExprContext.{ArExpr, ExprConstructor, WrapExpr, Variable, ParameterVariable}
      import ExprConstructor.MethodCallOwnerType



      type VT = VTable
      type Entry = VTableEntry

      private def signatureMatches[PS[_, _], PSSlot[_, _]](method: ArMethod)(slotSig: EntrySignature): Comp[Boolean] =
        val holesExprContext = new HolesExprContext {
          override val context: ctx.type = ctx
        }

        val exprUtil = new ExprUtilWithHoles {
          override val context: ctx.type = ctx
          override val exprContext: holesExprContext.type = holesExprContext
          override val tubeImporter: TubeImporter & HasContext[context.type] = tubeImporter2
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

      private def overrideMethod(methodName: Option[IdentifierExpr], method: ArMethod, ownerType: MethodCallOwnerType)(source: EntrySource)(baseTypeVTable: VT): Comp[VT] =

        val newEntryImpl =
          if(method.isAbstract)
            VTableEntryAbstract
          else
            VTableEntryMethod(method, ownerType)

        for
          sig <- method.signatureUnsubstituted
          baseVTable <- (
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
              ) { case (_, VTableEntry(_, slotSig, _, _, _)) => signatureMatches(method)(slotSig) }
                .map { slotMethods =>
                  VTable(
                    methodMap = slotMethods
                      .map {
                        case (slotMethod, VTableEntry(slotName, slotSig, slotOwnerType, _, _)) =>
                          slotMethod -> VTableEntry(slotName, slotSig, slotOwnerType, source, newEntryImpl)
                      }
                      .toMap[ArMethod, VTableEntry]
                  )
                }
            else
              ZIO.succeed(VTable(
                methodMap = Map.empty
              ))
          )

        yield VTable(
          methodMap = baseVTable.methodMap + (method -> VTableEntry(methodName, sig, ownerType, source, newEntryImpl))
        )
      end overrideMethod

      private def addNewMethods(methodGroups: Map[Option[IdentifierExpr], Seq[ArMethod]], ownerType: MethodCallOwnerType)(source: EntrySource)(baseTypeVTable: VT): Comp[VT] =
        ZIO.foreach(methodGroups.toSeq) { case (methodName, methods) =>
          ZIO.foreach(methods) { method =>
            overrideMethod(methodName, method, ownerType)(source)(baseTypeVTable)
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
          case VTableEntry(_, _, _, _, VTableEntryMethod(_, _)) => ZIO.unit
          case VTableEntry(_, _, _, _, VTableEntryAbstract) =>
            ZIO.fail(DiagnosticError.AbstractMethodNotImplemented(source))

          case VTableEntry(_, _, _, _, VTableEntryAmbiguous(_)) => ???
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
            case (key, VTableEntry(entryName, signature, slotInstanceType, entrySource, impl)) =>
              val subst = getSigParameterVars(bt.constructor.arTrait, btSig, Seq())
                .zip(bt.args)
                .toMap

              val newSig = substituteSignatureMany(subst)(functionSigHandler)(signature)
              val newInstanceType = substituteMethodCallOwnerType(subst)(slotInstanceType)
              key -> VTableEntry(entryName, newSig, newInstanceType, entrySource, impl)
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
                  case (key, VTableEntry(entryName, signature, slotInstanceType, entrySource, impl)) =>
                    val subst = getSigParameterVars(bc.constructor.arClass, bcSig, Seq())
                      .zip(bc.args)
                      .toMap

                    val substSig = substituteSignatureMany(subst)(functionSigHandler)(signature)
                    val newInstanceType = substituteMethodCallOwnerType(subst)(slotInstanceType)
                    key -> VTableEntry(
                      entryName,
                      substSig,
                      newInstanceType,
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

            instanceType = MethodCallOwnerType.OwnedByClass(ArExpr(
              ExprConstructor.ClassType(arClass),
              getSigParameterVars(arClass, sig, Seq()).map { variable =>
                WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple))
              }
            ))

            allBaseClasses <- findAllBaseClasses(baseClass)
            allBaseTraits <- findAllBaseTraits(allBaseClasses, baseTraits)
            source = EntrySourceClass(arClass, allBaseClasses, allBaseTraits)

            newVTable <- addNewMethods(methods, instanceType)(source)(baseTypeOnlyVTable)

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

            instanceType = MethodCallOwnerType.OwnedByTrait(ArExpr(
              ExprConstructor.TraitType(arTrait),
              getSigParameterVars(arTrait, sig, Seq()).map { variable =>
                WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple))
              }
            ))

            allBaseTraits <- findAllBaseTraits(Vector.empty, baseTraits)
            source = EntrySourceTrait(arTrait, allBaseTraits)

            newVTable <- addNewMethods(methods, instanceType)(source)(baseTraitOnlyVTable)

          } yield newVTable
        }

      private def diffVTable(current: VT, base: VT): VT =
        VTable(current.methodMap.filterNot {
          case (slot, entry) =>
            base.methodMap.get(slot).exists { baseEntry => entry.impl == baseEntry.impl }
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
