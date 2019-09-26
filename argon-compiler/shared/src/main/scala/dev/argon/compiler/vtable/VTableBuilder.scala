package dev.argon.compiler.vtable

import dev.argon.compiler._
import dev.argon.compiler.core._
import cats._
import cats.implicits._
import shapeless.Nat

sealed abstract class VTableBuilder {

  val vtableContext: VTableContext
  import vtableContext._

  def fromClass[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): context.Comp[VTable]
  def fromTrait[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): context.Comp[VTable]
  def fromDataConstructor[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): context.Comp[VTable]
}

object VTableBuilder {

  type Aux[TContext <: Context with Singleton] = VTableBuilder {
    val vtableContext: VTableContext.Aux[TContext]
  }

  def apply(context: Context): context.Comp[VTableBuilder.Aux[context.type]] = {
    import context._
    val ctx: context.type = context

    val vtableCtx: VTableContext.Aux[ctx.type] = new VTableContext {
      override val context: ctx.type = ctx
    }

    import vtableCtx.{ context => _, _ }

    for {
      classVtableCache <- Compilation[Comp].createMemo[AbsRef[context.type, ArClass], VTable]
      traitVtableCache <- Compilation[Comp].createMemo[AbsRef[context.type, ArTrait], VTable]
      dataCtorVtableCache <- Compilation[Comp].createMemo[AbsRef[context.type, DataConstructor], VTable]
    } yield new VTableBuilder {

      override val vtableContext: vtableCtx.type = vtableCtx

      type VT = VTable
      type Entry = VTableEntry

      import context.typeSystem.{ context => _, _ }

      private def signatureMatches[PS[_, _], PSSlot[_, _]](method: ArMethod[context.type, PS])(slotMethod: ArMethod[context.type, PSSlot], slotSig: EntrySignature): Comp[Boolean] = {

        import context.signatureContext.{ context => _, _ }

        def impl(sig: Signature[FunctionResultInfo, _ <: Nat], slotSig: Signature[FunctionResultInfo, _ <: Nat]): Comp[Boolean] =
          (sig, slotSig) match {
            case (aParam @ SignatureParameters(_, _), bParam @ SignatureParameters(_, _)) =>
              isSubType(aParam.parameter.paramType, bParam.parameter.paramType).map { _.isDefined }
                .flatMap {
                  case true => isSubType(bParam.parameter.paramType, aParam.parameter.paramType).map { _.isDefined }
                  case false => false.pure[Comp]
                }
                .flatMap {
                  case true => impl(aParam.nextUnsubstituted, bParam.next(LoadVariable(aParam.parameter.paramVar)))
                  case false => false.pure[Comp]
                }

            case (SignatureResult(aResult), SignatureResult(bResult)) =>
              isSubType(aResult.returnType, bResult.returnType).map { _.isDefined }

            case _ => false.pure[Comp]
          }

        method.signatureUnsubstituted.flatMap { sig =>
          impl(sig, slotSig)
        }
      }

      private def overrideMethod[TPayloadSpec[_, _]](method: MethodBinding[context.type, TPayloadSpec])(source: EntrySource)(baseTypeVTable: VT): Comp[VT] = {

        val newEntryImpl =
          if(method.method.isAbstract)
            VTableEntryAbstract
          else
            VTableEntryMethod(AbsRef(method.method))

        for {
          sig <- method.method.signatureUnsubstituted

          VTable(methodMap) <-
            if(method.method.isImplicitOverride)
              baseTypeVTable.methodMap
                .toSeq
                .filter { case (slotMethod, _) =>
                  slotMethod.value.isVirtual &&
                    !slotMethod.value.isFinal &&
                    slotMethod.value.descriptor.name === method.name &&
                    slotMethod.value.descriptor.name =!= MemberName.Unnamed
                }
                .toVector
                .filterA { case (slotMethod, VTableEntry(slotSig, _, _)) => signatureMatches(method.method)(slotMethod.value, slotSig) }
                .map { slotMethods =>
                  VTable(
                    methodMap = slotMethods
                      .map {
                        case (slotMethod, VTableEntry(slotSig, _, _)) =>
                          slotMethod -> VTableEntry(slotSig, source, newEntryImpl)
                      }
                      .toMap[AbsRef[context.type, ArMethod], VTableEntry]
                  )
                }
            else
              VTable(
                methodMap = Map.empty
              ).pure[Comp]


        } yield VTable(
          methodMap = methodMap + (AbsRef(method.method) -> VTableEntry(sig, source, newEntryImpl))
        )
      }

      private def addNewMethods[TPayloadSpec[_, _]](methods: Vector[MethodBinding[context.type, TPayloadSpec]])(source: EntrySource)(baseTypeVTable: VT): Comp[VT] =
        methods
          .traverse { method => overrideMethod(method)(source)(baseTypeVTable) }
          .map { baseTypeVTable |+| _.combineAll }

      private def findAllBaseClasses(baseClass: Option[ClassType]): Comp[Vector[AbsRef[context.type, ArClass]]] = {

        def addClass(acc: Vector[AbsRef[context.type, ArClass]], baseClass: ClassType): Comp[Vector[AbsRef[context.type, ArClass]]] =
          if(acc.exists { _.value.descriptor === baseClass.arClass.value.descriptor })
            acc.pure[Comp]
          else
            for {
              sig <- baseClass.arClass.value.signature
              classes <- sig.unsubstitutedResult.baseTypes.baseClass.foldLeftM(acc :+ baseClass.arClass)(addClass(_, _))
            } yield classes

        baseClass.foldLeftM(Vector.empty[AbsRef[context.type, ArClass]])(addClass(_, _))
      }

      private def findAllBaseTraits(baseClasses: Vector[AbsRef[context.type, ArClass]], baseTraits: Vector[TraitType]): Comp[Vector[AbsRef[context.type, ArTrait]]] = {

        def addTrait(acc: Vector[AbsRef[context.type, ArTrait]], baseTrait: TraitType): Comp[Vector[AbsRef[context.type, ArTrait]]] =
          if(acc.exists { _.value.descriptor === baseTrait.arTrait.value.descriptor })
            acc.pure[Comp]
          else
            for {
              sig <- baseTrait.arTrait.value.signature
              traits <- sig.unsubstitutedResult.baseTypes.baseTraits.foldLeftM(acc :+ baseTrait.arTrait)(addTrait(_, _))
            } yield traits

        def addTraitsFromClass(acc: Vector[AbsRef[context.type, ArTrait]], baseClass: AbsRef[context.type, ArClass]): Comp[Vector[AbsRef[context.type, ArTrait]]] =
          for {
            sig <- baseClass.value.signature
            traits <- sig.unsubstitutedResult.baseTypes.baseTraits.foldLeftM(acc)(addTrait(_, _))
          } yield traits

        val acc = Vector.empty[AbsRef[context.type, ArTrait]]
        for {
          acc <- baseTraits.foldLeftM(acc)(addTrait(_, _))
          acc <- baseClasses.foldLeftM(acc)(addTraitsFromClass(_, _))
        } yield acc
      }

      private def ensureNonAbstract(vtable: VT, source: CompilationMessageSource): Comp[Unit] =
        vtable.methodMap.values.toVector.traverse_ {
          case VTableEntry(_, _, VTableEntryMethod(_)) => ().pure[Comp]
          case VTableEntry(_, _, VTableEntryAbstract) =>
            Compilation[Comp].forErrors(CompilationError.AbstractMethodNotImplementedError(source))

          case VTableEntry(_, _, VTableEntryAmbiguous(_)) => ???
        }

      private def getBaseTraitVTable(bt: TraitType): Comp[VT] =
        for {
          btSig <- bt.arTrait.value.signature
          baseTraitVTable <- fromTrait(bt.arTrait.value)
        } yield VTable(
          baseTraitVTable.methodMap.view.mapValues {
            case VTableEntry(signature, entrySource, impl) =>
              val newSig = signature.substituteTypeArguments(btSig.unsubstitutedParameters)(bt.args)
              VTableEntry(newSig, entrySource, impl)
          }.toMap
        )

      override def fromClass[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): Comp[VT] =
        classVtableCache { arClassWrap =>
          val arClass = arClassWrap.value

          for {
            sig <- arClass.signature
            baseClass = sig.unsubstitutedResult.baseTypes.baseClass
            baseTraits = sig.unsubstitutedResult.baseTypes.baseTraits
            baseClassVTable <- baseClass.traverse[Comp, VT] { bc =>
              for {
                bcSig <- bc.arClass.value.signature
                baseClassVTable <- fromClass(bc.arClass.value)
              } yield VTable(
                baseClassVTable.methodMap.view.mapValues {
                  case VTableEntry(signature, entrySource, impl) =>
                    VTableEntry(
                      signature.substituteTypeArguments(bcSig.unsubstitutedParameters)(bc.args),
                      entrySource,
                      impl
                    )
                }.toMap
              )
            }
            baseTraitVTables <- baseTraits.traverse(getBaseTraitVTable(_))

            baseTypeOnlyVTable = baseClassVTable.combineAll |+| baseTraitVTables.combineAll

            methods <- arClass.methods
            allBaseClasses <- findAllBaseClasses(baseClass)
            allBaseTraits <- findAllBaseTraits(allBaseClasses, baseTraits)
            source = EntrySourceClass(AbsRef(arClass), allBaseClasses, allBaseTraits)
            newVTable <- addNewMethods(methods)(source)(baseTypeOnlyVTable)

            _ <- if(!arClass.isAbstract) ensureNonAbstract(newVTable, arClass.classMessageSource) else ().pure[Comp]

          } yield newVTable
        }(AbsRef(arClass))

      override def fromTrait[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): Comp[VT] =
        traitVtableCache { arTraitWrap =>
          val arTrait = arTraitWrap.value

          for {
            sig <- arTrait.signature
            baseTraits = sig.unsubstitutedResult.baseTypes.baseTraits
            baseTraitVTables <- baseTraits.traverse(getBaseTraitVTable(_))

            baseTraitOnlyVTable = baseTraitVTables.combineAll

            methods <- arTrait.methods
            allBaseTraits <- findAllBaseTraits(Vector.empty, baseTraits)
            source = EntrySourceTrait(AbsRef(arTrait), allBaseTraits)
            newVTable <- addNewMethods(methods)(source)(baseTraitOnlyVTable)

          } yield newVTable
        }(AbsRef(arTrait))

      override def fromDataConstructor[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Comp[VT] =
        dataCtorVtableCache { dataCtorWrap =>
          val ctor = dataCtorWrap.value

          for {
            sig <- ctor.signature
            baseTrait = sig.unsubstitutedResult.instanceType
            baseTraitVTable <- getBaseTraitVTable(baseTrait)

            methods <- ctor.methods
            allBaseTraits <- findAllBaseTraits(Vector.empty, Vector(baseTrait))
            source = EntrySourceDataCtor(AbsRef(ctor), allBaseTraits)
            newVTable <- addNewMethods(methods)(source)(baseTraitVTable)

            _ <- ensureNonAbstract(newVTable, ctor.ctorMessageSource)

          } yield newVTable
        }(AbsRef(ctor))
    }
  }

}
