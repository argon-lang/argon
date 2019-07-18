package dev.argon.compiler.vtable

import dev.argon.compiler._
import dev.argon.compiler.core._
import cats._
import cats.implicits._

sealed abstract class VTableBuilder[TContext <: Context with Singleton] {
  def fromClass[TPayloadSpec[_, _]](arClass: ArClass[TContext, TPayloadSpec]): TContext#Comp[VTable[TContext]]
  def fromTrait[TPayloadSpec[_, _]](arTrait: ArTrait[TContext, TPayloadSpec]): TContext#Comp[VTable[TContext]]
  def fromDataConstructor[TPayloadSpec[_, _]](ctor: DataConstructor[TContext, TPayloadSpec]): TContext#Comp[VTable[TContext]]
}

object VTableBuilder {

  def apply(context: Context): context.Comp[VTableBuilder[context.type]] = {
    import context._
    for {
      classVtableCache <- Compilation[Comp].createMemo[AbsRef[context.type, ArClass], VTable[context.type]]
      traitVtableCache <- Compilation[Comp].createMemo[AbsRef[context.type, ArTrait], VTable[context.type]]
      dataCtorVtableCache <- Compilation[Comp].createMemo[AbsRef[context.type, DataConstructor], VTable[context.type]]
    } yield new VTableBuilder[context.type] {

      type VT = VTable[context.type]
      type Entry = VTableEntry[context.type]

      import context.typeSystem.{ context => _, _ }

      private def signatureMatches[PS[_, _], PSSlot[_, _]](method: ArMethod[context.type, PS])(slotMethod: ArMethod[context.type, PSSlot]): Comp[Boolean] = {

        import context.signatureContext.{ context => _, _ }

        def impl(sig: Signature[FunctionResultInfo], slotSig: Signature[FunctionResultInfo]): Comp[Boolean] =
          sig.visit(
            aParam => slotSig.visit(
              bParam => isSubType[Comp](aParam.parameter.paramType, bParam.parameter.paramType).map { _.isDefined }
                .flatMap {
                  case true => isSubType[Comp](bParam.parameter.paramType, aParam.parameter.paramType).map { _.isDefined }
                  case false => false.pure[Comp]
                }
                .flatMap {
                  case true => impl(aParam.nextUnsubstituted, bParam.nextUnsubstituted)
                  case false => false.pure[Comp]
                },
              _ => false.pure[Comp]
            ),
            aResult => slotSig.visit(
              _ => false.pure[Comp],
              bResult => isSubType[Comp](aResult.result.returnType, bResult.result.returnType).map { _.isDefined }
            )
          )

        method.signature.flatMap { sig =>
          slotMethod.signature.flatMap { slotSig =>
            impl(sig, slotSig)
          }
        }
      }

      private def overrideMethod[TPayloadSpec[_, _]](method: MethodBinding[context.type, TPayloadSpec])(source: EntrySource[context.type])(baseTypeVTable: VT): Comp[VT] = {

        val newEntry =
          if(method.method.isAbstract)
            VTableEntryAbstract(source)
          else
            VTableEntryMethod(AbsRef(method.method), source)

        (
          if(method.method.isImplicitOverride)
            baseTypeVTable.methodMap
              .keys
              .filter { slotMethod =>
                slotMethod.value.isVirtual &&
                  !slotMethod.value.isFinal &&
                  slotMethod.value.descriptor.name === method.name &&
                  slotMethod.value.descriptor.name =!= MemberName.Unnamed
              }
              .toVector
              .filterA { slotMethod => signatureMatches(method.method)(slotMethod.value) }
              .map { slotMethods =>
                VTable(
                  methodMap = slotMethods.map { slotMethod => slotMethod -> newEntry }.toMap[AbsRef[context.type, ArMethod], VTableEntry[context.type]]
                )
              }
          else
            VTable[context.type](
              methodMap = Map.empty
            ).pure[Comp]
          ).map { case VTable(methodMap) =>
          VTable(
            methodMap = methodMap + (AbsRef(method.method) -> newEntry)
          )
        }
      }

      private def addNewMethods[TPayloadSpec[_, _]](methods: Vector[MethodBinding[context.type, TPayloadSpec]])(source: EntrySource[context.type])(baseTypeVTable: VT): Comp[VT] =
        methods
          .traverse { method => overrideMethod(method)(source)(baseTypeVTable) }
          .map { baseTypeVTable |+| _.combineAll }

      private def findAllBaseClasses(baseClass: Option[ClassType]): Vector[AbsRef[context.type, ArClass]] = {

        def addClass(acc: Vector[AbsRef[context.type, ArClass]], baseClass: ClassType): Vector[AbsRef[context.type, ArClass]] =
          if(acc.exists { _.value.descriptor === baseClass.arClass.value.descriptor })
            acc
          else
            baseClass.baseTypes.baseClass.foldLeft(acc :+ baseClass.arClass)(addClass(_, _))

        baseClass.foldLeft(Vector.empty[AbsRef[context.type, ArClass]])(addClass(_, _))
      }

      private def findAllBaseTraits(baseTraits: Vector[TraitType]): Vector[AbsRef[context.type, ArTrait]] = {

        def addTrait(acc: Vector[AbsRef[context.type, ArTrait]], baseTrait: TraitType): Vector[AbsRef[context.type, ArTrait]] =
          if(acc.exists { _.value.descriptor === baseTrait.arTrait.value.descriptor })
            acc
          else
            baseTrait.baseTypes.baseTraits.foldLeft(acc :+ baseTrait.arTrait)(addTrait(_, _))

        baseTraits.foldLeft(Vector.empty[AbsRef[context.type, ArTrait]])(addTrait(_, _))
      }

      private def ensureNonAbstract(vtable: VT, source: CompilationMessageSource): Comp[Unit] =
        vtable.methodMap.values.toVector.traverse_ {
          case VTableEntryMethod(_, _) => ().pure[Comp]
          case VTableEntryAbstract(_) =>
            Compilation[Comp].forErrors(CompilationError.AbstractMethodNotImplementedError(source))

          case VTableEntryAmbiguous(_, _) => ???
        }

      override def fromClass[TPayloadSpec[_, _]](arClass: ArClass[context.type, TPayloadSpec]): Comp[VT] =
        classVtableCache { arClassWrap =>
          val arClass = arClassWrap.value

          for {
            sig <- arClass.signature
            baseClass = sig.unsubstitutedResult.baseTypes.baseClass
            baseTraits = sig.unsubstitutedResult.baseTypes.baseTraits
            baseClassVTable <- baseClass.traverse(bc => fromClass(bc.arClass.value))
            baseTraitVTables <- baseTraits.traverse(bt => fromTrait(bt.arTrait.value))

            baseTypeOnlyVTable = baseClassVTable.combineAll |+| baseTraitVTables.combineAll

            methods <- arClass.methods
            source = EntrySourceClass(AbsRef(arClass), findAllBaseClasses(baseClass), findAllBaseTraits(baseTraits))
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
            baseTraitVTables <- baseTraits.traverse(bt => fromTrait(bt.arTrait.value))

            baseTraitOnlyVTable = baseTraitVTables.combineAll

            methods <- arTrait.methods
            source = EntrySourceTrait(AbsRef(arTrait), findAllBaseTraits(baseTraits))
            newVTable <- addNewMethods(methods)(source)(baseTraitOnlyVTable)

          } yield newVTable
        }(AbsRef(arTrait))

      override def fromDataConstructor[TPayloadSpec[_, _]](ctor: DataConstructor[context.type, TPayloadSpec]): Comp[VT] =
        dataCtorVtableCache { dataCtorWrap =>
          val ctor = dataCtorWrap.value

          for {
            sig <- ctor.signature
            baseTrait = sig.unsubstitutedResult.instanceType
            baseTraitVTable <- fromTrait(baseTrait.arTrait.value)

            methods <- ctor.methods
            source = EntrySourceDataCtor(AbsRef(ctor), findAllBaseTraits(Vector(baseTrait)))
            newVTable <- addNewMethods(methods)(source)(baseTraitVTable)

            _ <- ensureNonAbstract(newVTable, ctor.ctorMessageSource)

          } yield newVTable
        }(AbsRef(ctor))
    }
  }

}
