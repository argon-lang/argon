package dev.argon.compiler

import dev.argon.expr.Unification
import zio.*
import zio.stream.ZStream

trait VTableBuilder extends UsingContext {

  import context.DefaultExprContext.{Expr, ExpressionOwner, MethodInstanceType}


  final class VTable(
    val entries: Map[VTableSlot, VTableSlotValue]
  )

  final case class VTableSlot(
    method: ArMethod,
    sig: ErasedSignatureType,
  )

  final case class VTableSlotValue(
    slotInstanceType: MethodInstanceType,
    signature: FunctionSignature,
    target: VTableTarget,
  )

  enum VTableTarget derives CanEqual {
    case Abstract
    case Implementation(method: ArMethod)
    case Ambiguous(implementations: Set[ArMethod])
  }

  private final case class VTableWithParent(
    parent: Option[VTable],
    current: VTable,
  )

  private def getVTable(t: MethodOwner[context.type]): Comp[VTableWithParent] =
    t match {
      case MethodOwner.ByTrait(t) =>
        for
          methods <- t.methods
          traitSig <- t.signature
          owner = ExpressionOwner.Trait(t)
          
          traitType: MethodInstanceType = Expr.TraitType(
            t,
            traitSig.parameters
              .zipWithIndex
              .map { case (param, i) =>
                Expr.Variable(param.asParameterVar(owner, i))
              },
          )
          
          vtable <- buildSingleVTable(
            VTable(Map.empty),
            methods,
            traitType,
          )
        yield VTableWithParent(None, vtable)

      case MethodOwner.ByInstance(i) =>
        for
          sig <- i.signature
          traitType <- sig.returnType match {
            case traitType: Expr.TraitType =>
              ZIO.succeed(traitType)

            case _ => ???
          }

          VTableWithParent(_, parentVTable) <- getVTable(MethodOwner.ByTrait(traitType.t))
          traitSig <- traitType.t.signature
          parentVTableSubst = substituteVTable(parentVTable, ExpressionOwner.Trait(traitType.t), traitSig, traitType.args)

          methods <- i.methods

          vtable <- buildSingleVTable(
            parentVTableSubst,
            methods,
            traitType,
          )

          _ <- checkConcreteVTable(vtable)

        yield VTableWithParent(Some(parentVTable), vtable)
    }

  private def substituteVTable(vtable: VTable, owner: ExpressionOwner, sig: FunctionSignature, args: Seq[Expr]): VTable =
    VTable(
      vtable.entries
        .map { (slot, value) =>
          // TODO: Fix slot and slot instance type
          
          
          slot -> value.copy(
            signature =
              sig.parameters.zip(args).zipWithIndex.foldLeft(value.signature) { case (sig, ((param, arg), i)) =>
                sig.substituteVar(param.asParameterVar(owner, i), arg)
              }
          )
        }
    )

  private def buildSingleVTable(parent: VTable, methods: Seq[ArMethod], instanceType: MethodInstanceType): Comp[VTable] =
    ZIO.foreach(methods)(buildOverride(parent, instanceType))
      .flatMap { vtables =>
        val vtable = mergeSiblingVTablesSameType(vtables)

        if vtable.entries.exists { case (_, value) => value.target == VTableTarget.Abstract } then
          ???
        else
          ZIO.succeed(VTable(parent.entries ++ vtable.entries))
      }

  private def mergeSiblingVTablesSameType(vtables: Seq[VTable]): VTable =
    VTable(
      vtables
        .flatMap { _.entries }
        .groupBy { (k, _) => k }
        .view
        .mapValues { entries =>
          val firstEntry = entries.head._2
          
          if entries.size > 1 then
            val methods = entries
              .view
              .flatMap { (_, value) =>
                value.target match {
                  case VTableTarget.Abstract => ???
                  case VTableTarget.Implementation(method) => Seq(method)
                  case VTableTarget.Ambiguous(methods) => methods
                }
              }
              .toSet

            firstEntry.copy(target = VTableTarget.Ambiguous(methods))
          else
            firstEntry
        }
        .toMap
    )

  private def buildOverride(parent: VTable, instanceType: MethodInstanceType)(method: ArMethod): Comp[VTable] =
    val target = method.slot match {
      case MethodSlot.Abstract | MethodSlot.AbstractOverride =>
        VTableTarget.Abstract

      case MethodSlot.Virtual | MethodSlot.Override |
           MethodSlot.Final | MethodSlot.FinalOverride =>
        VTableTarget.Implementation(method)
    }
    
    method.slot match {
      case MethodSlot.Virtual | MethodSlot.Final | MethodSlot.Abstract =>
        for
          sig <- method.signature
          instanceTypeErasedSig <- SignatureEraser(context).getErasedType(instanceType)
        yield VTable(Map(
          VTableSlot(method, instanceTypeErasedSig) -> VTableSlotValue(
            slotInstanceType = instanceType,
            signature = sig,
            target = target,
          )
        ))

      case MethodSlot.Override | MethodSlot.FinalOverride | MethodSlot.AbstractOverride =>
        for
          sig <- method.signature
          overridden <- findOverriddenSlots(parent, method, sig)
          _ <- ZIO.succeed(???).when(overridden.entries.isEmpty)
          instanceTypeErasedSig <- SignatureEraser(context).getErasedType(instanceType)
        yield VTable(
          overridden
            .entries
            .map { (slot, value) =>
              slot -> value.copy(target = target)
            } ++
            Map(
              VTableSlot(method, instanceTypeErasedSig) -> VTableSlotValue(
                slotInstanceType = instanceType,
                signature = sig,
                target = target,
              )
            )
        )
    }
  end buildOverride

  private def findOverriddenSlots(parent: VTable, method: ArMethod, sig: FunctionSignature): Comp[VTable] =
    ZStream.fromIterable(parent.entries)
      .filter { (slot, _) => slot.method.name == method.name }
      .filterZIO { (slot, value) =>
        signatureMatches(ExpressionOwner.Method(slot.method), value.signature, ExpressionOwner.Method(method), sig, 0)
      }
      .runCollect
      .map { entries => VTable(entries.toMap) }

  private def signatureMatches(parentOwner: ExpressionOwner, parent: FunctionSignature, childOwner: ExpressionOwner, child: FunctionSignature, index: Int): Comp[Boolean] =
    (parent.parameters, child.parameters) match {
      case (parentParam +: parentTailParams, childParam +: childTailParams) =>
        typeMatches(parentParam.paramType, childParam.paramType) &&
          signatureMatches(
            parentOwner,
            FunctionSignature(parentTailParams, parent.returnType, parent.ensuresClauses)
              .substituteVar(
                parentParam.asParameterVar(parentOwner, index),
                Expr.Variable(childParam.asParameterVar(childOwner, index)),
              ),
            childOwner,
            FunctionSignature(childTailParams, child.returnType, child.ensuresClauses),
            index + 1,
          )

      case (Seq(), Seq()) =>
        typeMatches(parent.returnType, child.returnType)

      case _ => ZIO.succeed(false)
    }

  private def typeMatches(a: Expr, b: Expr): Comp[Boolean] =
    Ref.make[context.DefaultExprContext.Model](context.DefaultExprContext.Model.empty)
      .flatMap { m =>
        val eval = ArgonEvaluator(context)
        Unification.unify(context.DefaultExprContext)(m, eval, context.Config.evaluatorFuel)(a, b)
      }

  private def vtableDiff(parent: VTable, current: VTable): VTable =
    VTable(
      current.entries.view
        .filter { (slot, value) =>
          parent.entries.get(slot)
            .exists { parentValue =>
              (value.target, parentValue.target) match {
                case (VTableTarget.Abstract, VTableTarget.Abstract) => false
                case (VTableTarget.Implementation(method), VTableTarget.Implementation(method2)) =>
                  method != method2
                case _ => true
              }
            }
        }
        .toMap
    )

  private def checkConcreteVTable(vtable: VTable): Comp[Unit] =
    ZIO.succeed {
      vtable.entries.values.foreach { value =>
        value.target match {
          case VTableTarget.Abstract => ???
          case VTableTarget.Ambiguous(_) => ???
          case VTableTarget.Implementation(_) =>
        }
      }
    }

  def buildVTable(t: MethodOwner[context.type]): Comp[VTable] =
    getVTable(t).map { vtwp =>
      vtwp.parent match {
        case Some(parent) => vtableDiff(parent, vtwp.current)
        case None => VTable(Map())
      }
    }

}

object VTableBuilder {
  def apply(ctx: Context): VTableBuilder & HasContext[ctx.type] =
    new VTableBuilder {
      override val context: ctx.type = ctx
    }
}
