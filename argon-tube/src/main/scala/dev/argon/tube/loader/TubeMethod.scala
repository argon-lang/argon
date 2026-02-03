package dev.argon.tube.loader

import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.expr.ErasureMode
import dev.argon.tube as t
import dev.argon.tube.EffectInfo
import dev.argon.util.*
import zio.*

private[loader] object TubeMethod {
  def apply(ctx: TubeLoader.TubeLoadContext, elemLoader: ElementLoader & HasContext[ctx.type], method: t.MethodDefinition, methodOwner: MethodOwner[ctx.type]): ctx.Comp[ArMethodC & HasContext[ctx.type]] =
    for
      methodId <- UniqueIdentifier.make

      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.MethodImplementation]

    yield new ArMethodC with LoaderUtils {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = methodId

      override val name: IdentifierExpr = decodeIdentifier(method.name)

      override val owner: MethodOwner[ctx.type] = methodOwner

      override def isInline: Boolean = method.inline

      override def erasureMode: ErasureMode.DeclaredNonToken = LoaderUtils.decodeErasure(method.erased)

      override def isWitness: Boolean = method.witness

      override def slot: MethodSlot = method.slot match {
        case t.MethodSlot.Abstract() => MethodSlot.Abstract
        case t.MethodSlot.AbstractOverride() => MethodSlot.AbstractOverride
        case t.MethodSlot.Virtual() => MethodSlot.Virtual
        case t.MethodSlot.Override() => MethodSlot.Override
        case t.MethodSlot.Final() => MethodSlot.Final
        case t.MethodSlot.FinalOverride() => MethodSlot.FinalOverride
      }

      override def effects: context.DefaultExprContext.EffectInfo =
        method.effects match {
          case EffectInfo.Pure() => context.DefaultExprContext.EffectInfo.Pure
          case EffectInfo.Effectful() => context.DefaultExprContext.EffectInfo.Effectful
        }

      override def instanceParam: Comp[context.DefaultSignatureContext.InstanceParameter] =
        ZIO.succeed(context.DefaultSignatureContext.InstanceParameter(
          name = method.instanceParameter.name.map(decodeIdentifier),
        ))

      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(method.signature))

      override def implementation: Option[Comp[context.implementations.MethodImplementation]] =
        method.implementation.map { impl =>
          implCell.get(impl match {
            case t.MethodImplementation.Abstract() =>
              ZIO.succeed(context.implementations.MethodImplementation.Abstract)

            case t.MethodImplementation.Expr(body) =>
              for
                body <- decodeExpr(body)
              yield context.implementations.MethodImplementation.Expr(body)

            case t.MethodImplementation.Extern(externMap) =>
              ZIO.succeed(context.implementations.MethodImplementation.Extern(externMap))
          })
        }


      override def toString: String =
        s"$owner.$name"
    }
} 

