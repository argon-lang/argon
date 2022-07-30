package dev.argon.compiler.signature

import dev.argon.compiler.*
import dev.argon.compiler.expr.CompleteExprContext
import zio.*

trait SignatureEraser extends UsingContext {
  import context.ExprContext.{WrapExpr, ExprConstructor, ArExpr}

  private def getErasedSigType(t: WrapExpr): Comp[ErasedSignatureType] =
    t match
      case WrapExpr.OfExpr(expr) => getErasedSigTypeArType(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def getErasedArgs(sig: Signature[WrapExpr, ?], args: Seq[WrapExpr], prev: Seq[ErasedSignatureType]): Comp[Seq[ErasedSignatureType]] =
    (sig, args) match
      case (Signature.Parameter(_, true, _, next), _ +: tailArgs) =>
        getErasedArgs(next, tailArgs, prev :+ ErasedSignatureType.Erased)

      case (Signature.Parameter(_, false, t, next), arg +: tailArgs) =>
        getErasedSigType(t).flatMap { erasedT =>
          getErasedArgs(next, tailArgs, prev :+ erasedT)
        }

      case (_, _) => ZIO.succeed(prev)
    end match

  private def getErasedSigTypeArType(t: ArExpr[ExprConstructor]): Comp[ErasedSignatureType] =
    (t.constructor: t.constructor.type & ExprConstructor) match {
      case ctor: (t.constructor.type & ExprConstructor.ClassType) =>
        for
          classSig <- ctor.arClass.signature
          classSigErased <- erasedNoResult(classSig)

          specifier = ImportSpecifier(
            ctor.arClass.owner.module.tube.tubeName,
            ctor.arClass.owner.module.moduleName.path,
            ctor.arClass.owner.ownedName,
            classSigErased
          )

          args <- getErasedArgs(classSig, t.getArgs(ctor), Seq())

        yield ErasedSignatureType.Class(specifier, args)

      case ctor: (t.constructor.type & ExprConstructor.TraitType) =>
        for
          traitSig <- ctor.arTrait.signature
          traitSigErased <- erasedNoResult(traitSig)

          specifier = ImportSpecifier(
            ctor.arTrait.owner.module.tube.tubeName,
            ctor.arTrait.owner.module.moduleName.path,
            ctor.arTrait.owner.ownedName,
            traitSigErased
          )

          args <- getErasedArgs(traitSig, t.getArgs(ctor), Seq())

        yield ErasedSignatureType.Trait(specifier, args)

      case ctor: (t.constructor.type & ExprConstructor.FunctionType.type) =>
        val (arg, res) = t.getArgs(ctor)
        for
          arg2 <- getErasedSigType(arg)
          res2 <- getErasedSigType(res)
        yield ErasedSignatureType.Function(arg2, res2)

      case ctor: (t.constructor.type & ExprConstructor.LoadTuple.type) =>
        for
          args <- ZIO.foreach(t.getArgs(ctor))(getErasedSigType)
        yield ErasedSignatureType.Tuple(args)

      case _ => ZIO.succeed(ErasedSignatureType.Erased)
    }

  def erasedNoResult(sig: Signature[WrapExpr, ?]): Comp[ErasedSignature] =
    erasedImpl(Seq.empty, sig) { (params, _) =>
      ZIO.succeed(ErasedSignatureNoResult(params))
    }

  def erasedWithResult(sig: Signature[WrapExpr, WrapExpr]): Comp[ErasedSignature] =
    erasedImpl(Seq.empty, sig) { (params, res) =>
      for
        erased <- getErasedSigType(res)
      yield ErasedSignatureWithResult(params, erased)
    }

  private def erasedImpl[TRes](prev: Seq[ErasedSignatureType], sig: Signature[WrapExpr, TRes])(f: (Seq[ErasedSignatureType], TRes) => Comp[ErasedSignature]): Comp[ErasedSignature] =
    sig match
      case Signature.Parameter(_, _, paramType, next) =>
        getErasedSigType(paramType).flatMap { erased =>
          erasedImpl(prev :+ erased, next)(f)
        }

      case Signature.Result(res) =>
        f(prev, res)
    end match

}

object SignatureEraser {
  def apply(ctx: Context): SignatureEraser with HasContext[ctx.type] =
    new SignatureEraser:
      override val context: ctx.type = ctx
    end new
}
