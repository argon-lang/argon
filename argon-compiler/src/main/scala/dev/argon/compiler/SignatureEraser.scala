package dev.argon.compiler

import dev.argon.expr.BuiltinType
import zio.*

sealed trait SignatureEraser extends UsingContext {
  import context.DefaultExprContext.*

  def eraseSignature(sig: FunctionSignature): Comp[ErasedSignature] =
    for
      params <- ZIO.foreach(sig.parameters)(p => getErasedType(p.paramType))
      result <- getErasedType(sig.returnType)
    yield ErasedSignature(
      params = params,
      result = result,
    )

  def getErasedType(t: Expr): Comp[ErasedSignatureType] =
    ArgonEvaluator(context).normalizeToValue(t, context.Config.evaluatorFuel).flatMap {
      case Expr.Builtin(Builtin.Nullary(t: BuiltinType)) =>
        ZIO.succeed(ErasedSignatureType.Builtin(t, Seq()))

      case Expr.Builtin(Builtin.Unary(t: BuiltinType, a)) =>
        for
          a <- getErasedType(a)
        yield ErasedSignatureType.Builtin(t, Seq(a))

      case Expr.Builtin(Builtin.Binary(t: BuiltinType, a, b)) =>
        for
          a <- getErasedType(a)
          b <- getErasedType(b)
        yield ErasedSignatureType.Builtin(t, Seq(a, b))

      case Expr.FunctionType(a, b) =>
        for
          a <- getErasedType(a.varType)
          b <- getErasedType(b)
        yield ErasedSignatureType.Function(a, b)

      case Expr.RecordType(record, args) =>
        for
          recordImport <- record.importSpecifier
          args <- ZIO.foreach(args)(getErasedType)
        yield ErasedSignatureType.Record(recordImport, args)

      case Expr.Tuple(items) =>
        for
          items <- ZIO.foreach(items)(getErasedType)
        yield ErasedSignatureType.Tuple(items)

      case _ => ZIO.succeed(ErasedSignatureType.Erased)
    }

}

object SignatureEraser {
  def apply(ctx: Context): SignatureEraser & HasContext[ctx.type] =
    new SignatureEraser {
      override val context: ctx.type = ctx
    }
      
}
