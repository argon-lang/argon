package dev.argon.compiler.core

trait ArFunc[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._, signatureContext.Signature

  val descriptor: FuncDescriptor
  val effectInfo: EffectInfo

  val signature: Comp[Signature[FunctionResultInfo]]

  val payload: TPayloadSpec[Comp[TFunctionImplementation], TFunctionMetadata]
}

object ArFunc {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    ArFunc[TContext, TPayloadSpec] { val descriptor: FuncDescriptor.InNamespace }

}
