package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler.core.ContextComp
import com.mi3software.argon.compiler.loaders.ModuleLoader
import com.mi3software.argon.compiler.loaders.armodule.ArgonModuleLoader
import com.mi3software.argon.compiler.types.ArgonTypeSystem
import com.mi3software.argon.compiler.{Compilation, _}

final class JSContext[TComp[+_] : Compilation] extends ContextComp[TComp] {

  override type TTraitMetadata = JSMetadata.Trait
  override type TClassMetadata = JSMetadata.Class

  override type TFunctionImplementation = JSImpl.Function

  override def createExprFunctionImplementation(expr: typeSystem.ArExpr): JSImpl.Function =
    JSImpl.Function.ExpressionBody(expr)

  override type Comp[+T] = TComp[T]

  override val compCompilationInstance: Compilation[Comp] = implicitly

  override val moduleLoaders: Vector[ModuleLoader] = Vector(ArgonModuleLoader)


  object JSImpl {
    import typeSystem._

    sealed trait Function
    object Function {
      final case class ExpressionBody(expr: ArExpr) extends Function
    }
  }

}
