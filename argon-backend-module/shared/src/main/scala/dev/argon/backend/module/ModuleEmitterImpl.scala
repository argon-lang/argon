package dev.argon.backend.module

import dev.argon.armodule.emitter.ModuleEmitter
import dev.argon.compiler.core.Context
import dev.argon.module
import zio.ZIO
import cats.implicits._
import zio.interop.catz.core._

abstract class ModuleEmitterImpl extends ModuleEmitter {

  override val context: ModuleContext

  override def convertFunctionBody(body: context.TFunctionImplementation): Emit[module.FunctionBody] =
    convertExpr(body).map { expr => module.FunctionBody(module.FunctionBody.BodyType.ExpressionBody(expr)) }

  override def convertMethodBody(body: context.TMethodImplementation): Emit[Option[module.FunctionBody]] =
    ZIO.foreach(body) { body2 =>
      convertExpr(body2).map { expr => module.FunctionBody(module.FunctionBody.BodyType.ExpressionBody(expr)) }
    }

  override def convertClassConstructorBody(body: context.TClassConstructorImplementation): Emit[module.ClassConstructorBody] =
    convertClassConstructorExpressionBody(body)
}
