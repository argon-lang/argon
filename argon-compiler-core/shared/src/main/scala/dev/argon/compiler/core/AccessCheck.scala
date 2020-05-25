package dev.argon.compiler.core

import dev.argon.compiler.{Comp, Compilation}
import dev.argon.util.FileSpec
import cats._
import cats.implicits._
import zio.IO

object AccessCheck {

  def checkInstance[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (
    callerId: CallerId,
    fileSpec: FileSpec,
    methodBinding: MethodBinding[TContext, TPayloadSpec]
  ): Comp[Boolean] = methodBinding.accessModifier match {
    case global: AccessModifierGlobal => IO.succeed(checkGlobal(callerId, fileSpec, global))
    case AccessModifier.Protected => ???
    case AccessModifier.ProtectedInternal => ???
    case AccessModifier.Private => ???
  }

  def checkGlobal(callerId: CallerId, fileSpec: FileSpec, accessModifier: AccessModifierGlobal): Boolean = accessModifier match {
    case AccessModifier.Public => true
    case AccessModifier.Internal => ???
    case AccessModifier.PrivateInternal => ???
  }

}
