package dev.argon.compiler.core

import dev.argon.compiler.{Comp, Compilation}
import dev.argon.util.FileSpec
import cats._
import cats.implicits._
import zio.IO

object AccessCheck {

  def checkInstance[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (
    callerDescriptor: Descriptor,
    fileSpec: FileSpec,
    methodBinding: MethodBinding[TContext, TPayloadSpec]
  ): Comp[Boolean] = methodBinding.accessModifier match {
    case global: AccessModifierGlobal => IO.succeed(checkGlobal(callerDescriptor, fileSpec, global))
    case AccessModifier.Protected => ???
    case AccessModifier.ProtectedInternal => ???
    case AccessModifier.Private =>
      IO.succeed(callerDescriptor === methodBinding.method.descriptor)
  }

  def checkGlobal(callerDescriptor: Descriptor, fileSpec: FileSpec, accessModifier: AccessModifierGlobal): Boolean = accessModifier match {
    case AccessModifier.Public => true
    case AccessModifier.Internal => ???
    case AccessModifier.PrivateInternal => ???
  }

}
