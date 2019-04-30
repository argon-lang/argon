package dev.argon.compiler.core

import dev.argon.compiler.Compilation
import dev.argon.util.FileSpec

import scalaz._
import Scalaz._

object AccessCheck {

  def checkInstance[TComp[+_] : Compilation, TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (
    callerDescriptor: Descriptor,
    fileSpec: FileSpec,
    methodBinding: MethodBinding[TContext, TPayloadSpec]
  ): TComp[Boolean] = methodBinding.accessModifier match {
    case global: AccessModifierGlobal => checkGlobal(callerDescriptor, fileSpec, global).point[TComp]
    case AccessModifier.Protected => ???
    case AccessModifier.ProtectedInternal => ???
    case AccessModifier.Private =>
      (callerDescriptor === methodBinding.method.descriptor).point[TComp]
  }

  def checkGlobal(callerDescriptor: Descriptor, fileSpec: FileSpec, accessModifier: AccessModifierGlobal): Boolean = accessModifier match {
    case AccessModifier.Public => true
    case AccessModifier.Internal => ???
    case AccessModifier.PrivateInternal => ???
  }

}
