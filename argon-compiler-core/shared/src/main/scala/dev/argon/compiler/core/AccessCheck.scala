package dev.argon.compiler.core

import dev.argon.compiler.Comp
import dev.argon.util.FileID
import cats.implicits._
import zio.IO

object AccessCheck {

  def checkInstance[TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (
    accessTokens: Set[AccessToken],
    instanceAccessToken: AccessToken,
    callerModule: ModuleId,
    callerFileID: FileID,
    methodBinding: MethodBinding[TContext, TPayloadSpec]
  ): Comp[Boolean] = methodBinding.accessModifier match {
    case global: AccessModifierGlobal => IO.succeed(checkGlobal(callerModule, callerFileID, methodBinding.method.owner.module.id, methodBinding.method.fileId, global))
    case AccessModifier.ProtectedInternal if callerModule === methodBinding.method.owner.module.id => IO.succeed(true)
    case AccessModifier.Protected | AccessModifier.ProtectedInternal => ???
    case AccessModifier.Private =>
      IO.succeed(accessTokens.exists {
        case AccessToken.OfClass(arClass) =>
          methodBinding.method.owner match {
            case MethodOwner.ByClass(ownerClass) => arClass.value.id === ownerClass.id
            case MethodOwner.ByClassObject(ownerClass) => arClass.value.id === ownerClass.id
            case _ => false
          }

        case AccessToken.OfTrait(arTrait) =>
          methodBinding.method.owner match {
            case MethodOwner.ByTrait(ownerTrait) => arTrait.value.id === ownerTrait.id
            case MethodOwner.ByTraitObject(ownerTrait) => arTrait.value.id === ownerTrait.id
            case _ => false
          }

        case AccessToken.OfDataConstructor(ctor) =>
          methodBinding.method.owner match {
            case MethodOwner.ByDataCtor(dataCtor) => ctor.value.id === dataCtor.id
            case _ => false
          }
      })
  }

  def checkGlobal(callerModule: ModuleId, callerFileID: FileID, calleeModule: ModuleId, calleeFileID: FileID, accessModifier: AccessModifierGlobal): Boolean = accessModifier match {
    case AccessModifier.Public => true
    case AccessModifier.Internal => callerModule === calleeModule
    case AccessModifier.PrivateInternal => callerModule === calleeModule && callerFileID === calleeFileID
  }

}
