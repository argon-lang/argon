package dev.argon.compiler.verifier
import dev.argon.compiler.Compilation
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers._

import scalaz._
import Scalaz._

object SealedOpenPass extends VerifyPass {
  override def verifyGlobal[TComp[+ _] : Compilation](context: ContextComp[TComp])(binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): TComp[Unit] =
    binding match {
      case GlobalBinding.GlobalClass(_, _, arClass) =>
        ???

      case GlobalBinding.GlobalTrait(_, _, arTrait) =>
        ???

      case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) =>
        ???

      case GlobalBinding.GlobalFunction(_, _, _) => ().point[TComp]
    }
}
