package dev.argon.compiler.verifier

import dev.argon.compiler.Compilation
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._

import scalaz._
import Scalaz._

class Verifier {

  def passes: Set[VerifyPass] = Set(SealedOpenPass)

  def verifyModule[TComp[+_] : Compilation](context: ContextComp[TComp])(module: ArModule[context.type, DeclarationPayloadSpecifier]): TComp[Unit] =
    module.globalNamespace.flatMap { globalNamespace =>
      passes.traverse_ { _.verifyNamespace(context)(globalNamespace) }
    }

}
