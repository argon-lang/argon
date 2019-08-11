package dev.argon.compiler.verifier

import dev.argon.compiler.Compilation
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._

import cats._
import cats.implicits._

class Verifier {

  def passes: Set[VerifyPass] = Set()

  def verifyModule(context: Context)(module: ArModule[context.type, DeclarationPayloadSpecifier]): context.Comp[Unit] = {
    import context._
    module.globalNamespace.flatMap { globalNamespace =>
      passes.toVector.traverse_ { _.verifyNamespace(context)(globalNamespace) }
    }
  }

}
