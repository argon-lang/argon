package dev.argon.compiler.verifier

import dev.argon.compiler.{Comp, Compilation}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._
import cats._
import cats.implicits._
import zio.interop.catz.core._

class Verifier {

  def passes: Set[VerifyPass] = Set()

  def verifyModule(context: Context)(module: ArModule[context.type, DeclarationPayloadSpecifier]): Comp[Unit] =
    module.namespaces.foreach { ns =>
      passes.toVector.traverse_ { _.verifyNamespace(context)(module, ns) }
    }

}
