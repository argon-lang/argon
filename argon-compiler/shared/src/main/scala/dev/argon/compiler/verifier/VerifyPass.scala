package dev.argon.compiler.verifier

import dev.argon.compiler.Compilation
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._

import cats._
import cats.implicits._

trait VerifyPass {

  def verifyGlobal(context: Context)(binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): context.Comp[Unit]

  final def verifyNamespace(context: Context)(ns: Namespace[context.type, DeclarationPayloadSpecifier]): context.Comp[Unit] = {
    import context._
    ns.bindings.traverse_ {
      case GlobalBinding.NestedNamespace(_, nested) =>
        verifyNamespace(context)(nested)

      case nonNS: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] =>
        verifyGlobal(context)(nonNS)
    }
  }

}
