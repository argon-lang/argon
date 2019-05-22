package dev.argon.compiler.verifier

import dev.argon.compiler.Compilation
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._

import cats._
import cats.implicits._

trait VerifyPass {

  def verifyGlobal[TComp[+_] : Compilation](context: ContextComp[TComp])(binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): TComp[Unit]

  final def verifyNamespace[TComp[+_] : Compilation](context: ContextComp[TComp])(ns: Namespace[context.type, DeclarationPayloadSpecifier]): TComp[Unit] =
    ns.bindings.traverse_ {
      case GlobalBinding.NestedNamespace(_, nested) =>
        verifyNamespace(context)(nested)

      case nonNS: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] =>
        verifyGlobal(context)(nonNS)
    }

}
