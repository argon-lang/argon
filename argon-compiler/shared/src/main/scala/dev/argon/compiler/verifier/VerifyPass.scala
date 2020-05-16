package dev.argon.compiler.verifier

import dev.argon.compiler.{Comp, Compilation}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._
import cats._
import cats.implicits._
import zio.interop.catz.core._

trait VerifyPass {

  def verifyGlobal(context: Context)(binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): Comp[Unit]

  final def verifyNamespace(context: Context)(ns: Namespace[context.type, DeclarationPayloadSpecifier]): Comp[Unit] =
    ns.bindings.traverse_ {
      case GlobalBinding.NestedNamespace(_, nested) =>
        verifyNamespace(context)(nested)

      case nonNS: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] =>
        verifyGlobal(context)(nonNS)
    }

}
