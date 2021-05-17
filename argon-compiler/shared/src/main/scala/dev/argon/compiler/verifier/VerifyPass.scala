package dev.argon.compiler.verifier

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.util.NamespacePath

trait VerifyPass {

  def verifyGlobal(context: Context)(binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): Comp[Unit]

  final def verifyNamespace(context: Context)(module: ArModule[context.type, DeclarationPayloadSpecifier], ns: NamespacePath): Comp[Unit] =
    module.getNamespace(ns).foreach(verifyGlobal(context))

}
