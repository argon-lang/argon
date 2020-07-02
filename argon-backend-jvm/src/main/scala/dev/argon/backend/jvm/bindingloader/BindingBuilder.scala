package dev.argon.backend.jvm.bindingloader

import dev.argon.backend.jvm.JVMContext
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import zio.UIO
import org.objectweb.asm.signature.SignatureReader

abstract class BindingBuilder {

  def withOuterClass(ownerClassName: String, methodName: String, methodDescriptor: String): BindingBuilder
  def withMethod(methodBuilder: MethodBuilder): BindingBuilder

  def toBinding(context: JVMContext): UIO[GlobalBinding[context.type, ReferencePayloadSpecifier]]

}
