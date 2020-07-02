package dev.argon.backend.jvm.bindingloader

import dev.argon.backend.jvm.JVMContext
import dev.argon.compiler.core.{ClassOwner, GlobalBinding}
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import zio.UIO
import org.objectweb.asm.signature.SignatureReader

final class ClassBuilder private(methods: Seq[MethodBuilder]) extends BindingBuilder {


  override def withOuterClass(ownerClassName: String, methodName: String, methodDescriptor: String): BindingBuilder = ???

  override def withMethod(methodBuilder: MethodBuilder): BindingBuilder =
    new ClassBuilder(methods = methods :+ methodBuilder)

  override def toBinding(context: JVMContext): UIO[GlobalBinding[context.type, ReferencePayloadSpecifier]] = ???
}
