package dev.argon.plugin

import zio.*
import dev.argon.plugin.{api => japi}
import dev.argon.io.BinaryResource
import dev.argon.util.ErrorWrapper
import java.io.IOException
import dev.argon.util.JavaExecuteIO
import scala.reflect.TypeTest


final case class JavaPlatformWrap[E >: IOException, EX <: Exception, EFunc, EMethod, EClassCtor](inner: Platform[E] { type ExternFunction = EFunc; type ExternMethod = EMethod; type ExternClassConstructor = EClassCtor })(using Runtime[Any], ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends japi.Platform[EX, EFunc, EMethod, EClassCtor] {
  

  override val id: String = inner.id
  override val name: String = inner.name


  private def externCodec[T](innerCodec: ExternCodec[E, T]): japi.ExternCodec[EX, T] = new japi.ExternCodec[EX, T] {
    override def decode(resource: japi.resource.BinaryResource[EX]): T =
      JavaExecuteIO.runInterruptable(innerCodec.decode(new JavaBinaryResourceUnwrap(resource)))

    override def encode(value: T): japi.resource.BinaryResource[EX] =
      new JavaBinaryResourceWrap(innerCodec.encode(value))
  }

  override val externFunctionCodec: japi.ExternCodec[EX, EFunc] = externCodec(inner.externFunctionCodec)

  override val externMethodCodec: japi.ExternCodec[EX, EMethod] = externCodec(inner.externMethodCodec)

  override val externClassConstructorCodec: japi.ExternCodec[EX, EClassCtor] = externCodec(inner.externClassConstructorCodec)
}
