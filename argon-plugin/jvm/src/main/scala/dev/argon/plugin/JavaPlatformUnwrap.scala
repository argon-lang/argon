package dev.argon.plugin

import zio.*
import dev.argon.plugin.{api => japi}
import dev.argon.io.BinaryResource
import dev.argon.util.ErrorWrapper
import java.io.IOException
import scala.reflect.TypeTest


final case class JavaPlatformUnwrap[E >: IOException, EX <: Exception, EFunc, EMethod, EClassCtor](inner: japi.Platform[EX, EFunc, EMethod, EClassCtor])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends Platform[E] {
  

  override val id: String = inner.id
  override val name: String = inner.name

  override type ExternFunction = EFunc
  override type ExternMethod = EMethod

  override type ExternClassConstructor = EClassCtor

  private def externCodec[T](innerCodec: japi.ExternCodec[EX, T]): ExternCodec[E, T] = new ExternCodec[E, T] {
    override def decode(resource: BinaryResource[E]): IO[E, T] =
      IO.runtime.flatMap { runtime =>
        given Runtime[Any] = runtime
        val res2 = new JavaBinaryResourceWrap(resource)
        IO.attemptBlockingInterrupt {
          innerCodec.decode(res2)
        }.catchAll(JavaErrorHandler.handleErrors[E, EX])
      }

    override def encode(value: T): BinaryResource[E] =
      new JavaBinaryResourceUnwrap(innerCodec.encode(value))
  }

  override val externFunctionCodec: ExternCodec[E, ExternFunction] = externCodec(inner.externFunctionCodec)

  override val externMethodCodec: ExternCodec[E, ExternMethod] = externCodec(inner.externMethodCodec)

  override val externClassConstructorCodec: ExternCodec[E, ExternClassConstructor] = externCodec(inner.externClassConstructorCodec)

}
