package dev.argon.plugin

import dev.argon.plugin.{api => japi}
import zio.*
import dev.argon.util.ErrorWrapper
import java.io.IOException
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import scala.reflect.TypeTest

final class JavaSerializedTubeUnwrap[E >: IOException, EX <: Exception](inner: japi.tube.SerializedTube[EX])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends SerializedTube[E] {

  private def convertImpl[TJava, TScala](
    jCodec: dev.argon.verilization.runtime.Codec[TJava],
    sCodec: dev.argon.verilization.runtime.zio.Codec[TScala],
  )(
    javaValue: => TJava,
  ): IO[E, TScala] =
    ZIO.attemptBlockingInterrupt {
      javaValue
    }
      .catchAll(JavaErrorHandler.handleErrors[E, EX])
      .flatMap { value =>
        ZIO.suspendSucceed {
          val os = new ByteArrayOutputStream()
          val writer = new japi.util.OutputStreamFormatWriter(os)
          jCodec.write(writer, value)

          val is = new ByteArrayInputStream(os.toByteArray)
          val reader = new ScalaFormatReader(is)
          sCodec.read(reader)
        }
      }

  override def metadata(): IO[E, dev.argon.tube.Metadata.V1] =
    convertImpl(japi.tube.Metadata.V1.codec, dev.argon.tube.Metadata.V1.codec) {
      inner.metadata()
    }
    

  override def moduleDeclaration(id: BigInt): IO[E, dev.argon.tube.ModuleDeclaration.V1] =
    convertImpl(japi.tube.ModuleDeclaration.V1.codec, dev.argon.tube.ModuleDeclaration.V1.codec) {
      inner.moduleDeclaration(id.bigInteger)
    }

  override def moduleDefinition(id: BigInt): IO[E, dev.argon.tube.ModuleDefinition.V1] =
    convertImpl(japi.tube.ModuleDefinition.V1.codec, dev.argon.tube.ModuleDefinition.V1.codec) {
      inner.moduleDefinition(id.bigInteger)
    }


  override def traitRef(id: BigInt): IO[E, dev.argon.tube.TraitReference.V1] =
    convertImpl(japi.tube.TraitReference.V1.codec, dev.argon.tube.TraitReference.V1.codec) {
      inner.traitRef(id.bigInteger)
    }

  override def traitDef(id: BigInt): IO[E, dev.argon.tube.TraitDefinition.V1] =
    convertImpl(japi.tube.TraitDefinition.V1.codec, dev.argon.tube.TraitDefinition.V1.codec) {
      inner.traitDef(id.bigInteger)
    }


  override def classRef(id: BigInt): IO[E, dev.argon.tube.ClassReference.V1] =
    convertImpl(japi.tube.ClassReference.V1.codec, dev.argon.tube.ClassReference.V1.codec) {
      inner.classRef(id.bigInteger)
    }

  override def classDef(id: BigInt): IO[E, dev.argon.tube.ClassDefinition.V1] =
    convertImpl(japi.tube.ClassDefinition.V1.codec, dev.argon.tube.ClassDefinition.V1.codec) {
      inner.classDef(id.bigInteger)
    }


  override def functionRef(id: BigInt): IO[E, dev.argon.tube.FunctionReference.V1] =
    convertImpl(japi.tube.FunctionReference.V1.codec, dev.argon.tube.FunctionReference.V1.codec) {
      inner.functionRef(id.bigInteger)
    }

  override def functionDef(id: BigInt): IO[E, dev.argon.tube.FunctionDefinition.V1] =
    convertImpl(japi.tube.FunctionDefinition.V1.codec, dev.argon.tube.FunctionDefinition.V1.codec) {
      inner.functionDef(id.bigInteger)
    }

  override def externFunctionImplementation(id: BigInt, platform: Platform[E]): IO[E, platform.ExternFunction] =
    ZIO.runtime.flatMap { runtime =>
      given Runtime[Any] = runtime
      ZIO.attemptBlockingInterrupt {
        inner.externFunctionImplementation(id.bigInteger, new JavaPlatformWrap(platform))
      }
        .catchAll(JavaErrorHandler.handleErrors[E, EX])
    }
    

  override def methodRef(id: BigInt): IO[E, dev.argon.tube.MethodReference.V1] =
    convertImpl(japi.tube.MethodReference.V1.codec, dev.argon.tube.MethodReference.V1.codec) {
      inner.methodRef(id.bigInteger)
    }

  override def methodDef(id: BigInt): IO[E, dev.argon.tube.MethodDefinition.V1] =
    convertImpl(japi.tube.MethodDefinition.V1.codec, dev.argon.tube.MethodDefinition.V1.codec) {
      inner.methodDef(id.bigInteger)
    }

  override def externMethodImplementation(id: BigInt, platform: Platform[E]): IO[E, platform.ExternMethod] =
    ZIO.runtime.flatMap { runtime =>
      given Runtime[Any] = runtime
      ZIO.attemptBlockingInterrupt {
        inner.externMethodImplementation(id.bigInteger, new JavaPlatformWrap(platform))
      }
        .catchAll(JavaErrorHandler.handleErrors[E, EX])
    }

  override def classConstructorRef(id: BigInt): IO[E, dev.argon.tube.ClassConstructorReference.V1] =
    convertImpl(japi.tube.ClassConstructorReference.V1.codec, dev.argon.tube.ClassConstructorReference.V1.codec) {
      inner.classConstructorRef(id.bigInteger)
    }

  override def classConstructorDef(id: BigInt): IO[E, dev.argon.tube.ClassConstructorDefinition.V1] =
    convertImpl(japi.tube.ClassConstructorDefinition.V1.codec, dev.argon.tube.ClassConstructorDefinition.V1.codec) {
      inner.classConstructorDef(id.bigInteger)
    }

  override def externClassConstructorImplementation(id: BigInt, platform: Platform[E]): IO[E, platform.ExternClassConstructor] =
    ZIO.runtime.flatMap { runtime =>
      given Runtime[Any] = runtime
      ZIO.attemptBlockingInterrupt {
        inner.externClassConstructorImplementation(id.bigInteger, new JavaPlatformWrap(platform))
      }
        .catchAll(JavaErrorHandler.handleErrors[E, EX])
    }


}
