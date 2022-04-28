package dev.argon.plugin

import dev.argon.plugin.{api => japi}
import dev.argon.util.ErrorWrapper
import zio.*
import java.math.BigInteger
import dev.argon.util.JavaExecuteIO
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.IOException
import scala.reflect.TypeTest

final class JavaSerializedTubeWrap[E >: IOException, EX <: Exception](inner: SerializedTube[E])(using Runtime[Any], ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends japi.tube.SerializedTube[EX] {
  
  private def convertImpl[TJava, TScala](
    jCodec: dev.argon.verilization.runtime.Codec[TJava],
    sCodec: dev.argon.verilization.runtime.zio.Codec[TScala],
  )(
    scalaValue: IO[E, TScala],
  ): TJava =
    JavaExecuteIO.runInterruptable(
      for {
        value <- scalaValue
        os <- IO.succeed { new ByteArrayOutputStream() }
        writer = new ScalaFormatWriter(os)
        _ <- sCodec.write(writer, value)

        is <- IO.succeed { new ByteArrayInputStream(os.toByteArray) }
        reader = new japi.util.InputStreamFormatReader(is)
        res <- IO.succeed { jCodec.read(reader) }
      } yield res
    )
  
  override def metadata(): japi.tube.Metadata.V1 =
    convertImpl(japi.tube.Metadata.V1.codec, dev.argon.tube.Metadata.V1.codec)(inner.metadata())
  

  override def moduleDeclaration(id: BigInteger): japi.tube.ModuleDeclaration.V1 =
    convertImpl(japi.tube.ModuleDeclaration.V1.codec, dev.argon.tube.ModuleDeclaration.V1.codec)(inner.moduleDeclaration(id))

  override def moduleDefinition(id: BigInteger): japi.tube.ModuleDefinition.V1 =
    convertImpl(japi.tube.ModuleDefinition.V1.codec, dev.argon.tube.ModuleDefinition.V1.codec)(inner.moduleDefinition(id))
  

  override def traitRef(id: BigInteger): japi.tube.TraitReference.V1 =
    convertImpl(japi.tube.TraitReference.V1.codec, dev.argon.tube.TraitReference.V1.codec)(inner.traitRef(id))

  override def traitDef(id: BigInteger): japi.tube.TraitDefinition.V1 =
    convertImpl(japi.tube.TraitDefinition.V1.codec, dev.argon.tube.TraitDefinition.V1.codec)(inner.traitDef(id))
  

  override def classRef(id: BigInteger): japi.tube.ClassReference.V1 =
    convertImpl(japi.tube.ClassReference.V1.codec, dev.argon.tube.ClassReference.V1.codec)(inner.classRef(id))

  override def classDef(id: BigInteger): japi.tube.ClassDefinition.V1 =
    convertImpl(japi.tube.ClassDefinition.V1.codec, dev.argon.tube.ClassDefinition.V1.codec)(inner.classDef(id))

  
  override def functionRef(id: BigInteger): japi.tube.FunctionReference.V1 =
    convertImpl(japi.tube.FunctionReference.V1.codec, dev.argon.tube.FunctionReference.V1.codec)(inner.functionRef(id))

  override def functionDef(id: BigInteger): japi.tube.FunctionDefinition.V1 =
    convertImpl(japi.tube.FunctionDefinition.V1.codec, dev.argon.tube.FunctionDefinition.V1.codec)(inner.functionDef(id))

  override def externFunctionImplementation[ExternFunction](id: BigInteger, platform: japi.Platform[EX, ExternFunction, ?, ?]): ExternFunction =
    JavaExecuteIO.runInterruptable(
      inner.externFunctionImplementation(id, new JavaPlatformUnwrap(platform))
    )
  
  override def methodRef(id: BigInteger): japi.tube.MethodReference.V1 =
    convertImpl(japi.tube.MethodReference.V1.codec, dev.argon.tube.MethodReference.V1.codec)(inner.methodRef(id))

  override def methodDef(id: BigInteger): japi.tube.MethodDefinition.V1 =
    convertImpl(japi.tube.MethodDefinition.V1.codec, dev.argon.tube.MethodDefinition.V1.codec)(inner.methodDef(id))

  override def externMethodImplementation[ExternMethod](id: BigInteger, platform: japi.Platform[EX, ?, ExternMethod, ?]): ExternMethod =
    JavaExecuteIO.runInterruptable(
      inner.externMethodImplementation(id, new JavaPlatformUnwrap(platform))
    )
  
  override def classConstructorRef(id: BigInteger): japi.tube.ClassConstructorReference.V1 =
    convertImpl(japi.tube.ClassConstructorReference.V1.codec, dev.argon.tube.ClassConstructorReference.V1.codec)(inner.classConstructorRef(id))

  override def classConstructorDef(id: BigInteger): japi.tube.ClassConstructorDefinition.V1 =
    convertImpl(japi.tube.ClassConstructorDefinition.V1.codec, dev.argon.tube.ClassConstructorDefinition.V1.codec)(inner.classConstructorDef(id))

  override def externClassConstructorImplementation[ExternClassCtor](id: BigInteger, platform: japi.Platform[EX, ?, ?, ExternClassCtor]): ExternClassCtor =
    JavaExecuteIO.runInterruptable(
      inner.externClassConstructorImplementation(id, new JavaPlatformUnwrap(platform))
    )

}
