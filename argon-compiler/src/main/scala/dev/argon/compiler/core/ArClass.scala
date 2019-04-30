package dev.argon.compiler.core

import dev.argon.compiler.types._

import scala.collection.immutable._
import scalaz._
import Scalaz._
import dev.argon.util.FileID

abstract class ArClass[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._, signatureContext.Signature

  val descriptor: ClassDescriptor
  val fileId: FileID

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val signature: Comp[Signature[ArClass.ResultInfo]]

  val fields: Comp[Vector[typeSystem.FieldVariable]]
  val methods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]
  val staticMethods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]
  val classConstructors: Comp[Vector[ClassConstructorBinding[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Unit, TClassMetadata]

  override def hashCode(): Int = descriptor.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArClass[_, _] => other.descriptor === descriptor
    case _ => false
  }
}

object ArClass {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    ArClass[TContext, TPayloadSpec] { val descriptor: ClassDescriptor.InNamespace }

  sealed trait ResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
    val typeSystem: TS
    val baseTypes: typeSystem.BaseTypeInfoClass
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(bt: ts.BaseTypeInfoClass): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override val baseTypes: typeSystem.BaseTypeInfoClass = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[F[_]: Monad]
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverter[context.type, ts1.type, ts2.type, F])
      (result: ResultInfo[context.type, ts1.type])
      : F[ResultInfo[context.type, ts2.type]] = for {
        baseClass <- result.baseTypes.baseClass.traverse(TypeSystem.convertClassType(context)(ts1)(ts2)(converter)(_))
        baseTraits <- result.baseTypes.baseTraits.traverse(TypeSystem.convertTraitType(context)(ts1)(ts2)(converter)(_))
      } yield ResultInfo(ts2)(ts2.BaseTypeInfoClass(baseClass, baseTraits))
    }

  }

}
