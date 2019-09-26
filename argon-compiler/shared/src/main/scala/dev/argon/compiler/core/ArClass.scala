package dev.argon.compiler.core

import dev.argon.compiler.types._

import scala.collection.immutable._
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.CompilationMessageSource
import dev.argon.util.FileID
import shapeless.Nat

abstract class ArClass[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val descriptor: ClassDescriptor
  val fileId: FileID
  val classMessageSource: CompilationMessageSource

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val signature: Comp[Signature[ArClass.ResultInfo, _ <: Nat]]

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
    def baseTypes: typeSystem.BaseTypeInfoClass
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(bt: => ts.BaseTypeInfoClass): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override lazy val baseTypes: typeSystem.BaseTypeInfoClass = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[F[_]: Monad]
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverter.Aux[context.type, ts1.type, ts2.type, F])
      (result: ResultInfo[context.type, ts1.type])
      : F[ResultInfo[context.type, ts2.type]] = for {
        baseClass <- result.baseTypes.baseClass.traverse(converter.convertClassType(_))
        baseTraits <- result.baseTypes.baseTraits.traverse(converter.convertTraitType(_))
      } yield ResultInfo(ts2)(ts2.BaseTypeInfoClass(baseClass, baseTraits))

      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : Boolean =
        result.baseTypes.baseClass.exists(refChecker.checkArExpr) ||
          result.baseTypes.baseTraits.exists(refChecker.checkArExpr)

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type] =
        ResultInfo(signatureContext.typeSystem)(signatureContext.typeSystem.BaseTypeInfoClass(
          result.baseTypes.baseClass.map(subst.substClassType(_)),
          result.baseTypes.baseTraits.map(subst.substTraitType(_))
        ))
    }

  }

}
