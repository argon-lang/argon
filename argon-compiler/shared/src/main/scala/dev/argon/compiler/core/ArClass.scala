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
    def baseTypes: typeSystem.TSComp[typeSystem.BaseTypeInfoClass]
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(bt: => ts.TSComp[ts.BaseTypeInfoClass]): ResultInfo[TContext, ts.type] = new ResultInfo[TContext, ts.type] {
      override val typeSystem: ts.type = ts
      override lazy val baseTypes: typeSystem.TSComp[typeSystem.BaseTypeInfoClass] = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[F[_]: Monad]
      (context: Context)
      (ts1: TypeSystem[context.type])
      (ts2: TypeSystem[context.type])
      (converter: TypeSystemConverterEffect.Aux[context.type, ts1.type, ts2.type, F])
      (result: ResultInfo[context.type, ts1.type])
      : F[ResultInfo[context.type, ts2.type]] = {
        import ts2.tscompCompilationInstance

        ResultInfo(ts2)(
          for {
            baseTypes <- converter.convertEffect(result.baseTypes)
            baseClass <- converter.conversionEffectToResultTS(baseTypes.baseClass.traverse(converter.convertClassType(_)))
            baseTraits <- converter.conversionEffectToResultTS(baseTypes.baseTraits.traverse(converter.convertTraitType(_)))
          } yield ts2.BaseTypeInfoClass(baseClass, baseTraits)
        ).pure[F]
      }


      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : signatureContext.typeSystem.TSComp[Boolean] = {
        import signatureContext.typeSystem.tscompCompilationInstance

        result.baseTypes.map { baseTypes =>
          baseTypes.baseClass.exists(refChecker.checkArExpr) ||
            baseTypes.baseTraits.exists(refChecker.checkArExpr)
        }
      }

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type])
      : ResultInfo[signatureContext.context.type, signatureContext.typeSystem.type] = {
        import signatureContext.typeSystem.tscompCompilationInstance

        ResultInfo(signatureContext.typeSystem)(
          result.baseTypes.map { baseTypes =>
            signatureContext.typeSystem.BaseTypeInfoClass(
              baseTypes.baseClass.map(subst.substClassType(_)),
              baseTypes.baseTraits.map(subst.substTraitType(_))
            )
          }
        )
      }
    }

  }

}
