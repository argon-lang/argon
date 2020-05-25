package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._

import scala.collection.immutable._
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.CompilationMessageSource
import dev.argon.compiler.expr.BaseTypeInfoClass
import dev.argon.util.FileID
import shapeless.Nat
import zio.{IO, ZIO}

abstract class ArClass[TContext <: Context with Singleton, TPayloadSpec[_, _]] extends CallableClass {
  val context: TContext
  val contextProof: context.type Is TContext
  import context._, signatureContext.Signature

  val id: ClassId
  val owner: ClassOwner
  val fileId: FileID
  val classMessageSource: CompilationMessageSource

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val signature: Comp[Signature[ArClass.ResultInfo, _ <: Nat]]

  val fields: Comp[Vector[typeSystem.TFieldVariable]]
  val methods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]
  val staticMethods: Comp[Vector[MethodBinding[TContext, TPayloadSpec]]]
  val classConstructors: Comp[Vector[ClassConstructorBinding[TContext, TPayloadSpec]]]

  val payload: TPayloadSpec[Unit, TClassMetadata]

  override def hashCode(): Int = id.hashCode()

  override def equals(o: Any): Boolean = o match {
    case other: ArClass[_, _] => other.id === id
    case _ => false
  }
}

object ArClass {

  type InNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]] =
    ArClass[TContext, TPayloadSpec] { val owner: ClassOwner.ByNamespace }

  sealed trait ResultInfo[TContext <: Context with Singleton, Wrap[+_]] {
    def baseTypes: Comp[BaseTypeInfoClass[TContext, Wrap]]
  }

  object ResultInfo {
    def apply[TContext <: Context with Singleton, Wrap[+_]](bt: => Comp[BaseTypeInfoClass[TContext, Wrap]]): ResultInfo[TContext, Wrap] = new ResultInfo[TContext, Wrap] {
      override lazy val baseTypes: Comp[BaseTypeInfoClass[TContext, Wrap]] = bt
    }

    implicit val sigResConverterInstance: SignatureResultConverter[ResultInfo] = new SignatureResultConverter[ResultInfo] {
      override def convertTypeSystem[Wrap1[+_], Wrap2[+_]]
      (context: Context)
      (converter: TypeSystemConverter.Aux[context.type, Wrap1, Wrap2])
      (result: ResultInfo[context.type, Wrap1])
      : Comp[ResultInfo[context.type, Wrap2]] =
        IO.succeed(ResultInfo(
          for {
            baseTypes <- result.baseTypes
            baseClass <- ZIO.foreach(baseTypes.baseClass)(converter.convertClassType(_))
            baseTraits <- ZIO.foreach(baseTypes.baseTraits)(converter.convertTraitType(_))
          } yield BaseTypeInfoClass(baseClass, baseTraits.toVector)
        ))


      override def referencesParameter
      (signatureContext: SignatureContext)
      (refChecker: signatureContext.RefChecker)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : Comp[Boolean] =
        result.baseTypes.map { baseTypes =>
          baseTypes.baseClass.exists(refChecker.checkArExpr) ||
            baseTypes.baseTraits.exists(refChecker.checkArExpr)
        }

      override def substitute
      (signatureContext: SignatureContext)
      (subst: signatureContext.Subst)
      (result: ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper])
      : ResultInfo[signatureContext.context.type, signatureContext.TTypeWrapper] =
        ResultInfo(
          result.baseTypes.map { baseTypes =>
            BaseTypeInfoClass(
              baseTypes.baseClass.map(subst.substClassType(_)),
              baseTypes.baseTraits.map(subst.substTraitType(_))
            )
          }
        )
    }

  }

}
