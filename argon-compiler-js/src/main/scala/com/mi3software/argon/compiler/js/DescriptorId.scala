package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.NamespacePath

object DescriptorId {

  private val specialChars = Vector("\\", ".", ",", ":", "-", "@", "#", "$", "?", "(", ")", "[", "]", "{", "}", "<", ">")

  private def encodeIdentifier(id: String): String =
    specialChars.foldLeft(id) { (id, ch) => id.replace(ch, "\\" + ch) }

  private def encodeInNamespace(namespace: NamespacePath, name: GlobalName): String =
      namespace.ns.map(encodeIdentifier).mkString(".") + "." +
      ((name match {
        case GlobalName.Normal(id) => encodeIdentifier(id)
        case GlobalName.Unnamed(fileId, index) => "#" + fileId.id.toString + "-" + index.toString
      }) : String)

  private def encodeType[TContext <: Context](t: ErasedSignature.SigType[TContext]): String =
    t match {
      case ErasedSignature.BlankType() => "?"
      case ErasedSignature.TraitType(arTrait) => s"(${forTrait(arTrait.value.descriptor)})"
      case ErasedSignature.ClassType(arClass) => s"(${forClass(arClass.value.descriptor)})"
      case ErasedSignature.DataConstructorType(ctor) => s"(${forDataConstructor(ctor.value.descriptor)})"
      case ErasedSignature.FunctionType(argumentType, resultType) => s"(${encodeType(argumentType)}->${encodeType(resultType)})"
      case ErasedSignature.TupleType(elements) => s"(${elements.map(e => "$_:" + encodeType(e)).mkString(",")})"
    }

  private def encodeSignatureParameters[TContext <: Context](signature: ErasedSignature[TContext]): String =
    signature match {
      case ErasedSignature.Parameter(paramType, next) =>
        encodeType(paramType) + "->" + encodeSignatureParameters(next)

      case ErasedSignature.Result(resultType) =>
        encodeType(resultType)
    }


  def forClass(descriptor: ClassDescriptor): String =
    descriptor match {
      case ClassDescriptor.InNamespace(_, namespace, name, _) => encodeInNamespace(namespace, name)
      case ClassDescriptor.MetaClass(ownerClass) => forClass(ownerClass) + "[MetaClass]"
      case ClassDescriptor.TraitMetaClass(ownerTrait) => forTrait(ownerTrait) + "[TraitMetaClass]"
    }

  def forTrait(descriptor: TraitDescriptor): String =
    descriptor match {
      case TraitDescriptor.InNamespace(_, namespace, name, _) => encodeInNamespace(namespace, name)
    }

  def forDataConstructor(descriptor: DataConstructorDescriptor): String =
    descriptor match {
      case DataConstructorDescriptor.InNamespace(_, namespace, name, _) => encodeInNamespace(namespace, name)
    }

  def forFunc[TContext <: Context](descriptor: FuncDescriptor, signature: ErasedSignature[TContext]): String =
    descriptor match {
      case FuncDescriptor.InNamespace(_, namespace, name, _) => s"${encodeInNamespace(namespace, name)}:${encodeSignatureParameters(signature)}"
    }
}
