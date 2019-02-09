package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.NamespacePath
import scalaz._
import Scalaz._

object DescriptorId {

  private val specialChars = Vector("\\", ".", ",", ":", "-", "@", "#", "$", "?", "(", ")", "[", "]", "{", "}", "<", ">")

  private def encodeIdentifier(id: String): String =
    specialChars.foldLeft(id) { (id, ch) => id.replace(ch, "\\" + ch) }

  private def encodeInNamespace(descriptor: InNamespaceDescriptor): String =
    descriptor.namespace.ns.map(encodeIdentifier).map(_ + ".").mkString +
      ((descriptor.name match {
        case GlobalName.Normal(id) => encodeIdentifier(id)
        case GlobalName.Unnamed => "$_"
      }) : String) +
      s"#${descriptor.fileId.id}-${descriptor.index}"

  private def encodeType[TContext <: Context](t: ErasedSignature.SigType[TContext]): String =
    t match {
      case ErasedSignature.BlankType() => "?"
      case ErasedSignature.TraitType(arTrait) => s"(${forTrait(arTrait.value.descriptor)})"
      case ErasedSignature.ClassType(arClass) => s"(${forClass(arClass.value.descriptor)})"
      case ErasedSignature.DataConstructorType(ctor) => s"(${forDataConstructor(ctor.value.descriptor)})"
      case ErasedSignature.FunctionType(argumentType, resultType) => s"(${encodeType(argumentType)}->${encodeType(resultType)})"
      case ErasedSignature.TupleType(elements) => s"(${elements.map(e => "$_:" + encodeType(e)).toVector.mkString(",")})"
    }

  private def encodeMemberName[TContext <: Context](memberName: MemberName): String =
    memberName match {
      case MemberName.Normal(name) => encodeIdentifier(name)
      case MemberName.Unnamed => "$_"
      case MemberName.Call => "$call"
      case MemberName.New => "$new"
    }

  def forClass(descriptor: ClassDescriptor): String =
    descriptor match {
      case descriptor @ ClassDescriptor.InNamespace(_, _, _, _, _, _) => encodeInNamespace(descriptor)
    }

  def forTrait(descriptor: TraitDescriptor): String =
    descriptor match {
      case descriptor @ TraitDescriptor.InNamespace(_, _, _, _, _, _) => encodeInNamespace(descriptor)
    }

  def forDataConstructor(descriptor: DataConstructorDescriptor): String =
    descriptor match {
      case descriptor @ DataConstructorDescriptor.InNamespace(_, _, _, _, _, _) => encodeInNamespace(descriptor)
    }

  def forFunc[TContext <: Context](descriptor: FuncDescriptor, signature: ErasedSignature[TContext]): String =
    descriptor match {
      case descriptor @ FuncDescriptor.InNamespace(_, _, _, _, _, _) => encodeInNamespace(descriptor)
    }

  def forMethod[TContext <: Context](descriptor: MethodDescriptor, signature: ErasedSignature[TContext]): String =
    s"${encodeMemberName(descriptor.name)}#${descriptor.index}"

  def forClassConstructor[TContext <: Context](descriptor: ClassConstructorDescriptor): String =
    s"\$new#${descriptor.index}"

}
