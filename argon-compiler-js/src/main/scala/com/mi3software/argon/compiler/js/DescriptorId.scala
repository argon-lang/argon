package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
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

  private def encodeType[TContext <: Context](t: SignatureTypeSystem[TContext]#TType): String =
    t match {
      case Some(TraitType(traitInfo)) => s"(${forTrait(traitInfo.descriptor)})"
      case Some(ClassType(classInfo)) => s"(${forClass(classInfo.descriptor)})"
      case Some(DataConstructorType(ctor)) => s"(${forDataConstructor(ctor.descriptor)})"
      case Some(FunctionType(argumentType, resultType)) => s"(${encodeType(argumentType)}->${encodeType(resultType)})"
      case Some(TupleType(elements)) => s"(${elements.map(e => "$_:" + encodeType(e.elementType)).mkString(",")})"
      case None => "?"
    }

  private def encodeSignatureParameters[TContext <: Context, TResultInfo[+_ <: TypeSystem]](signature: Signature[SignatureTypeSystem[TContext], TResultInfo]): String =
    signature.unsubstitutedParameters
      .map(param => param.tupleVars.map(v => "$_:" + encodeType(v.varType)).mkString(","))
      .mkString("->")

  private def encodeFunctionResult[TContext <: Context](functionResultInfo: FunctionResultInfo[SignatureTypeSystem[TContext]]): String =
    encodeType(functionResultInfo.returnType)

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

  def forFunc[TContext <: Context](descriptor: FuncDescriptor, signature: Signature[SignatureTypeSystem[TContext], FunctionResultInfo]): String =
    descriptor match {
      case FuncDescriptor.InNamespace(_, namespace, name, _) => s"${encodeInNamespace(namespace, name)}:${encodeSignatureParameters(signature)}->${encodeFunctionResult(signature.unsubstitutedResult)}"
    }
}
