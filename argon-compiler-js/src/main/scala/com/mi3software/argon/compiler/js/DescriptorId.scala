package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler.{ClassDescriptor, FuncDescriptor, GlobalName, TraitDescriptor}
import com.mi3software.argon.util.NamespacePath

object DescriptorId {

  private val specialChars = Vector("\\", ".", ",", ":", "@", "#", "$", "(", ")", "[", "]", "{", "}", "<", ">")

  private def encodeIdentifier(id: String): String =
    specialChars.foldLeft(id) { (id, ch) => id.replace(ch, "\\" + ch) }

  private def encodeInNamespace(namespace: NamespacePath, name: GlobalName): String =
      namespace.ns.map(encodeIdentifier).mkString(".") + ":" +
      ((name match {
        case GlobalName.Normal(id) => encodeIdentifier(id)
        case GlobalName.Unnamed(fileId, index) => "#" + fileId.id.toString + "-" + index.toString
      }) : String)

  def forClass(descriptor: ClassDescriptor): String =
    descriptor match {
      case ClassDescriptor.InNamespace(_, namespace, name, _) => encodeInNamespace(namespace, name)
      case ClassDescriptor.MetaClass(ownerClass) => forClass(ownerClass) + "<MetaClass>"
      case ClassDescriptor.TraitMetaClass(ownerTrait) => forTrait(ownerTrait) + "<TraitMetaClass>"
    }

  def forTrait(descriptor: TraitDescriptor): String =
    descriptor match {
      case TraitDescriptor.InNamespace(_, namespace, name, _) => encodeInNamespace(namespace, name)
    }

  def forFunc(descriptor: FuncDescriptor): String =
    descriptor match {
      case FuncDescriptor.InNamespace(_, namespace, name, _) => encodeInNamespace(namespace, name)
    }
}
