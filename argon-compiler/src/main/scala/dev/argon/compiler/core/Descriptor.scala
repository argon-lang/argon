package dev.argon.compiler.core

import dev.argon.util.NamespacePath
import cats._
import cats.implicits._
import cats.Eq

final case class ModuleDescriptor(name: String)

object ModuleDescriptor {
  implicit val eqInstance: Eq[ModuleDescriptor] = cats.derived.semi.eq
}

sealed trait Descriptor {
  def moduleDescriptor: ModuleDescriptor
}
object Descriptor {
  implicit val eqInstance: Eq[Descriptor] = cats.derived.semi.eq
}

sealed trait VariableOwnerDescriptor extends Descriptor
object VariableOwnerDescriptor {
  implicit val eqInstance: Eq[VariableOwnerDescriptor] = cats.derived.semi.eq
}

sealed trait ParameterOwnerDescriptor extends VariableOwnerDescriptor
object ParameterOwnerDescriptor {
  implicit val eqInstance: Eq[Descriptor] = cats.derived.semi.eq
}

sealed trait MethodOwnerDescriptor extends ParameterOwnerDescriptor
object MethodOwnerDescriptor {
  implicit val eqInstance: Eq[MethodOwnerDescriptor] = cats.derived.semi.eq
}

sealed trait InNamespaceDescriptor extends Descriptor {
  val moduleDescriptor: ModuleDescriptor
  val id: BigInt
  val namespace: NamespacePath
  val name: GlobalName
}

sealed trait TraitDescriptor extends MethodOwnerDescriptor
object TraitDescriptor {

  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends TraitDescriptor with InNamespaceDescriptor

  implicit val eqInstance: Eq[TraitDescriptor] = cats.derived.semi.eq
}

sealed trait ClassDescriptor extends MethodOwnerDescriptor
object ClassDescriptor {

  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends ClassDescriptor with InNamespaceDescriptor

  implicit val eqInstance: Eq[ClassDescriptor] = cats.derived.semi.eq
}

sealed trait DataConstructorDescriptor extends MethodOwnerDescriptor
object DataConstructorDescriptor {

  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends DataConstructorDescriptor with InNamespaceDescriptor

  implicit val eqInstance: Eq[DataConstructorDescriptor] = cats.derived.semi.eq
}

final case class TraitObjectDescriptor(traitDescriptor: TraitDescriptor) extends MethodOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = traitDescriptor.moduleDescriptor
}

final case class ClassObjectDescriptor(classDescriptor: ClassDescriptor) extends MethodOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = classDescriptor.moduleDescriptor
}

sealed trait FuncDescriptor extends ParameterOwnerDescriptor
object FuncDescriptor {
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends FuncDescriptor with InNamespaceDescriptor

  implicit val eqInstance: Eq[FuncDescriptor] = cats.derived.semi.eq
}

final case class MethodDescriptor(typeDescriptor: MethodOwnerDescriptor, index: Int, name: MethodName) extends ParameterOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = typeDescriptor.moduleDescriptor
}
object MethodDescriptor {
  implicit val eqInstance: Eq[MethodDescriptor] = cats.derived.semi.eq
}

final case class ClassConstructorDescriptor(ownerClass: ClassDescriptor, index: Int) extends ParameterOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = ownerClass.moduleDescriptor
}


sealed trait VariableLikeDescriptor extends Descriptor

final case class ParameterDescriptor(owner: ParameterOwnerDescriptor, index: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

final case class DeconstructedParameterDescriptor(owner: ParameterOwnerDescriptor, index: Int, tupleIndex: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

final case class VariableDescriptor(owner: VariableOwnerDescriptor, index: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

final case class FieldDescriptor(owner: ClassDescriptor, name: String) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

sealed trait VariableName
object VariableName {
  final case class Normal(name: String) extends VariableName
  case object Unnamed extends VariableName

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[VariableName] = cats.derived.semi.eq

}
