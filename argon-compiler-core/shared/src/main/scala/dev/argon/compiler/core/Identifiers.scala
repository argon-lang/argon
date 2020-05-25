package dev.argon.compiler.core

import dev.argon.util.{NamespacePath, UniqueIdentifier}
import cats._
import cats.implicits._
import cats.Eq
import zio.{IO, UIO}

final case class ModuleId(name: String)

object ModuleId {
  implicit val eqInstance: Eq[ModuleId] = cats.derived.semi.eq
}

sealed trait CallerId
object CallerId {
  implicit val eqInstance: Eq[CallerId] = cats.derived.semi.eq
}

sealed trait MethodOwnerId
object MethodOwnerId {
  implicit val eqInstance: Eq[MethodOwnerId] = cats.derived.semi.eq
}

sealed trait CallableId
object CallableId {
  implicit val eqInstance: Eq[CallableId] = cats.derived.semi.eq
}

final case class ClassId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId
object ClassId {
  implicit val eqInstance: Eq[ClassId] = cats.derived.semi.eq
}

final case class TraitId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId
object TraitId {
  implicit val eqInstance: Eq[TraitId] = cats.derived.semi.eq
}

final case class DataConstructorId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId
object DataConstructorId {
  implicit val eqInstance: Eq[DataConstructorId] = cats.derived.semi.eq
}

final case class FunctionId(id: UniqueIdentifier) extends CallerId with CallableId
object FunctionId {
  implicit val eqInstance: Eq[FunctionId] = cats.derived.semi.eq
}

final case class MethodId(id: UniqueIdentifier) extends CallerId with CallableId
object MethodId {
  implicit val eqInstance: Eq[MethodId] = cats.derived.semi.eq
}

final case class ClassConstructorId(id: UniqueIdentifier) extends CallerId with CallableId
object ClassConstructorId {
  implicit val eqInstance: Eq[ClassConstructorId] = cats.derived.semi.eq
}

final case class VariableId(id: UniqueIdentifier)
object VariableId {
  implicit val eqInstance: Eq[VariableId] = cats.derived.semi.eq
}


/*


sealed trait VariableLikeDescriptor extends Descriptor
object VariableLikeDescriptor {
  implicit val eqInstance: Eq[VariableLikeDescriptor] = cats.derived.semi.eq
}

final case class ParameterDescriptor(owner: ParameterOwnerDescriptor, index: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleId = owner.moduleDescriptor
}

final case class VariableDescriptor(owner: VariableOwnerDescriptor, id: UniqueIdentifier) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleId = owner.moduleDescriptor
}

final case class FieldDescriptor(owner: ClassDescriptor, name: String) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleId = owner.moduleDescriptor
}


*/