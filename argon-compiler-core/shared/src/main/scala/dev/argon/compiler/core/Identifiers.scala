package dev.argon.compiler.core

import dev.argon.util.{DeriveHelpers, UniqueIdentifier}
import cats.implicits._
import cats.Eq

final case class ModuleId(name: String)

object ModuleId {
  implicit val eqInstance: Eq[ModuleId] = DeriveHelpers.eq
}

sealed trait CallerId
object CallerId {
  implicit val eqInstance: Eq[CallerId] = DeriveHelpers.eq
}

sealed trait MethodOwnerId
object MethodOwnerId {
  implicit val eqInstance: Eq[MethodOwnerId] = DeriveHelpers.eq
}

sealed trait CallableId
object CallableId {
  implicit val eqInstance: Eq[CallableId] = DeriveHelpers.eq
}

sealed trait GlobalId
object GlobalId {
  implicit val eqInstance: Eq[GlobalId] = DeriveHelpers.eq
}

final case class ClassId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId with GlobalId
object ClassId {
  implicit val eqInstance: Eq[ClassId] = DeriveHelpers.eq
}

final case class TraitId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId with GlobalId
object TraitId {
  implicit val eqInstance: Eq[TraitId] = DeriveHelpers.eq
}

final case class DataConstructorId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId with GlobalId
object DataConstructorId {
  implicit val eqInstance: Eq[DataConstructorId] = DeriveHelpers.eq
}

final case class FunctionId(id: UniqueIdentifier) extends CallerId with CallableId with GlobalId
object FunctionId {
  implicit val eqInstance: Eq[FunctionId] = DeriveHelpers.eq
}

final case class MethodId(id: UniqueIdentifier) extends CallerId with CallableId
object MethodId {
  implicit val eqInstance: Eq[MethodId] = DeriveHelpers.eq
}

final case class ClassConstructorId(id: UniqueIdentifier) extends CallerId with CallableId
object ClassConstructorId {
  implicit val eqInstance: Eq[ClassConstructorId] = DeriveHelpers.eq
}



sealed trait VariableId

final case class LocalVariableId(id: UniqueIdentifier) extends VariableId
object LocalVariableId {
  implicit val eqInstance: Eq[LocalVariableId] = DeriveHelpers.eq
}

final case class ParameterVariableId(ownerId: CallableId, index: Int) extends VariableId
final case class ThisParameterVariableId(ownerId: CallableId) extends VariableId

final case class FieldVariableId(ownerId: ClassId, name: String) extends VariableId

