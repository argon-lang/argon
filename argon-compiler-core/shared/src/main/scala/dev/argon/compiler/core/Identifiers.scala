package dev.argon.compiler.core

import dev.argon.util.{NamespacePath, UniqueIdentifier}
import cats._
import cats.implicits._
import cats.Eq
import zio.{IO, UIO}

final case class ModuleId(name: String)

object ModuleId {
  implicit val eqInstance: Eq[ModuleId] = cats.derived.semiauto.eq
}

sealed trait CallerId
object CallerId {
  implicit val eqInstance: Eq[CallerId] = cats.derived.semiauto.eq
}

sealed trait MethodOwnerId
object MethodOwnerId {
  implicit val eqInstance: Eq[MethodOwnerId] = cats.derived.semiauto.eq
}

sealed trait CallableId
object CallableId {
  implicit val eqInstance: Eq[CallableId] = cats.derived.semiauto.eq
}

sealed trait GlobalId
object GlobalId {
  implicit val eqInstance: Eq[GlobalId] = cats.derived.semiauto.eq
}

final case class ClassId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId with GlobalId
object ClassId {
  implicit val eqInstance: Eq[ClassId] = cats.derived.semiauto.eq
}

final case class TraitId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId with GlobalId
object TraitId {
  implicit val eqInstance: Eq[TraitId] = cats.derived.semiauto.eq
}

final case class DataConstructorId(id: UniqueIdentifier) extends CallerId with MethodOwnerId with CallableId with GlobalId
object DataConstructorId {
  implicit val eqInstance: Eq[DataConstructorId] = cats.derived.semiauto.eq
}

final case class FunctionId(id: UniqueIdentifier) extends CallerId with CallableId with GlobalId
object FunctionId {
  implicit val eqInstance: Eq[FunctionId] = cats.derived.semiauto.eq
}

final case class MethodId(id: UniqueIdentifier) extends CallerId with CallableId
object MethodId {
  implicit val eqInstance: Eq[MethodId] = cats.derived.semiauto.eq
}

final case class ClassConstructorId(id: UniqueIdentifier) extends CallerId with CallableId
object ClassConstructorId {
  implicit val eqInstance: Eq[ClassConstructorId] = cats.derived.semiauto.eq
}



sealed trait VariableId

final case class LocalVariableId(id: UniqueIdentifier) extends VariableId
object LocalVariableId {
  implicit val eqInstance: Eq[LocalVariableId] = cats.derived.semiauto.eq
}

final case class ParameterVariableId(ownerId: CallableId, index: Int) extends VariableId
final case class ThisParameterVariableId(ownerId: CallableId) extends VariableId

final case class FieldVariableId(ownerId: ClassId, name: String) extends VariableId

