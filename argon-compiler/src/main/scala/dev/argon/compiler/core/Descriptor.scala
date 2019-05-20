package dev.argon.compiler.core

import dev.argon.util.NamespacePath
import scalaz._
import Scalaz._
import cats.Eq

@deriving(Equal)
final case class ModuleDescriptor(name: String)

object ModuleDescriptor {
  import cats.implicits._

  implicit val eqInstance: Eq[ModuleDescriptor] = cats.derived.semi.eq

}

sealed trait Descriptor {
  def moduleDescriptor: ModuleDescriptor
}
object Descriptor {

  implicit val equalInstance: Equal[Descriptor] = {
    case (a: MethodOwnerDescriptor, b: MethodOwnerDescriptor) => a === b
    case (_: MethodOwnerDescriptor, _) | (_, _: MethodOwnerDescriptor) => false


    case (a: FuncDescriptor, b: FuncDescriptor) => a === b
    case (_: FuncDescriptor, _) | (_, _: FuncDescriptor) => false

    case (a: MethodDescriptor, b: MethodDescriptor) => a === b
    case (_: MethodDescriptor, _) | (_, _: MethodDescriptor) => false

    case (a: ClassConstructorDescriptor, b: ClassConstructorDescriptor) => a === b
    case (_: ClassConstructorDescriptor, _) | (_, _: ClassConstructorDescriptor) => false

    case (a: ParameterDescriptor, b: ParameterDescriptor) => a === b
    case (_: ParameterDescriptor, _) | (_, _: ParameterDescriptor) => false

    case (a: DeconstructedParameterDescriptor, b: DeconstructedParameterDescriptor) => a === b
    case (_: DeconstructedParameterDescriptor, _) | (_, _: DeconstructedParameterDescriptor) => false

    case (a: VariableDescriptor, b: VariableDescriptor) => a === b
    case (_: VariableDescriptor, _) | (_, _: VariableDescriptor) => false

    case (a: FieldDescriptor, b: FieldDescriptor) => a === b
  }

}

sealed trait VariableOwnerDescriptor extends Descriptor
object VariableOwnerDescriptor {

  implicit val equalInstance: Equal[VariableOwnerDescriptor] =
    (a, b) => (a: Descriptor) === (b: Descriptor)

}

sealed trait ParameterOwnerDescriptor extends VariableOwnerDescriptor
object ParameterOwnerDescriptor {

  implicit val equalInstance: Equal[ParameterOwnerDescriptor] =
    (a, b) => (a: Descriptor) === (b: Descriptor)

}

sealed trait MethodOwnerDescriptor extends ParameterOwnerDescriptor
object MethodOwnerDescriptor {

  implicit val equalInstance: Equal[MethodOwnerDescriptor] = {
    case (a: TraitDescriptor, b: TraitDescriptor) => a === b
    case (_: TraitDescriptor, _) | (_, _: TraitDescriptor) => false

    case (a: ClassDescriptor, b: ClassDescriptor) => a === b
    case (_: ClassDescriptor, _) | (_, _: ClassDescriptor) => false

    case (a: DataConstructorDescriptor, b: DataConstructorDescriptor) => a === b
    case (_: DataConstructorDescriptor, _) | (_, _: DataConstructorDescriptor) => false

    case (a: TraitObjectDescriptor, b: TraitObjectDescriptor) => a === b
    case (_: TraitObjectDescriptor, _) | (_, _: TraitObjectDescriptor) => false

    case (a: ClassObjectDescriptor, b: ClassObjectDescriptor) => a === b
  }

}

sealed trait InNamespaceDescriptor extends Descriptor {
  val moduleDescriptor: ModuleDescriptor
  val id: BigInt
  val namespace: NamespacePath
  val name: GlobalName
}

sealed trait TraitDescriptor extends MethodOwnerDescriptor
object TraitDescriptor {

  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends TraitDescriptor with InNamespaceDescriptor

  implicit val equalInstance: Equal[TraitDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }

}

sealed trait ClassDescriptor extends MethodOwnerDescriptor
object ClassDescriptor {

  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends ClassDescriptor with InNamespaceDescriptor

  implicit val equalInstance: Equal[ClassDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

sealed trait DataConstructorDescriptor extends MethodOwnerDescriptor
object DataConstructorDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends DataConstructorDescriptor with InNamespaceDescriptor

  implicit val equalInstance: Equal[DataConstructorDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

@deriving(Equal)
final case class TraitObjectDescriptor(traitDescriptor: TraitDescriptor) extends MethodOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = traitDescriptor.moduleDescriptor
}

@deriving(Equal)
final case class ClassObjectDescriptor(classDescriptor: ClassDescriptor) extends MethodOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = classDescriptor.moduleDescriptor
}

sealed trait FuncDescriptor extends ParameterOwnerDescriptor
object FuncDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, id: BigInt, namespace: NamespacePath, name: GlobalName) extends FuncDescriptor with InNamespaceDescriptor

  implicit val equalInstance: Equal[FuncDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

@deriving(Equal)
final case class MethodDescriptor(typeDescriptor: MethodOwnerDescriptor, index: Int, name: MethodName) extends ParameterOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = typeDescriptor.moduleDescriptor
}

@deriving(Equal)
final case class ClassConstructorDescriptor(ownerClass: ClassDescriptor, index: Int) extends ParameterOwnerDescriptor {
  override def moduleDescriptor: ModuleDescriptor = ownerClass.moduleDescriptor
}


sealed trait VariableLikeDescriptor extends Descriptor

@deriving(Equal)
final case class ParameterDescriptor(owner: ParameterOwnerDescriptor, index: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

@deriving(Equal)
final case class DeconstructedParameterDescriptor(owner: ParameterOwnerDescriptor, index: Int, tupleIndex: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

@deriving(Equal)
final case class VariableDescriptor(owner: VariableOwnerDescriptor, index: Int) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

@deriving(Equal)
final case class FieldDescriptor(owner: ClassDescriptor, name: String) extends VariableLikeDescriptor {
  override def moduleDescriptor: ModuleDescriptor = owner.moduleDescriptor
}

sealed trait VariableName
object VariableName {
  final case class Normal(name: String) extends VariableName
  case object Unnamed extends VariableName

  implicit val equalInstance: Equal[VariableName] = {
    case (Normal(a), Normal(b)) => a === b
    case (Normal(_), _) => false
    case (_, Normal(_)) => false

    case (Unnamed, Unnamed) => true
  }
}
