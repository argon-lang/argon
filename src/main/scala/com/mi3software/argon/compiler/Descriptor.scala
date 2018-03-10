package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

import scalaz._
import Scalaz._

sealed trait Descriptor
object Descriptor {

  implicit val equalInstance: Equal[Descriptor] = {
    case (a: ClassLikeDescriptor, b: ClassLikeDescriptor) => a === b
    case (_: ClassLikeDescriptor, _) | (_, _: ClassLikeDescriptor) => false


    case (a: FuncDescriptor, b: FuncDescriptor) => a === b
    case (_: FuncDescriptor, _) | (_, _: FuncDescriptor) => false

    case (a: MethodDescriptor, b: MethodDescriptor) => a === b
    case (_: MethodDescriptor, _) | (_, _: MethodDescriptor) => false

    case (a: ClassConstructorDescriptor, b: ClassConstructorDescriptor) => a === b
    case (_: ClassConstructorDescriptor, _) | (_, _: ClassConstructorDescriptor) => false

    case (a: ParameterDescriptor, b: ParameterDescriptor) => a === b
    case (_: ParameterDescriptor, _) | (_, _: ParameterDescriptor) => false

    case (a: VariableDescriptor, b: VariableDescriptor) => a === b
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

sealed trait ClassLikeDescriptor extends VariableOwnerDescriptor
object ClassLikeDescriptor {

  implicit val equalInstance: Equal[ClassLikeDescriptor] = {
    case (a: TraitDescriptor, b: TraitDescriptor) => a === b
    case (_: TraitDescriptor, _) | (_, _: TraitDescriptor) => false

    case (a: ClassDescriptor, b: ClassDescriptor) => a === b
    case (_: ClassDescriptor, _) | (_, _: ClassDescriptor) => false

    case (a: DataConstructorDescriptor, b: DataConstructorDescriptor) => a === b
  }

}

sealed trait TraitDescriptor extends ClassLikeDescriptor with ParameterOwnerDescriptor
object TraitDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends TraitDescriptor

  implicit val equalInstance: Equal[TraitDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

sealed trait ClassDescriptor extends ClassLikeDescriptor with ParameterOwnerDescriptor
object ClassDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends ClassDescriptor

  implicit val equalInstance: Equal[ClassDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

sealed trait DataConstructorDescriptor extends ClassLikeDescriptor with ParameterOwnerDescriptor
object DataConstructorDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends DataConstructorDescriptor

  implicit val equalInstance: Equal[DataConstructorDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

sealed trait FuncDescriptor extends ParameterOwnerDescriptor
object FuncDescriptor {
  @deriving(Equal)
  final case class InNamespace(module: ModuleDescriptor, namespace: NamespacePath, name: GlobalName, accessModifier: AccessModifierGlobal) extends FuncDescriptor

  implicit val equalInstance: Equal[FuncDescriptor] = {
    case (a @ InNamespace(_, _, _, _), b @ InNamespace(_, _, _, _)) => a === b
  }
}

@deriving(Equal)
final case class MethodDescriptor(typeDescriptor: ClassLikeDescriptor, name: String, accessModifier: AccessModifier) extends ParameterOwnerDescriptor

@deriving(Equal)
final case class ClassConstructorDescriptor(ownerClass: ClassDescriptor, accessModifier: AccessModifier) extends ParameterOwnerDescriptor


sealed trait VariableLikeDescriptor extends Descriptor

@deriving(Equal)
final case class ParameterDescriptor(owner: ParameterOwnerDescriptor, index: Int) extends VariableLikeDescriptor

@deriving(Equal)
final case class VariableDescriptor(owner: VariableOwnerDescriptor, index: Int) extends VariableLikeDescriptor
