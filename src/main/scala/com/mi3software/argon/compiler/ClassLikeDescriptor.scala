package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

import scalaz._
import Scalaz._

sealed trait ClassLikeDescriptor
object ClassLikeDescriptor {

  implicit val equalInstance: Equal[ClassLikeDescriptor] = {
    case (a: TraitDescriptor, b: TraitDescriptor) => a === b
    case (_: TraitDescriptor, _) | (_, _: TraitDescriptor) => false

    case (a: ClassDescriptor, b: ClassDescriptor) => a === b
    case (_: ClassDescriptor, _) | (_, _: ClassDescriptor) => false

    case (a: DataConstructorDescriptor, b: DataConstructorDescriptor) => a === b
  }

}

sealed trait TraitDescriptor extends ClassLikeDescriptor
object TraitDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, namespace: NamespacePath, name: String) extends TraitDescriptor

  implicit val equalInstance: Equal[TraitDescriptor] = {
    case (a @ InNamespace(_, _, _), b @ InNamespace(_, _, _)) => a === b
  }
}

sealed trait ClassDescriptor extends ClassLikeDescriptor
object ClassDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, namespace: NamespacePath, name: String) extends ClassDescriptor

  implicit val equalInstance: Equal[ClassDescriptor] = {
    case (a @ InNamespace(_, _, _), b @ InNamespace(_, _, _)) => a === b
  }
}

sealed trait DataConstructorDescriptor extends ClassLikeDescriptor
object DataConstructorDescriptor {
  @deriving(Equal)
  final case class InNamespace(moduleDescriptor: ModuleDescriptor, namespace: NamespacePath, name: String) extends DataConstructorDescriptor

  implicit val equalInstance: Equal[DataConstructorDescriptor] = {
    case (a @ InNamespace(_, _, _), b @ InNamespace(_, _, _)) => a === b
  }
}

