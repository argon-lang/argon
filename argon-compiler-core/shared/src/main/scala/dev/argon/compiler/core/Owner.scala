package dev.argon.compiler.core

import cats.Eq
import cats.implicits._
import dev.argon.util.NamespacePath

sealed trait ClassOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
object ClassOwner {
  final case class ByNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]](module: ArModule[TContext, TPayloadSpec], namespace: NamespacePath, name: GlobalName) extends ClassOwner[TContext, TPayloadSpec]
}

sealed trait TraitOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
object TraitOwner {
  final case class ByNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]](moduleId: ArModule[TContext, TPayloadSpec], namespace: NamespacePath, name: GlobalName) extends TraitOwner[TContext, TPayloadSpec]
}

sealed trait DataConstructorOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
object DataConstructorOwner {
  final case class ByNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]](moduleId: ArModule[TContext, TPayloadSpec], namespace: NamespacePath, name: GlobalName) extends DataConstructorOwner[TContext, TPayloadSpec]
}

sealed trait FunctionOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
object FunctionOwner {
  final case class ByNamespace[TContext <: Context with Singleton, TPayloadSpec[_, _]](moduleId: ArModule[TContext, TPayloadSpec], namespace: NamespacePath, name: GlobalName) extends FunctionOwner[TContext, TPayloadSpec]
}


sealed trait MethodOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]]
object MethodOwner {
  final case class ByClass[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec]) extends MethodOwner[TContext, TPayloadSpec]
  final case class ByClassObject[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec]) extends MethodOwner[TContext, TPayloadSpec]
  final case class ByTrait[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerTrait: ArTrait[TContext, TPayloadSpec]) extends MethodOwner[TContext, TPayloadSpec]
  final case class ByTraitObject[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerTrait: ArTrait[TContext, TPayloadSpec]) extends MethodOwner[TContext, TPayloadSpec]
  final case class ByDataCtor[TContext <: Context with Singleton, TPayloadSpec[_, _]](dataCtor: DataConstructor[TContext, TPayloadSpec]) extends MethodOwner[TContext, TPayloadSpec]
}

final case class ClassConstructorOwner[TContext <: Context with Singleton, TPayloadSpec[_, _]](ownerClass: ArClass[TContext, TPayloadSpec])

sealed trait VariableOwner[TContext]

sealed trait LocalVariableOwner[TContext] extends VariableOwner[TContext]
object LocalVariableOwner {
  final case class ByClass[TContext <: Context with Singleton](ownerClass: AbsRef[TContext, ArClass]) extends LocalVariableOwner[TContext]
  final case class ByTrait[TContext <: Context with Singleton](ownerTrait: AbsRef[TContext, ArTrait]) extends LocalVariableOwner[TContext]
  final case class ByDataConstructor[TContext <: Context with Singleton](ownerCtor: AbsRef[TContext, DataConstructor]) extends LocalVariableOwner[TContext]
  final case class ByFunction[TContext <: Context with Singleton](ownerFunc: AbsRef[TContext, ArFunc]) extends LocalVariableOwner[TContext]
  final case class ByMethod[TContext <: Context with Singleton](ownerMethod: AbsRef[TContext, ArMethod]) extends LocalVariableOwner[TContext]
  final case class ByClassConstructor[TContext <: Context with Singleton](ownerCtor: AbsRef[TContext, ClassConstructor]) extends LocalVariableOwner[TContext]
}

sealed trait ParameterVariableOwner[TContext] extends VariableOwner[TContext]
object ParameterVariableOwner {
  implicit def eqInstance[TContext <: Context with Singleton]: Eq[ParameterVariableOwner[TContext]] =
    new Eq[ParameterVariableOwner[TContext]] {
      override def eqv(x: ParameterVariableOwner[TContext], y: ParameterVariableOwner[TContext]): Boolean =
        (x, y) match {
          case (x: ByClass[TContext], y: ByClass[TContext]) => x.ownerClass.value.id === y.ownerClass.value.id
          case (_: ByClass[TContext], _) | (_, _: ByClass[TContext]) => false

          case (x: ByTrait[TContext], y: ByTrait[TContext]) => x.ownerTrait.value.id === y.ownerTrait.value.id
          case (_: ByTrait[TContext], _) | (_, _: ByTrait[TContext]) => false

          case (x: ByDataConstructor[TContext], y: ByDataConstructor[TContext]) => x.ownerCtor.value.id === y.ownerCtor.value.id
          case (_: ByDataConstructor[TContext], _) | (_, _: ByDataConstructor[TContext]) => false

          case (x: ByFunction[TContext], y: ByFunction[TContext]) => x.ownerFunc.value.id === y.ownerFunc.value.id
          case (_: ByFunction[TContext], _) | (_, _: ByFunction[TContext]) => false

          case (x: ByMethod[TContext], y: ByMethod[TContext]) => x.ownerMethod.value.id === y.ownerMethod.value.id
          case (_: ByMethod[TContext], _) | (_, _: ByMethod[TContext]) => false

          case (x: ByClassConstructor[TContext], y: ByClassConstructor[TContext]) => x.ownerCtor.value.id === y.ownerCtor.value.id
        }
    }

  final case class ByClass[TContext <: Context with Singleton](ownerClass: AbsRef[TContext, ArClass]) extends ParameterVariableOwner[TContext]
  final case class ByTrait[TContext <: Context with Singleton](ownerTrait: AbsRef[TContext, ArTrait]) extends ParameterVariableOwner[TContext]
  final case class ByDataConstructor[TContext <: Context with Singleton](ownerCtor: AbsRef[TContext, DataConstructor]) extends ParameterVariableOwner[TContext]
  final case class ByFunction[TContext <: Context with Singleton](ownerFunc: AbsRef[TContext, ArFunc]) extends ParameterVariableOwner[TContext]
  final case class ByMethod[TContext <: Context with Singleton](ownerMethod: AbsRef[TContext, ArMethod]) extends ParameterVariableOwner[TContext]
  final case class ByClassConstructor[TContext <: Context with Singleton](ownerCtor: AbsRef[TContext, ClassConstructor]) extends ParameterVariableOwner[TContext]
}

final case class FieldVariableOwner[TContext <: Context with Singleton](ownerClass: AbsRef[TContext, ArClass]) extends VariableOwner[TContext]
