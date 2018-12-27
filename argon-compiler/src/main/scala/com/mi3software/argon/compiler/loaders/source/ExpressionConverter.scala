package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.HoleType
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.types.{ExpandTypeSystemConverter, TypeSystem, TypeSystemConverter}
import com.mi3software.argon.parser
import com.mi3software.argon.util.{FileSpec, WithSource}

sealed trait ExpressionConverter extends VariableContext with SignatureContext with ScopeContext {

  type Env = ExpressionConverter.Env[Scope]

  def createHole: TypeSystem {
    val context: typeSystem.context.type
    type TTypeWrapper[A] = HoleType[typeSystem.TTypeWrapper[A]]
  } = ExpressionConverter.holeTypeSystem(typeSystem)

}

object ExpressionConverter {

  final case class Env[TScope]
  (
    descriptor: Descriptor,
    scope: TScope,
    fileSpec: FileSpec
  )

  def convertExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : TComp[context.ArExpr] = ???

  def convertTypeExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.Scope])
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.TType] = ???


  sealed trait HoleType[T]
  private final case class HoleTypeType[T](t: T) extends HoleType[T]
  private final case class HoleTypeHole[T]() extends HoleType[T]

  private def holeTypeSystem(innerTS: TypeSystem): TypeSystem {
    val context: innerTS.context.type
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  } = {
    final class HoleTypeSystem extends TypeSystem {
      override val context: innerTS.context.type = innerTS.context
      override type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]

      val expandConverter: TypeSystemConverter[innerTS.type, this.type] =
        ExpandTypeSystemConverter[HoleType](innerTS)(this)(new ExpandTypeSystemConverter.Expander[HoleType] {
          override def apply[A](a: A): HoleType[A] = HoleTypeType(a)
        })

      override def fromArType(arType: context.typeSystem.TType): TType =
        innerTS.convertTypeSystem(this)(expandConverter)(innerTS.fromArType(arType))

      override def wrapType[A](a: A): HoleType[innerTS.TTypeWrapper[A]] =
        HoleTypeType(innerTS.wrapType(a))

      override def mapTypeWrapper[A, B](t: HoleType[innerTS.TTypeWrapper[A]])(f: A => B): HoleType[innerTS.TTypeWrapper[B]] =
        t match {
          case HoleTypeType(inner) => HoleTypeType(innerTS.mapTypeWrapper(inner)(f))
          case HoleTypeHole() => HoleTypeHole()
        }

      override def isSubTypeWrapper[TComp[+ _] : Compilation, T](f: (T, T) => TComp[Boolean])(a: HoleType[innerTS.TTypeWrapper[T]], b: HoleType[innerTS.TTypeWrapper[T]]): TComp[Boolean] = ???
    }

    new HoleTypeSystem
  }

}
