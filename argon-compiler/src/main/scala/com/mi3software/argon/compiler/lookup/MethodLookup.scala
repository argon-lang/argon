package com.mi3software.argon.compiler.lookup

import com.mi3software.argon.compiler.core.{AbsRef, ClassLikeDescriptor, Context}
import com.mi3software.argon.compiler.types.TypeSystem
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.Compilation

object MethodLookup {

  def lookupMethods(context: Context)(ts: TypeSystem[context.type])(instanceType: ts.SimpleType): context.Comp[OverloadResult[MemberValue[context.type]]] =
    lookupMethodsImpl(context)(ts)(Vector(instanceType))(Set.empty)

  private def lookupMethodsImpl(context: Context)(ts: TypeSystem[context.type])(instanceTypes: Vector[ts.SimpleType])(seenTypes: Set[ClassLikeDescriptor]): context.Comp[OverloadResult[MemberValue[context.type]]] = {
    val newSeenTypes = seenTypes ++ instanceTypes.flatMap(getDescriptor(context)(ts)(_).toVector)

    val unseenInstanceTypes = instanceTypes.filterNot { t => getDescriptor(context)(ts)(t).exists(seenTypes.contains) }

    val newBaseTypes = unseenInstanceTypes.flatMap {
      case ts.ClassType(_, _, baseTypes) => baseTypes.baseClass.toVector ++ baseTypes.baseTraits
      case ts.TraitType(_, _, baseTypes) => baseTypes.baseTraits
      case ts.DataConstructorType(_, _, instanceType) => Vector(instanceType)
      case _ => ???
    }

    context.compCompilationInstance.bind(
      unseenInstanceTypes
        .traverseM {
          case ts.ClassType(arClass, _, _) => context.compCompilationInstance.map(arClass.value.methods) { _.map { method => MemberValue.Method(AbsRef(method)) } }
          case ts.TraitType(arTrait, _, _) => context.compCompilationInstance.map(arTrait.value.methods) { _.map { method => MemberValue.Method(AbsRef(method)) } }
          case ts.DataConstructorType(ctor, _, _) => context.compCompilationInstance.map(ctor.value.methods) { _.map { method => MemberValue.Method(AbsRef(method)) } }
          case _ => ???
        }(context.compCompilationInstance, implicitly)
    ) { memberValues =>
      context.compCompilationInstance.map(
        lookupMethodsImpl(context)(ts)(newBaseTypes)(newSeenTypes)
      ) { baseTypeOverloads =>
          if(memberValues.nonEmpty)
            OverloadResult.List(memberValues, baseTypeOverloads)
          else
            baseTypeOverloads
        }
      }
  }

  private def getDescriptor[TComp[_]](context: Context)(ts: TypeSystem[context.type])(t: ts.SimpleType): Option[ClassLikeDescriptor] =
    t match {
      case ts.ClassType(arClass, _, _) => Some(arClass.value.descriptor)
      case ts.TraitType(arTrait, _, _) => Some(arTrait.value.descriptor)
      case ts.DataConstructorType(ctor, _, _) => Some(ctor.value.descriptor)
      case _ => None
    }

}
