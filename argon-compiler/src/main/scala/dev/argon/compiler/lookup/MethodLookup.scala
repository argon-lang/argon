package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.FileSpec
import dev.argon.compiler.types.TypeSystem
import cats._
import cats.data.NonEmptyVector
import cats.implicits._
import dev.argon.compiler.Compilation

object MethodLookup {

  def lookupMethods(context: Context)(ts: TypeSystem[context.type])(instanceType: ts.TypeWithMethods)(callerDescriptor: Descriptor, fileSpec: FileSpec)(memberName: MemberName): context.Comp[OverloadResult[MemberValue[context.type]]] =
    lookupMethodsImpl(context)(ts)(callerDescriptor, fileSpec)(memberName)(Vector(instanceType))(Set.empty)

  private def lookupMethodsImpl(context: Context)(ts: TypeSystem[context.type])(callerDescriptor: Descriptor, fileSpec: FileSpec)(memberName: MemberName)(instanceTypes: Vector[ts.TypeWithMethods])(seenTypes: Set[MethodOwnerDescriptor]): context.Comp[OverloadResult[MemberValue[context.type]]] = {
    import context._

    if(instanceTypes.isEmpty)
      OverloadResult.End.pure[Comp]
    else {
      val newSeenTypes = seenTypes ++ instanceTypes.flatMap(getDescriptor(context)(ts)(_).toList.toVector)

      val unseenInstanceTypes = instanceTypes.filterNot { t => getDescriptor(context)(ts)(t).exists(seenTypes.contains) }

      val newBaseTypes = unseenInstanceTypes.flatMap {
        case ts.ClassType(_, _, baseTypes) => baseTypes.baseClass.toList.toVector ++ baseTypes.baseTraits
        case ts.TraitType(_, _, baseTypes) => baseTypes.baseTraits
        case ts.DataConstructorType(_, _, instanceType) => Vector(instanceType)
      }

      unseenInstanceTypes
        .flatTraverse[Comp, MemberValue.Method[context.type]] {
          case ts.ClassType(arClass, _, _) => arClass.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
          case ts.TraitType(arTrait, _, _) => arTrait.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
          case ts.DataConstructorType(ctor, _, _) => ctor.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
        }
        .flatMap { memberValues =>
          memberValues
            .filter { method =>
              val methodName = method.arMethod.value.name
              methodName =!= MemberName.Unnamed && memberName === methodName
            }
            .filterA { method =>
              AccessCheck.checkInstance[context.Comp, context.type, method.arMethod.PayloadSpec](callerDescriptor, fileSpec, method.arMethod.value)(context.compCompilationInstance)
            }
              .flatMap { filteredMembers =>
                lookupMethodsImpl(context)(ts)(callerDescriptor, fileSpec)(memberName)(newBaseTypes)(newSeenTypes)
                  .map { baseTypeOverloads =>
                    NonEmptyVector.fromVector(filteredMembers) match {
                      case Some(filteredMembers) => OverloadResult.List(filteredMembers, baseTypeOverloads)
                      case None => baseTypeOverloads
                    }
                  }
              }
        }
    }
  }

  private def getDescriptor[TComp[_]](context: Context)(ts: TypeSystem[context.type])(t: ts.ArExpr): Option[MethodOwnerDescriptor] =
    t match {
      case ts.ClassType(arClass, _, _) => Some(arClass.value.descriptor)
      case ts.TraitType(arTrait, _, _) => Some(arTrait.value.descriptor)
      case ts.DataConstructorType(ctor, _, _) => Some(ctor.value.descriptor)
      case _ => None
    }

}
