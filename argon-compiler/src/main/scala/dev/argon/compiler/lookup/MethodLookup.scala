package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.FileSpec
import dev.argon.compiler.types.TypeSystem
import scalaz._
import Scalaz._
import dev.argon.compiler.Compilation

object MethodLookup {

  def lookupMethods(context: Context)(ts: TypeSystem[context.type])(instanceType: ts.TypeWithMethods)(callerDescriptor: Descriptor, fileSpec: FileSpec)(memberName: MemberName): context.Comp[OverloadResult[MemberValue[context.type]]] =
    lookupMethodsImpl(context)(ts)(callerDescriptor, fileSpec)(memberName)(Vector(instanceType))(Set.empty)

  private def lookupMethodsImpl(context: Context)(ts: TypeSystem[context.type])(callerDescriptor: Descriptor, fileSpec: FileSpec)(memberName: MemberName)(instanceTypes: Vector[ts.TypeWithMethods])(seenTypes: Set[MethodOwnerDescriptor]): context.Comp[OverloadResult[MemberValue[context.type]]] =
    if(instanceTypes.isEmpty)
      context.compCompilationInstance.point(OverloadResult.End)
    else {
      val newSeenTypes = seenTypes ++ instanceTypes.flatMap(getDescriptor(context)(ts)(_).toVector)

      val unseenInstanceTypes = instanceTypes.filterNot { t => getDescriptor(context)(ts)(t).exists(seenTypes.contains) }

      val newBaseTypes = unseenInstanceTypes.flatMap {
        case ts.ClassType(_, _, baseTypes) => baseTypes.baseClass.toVector ++ baseTypes.baseTraits
        case ts.TraitType(_, _, baseTypes) => baseTypes.baseTraits
        case ts.DataConstructorType(_, _, instanceType) => Vector(instanceType)
      }

      context.compCompilationInstance.bind(
        unseenInstanceTypes
          .traverseM {
            case ts.ClassType(arClass, _, _) => context.compCompilationInstance.map(arClass.value.methods) { _.map { method => MemberValue.Method(AbsRef(method)) } }
            case ts.TraitType(arTrait, _, _) => context.compCompilationInstance.map(arTrait.value.methods) { _.map { method => MemberValue.Method(AbsRef(method)) } }
            case ts.DataConstructorType(ctor, _, _) => context.compCompilationInstance.map(ctor.value.methods) { _.map { method => MemberValue.Method(AbsRef(method)) } }
          }(context.compCompilationInstance, implicitly)
      ) { memberValues =>
        context.compCompilationInstance.bind(
          memberValues
            .filter { method =>
              val methodName = method.arMethod.value.name
              methodName =/= MemberName.Unnamed && memberName === methodName
            }
            .filterM { method =>
              AccessCheck.checkInstance[context.Comp, context.type, method.arMethod.PayloadSpec](callerDescriptor, fileSpec, method.arMethod.value)(context.compCompilationInstance)
            }(context.compCompilationInstance)
        ) { filteredMembers =>
            context.compCompilationInstance.map(
              lookupMethodsImpl(context)(ts)(callerDescriptor, fileSpec)(memberName)(newBaseTypes)(newSeenTypes)
            ) { baseTypeOverloads =>
              if(filteredMembers.nonEmpty)
                OverloadResult.List(filteredMembers, baseTypeOverloads)
              else
                baseTypeOverloads
            }
          }
      }

    }

  private def getDescriptor[TComp[_]](context: Context)(ts: TypeSystem[context.type])(t: ts.SimpleType): Option[MethodOwnerDescriptor] =
    t match {
      case ts.ClassType(arClass, _, _) => Some(arClass.value.descriptor)
      case ts.TraitType(arTrait, _, _) => Some(arTrait.value.descriptor)
      case ts.DataConstructorType(ctor, _, _) => Some(ctor.value.descriptor)
      case _ => None
    }

}
