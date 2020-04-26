package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.FileSpec
import dev.argon.compiler.types.TypeSystem
import cats._
import cats.data.NonEmptyVector
import cats.implicits._
import dev.argon.compiler.{Comp, Compilation}
import zio.IO
import zio.interop.catz._

object MethodLookup {

  def lookupMethods(context: Context)(ts: TypeSystem[context.type])(instanceType: ts.TypeWithMethods)(callerDescriptor: Descriptor, fileSpec: FileSpec)(memberName: MemberName): Comp[OverloadResult[MemberValue[context.type]]] =
    lookupMethodsImpl(context)(ts)(callerDescriptor, fileSpec)(memberName)(Vector(instanceType))(Set.empty)

  private def lookupMethodsImpl(context: Context)(ts: TypeSystem[context.type])(callerDescriptor: Descriptor, fileSpec: FileSpec)(memberName: MemberName)(instanceTypes: Vector[ts.TypeWithMethods])(seenTypes: Set[MethodOwnerDescriptor]): Comp[OverloadResult[MemberValue[context.type]]] = {
    if(instanceTypes.isEmpty)
      IO.succeed(OverloadResult.End)
    else {
      val newSeenTypes = seenTypes ++ instanceTypes.map(getDescriptor(context)(ts)(_))

      val unseenInstanceTypes = instanceTypes.filterNot { t => seenTypes.contains(getDescriptor(context)(ts)(t)) }

      unseenInstanceTypes
        .flatTraverse {
          case ts.ClassType(arClass, args) =>
            arClass.value.signature
              .flatMap { sig =>
                ts.liftSignatureResult(sig, args)
              }
              .flatMap { result =>
                result.baseTypes.map { baseTypes =>
                  baseTypes.baseClass.toList.toVector ++ baseTypes.baseTraits
                }
              }

          case ts.TraitType(arTrait, args) =>
            arTrait.value.signature
              .flatMap { sig =>
                ts.liftSignatureResult(sig, args)
              }
              .flatMap { result =>
                result.baseTypes.map { baseTypes => baseTypes.baseTraits }
              }


          case ts.DataConstructorType(_, _, instanceType) =>
            IO.succeed(Vector(instanceType))
        }
          .flatMap { newBaseTypes =>
            unseenInstanceTypes
              .flatTraverse {
                case ts.ClassType(arClass, _) => arClass.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
                case ts.TraitType(arTrait, _) => arTrait.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
                case ts.DataConstructorType(ctor, _, _) => ctor.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
              }
              .flatMap { memberValues =>
                memberValues
                  .distinctBy { _.arMethod.value.method.descriptor }
                  .filter { method =>
                    val methodName = method.arMethod.value.name
                    methodName =!= MemberName.Unnamed && memberName === methodName
                  }
                  .filterA { method =>
                    AccessCheck.checkInstance[context.type, method.arMethod.PayloadSpec](callerDescriptor, fileSpec, method.arMethod.value)
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
  }

  private def getDescriptor[TComp[_]](context: Context)(ts: TypeSystem[context.type])(t: ts.TypeWithMethods): MethodOwnerDescriptor =
    t match {
      case ts.ClassType(arClass, _) => arClass.value.descriptor
      case ts.TraitType(arTrait, _) => arTrait.value.descriptor
      case ts.DataConstructorType(ctor, _, _) => ctor.value.descriptor
    }

}
