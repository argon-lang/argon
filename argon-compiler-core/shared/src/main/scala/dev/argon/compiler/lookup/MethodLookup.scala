package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.{FileID, FileSpec}
import dev.argon.compiler.types.TypeSystem
import cats._
import cats.data.NonEmptyVector
import cats.implicits._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.{Comp, Compilation}
import zio.IO
import zio.interop.catz.core._

object MethodLookup {

  def instanceAccessToken(context: Context)(ts: TypeSystem.Aux[context.type])(instanceType: TypeWithMethods[context.type, ts.TTypeWrapper]): AccessToken =
    instanceType match {
      case TraitType(arTrait, _) => AccessToken.OfTrait(arTrait)
      case ClassType(arClass, _) => AccessToken.OfClass(arClass)
      case DataConstructorType(ctor, _, _) => AccessToken.OfDataConstructor(ctor)
    }

  def lookupMethods
  (context: Context)
  (ts: TypeSystem.Aux[context.type])
  (instanceType: TypeWithMethods[context.type, ts.TTypeWrapper])
  (
    accessTokens: Set[AccessToken],
    callerModule: ModuleId,
    callerFileID: FileID
  )
  (memberName: MemberName)
  : Comp[OverloadResult[MemberValue[context.type]]] =
    lookupMethodsImpl(context)(ts)(accessTokens, instanceAccessToken(context)(ts)(instanceType), callerModule, callerFileID)(memberName)(Vector(instanceType))(Set.empty)

  private def lookupMethodsImpl
  (context: Context)
  (ts: TypeSystem.Aux[context.type])
  (
    accessTokens: Set[AccessToken],
    instanceAccessToken: AccessToken,
    callerModule: ModuleId,
    callerFileID: FileID
  )
  (memberName: MemberName)
  (instanceTypes: Vector[TypeWithMethods[context.type, ts.TTypeWrapper]])
  (seenTypes: Set[MethodOwnerId])
  : Comp[OverloadResult[MemberValue[context.type]]] = {
    import ts.typeWrapperInstances

    if(instanceTypes.isEmpty)
      IO.succeed(OverloadResult.End)
    else {
      val newSeenTypes = seenTypes ++ instanceTypes.map(getId(context)(ts)(_))

      val unseenInstanceTypes = instanceTypes.filterNot { t => seenTypes.contains(getId(context)(ts)(t)) }

      unseenInstanceTypes
        .flatTraverse {
          case ClassType(arClass, args) =>
            arClass.value.signature
              .flatMap { sig =>
                SignatureContext.liftSignatureResult(context)(sig, args)
              }
              .flatMap { result =>
                result.baseTypes.map { baseTypes =>
                  baseTypes.baseClass.toList.toVector ++ baseTypes.baseTraits
                }
              }

          case TraitType(arTrait, args) =>
            arTrait.value.signature
              .flatMap { sig =>
                SignatureContext.liftSignatureResult(context)(sig, args)
              }
              .flatMap { result =>
                result.baseTypes.map { baseTypes => baseTypes.baseTraits }
              }


          case DataConstructorType(_, _, instanceType) =>
            IO.succeed(Vector(instanceType))
        }
          .flatMap { newBaseTypes =>
            unseenInstanceTypes
              .flatTraverse {
                case ClassType(arClass, _) => arClass.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
                case TraitType(arTrait, _) => arTrait.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
                case DataConstructorType(ctor, _, _) => ctor.value.methods.map { _.map { method => MemberValue.Method(AbsRef(method)) } }
              }
              .flatMap { memberValues =>
                memberValues
                  .distinctBy { _.arMethod.value.method.id }
                  .filter { method =>
                    val methodName = method.arMethod.value.method.name
                    methodName =!= MemberName.Unnamed && memberName === methodName
                  }
                  .filterA { method =>
                    AccessCheck.checkInstance[context.type, method.arMethod.PayloadSpec](accessTokens, instanceAccessToken, callerModule, callerFileID, method.arMethod.value)
                  }
                  .flatMap { filteredMembers =>
                    lookupMethodsImpl(context)(ts)(accessTokens, instanceAccessToken, callerModule, callerFileID)(memberName)(newBaseTypes)(newSeenTypes)
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

  private def getId[TComp[_]](context: Context)(ts: TypeSystem.Aux[context.type])(t: TypeWithMethods[context.type, ts.TTypeWrapper]): MethodOwnerId =
    t match {
      case ClassType(arClass, _) => arClass.value.id
      case TraitType(arTrait, _) => arTrait.value.id
      case DataConstructorType(ctor, _, _) => ctor.value.id
    }

}
