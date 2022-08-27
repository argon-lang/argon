package dev.argon.compiler.expr

import dev.argon.compiler.definitions.*
import zio.*
import zio.stm.*

trait ExprUtilAccess extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}


  sealed trait AccessToken {

    import AccessToken.AccessType

    def canAccessGlobal(modifier: AccessModifierGlobal, module: ArModule): Boolean

    final def canAccessMember
    (
      modifier: AccessModifier,
      instanceTypes: Seq[AccessType],
      declaringType: AccessType,
    ): Comp[Boolean] =
      modifier match
        case modifier: AccessModifierSimple =>
          ZIO.exists(instanceTypes) { instanceType =>
            canAccessMemberImpl(modifier, instanceType, declaringType)
          }

        case AccessModifier.TubeOrProtected =>
          canAccessMember(AccessModifier.TubePrivate, instanceTypes, declaringType) ||
            canAccessMember(AccessModifier.Protected, instanceTypes, declaringType)

        case AccessModifier.TubeAndProtected =>
          canAccessMember(AccessModifier.TubePrivate, instanceTypes, declaringType) &&
            canAccessMember(AccessModifier.Protected, instanceTypes, declaringType)
      end match

    protected def canAccessMemberImpl
    (
      modifier: AccessModifierSimple,
      instanceType: AccessType,
      declaringType: AccessType,
    ): Comp[Boolean]

    def add(token: AccessToken): AccessToken =
      AccessToken.Multi(Set(this, token))
  }

  object AccessToken {
    type AccessType = ArClass | ArTrait

    final case class Multi(tokens: Set[AccessToken]) extends AccessToken {
      override def canAccessGlobal(modifier: AccessModifierGlobal, module: ArModule): Boolean =
        tokens.exists(_.canAccessGlobal(modifier, module))

      override def canAccessMemberImpl
      (
        modifier: AccessModifierSimple,
        instanceType: AccessType,
        declaringType: AccessType,
      ): Comp[Boolean] =
        ZIO.exists(tokens)(_.canAccessMemberImpl(modifier, instanceType, declaringType))

      override def add(token: AccessToken): AccessToken =
        token match {
          case other: Multi => AccessToken.Multi(tokens ++ other.tokens)
          case _ => AccessToken.Multi(tokens + token)
        }
    }

    final case class ModuleToken(module: ArModule) extends AccessToken {
      override def canAccessGlobal(modifier: AccessModifierGlobal, module: ArModule): Boolean =
        modifier match
          case AccessModifier.Public => true
          case AccessModifier.TubePrivate => module.tube.tubeName == this.module.tube.tubeName
          case AccessModifier.ModulePrivate => module.moduleName == this.module.moduleName
        end match

      override def canAccessMemberImpl
      (
        modifier: AccessModifierSimple,
        instanceType: AccessType,
        declaringType: AccessType,
      ): Comp[Boolean] =
        modifier match
          case modifier: AccessModifierGlobal =>
            ZIO.succeed(canAccessGlobal(modifier, getTypeOwningModule(declaringType)))
          case _ => ZIO.succeed(false)
        end match


      protected def getTypeOwningModule(t: AccessType): ArModule =
        t match
          case t: ArClass =>
            ArClassC.getOwningModule(t.owner)

          case t: ArTrait =>
            ArTraitC.getOwningModule(t.owner)
        end match
    }

    sealed trait TypeWithMethodsTokenCommon {
      self: AccessToken =>


      protected def checkSubTypes[T](superType: T, subType: T, getBaseTypes: T => Comp[Iterable[T]], seenTypes: TSet[? >: T])(using CanEqual[T, T]): Comp[Boolean] =
        seenTypes.contains(subType).commit.flatMap {
          case true => ZIO.succeed(false)
          case false =>
            seenTypes.put(subType).commit *>
              (
                if subType == superType then
                  ZIO.succeed(true)
                else
                  getBaseTypes(subType).flatMap { baseTypes =>
                    ZIO.exists(baseTypes) { baseType =>
                      checkSubTypes(superType, baseType, getBaseTypes, seenTypes)
                    }
                  }
                )
        }

      protected def getBaseTraits(t: ArTrait): Comp[Seq[ArTrait]] =
        for
          sig <- t.signature
          baseTraits <- sig.unsubstitutedResult.baseTraits
        yield baseTraits.map { traitType => traitType.constructor.arTrait }
    }

    final case class ClassToken(arClass: ArClass) extends AccessToken with TypeWithMethodsTokenCommon {
      override def canAccessGlobal(modifier: AccessModifierGlobal, module: ArModule): Boolean = false

      override def canAccessMemberImpl
      (
        modifier: AccessModifierSimple,
        instanceType: AccessType,
        declaringType: AccessType,
      ): Comp[Boolean] =
        modifier match
          case AccessModifier.Public => ZIO.succeed(true)
          case _: AccessModifierGlobal => ZIO.succeed(false)
          case AccessModifier.Protected =>
            instanceType match
              case instanceType: ArClass =>
                TSet.empty[ArClass].commit.flatMap { seenClasses =>
                  checkSubTypes(instanceType, arClass, getBaseClass, seenClasses)
                }

              case instanceType: ArTrait =>
                TSet.empty[AccessType].commit.flatMap { seenTypes =>
                  checkImplementsTrait(instanceType, arClass, seenTypes)
                }
            end match

          case AccessModifier.Private =>
            declaringType match
              case declaringType: ArClass => ZIO.succeed(declaringType == arClass)
              case _ => ZIO.succeed(false)
            end match

        end match

      private def getBaseClass(c: ArClass): Comp[Seq[ArClass]] =
        for
          sig <- c.signature
          res = sig.unsubstitutedResult
          baseClass <- res.baseClass
        yield baseClass.toList.map { _.constructor.arClass }

      protected def checkImplementsTrait(superTrait: ArTrait, subClass: ArClass, seenTypes: TSet[AccessType]): Comp[Boolean] =
        seenTypes.contains(subClass).commit.flatMap {
          case true => ZIO.succeed(false)
          case false =>
            for
              _ <- seenTypes.put(subClass).commit
              sig <- subClass.signature
              res = sig.unsubstitutedResult
              baseTraits <- res.baseTraits
              baseClassOpt <- res.baseClass

              implementsRes <-
                ZIO.exists(baseTraits) { baseTrait =>
                  checkSubTypes[ArTrait](superTrait, baseTrait.constructor.arTrait, getBaseTraits, seenTypes)
                } ||
                  ZIO.exists(baseClassOpt) { baseClass =>
                    checkImplementsTrait(superTrait, baseClass.constructor.arClass, seenTypes)
                  }
            yield implementsRes
        }


    }

    final case class TraitToken(arTrait: ArTrait) extends AccessToken with TypeWithMethodsTokenCommon {
      override def canAccessGlobal(modifier: AccessModifierGlobal, module: ArModule): Boolean = false

      override def canAccessMemberImpl
      (
        modifier: AccessModifierSimple,
        instanceType: AccessType,
        declaringType: AccessType,
      ): Comp[Boolean] =
        modifier match
          case AccessModifier.Public => ZIO.succeed(true)
          case _: AccessModifierGlobal => ZIO.succeed(false)
          case AccessModifier.Protected =>
            instanceType match
              case instanceType: ArTrait =>
                TSet.empty[ArTrait].commit.flatMap { seenTypes =>
                  checkSubTypes(instanceType, arTrait, getBaseTraits, seenTypes)
                }
              case _ => ZIO.succeed(false)
            end match

          case AccessModifier.Private =>
            declaringType match
              case declaringType: ArTrait => ZIO.succeed(declaringType == arTrait)
              case _ => ZIO.succeed(false)
            end match

        end match

    }


  }
}
