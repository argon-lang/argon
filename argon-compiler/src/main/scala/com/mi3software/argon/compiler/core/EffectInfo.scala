package com.mi3software.argon.compiler.core

final case class EffectInfo(isPure: Boolean) {
  def canCall(callerEffects: EffectInfo): Boolean =
    !isPure || callerEffects.isPure

  def canDeclareVariable(mutability: Mutability): Boolean = mutability match {
    case Mutability.Mutable => !isPure
    case Mutability.NonMutable => true
  }

  def combineWith(other: EffectInfo): EffectInfo =
    EffectInfo(isPure = isPure && other.isPure)

}

object EffectInfo {
  val pure: EffectInfo = EffectInfo(isPure = true)
}
