package com.mi3software.argon.compiler.core

final case class EffectInfo(isPure: Boolean) {
  def canBeCalledWithin(callerEffects: EffectInfo): Boolean =
    isPure || !callerEffects.isPure

  def canDeclareMutableVariables: Boolean = !isPure

  def combineWith(other: EffectInfo): EffectInfo =
    EffectInfo(isPure = isPure && other.isPure)

}

object EffectInfo {
  val pure: EffectInfo = EffectInfo(isPure = true)
}
