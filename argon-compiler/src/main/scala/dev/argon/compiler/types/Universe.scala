package dev.argon.compiler.types

sealed trait Universe {
  val toBigInt: BigInt
}

case object ValueUniverse extends Universe {
  override val toBigInt: BigInt = 0
}

final case class TypeUniverse(prev: Universe) extends Universe {
  override val toBigInt: BigInt = prev.toBigInt + 1
}

object Universe {
  def union[U <: Universe](a: U, b: U): U =
    if(a.toBigInt < b.toBigInt)
      b
    else
      a
}