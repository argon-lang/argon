package dev.argon.compiler.types

sealed trait Universe {
  val toBigInt: BigInt
  def prevUnsafe: Universe
}

case object ValueUniverse extends Universe {
  override val toBigInt: BigInt = 0
  override def prevUnsafe: Universe = this
}

final case class TypeUniverse(prev: Universe) extends Universe {
  override val toBigInt: BigInt = prev.toBigInt + 1
  override def prevUnsafe: Universe = prev
}

object Universe {
  def union[U <: Universe](a: U, b: U): U =
    if(a.toBigInt < b.toBigInt)
      b
    else
      a
}