package com.mi3software.argon.util

import scalaz._
import Scalaz._

sealed trait IntValueProof[A <: BigInt, B <: BigInt] {
  val value: A with B
}

object IntValueProof {

  def apply(a: BigInt, b: BigInt): Option[IntValueProof[a.type, b.type]] =
    if(a === b)
      Some(new IntValueProof[a.type, b.type] {
        @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
        override val value: a.type with b.type = a.asInstanceOf[a.type with b.type]
      })
    else
      None

}
