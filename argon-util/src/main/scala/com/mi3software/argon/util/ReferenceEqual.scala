package com.mi3software.argon.util

import scalaz.Equal

final class ReferenceEqual[T <: AnyRef] extends Equal[T] {

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def equal(a1: T, a2: T): Boolean = a1 eq a2

}

object ReferenceEqual {
  def apply[T <: AnyRef]: ReferenceEqual[T] = new ReferenceEqual[T]
}
