package com.mi3software.argon.compiler.types

final case class SubTypeInfo[T]
(
  superType: T,
  subType: T,
  args: Vector[SubTypeInfo[T]],
)

