package com.mi3software.argon.compiler.js

object JSMetadata {
  sealed trait Trait
  object Trait {
    final case object Invalid extends Trait
  }

  sealed trait Class
  object Class {
    final case object Invalid extends Class
  }
}
