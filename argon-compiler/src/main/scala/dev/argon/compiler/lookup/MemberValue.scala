package dev.argon.compiler.lookup

import dev.argon.compiler.core.{AbsRef, Context, MethodBinding}

trait MemberValue[TContext <: Context with Singleton] {

}
object MemberValue {

  final case class Method[TContext <: Context with Singleton](arMethod: AbsRef[TContext, MethodBinding]) extends MemberValue[TContext]

}

