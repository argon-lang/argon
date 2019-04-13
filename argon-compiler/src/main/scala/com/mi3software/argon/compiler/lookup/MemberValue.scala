package com.mi3software.argon.compiler.lookup

import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.{AbsRef, Context, MethodBinding}

trait MemberValue[TContext <: Context with Singleton] {

}
object MemberValue {

  final case class Method[TContext <: Context with Singleton](arMethod: AbsRef[TContext, MethodBinding]) extends MemberValue[TContext]

}

