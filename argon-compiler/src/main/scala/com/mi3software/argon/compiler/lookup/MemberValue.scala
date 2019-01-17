package com.mi3software.argon.compiler.lookup

import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.{AbsRef, ArMethod, Context}
import com.mi3software.argon.compiler.types.TypeSystemConverter

trait MemberValue[TContext <: Context with Singleton] {

}
object MemberValue {

  final case class Method[TContext <: Context with Singleton](arMethod: AbsRef[TContext, ArMethod]) extends MemberValue[TContext]

}

