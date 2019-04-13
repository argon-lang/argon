package com.mi3software.argon.compiler.core

final case class ClassConstructorBinding[TContext <: Context with Singleton, TPayloadSpec[_, _]](index: Int, accessModifier: AccessModifier, ctor: ClassConstructor[TContext, TPayloadSpec])
