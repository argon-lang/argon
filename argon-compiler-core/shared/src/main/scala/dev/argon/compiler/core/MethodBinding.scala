package dev.argon.compiler.core

final case class MethodBinding[TContext <: Context with Singleton, TPayloadSpec[_, _]](accessModifier: AccessModifier, method: ArMethod[TContext, TPayloadSpec])
