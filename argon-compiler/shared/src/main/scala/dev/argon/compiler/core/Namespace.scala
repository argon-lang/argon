package dev.argon.compiler.core

import dev.argon.util.NamespacePath

import scala.collection.immutable._


final case class Namespace[TContext <: Context with Singleton, TPayloadSpec[_, _]](path: NamespacePath, bindings: Vector[GlobalBinding[TContext, TPayloadSpec]])
