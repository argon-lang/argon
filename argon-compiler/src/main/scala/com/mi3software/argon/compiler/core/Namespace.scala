package com.mi3software.argon.compiler.core

import com.mi3software.argon.util.NamespacePath

import scala.collection.immutable._


final case class Namespace[TContext <: Context with Singleton, TPayloadSpec[_, _]](path: NamespacePath, bindings: Vector[GlobalBinding[TContext, TPayloadSpec]])
