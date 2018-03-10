package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

import scala.collection.immutable._


final case class Namespace[+TScopeValue](path: NamespacePath, bindings: Vector[NamespaceBinding[TScopeValue]])

final case class NamespaceBinding[+TScopeValue](name: String, accessModifier: AccessModifierGlobal, namespaceElement: TScopeValue)
