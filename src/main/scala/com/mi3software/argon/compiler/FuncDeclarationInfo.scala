package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

sealed trait FuncDeclarationInfo
final case class FuncDeclarationInfoInNamespace(namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends FuncDeclarationInfo
