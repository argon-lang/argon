package com.mi3software.argon.compiler


sealed trait MethodDeclarationInfo[TContext <: Context] {
  val name: String
  val accessModifier: AccessModifier
  val instanceType: ArTypeTemplate[TContext]
}

final case class MethodDeclarationInfoDeclaration[TContext <: Context]
(
  name: String,
  accessModifier: AccessModifier,
  instanceType: ArTypeTemplateDeclaration[TContext]
) extends MethodDeclarationInfo[TContext]

final case class MethodDeclarationInfoReference[TContext <: Context]
(
  name: String,
  accessModifier: AccessModifier,
  instanceType: ArTypeTemplateReference[TContext]
) extends MethodDeclarationInfo[TContext]
