package com.mi3software.argon.compiler

import com.mi3software.argon.util.{FileSpec, NamespacePath}

sealed trait TypeDeclarationInfo[TContext <: Context] {
  def declaringModule: ArModule[TContext]
  def fileSpec: FileSpec
}





sealed trait ClassDeclarationInfo[TContext <: Context] extends TypeDeclarationInfo[TContext]
sealed trait ClassDeclarationInfoDeclaration[TContext <: Context] extends ClassDeclarationInfo[TContext]
sealed trait ClassDeclarationInfoReference[TContext <: Context] extends ClassDeclarationInfo[TContext]

final case class ClassDeclarationInNamespace[TContext <: Context](declaringModule: ArModule[TContext], fileSpec: FileSpec, namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends ClassDeclarationInfoDeclaration[TContext] with ClassDeclarationInfoReference[TContext]

sealed trait ClassMetaTypeDeclarationForTrait[TContext <: Context] extends ClassDeclarationInfo[TContext] {
  val outerTrait: ArTrait[TContext]

  override def declaringModule: ArModule[TContext] = outerTrait.declaration.declaringModule
  override def fileSpec: FileSpec = outerTrait.declaration.fileSpec
}

final case class ClassMetaTypeDeclarationForTraitDeclaration[TContext <: Context](outerTrait: ArTraitDeclaration[TContext]) extends ClassMetaTypeDeclarationForTrait[TContext] with ClassDeclarationInfoDeclaration[TContext]
final case class ClassMetaTypeDeclarationForTraitReference[TContext <: Context](outerTrait: ArTraitReference[TContext]) extends ClassMetaTypeDeclarationForTrait[TContext] with ClassDeclarationInfoReference[TContext]


sealed trait ClassMetaTypeDeclarationForClass[TContext <: Context] extends ClassDeclarationInfo[TContext] {
  val outerClass: ArClass[TContext]

  override def declaringModule: ArModule[TContext] = outerClass.declaration.declaringModule
  override def fileSpec: FileSpec = outerClass.declaration.fileSpec
}

final case class ClassMetaTypeDeclarationForClassDeclaration[TContext <: Context](outerClass: ArClassDeclaration[TContext]) extends ClassMetaTypeDeclarationForClass[TContext] with ClassDeclarationInfoDeclaration[TContext]
final case class ClassMetaTypeDeclarationForClassReference[TContext <: Context](outerClass: ArClassReference[TContext]) extends ClassMetaTypeDeclarationForClass[TContext] with ClassDeclarationInfoReference[TContext]




sealed trait TraitDeclarationInfo[TContext <: Context] extends TypeDeclarationInfo[TContext]
final case class TraitDeclarationInNamespace[TContext <: Context](declaringModule: ArModule[TContext], fileSpec: FileSpec, namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends TraitDeclarationInfo[TContext]

final case class ConstructorDeclarationInfo[TContext <: Context](declaringModule: ArModule[TContext], fileSpec: FileSpec, namespace: NamespacePath, name: String, accessModifier: AccessModifierGlobal) extends TypeDeclarationInfo[TContext]