package com.mi3software.argon.compiler

trait ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext <: Context, +TResult] {
  def visitTrait: ArTypeTemplateTraitDeclaration[TContext] => TResult
  def visitClass: ArTypeTemplateClassDeclaration[TContext] => TResult
}

trait ArTypeTemplateDeclarationVisitor[TContext <: Context, +TResult] extends ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] {
  def visitConstructorInstance: ArTypeTemplateDataConstructorInstanceDeclaration[TContext] => TResult
}

trait ArTypeTemplateReferenceWithMetaTypeVisitor[TContext <: Context, +TResult] {
  def visitTrait: ArTypeTemplateTraitReference[TContext] => TResult
  def visitClass: ArTypeTemplateClassReference[TContext] => TResult
}

trait ArTypeTemplateReferenceVisitor[TContext <: Context, +TResult] extends ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] {
  def visitConstructorInstance: ArTypeTemplateDataConstructorInstanceReference[TContext] => TResult
}

trait ArTypeTemplateWithMetaTypeVisitor[TContext <: Context, +TResult] extends ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] with ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] {
  def visitTrait: ArTypeTemplateTrait[TContext] => TResult
  def visitClass: ArTypeTemplateClass[TContext] => TResult
}

trait ArTypeTemplateVisitor[TContext <: Context, +TResult] extends ArTypeTemplateDeclarationVisitor[TContext, TResult] with ArTypeTemplateReferenceVisitor[TContext, TResult] with ArTypeTemplateWithMetaTypeVisitor[TContext, TResult] {
  def visitConstructorInstance: ArTypeTemplateDataConstructorInstance[TContext] => TResult
}

sealed trait ArTypeTemplate[TContext <: Context] {
  def declaration: TypeDeclarationInfo[TContext]
  def visit[TResult]: ArTypeTemplateVisitor[TContext, TResult] => TResult
}

sealed trait ArTypeTemplateDeclaration[TContext <: Context] extends ArTypeTemplate[TContext] {
  override def visit[TResult]: ArTypeTemplateDeclarationVisitor[TContext, TResult] => TResult
}

sealed trait ArTypeTemplateReference[TContext <: Context] extends ArTypeTemplate[TContext] {
  override def visit[TResult]: ArTypeTemplateReferenceVisitor[TContext, TResult] => TResult
}

sealed trait ArTypeTemplateWithMetaType[TContext <: Context] extends ArTypeTemplate[TContext] {
  def metaType: MetaClass[TContext, ArClass[TContext]]
  override def visit[TResult]: ArTypeTemplateWithMetaTypeVisitor[TContext, TResult] => TResult
}

sealed trait ArTypeTemplateDeclarationWithMetaType[TContext <: Context] extends ArTypeTemplateDeclaration[TContext] with ArTypeTemplateWithMetaType[TContext] {
  override def metaType: MetaClass[TContext, ArClassDeclaration[TContext]]
  override def visit[TResult]: ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] => TResult
}

sealed trait ArTypeTemplateReferenceWithMetaType[TContext <: Context] extends ArTypeTemplateReference[TContext] with ArTypeTemplateWithMetaType[TContext] {
  override def metaType: MetaClass[TContext, ArClassReference[TContext]]
  override def visit[TResult]: ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] => TResult
}

sealed trait ArTypeTemplateTrait[TContext <: Context] extends ArTypeTemplateWithMetaType[TContext] {
  val arTrait: ArTrait[TContext]

  override def declaration: TypeDeclarationInfo[TContext] = arTrait.declaration
}

object ArTypeTemplateTrait {
  def apply[TContext <: Context](arTrait: ArTrait[TContext]): ArTypeTemplateTrait[TContext] = arTrait match {
    case arTrait: ArTraitDeclaration[TContext] => ArTypeTemplateTraitDeclaration(arTrait)
    case arTrait: ArTraitReference[TContext] => ArTypeTemplateTraitReference(arTrait)
  }
}

sealed trait ArTypeTemplateTraitDeclaration[TContext <: Context] extends ArTypeTemplateTrait[TContext] with ArTypeTemplateDeclarationWithMetaType[TContext] {
  override val arTrait: ArTraitDeclaration[TContext]
  override def metaType: MetaClass[TContext, ArClassDeclaration[TContext]] = arTrait.metaType
  override def visit[TResult]: ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] => TResult
}

object ArTypeTemplateTraitDeclaration {
  def apply[TContext <: Context](arTrait2: ArTraitDeclaration[TContext]): ArTypeTemplateTraitDeclaration[TContext] = new ArTypeTemplateTraitDeclaration[TContext] {
    override val arTrait: ArTraitDeclaration[TContext] = arTrait2
    override def visit[TResult]: ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] => TResult = _.visitTrait(this)
  }
}

sealed trait ArTypeTemplateTraitReference[TContext <: Context] extends ArTypeTemplateTrait[TContext] with ArTypeTemplateReferenceWithMetaType[TContext] {
  override val arTrait: ArTraitReference[TContext]
  override def metaType: MetaClass[TContext, ArClassReference[TContext]] = arTrait.metaType
  override def visit[TResult]: ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] => TResult
}

object ArTypeTemplateTraitReference {
  def apply[TContext <: Context](arTrait2: ArTraitReference[TContext]): ArTypeTemplateTraitReference[TContext] = new ArTypeTemplateTraitReference[TContext] {
    override val arTrait: ArTraitReference[TContext] = arTrait2
    override def visit[TResult]: ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] => TResult = _.visitTrait(this)
  }
}

sealed trait ArTypeTemplateClass[TContext <: Context] extends ArTypeTemplateWithMetaType[TContext] {
  val arClass: ArClass[TContext]

  override def declaration: TypeDeclarationInfo[TContext] = arClass.declaration
}

object ArTypeTemplateClass {
  def apply[TContext <: Context](arClass: ArClass[TContext]): ArTypeTemplateClass[TContext] = arClass match {
    case arClass: ArClassDeclaration[TContext] => ArTypeTemplateClassDeclaration(arClass)
    case arClass: ArClassReference[TContext] => ArTypeTemplateClassReference(arClass)
  }
}

sealed trait ArTypeTemplateClassDeclaration[TContext <: Context] extends ArTypeTemplateClass[TContext] with ArTypeTemplateDeclarationWithMetaType[TContext] {
  override val arClass: ArClassDeclaration[TContext]
  override def metaType: MetaClass[TContext, ArClassDeclaration[TContext]] = arClass.metaType
  override def visit[TResult]: ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] => TResult
}

object ArTypeTemplateClassDeclaration {
  def apply[TContext <: Context](arClass2: ArClassDeclaration[TContext]): ArTypeTemplateClassDeclaration[TContext] = new ArTypeTemplateClassDeclaration[TContext] {
    override val arClass: ArClassDeclaration[TContext] = arClass2
    override def visit[TResult]: ArTypeTemplateDeclarationWithMetaTypeVisitor[TContext, TResult] => TResult = _.visitClass(this)
  }
}

sealed trait ArTypeTemplateClassReference[TContext <: Context] extends ArTypeTemplateClass[TContext] with ArTypeTemplateReferenceWithMetaType[TContext] {
  override val arClass: ArClassReference[TContext]
  override def metaType: MetaClass[TContext, ArClassReference[TContext]] = arClass.metaType
  override def visit[TResult]: ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] => TResult
}

object ArTypeTemplateClassReference {
  def apply[TContext <: Context](arClass2: ArClassReference[TContext]): ArTypeTemplateClassReference[TContext] = new ArTypeTemplateClassReference[TContext] {
    override val arClass: ArClassReference[TContext] = arClass2
    override def visit[TResult]: ArTypeTemplateReferenceWithMetaTypeVisitor[TContext, TResult] => TResult = _.visitClass(this)
  }
}

sealed trait ArTypeTemplateDataConstructorInstance[TContext <: Context] extends ArTypeTemplate[TContext] {
  val ctor: DataConstructor[TContext]

  override def declaration: TypeDeclarationInfo[TContext] = ctor.declaration
}

object ArTypeTemplateDataConstructorInstance {
  def apply[TContext <: Context](ctor: DataConstructor[TContext]): ArTypeTemplateDataConstructorInstance[TContext] = ctor match {
    case ctor: DataConstructorDeclaration[TContext] => ArTypeTemplateDataConstructorInstanceDeclaration(ctor)
    case ctor: DataConstructorReference[TContext] => ArTypeTemplateDataConstructorInstanceReference(ctor)
  }
}

sealed trait ArTypeTemplateDataConstructorInstanceDeclaration[TContext <: Context] extends ArTypeTemplateDataConstructorInstance[TContext] with ArTypeTemplateDeclaration[TContext] {
  override val ctor: DataConstructorDeclaration[TContext]
}

object ArTypeTemplateDataConstructorInstanceDeclaration {
  def apply[TContext <: Context](ctor2: DataConstructorDeclaration[TContext]): ArTypeTemplateDataConstructorInstanceDeclaration[TContext] = new ArTypeTemplateDataConstructorInstanceDeclaration[TContext] {
    override val ctor: DataConstructorDeclaration[TContext] = ctor2
    override def visit[TResult]: ArTypeTemplateDeclarationVisitor[TContext, TResult] => TResult = _.visitConstructorInstance(this)
  }
}

sealed trait ArTypeTemplateDataConstructorInstanceReference[TContext <: Context] extends ArTypeTemplateDataConstructorInstance[TContext] with ArTypeTemplateReference[TContext] {
  override val ctor: DataConstructorReference[TContext]
}

object ArTypeTemplateDataConstructorInstanceReference {
  def apply[TContext <: Context](ctor2: DataConstructorReference[TContext]): ArTypeTemplateDataConstructorInstanceReference[TContext] = new ArTypeTemplateDataConstructorInstanceReference[TContext] {
    override val ctor: DataConstructorReference[TContext] = ctor2
    override def visit[TResult]: ArTypeTemplateReferenceVisitor[TContext, TResult] => TResult = _.visitConstructorInstance(this)
  }
}
