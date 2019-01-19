package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import com.mi3software.argon.compiler.core.{ArClass, _}
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import com.mi3software.argon.parser
import com.mi3software.argon.parser.ClassDeclarationStmt
import com.mi3software.argon.util.WithSource
import scalaz._
import Scalaz._

private[compiler] object SourceClass {

  final case class GroupedStaticStatements[TContext <: Context with Singleton]
  (
    staticMethods: Vector[ArMethod[TContext, DeclarationPayloadSpecifier]],
    classCtors: Vector[ClassConstructor[TContext, DeclarationPayloadSpecifier]],
  )

  final case class GroupedInstanceStatements[TContext <: Context with Singleton]
  (
    methods: Vector[ArMethod[TContext, DeclarationPayloadSpecifier]],
    fields: Vector[parser.FieldDeclarationStmt],
  )

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (stmt: ClassDeclarationStmt)
  (desc: ClassDescriptor)
  : TComp[ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier]] = for {
    groupedStaticCache <- Compilation[TComp].createCache[GroupedStaticStatements[context2.type]]
    groupedInstCache <- Compilation[TComp].createCache[GroupedInstanceStatements[context2.type]]
  } yield new ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
    override val context: context2.type = context2
    import context.signatureContext.Signature

    override val contextProof: Leibniz[context.type, context2.type, context.type, context2.type] = Leibniz.refl

    override val descriptor: ClassDescriptor = desc

    override val isSealed: Boolean = stmt.modifiers.exists {
      case WithSource(parser.SealedModifier, _) => true
      case _ => false
    }
    override val isOpen: Boolean = stmt.modifiers.exists {
      case WithSource(parser.OpenModifier, _) => true
      case _ => false
    }
    override val isAbstract: Boolean = stmt.modifiers.exists {
      case WithSource(parser.AbstractModifier, _) => true
      case _ => false
    }

    private val groupedStatic =
      groupedStaticCache(
        stmt.body.foldLeftM(GroupedStaticStatements[context.type](Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) => ???
          case (group, WithSource(stmt: parser.ClassConstructorDeclarationStmt, location)) => ???
          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    private val groupedInst =
      groupedInstCache(
        stmt.instanceBody.foldLeftM(GroupedInstanceStatements[context.type](Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) => ???
          case (group, WithSource(stmt: parser.FieldDeclarationStmt, location)) => ???
          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    override val signature: TComp[Signature[ArClass.ResultInfo]] = ??? : TComp[Signature[ArClass.ResultInfo]]

    override lazy val methods: TComp[Vector[ArMethod[context2.type, DeclarationPayloadSpecifier]]] = for {
      inst <- groupedInst
    } yield inst.methods

    override val classConstructors: TComp[Vector[ClassConstructor[context2.type, DeclarationPayloadSpecifier]]] = for {
      statics <- groupedStatic
    } yield statics.classCtors

    override lazy val metaType: TComp[MetaClass[ArClass[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[MetaClass[ArClass[context2.type, DeclarationPayloadSpecifier]]]

    override val payload: Unit = ()
  }

}
