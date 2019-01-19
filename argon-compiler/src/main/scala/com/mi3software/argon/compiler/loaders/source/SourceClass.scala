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

private[compiler] object SourceClass extends AccessModifierHelpers {

  final case class GroupedStaticStatements
  (
    staticMethods: Vector[WithSource[parser.MethodDeclarationStmt]],
    classCtors: Vector[WithSource[parser.ClassConstructorDeclarationStmt]],
  )

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
    fields: Vector[WithSource[parser.FieldDeclarationStmt]],
  )

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (stmt: ClassDeclarationStmt)
  (desc: ClassDescriptor)
  : TComp[ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier]] = for {
    groupedStaticCache <- Compilation[TComp].createCache[GroupedStaticStatements]
    groupedInstCache <- Compilation[TComp].createCache[GroupedInstanceStatements]

    fieldCache <- Compilation[TComp].createCache[Vector[context2.typeSystem.Variable[FieldDescriptor]]]
    methodCache <- Compilation[TComp].createCache[Vector[ArMethod[context2.type, DeclarationPayloadSpecifier]]]

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
        stmt.body.foldLeftM(GroupedStaticStatements(Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)).point[TComp]

          case (group, WithSource(stmt: parser.ClassConstructorDeclarationStmt, location)) =>
            group.copy(classCtors = group.classCtors :+ WithSource(stmt, location)).point[TComp]

          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    private val groupedInst =
      groupedInstCache(
        stmt.instanceBody.foldLeftM(GroupedInstanceStatements(Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(methods = group.methods :+ WithSource(stmt, location)).point[TComp]

          case (group, WithSource(stmt: parser.FieldDeclarationStmt, location)) =>
            group.copy(fields = group.fields :+ WithSource(stmt, location)).point[TComp]

          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    override val signature: TComp[Signature[ArClass.ResultInfo]] = ??? : TComp[Signature[ArClass.ResultInfo]]

    override val fields: TComp[Vector[context.typeSystem.Variable[FieldDescriptor]]] =
      fieldCache(groupedInst.flatMap { inst =>
        inst.fields.traverse { field =>
          field.value.name match {
            case Some(fieldName) =>
              ExpressionConverter.convertTypeExpression(context)(env(context)(EffectInfo.pure, descriptor))(field.value.fieldType).map { fieldType =>
                context.typeSystem.Variable(
                  FieldDescriptor(descriptor, fieldName),
                  VariableName.Normal(fieldName),
                  Mutability.fromIsMutable(field.value.isMutable),
                  fieldType
                )
              }

            case None =>
              Compilation[TComp].forErrors(CompilationError.FieldMustHaveName(CompilationMessageSource.SourceFile(env.fileSpec, field.location)))
          }
        }
      })

    override val methods: TComp[Vector[ArMethod[context2.type, DeclarationPayloadSpecifier]]] =
      methodCache(groupedInst.flatMap { inst =>
        inst.methods.zipWithIndex.traverse { case (method, i) =>
          parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
            val memberName = method.value.name match {
              case Some(name) => MemberName.Normal(name)
              case None => MemberName.Unnamed(i)
            }

            val desc = MethodDescriptor(descriptor, memberName, modifiers)
            SourceMethod(context)(env)(method.value, method.location)(desc)
          }
        }
      })

    override val staticMethods: TComp[Vector[ArMethod[context2.type, DeclarationPayloadSpecifier]]] =
      methodCache(groupedStatic.flatMap { statics =>
        statics.staticMethods.zipWithIndex.traverse { case (method, i) =>
          parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
            val memberName = method.value.name match {
              case Some(name) => MemberName.Normal(name)
              case None => MemberName.Unnamed(i)
            }

            val desc = MethodDescriptor(ClassObjectDescriptor(descriptor), memberName, modifiers)
            SourceMethod(context)(env)(method.value, method.location)(desc)
          }
        }
      })

    override val classConstructors: TComp[Vector[ClassConstructor[context2.type, DeclarationPayloadSpecifier]]] = for {
      statics <- groupedStatic
    } yield ???

    override val payload: Unit = ()
  }

}
