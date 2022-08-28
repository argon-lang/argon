package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.util.{*, given}
import dev.argon.compiler.signature.Signature
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.parser
import dev.argon.parser.{ClassDeclarationStmt, IdentifierExpr}
import zio.*

import scala.reflect.TypeTest

object SourceClass {

  def make[TOwner]
  (ctx: Context)
  (exprConverter2: ExpressionConverter & HasContext[ctx.type])
  (vtableBuilder: VTableBuilder[ctx.type])
  (outerEnv: exprConverter2.Env)
  (classOwner: TOwner & ArClassC.Ownership[ctx.type])
  (stmt: ClassDeclarationStmt)
    : ctx.Comp[ArClassC & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[TOwner]] =
    for {
      traitId <- UniqueIdentifier.make

      innerEnvCell <- MemoCell.make[ctx.Env, ctx.Error, exprConverter2.Env]
      sigCell <-
        MemoCell.make[ctx.Env, ctx.Error,
          (
            Signature[
              ctx.ExprContext.WrapExpr,
              ctx.ExprContext.ClassResult,
            ],
            exprConverter2.Env
          )
        ]
      methodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[OwnedByClassC[ctx.type, classOwner.type]]]]]
      staticMethodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[OwnedByClassStaticC[ctx.type, classOwner.type]]]]]
      ctorsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[ClassConstructorC & HasContext[ctx.type] & HasDeclaration[true]]]
      fieldsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[ctx.ExprContext.MemberVariable]]

    } yield new ArClassC with MethodCreationHelper {
      override val owner: classOwner.type = classOwner
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = traitId

      override type IsDeclaration = true

      protected override val exprConverter: exprConverter2.type = exprConverter2

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor, ClassResult}


      override def isAbstract: Boolean = stmt.modifiers.exists { _.value == parser.AbstractModifier }
      override def isSealed: Boolean = stmt.modifiers.exists { _.value == parser.SealedModifier }
      override def isOpen: Boolean = stmt.modifiers.exists { modifier =>
        modifier.value == parser.OpenModifier ||
          modifier.value == parser.SealedModifier ||
          modifier.value == parser.AbstractModifier
      }

      override def classMessageSource: DiagnosticSource = DiagnosticSource.Location(stmt.name.location)



      private def sigEnv: Comp[(Signature[WrapExpr, ClassResult], exprConverter.Env)] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createClassResult(context)(exprConverter)(stmt)
          )
        )

      override def signature: Comp[Signature[WrapExpr, ClassResult]] =
        sigEnv.map { _._1 }

      private def innerEnvNoFields: Comp[exprConverter.Env] =
        sigEnv.map { _._2 }

      override def innerEnv: Comp[exprConverter.Env] =
        for
          env <- innerEnvNoFields
          fields <- this.fields
        yield env.withScope(_.addVariables(fields.map(ExprToHolesConverter(context)(exprConverter.exprContext).processVariable)))

      private def isValidClassStmt(s: parser.Stmt): Boolean =
        s match {
          case _: parser.MethodDeclarationStmt |
              _: parser.ClassConstructorDeclarationStmt |
              _: parser.FieldDeclarationStmt => true

          case _ => false
        }

      private def filteredBody[T <: parser.Stmt](body: Vector[WithSource[parser.Stmt]])(using TypeTest[parser.Stmt, T])
        : Vector[WithSource[parser.Stmt]] =
        body.filter {
          case WithSource(_: T, _) => true
          case WithSource(s, _) => !isValidClassStmt(s)
        }

      override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasDeclaration[true] & HasOwner[OwnedByClass[owner.type]]]]] =
        methodsCell.get {
          val body2 = filteredBody[parser.MethodDeclarationStmt](stmt.instanceBody)
          buildMethods[OwnedByClass[owner.type]](OwnedByClassC.apply)(body2)
        }

      override def staticMethods
        : Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasDeclaration[true] & HasOwner[OwnedByClassStatic[owner.type]]]]] =
        staticMethodsCell.get(buildMethods[OwnedByClassStatic[owner.type]](OwnedByClassStaticC.apply)(stmt.body))

      override def constructors: Comp[Seq[ClassConstructor & HasDeclaration[true]]] =
        ctorsCell.get {
          ZIO.collect(stmt.instanceBody) {
            case WithSource(ctorDecl: parser.ClassConstructorDeclarationStmt, _) =>
              (
                for {
                  access <- AccessUtil.parse(ctorDecl.modifiers)
                  env <- innerEnvNoFields
                  ctor <- SourceClassConstructor.make(context)(exprConverter)(env)(OwnedByClassC(this, None, access))(ctorDecl)
                } yield ctor
              ).asSomeError

            case _ => ZIO.fail(None)
          }
        }

      override def fields: Comp[Seq[context.ExprContext.MemberVariable]] =
        fieldsCell.get(
          ZIO.collect(stmt.instanceBody) {
            case WithSource(field: parser.FieldDeclarationStmt, _) =>
              val opt = exprConverter.ExprOptions(
                purity = true,
                accessToken = SignatureUtil.createAccessToken(exprConverter)(this),
                allowAbstractConstructorCall = false,
                allowErased = true,
                postconditions = None,
              )

              (
                for
                  env <- innerEnvNoFields
                  varType <- exprConverter.convertExpr(field.fieldType).check(env, opt, exprConverter.anyType)
                  resolvedVarType <- exprConverter.resolveHoles(varType.env, varType.expr)
                yield context.ExprContext.MemberVariable(this, resolvedVarType._1, Some(field.name), isMutable = field.isMutable)
              ).asSomeError

            case _ => ZIO.fail(None)
          }
        )

      override def vtable: Comp[context.VT.VTable] =
        vtableBuilder.fromClass(this)

      override def vtableDiff: Comp[context.VT.VTable] =
        vtableBuilder.diffFromClass(this)

      override def validate: Comp[Unit] =
        signature.flatMap { sig =>
          val sigResult = sig.unsubstitutedResult

          for
            baseClassOpt <- sigResult.baseClass
            baseTraits <- sigResult.baseTraits

            _ <- ZIO.foreachDiscard(baseClassOpt) { baseClassType =>
              val baseClass = baseClassType.constructor.arClass

              ZIO.fail(DiagnosticError.NonOpenClassExtended(DiagnosticSource.Location(stmt.name.location))).when(!baseClass.isOpen) *>
                ZIO.fail(DiagnosticError.SealedClassExtended(DiagnosticSource.Location(stmt.name.location))).when(baseClass.isSealed && owner.module.moduleName != baseClass.owner.module.moduleName)
            }

            _ <- ZIO.foreachDiscard(baseTraits) { baseTraitType =>
              val baseTrait = baseTraitType.constructor.arTrait
              ZIO.fail(DiagnosticError.SealedTraitExtended(DiagnosticSource.Location(stmt.name.location))).when(baseTrait.isSealed && owner.module.moduleName != baseTrait.owner.module.moduleName)
            }

          yield ()
        }
    }

}
