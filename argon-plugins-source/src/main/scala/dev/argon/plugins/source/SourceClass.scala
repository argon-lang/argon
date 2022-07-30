package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{ClassDeclarationStmt, IdentifierExpr}
import zio.*
import scala.reflect.TypeTest

object SourceClass {

  def make[TOwner]
  (ctx: Context)
  (exprConverter2: ExpressionConverter with HasContext[ctx.type])
  (outerEnv: exprConverter2.Env)
  (classOwner: TOwner & ArClassC.Ownership[ctx.type])
  (stmt: ClassDeclarationStmt)
    : ctx.Comp[ArClassC with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[TOwner]] =
    for {
      traitId <- UniqueIdentifier.make

      innerEnvCell <- MemoCell.make[ctx.Env, ctx.Error, exprConverter2.Env]
      sigCell <-
        MemoCell.make[ctx.Env, ctx.Error, Signature[
          ctx.ExprContext.WrapExpr,
          (
            ctx.ExprContext.WrapExpr,
            Option[ctx.ExprContext.ArExpr[ctx.ExprContext.ExprConstructor.ClassType]],
            Seq[ctx.ExprContext.ArExpr[ctx.ExprContext.ExprConstructor.TraitType]],
          ),
        ]]
      methodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[OwnedByClassC[ctx.type, classOwner.type]]]]]
      staticMethodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[OwnedByClassStaticC[ctx.type, classOwner.type]]]]]
      ctorsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[ClassConstructorC with HasContext[ctx.type]]]

    } yield new ArClassC with MethodCreationHelper {
      override val owner: classOwner.type = classOwner
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = traitId

      override type IsDeclaration = true

      protected override val exprConverter: exprConverter2.type = exprConverter2

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}


      override def isAbstract: Boolean = stmt.modifiers.exists { _.value == parser.AbstractModifier }

      override def classMessageSource: DiagnosticSource = DiagnosticSource.Location(stmt.name.location)

      override def signature: Comp[Signature[WrapExpr, ClassResult]] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createClassResult(context)(exprConverter)(stmt)
          )
            .flatMap { (sig, env) =>
              SignatureUtil.resolveHolesSig(context)(exprConverter)(env)(exprConverter.classSigHandler)(sig)
                .map { case (sig, _) => sig: Signature[WrapExpr, ClassResult] }
            }
        )

      override def innerEnv: Comp[exprConverter.Env] =
        EnvHelper.createInnerEnv(exprConverter)(innerEnvCell)(outerEnv)(this)(signature)

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

      override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[true] with HasOwner[OwnedByClass[owner.type]]]]] =
        methodsCell.get {
          val body2 = filteredBody[parser.MethodDeclarationStmt](stmt.instanceBody)
          buildMethods[OwnedByClass[owner.type]](OwnedByClassC.apply)(body2)
        }

      override def staticMethods
        : Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[true] with HasOwner[OwnedByClassStatic[owner.type]]]]] =
        staticMethodsCell.get(buildMethods[OwnedByClassStatic[owner.type]](OwnedByClassStaticC.apply)(stmt.body))

      override def constructors: Comp[Seq[ClassConstructor]] =
        ctorsCell.get {
          ZIO.foreach(filteredBody[parser.ClassConstructorDeclarationStmt](stmt.instanceBody)) {
            case WithSource(ctorDecl: parser.ClassConstructorDeclarationStmt, _) =>
              for {
                access <- AccessUtil.parse(ctorDecl.modifiers)
                ctor <- SourceClassConstructor.make(context)(exprConverter)(innerEnv)(this, access)(ctorDecl)
              } yield ctor

            case _ => ???
          }
        }

      override def fields: Comp[Seq[context.ExprContext.MemberVariable]] = ???
    }

}
