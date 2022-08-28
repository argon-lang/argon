package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.*
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import zio.*

trait MethodCreationHelper extends Definition {
  val context: Context
  import context.Comp

  protected val exprConverter: ExpressionConverter & HasContext[context.type]
  import exprConverter.Env

  protected def innerEnv: Comp[Env]

  protected final def buildMethods[TMethodOwner <: ArMethodC.Ownership[context.type]]
    (createMethodOwner: (this.type, Option[IdentifierExpr], AccessModifier) => TMethodOwner)
    (body: Vector[WithSource[parser.Stmt]])
    : Comp[Map[Option[IdentifierExpr], Seq[ArMethodC & HasContext[context.type] & HasDeclaration[true] & HasOwner[TMethodOwner]]]] =
    ZIO.foreach(body) {
      case WithSource(stmt: parser.MethodDeclarationStmt, _) =>
        buildMethod(createMethodOwner)(stmt)

      case _ => ???
    }
      .map { methods =>
        methods.groupMap(_._1)(_._2)
      }

  private def buildMethod[TMethodOwner <: ArMethodC.Ownership[context.type]]
    (createMethodOwner: (this.type, Option[IdentifierExpr], AccessModifier) => TMethodOwner)
    (methodDecl: parser.MethodDeclarationStmt)
    : Comp[(Option[IdentifierExpr], ArMethodC & HasContext[context.type] & HasDeclaration[true] & HasOwner[TMethodOwner])] =
    for {
      access <- AccessUtil.parse(methodDecl.modifiers)
      owner = createMethodOwner(this, methodDecl.name.value, access)
      innerEnv2 <- innerEnv
      method <- SourceMethod.make(context)(exprConverter)(innerEnv2)(owner)(methodDecl)
    } yield (methodDecl.name.value, method)

  end buildMethod

}
