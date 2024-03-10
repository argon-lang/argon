package dev.argon.plugins.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.util.*
import zio.*
import cats.*
import cats.implicits.given
import dev.argon.plugin.PlatformPluginSet
import zio.interop.catz.given

object SourceModule {
  def make(platforms: PlatformPluginSet)(ctx: platforms.ContextOnlyIncluding)(tn: TubeName, p: ModulePath)(sourceCode: ArgonSourceCodeResource[ctx.Error], platformOptions: platforms.PlatformOptions[ctx.Error]): ctx.Comp[ArModuleC & HasContext[ctx.type]] =
    for
      exportMapCell <- MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]]]
    yield new ArModuleC {
      override val context: ctx.type = ctx
      import context.Scopes.GlobalScopeBuilder

      override def tubeName: TubeName = tn
      override def path: ModulePath = p

      override def allExports: Comp[Map[Option[IdentifierExpr], Seq[ModuleExport]]] =
        exportMapCell.get(loadExports)

      override def getExports(id: Option[IdentifierExpr]): Comp[Option[Seq[ModuleExport]]] =
        allExports.map(_.get(id))

      private def loadExports: Comp[Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]]] =
        sourceCode.parsed
          .runFoldZIO((GlobalScopeBuilder.empty(this), Map.empty[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]])) {
            case ((scope, acc), stmt) =>
              processStmt(stmt, scope)
                .map { (scope, defs) =>
                  (scope, acc |+| defs)
                }
          }
          .map { (_, acc) => acc }

      private def processStmt(stmt: WithSource[ast.Stmt], scope: GlobalScopeBuilder): Comp[(GlobalScopeBuilder, Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]])] =
        stmt.value match {
          case importStmt: ast.ImportStmt =>
            ZIO.succeed((scope.addImport(importStmt), Map.empty))

          case funcDecl: ast.FunctionDeclarationStmt =>
            for
              f <- SourceFunction.make(context)(scope, createRefFactory(funcDecl.name.value))(funcDecl)
            yield (scope, Map(funcDecl.name.value -> Seq(ModuleExportC.Function(f))))

          case _ => ???
        }

      private def createRefFactory(name: Option[IdentifierExpr]): ExternFactory & HasContext[context.type] =
        new ExternFactory {
          override val context: ctx.type = ctx

          override def getExternFunctionImplementation(name: String): Comp[Option[context.implementations.ExternFunctionImplementation]] =
            platforms.externFunction.loadExtern(platformOptions)(name).value

          override def defineFunctionReference(sig: ErasedSignature): Comp[context.implementations.FunctionReference] =
            platforms.externFunction.defineReference(platformOptions)(DefinitionInfo.Global(tn, p, name, sig))
        }
    }
}
