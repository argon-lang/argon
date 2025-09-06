package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.util.*
import zio.*
import cats.*
import cats.implicits.given
import zio.interop.catz.given

private[source] object SourceModule {
  def make
  (ctx: Context { type Error >: SourceError })
  (tn: TubeName, p: ModulePath)
  (sourceCode: ArgonSourceCodeResource[ctx.Error])
  (using TubeImporter & HasContext[ctx.type], ExternProvider & HasContext[ctx.type])
  : ctx.Comp[ArModuleC & HasContext[ctx.type]] =
    for
      exportMapCell <- MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]]]
      moduleDef <- sourceCode.parsed
    yield new ArModuleC {
      override val context: ctx.type = ctx
      import context.Scopes.GlobalScopeBuilder

      override def tubeName: TubeName = tn
      override def path: ModulePath = ModulePath(moduleDef.modulePath)

      override def allExports(reexportingModules: Set[ModuleName]): Comp[Map[Option[IdentifierExpr], Seq[ModuleExport]]] =
        exportMapCell.get(loadExports(reexportingModules))

      override def getExports(reexportingModules: Set[ModuleName])(id: Option[IdentifierExpr]): Comp[Option[Seq[ModuleExport]]] =
        allExports(reexportingModules).map(_.get(id))

      private def loadExports(reexportingModules: Set[ModuleName]): Comp[Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]]] =
        if reexportingModules.contains(ModuleName(tubeName, path)) then
          ???
        else
          ZIO.foldLeft(moduleDef.stmts)((GlobalScopeBuilder.empty(this), Map.empty[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]])) {
            case ((scope, acc), stmt) =>
              processStmt(reexportingModules)(stmt, scope)
                .map { (scope, defs) =>
                  (scope, acc |+| defs)
                }
          }
            .map { (_, acc) => acc }

      private def processStmt(reexportingModules: Set[ModuleName])(stmt: WithSource[ast.Stmt], scope: GlobalScopeBuilder): Comp[(GlobalScopeBuilder, Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]])] =
        stmt.value match {
          case importStmt: ast.ImportStmt =>
            ZIO.succeed((scope.addImport(WithLocation(importStmt, stmt.location)), Map.empty))

          case funcDecl: ast.FunctionDeclarationStmt =>
            for
              f <- SourceFunction.make(context)(scope, createImportFactory(funcDecl.name.value))(funcDecl)
            yield (scope, Map(funcDecl.name.value -> Seq(ModuleExportC.Function(f))))

          case recordDecl: ast.RecordDeclarationStmt =>
            for
              r <- SourceRecord.make(context)(scope, createImportFactory(Some(recordDecl.name.value)))(recordDecl)
            yield (scope, Map(Some(recordDecl.name.value) -> Seq(ModuleExportC.Record(r))))

          case ast.ExportStmt(fromImport) =>
            ImportUtil.getModuleExports(context)(reexportingModules + ModuleName(tubeName, path))(tubeName, path)(fromImport)
              .map(exports => (scope, exports.map((k, v) => Some(k) -> v.map(ModuleExportC.Exported.apply))))

          case _ =>
            scala.Console.err.println(stmt.value.getClass)
            ???
        }

      private def createImportFactory(name: Option[IdentifierExpr]): ImportFactory =
        new ImportFactory {
          override def getImportSpecifier(sig: ErasedSignature): ImportSpecifier =
            ImportSpecifier(
              tube = tn,
              module = p,
              name = name,
              signature = sig,
            )
        }
    }
}
