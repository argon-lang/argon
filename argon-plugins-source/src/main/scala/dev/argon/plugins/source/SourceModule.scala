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
  def make
  (platforms: PlatformPluginSet)
  (ctx: platforms.ContextOnlyIncluding)
  (tn: TubeName, p: ModulePath)
  (tubeImporter: TubeImporter & HasContext[ctx.type])
  (sourceCode: ArgonSourceCodeResource[ctx.Error], platformOptions: platforms.PlatformOptions[ctx.Error])
  : ctx.Comp[ArModuleC & HasContext[ctx.type]] =
    for
      exportMapCell <- MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]]]
    yield new ArModuleC {
      override val context: ctx.type = ctx
      import context.Scopes.GlobalScopeBuilder

      override def tubeName: TubeName = tn
      override def path: ModulePath = p

      override def allExports(reexportingModules: Set[ModuleName]): Comp[Map[Option[IdentifierExpr], Seq[ModuleExport]]] =
        exportMapCell.get(loadExports(reexportingModules))

      override def getExports(reexportingModules: Set[ModuleName])(id: Option[IdentifierExpr]): Comp[Option[Seq[ModuleExport]]] =
        allExports(reexportingModules).map(_.get(id))

      private def loadExports(reexportingModules: Set[ModuleName]): Comp[Map[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]]] =
        if reexportingModules.contains(ModuleName(tubeName, path)) then
          ???
        else
          sourceCode.parsed
            .runFoldZIO((GlobalScopeBuilder.empty(tubeImporter, this), Map.empty[Option[IdentifierExpr], Seq[ModuleExportC[ctx.type]]])) {
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
              f <- SourceFunction.make(context)(scope, createRefFactory(funcDecl.name.value))(funcDecl)
            yield (scope, Map(funcDecl.name.value -> Seq(ModuleExportC.Function(f))))

          case recordDecl: ast.RecordDeclarationStmt =>
            for
              r <- SourceRecord.make(context)(scope, createRefFactory(Some(recordDecl.name.value)))(recordDecl)
            yield (scope, Map(Some(recordDecl.name.value) -> Seq(ModuleExportC.Record(r))))

          case ast.ExportStmt(fromImport) =>
            ImportUtil.getModuleExports(context)(tubeImporter)(reexportingModules + ModuleName(tubeName, path))(tubeName, path)(fromImport)
              .map(exports => (scope, exports.map((k, v) => Some(k) -> v.map(ModuleExportC.Exported.apply))))

          case _ =>
            scala.Console.err.println(stmt.value.getClass)
            ???
        }

      private def createRefFactory(name: Option[IdentifierExpr]): ExternFactory & HasContext[context.type] =
        new ExternFactory {
          override val context: ctx.type = ctx

          override def getImportSpecifier(sig: ErasedSignature): ImportSpecifier =
            ImportSpecifier(
              tube = tn,
              module = p,
              name = name,
              signature = sig,
            )

          override def getExternFunctionImplementation(name: String): Comp[Option[context.implementations.ExternFunctionImplementation]] =
            platforms.externFunction.loadExtern(platformOptions)(name).value

          override def defineFunctionReference(sig: ErasedSignature): Comp[context.implementations.FunctionReference] =
            platforms.externFunction.defineReference(platformOptions)(DefinitionInfo.Global(tn, p, name, sig))

          override def defineRecordReference(sig: ErasedSignature): Comp[context.implementations.RecordReference] =
            platforms.externRecord.defineReference(platformOptions)(DefinitionInfo.Global(tn, p, name, sig))
        }
    }
}
