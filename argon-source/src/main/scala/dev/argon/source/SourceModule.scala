package dev.argon.source

import dev.argon.ast
import dev.argon.ast.{DeclarationStmt, IdentifierExpr, Modifier}
import dev.argon.compiler.{AccessModifier, *}
import dev.argon.util.*
import zio.*
import cats.*
import cats.implicits.given
import zio.prelude.NonEmptyMap

private[source] object SourceModule {
  def make
  (ctx: Context { type Error >: SourceError })
  (tn: TubeName, p: ModulePath)
  (sourceCode: ArgonSourceCodeResource[ctx.Error])
  (using TubeImporter & HasContext[ctx.type], ExternProvider & HasContext[ctx.type])
  : ctx.Comp[ArModuleC & HasContext[ctx.type]] =
    for
      exportMapCell <- MemoCell.make[ctx.Env, ctx.Error, Map[IdentifierExpr, Seq[ModuleExportC[ctx.type]]]]
      moduleDef <- sourceCode.parsed
    yield new ArModuleC {
      override val context: ctx.type = ctx
      import context.Scopes.GlobalScopeBuilder

      override def tubeName: TubeName = tn
      override def path: ModulePath = ModulePath(moduleDef.modulePath)

      override def allExports(reexportingModules: Set[ModuleName]): Comp[Map[IdentifierExpr, Seq[ModuleExport]]] =
        exportMapCell.get(loadExports(reexportingModules))

      override def getExports(reexportingModules: Set[ModuleName])(id: IdentifierExpr): Comp[Option[Seq[ModuleExport]]] =
        allExports(reexportingModules).map(_.get(id))

      private def loadExports(reexportingModules: Set[ModuleName]): Comp[Map[IdentifierExpr, Seq[ModuleExportC[ctx.type]]]] =
        if reexportingModules.contains(ModuleName(tubeName, path)) then
          ???
        else
          ZIO.foldLeft(moduleDef.stmts)((GlobalScopeBuilder.empty(this), Map.empty[IdentifierExpr, Seq[ModuleExportC[ctx.type]]])) {
            case ((scope, acc), stmt) =>
              processStmt(reexportingModules)(stmt, scope)
                .map { (scope, defs) =>
                  (scope, acc |+| defs)
                }
          }
            .map { (_, acc) => acc }

      private def processStmt(reexportingModules: Set[ModuleName])(stmt: WithSource[ast.Stmt], scope: GlobalScopeBuilder): Comp[(GlobalScopeBuilder, Map[IdentifierExpr, Seq[ModuleExportC[ctx.type]]])] = {

        inline def buildBinding[DeclStmt <: DeclarationStmt, Decl](
          declStmt: DeclStmt,
          createRes: (context: Context) => (closure: DeclarationClosure & HasContext[context.type]) => DeclStmt => context.Comp[DeclarationResult[closure.Access, Decl & HasContext[context.type]]],
          createBinding: Decl & HasContext[ctx.type] => ModuleExportBindingC[ctx.type]
        ): Comp[(GlobalScopeBuilder, Map[IdentifierExpr, Seq[ModuleExportC[ctx.type]]])] =
          for
            res <- createRes(ctx)(createImportFactory(declStmt.name.value, scope.toScope))(declStmt)
          yield (scope, Map(declStmt.name.value -> Seq(ModuleExportC.Binding(res.access, createBinding(res.declaration)))))

        stmt.value match {
          case importStmt: ast.ImportStmt =>
            ZIO.succeed((scope.addImport(WithLocation(importStmt, stmt.location)), Map.empty))

          case funcDecl: ast.FunctionDeclarationStmt =>
            buildBinding(funcDecl, SourceFunction.make, ModuleExportBindingC.Function.apply)

          case recordDecl: ast.RecordDeclarationStmt =>
            buildBinding(recordDecl, SourceRecord.make, ModuleExportBindingC.Record.apply)

          case enumDecl: ast.EnumDeclarationStmt =>
            buildBinding(enumDecl, SourceEnum.make, ModuleExportBindingC.Enum.apply)

          case traitDecl: ast.TraitDeclarationStmt =>
            buildBinding(traitDecl, SourceTrait.make, ModuleExportBindingC.Trait.apply)

          case instanceDecl: ast.InstanceDeclarationStmt =>
            buildBinding(instanceDecl, SourceInstance.make, ModuleExportBindingC.Instance.apply)
            
          case ast.ExportStmt(fromImport) =>
            ImportUtil.getModuleExports(context)(reexportingModules + ModuleName(tubeName, path))(tubeName, path)(fromImport)
              .map(exports => (scope, exports.map((k, v) => k -> v.map(ModuleExportC.Exported.apply))))

          case _ =>
            scala.Console.err.println(stmt.value.getClass)
            ???
        }
      }
      end processStmt


      private def createImportFactory(name: IdentifierExpr, moduleScope: context.Scopes.Scope): DeclarationClosure & HasContext[ctx.type] { type Access = AccessModifier.Global } =
        new DeclarationClosure {
          override val context: ctx.type = ctx
          override type Access = AccessModifier.Global

          override def getImportSpecifier(sig: ErasedSignature): ImportSpecifier =
            ImportSpecifier.Global(
              tube = tn,
              module = p,
              name = name,
              signature = sig,
            )

          override def accessModifierParser: NonEmptyMap[Set[Modifier], AccessModifier.Global] =
            ModifierParser.accessModifierGlobal

          override def accessToken: AccessToken[ctx.type] =
            AccessToken(
              tubeName,
              path,
              Seq()
            )

          override def scope: context.Scopes.Scope = moduleScope
        }
    }
}
