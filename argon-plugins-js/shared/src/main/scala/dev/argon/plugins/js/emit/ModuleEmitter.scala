package dev.argon.plugins.js.emit

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.compiler.signature.ErasedSignatureType.Erased
import dev.argon.compiler.signature.*
import dev.argon.plugins.js.*
import dev.argon.plugin.PluginContextAdapter
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.parser.IdentifierExpr
import dev.argon.util.*
import zio.*
import zio.stm.*

import scala.collection.mutable

private[emit] trait ModuleEmitter extends EmitModuleCommon {

  import JSExpr.{*, given}

  def program: Comp[estree.Program] =
    for
      argonExports <- module.allExports()
      jsExports <- ZIO.foreach(argonExports)(elementExport)

      moduleOptions = options.modules.map.getOrElse(module.moduleName.path, JSModuleOptions(None, None))

      injectBefore <- moduleOptions.inject_before.fold(ZIO.succeed(Seq())) { res =>
        res.asModule.map { _.body }
      }
      injectAfter <- moduleOptions.inject_after.fold(ZIO.succeed(Seq())) { res =>
        res.asModule.map { _.body }
      }

      argonImports <- imports.toMap.commit
      jsImports <-
        ZIO.foreach(
          argonImports
            .toSeq
            .groupMap(
              { case (specifier, _) => specifier.moduleName }
            )(
              { case (specifier, name) => (getOverloadExportName(specifier.name, specifier.signature), name) }
            )
            .toSeq
        )(createJSImport)

      additionalArgonImports <- additionalImports.toMap.commit
      additionalJSImports <- ZIO.foreach(additionalArgonImports.toSeq) { (moduleName, names) =>
        names.toSet.commit.flatMap { namesSet =>
          createJSImport(moduleName, namesSet.toSeq.map { name => (name, name) })
        }
      }

    yield estree.Program(
      sourceType = "module",
      body =
        jsImports ++ additionalJSImports ++ injectBefore ++ jsExports ++ injectAfter
    )

  private def getTubeImportPath(tubeName: TubeName): Comp[String] =
    options.tubes.map.get(tubeName) match {
      case Some(tubeOptions) => ZIO.succeed(tubeOptions.import_path)
      case None => ZIO.fail(ImportPathNotSpecifiedError(tubeName))
    }

  private def createJSImport(moduleName: ModuleName, identifiers: Seq[(String, String)]): Comp[estree.ImportDeclaration] =
    for
      importedTube <- context.getTube(moduleName.tubeName)

      isLocal = moduleName.tubeName == tube.tubeName

      tubeImportPath <-
        if isLocal then
          ZIO.succeed(".")
        else
          getTubeImportPath(moduleName.tubeName)

      tubeImportPathParts = tubeImportPath.split("/").toList

      fileName = getModuleFileName(importedTube)(moduleName.path)

      packageRoot = Seq.fill(getModuleFileName(tube)(module.moduleName.path).dir.size)("..")

      dir =
        tubeImportPathParts match {
          case ("." | "..") :: _ =>
            normalizeDir((packageRoot ++ tubeImportPathParts ++ fileName.dir).toList) match {
              case dir @ (("." | "..") :: _) => dir
              case dir => "." :: dir
            }
          case _ => normalizeDir(tubeImportPathParts ++ fileName.dir)
        }
    yield `import`(
      identifiers.map {
        case (exportedName, identifier) if exportedName == identifier =>
          id(identifier)

        case (exportedName, identifier) =>
          id(exportedName) as id(identifier)
      }*
    ) from (dir.mkString("/") + "/" + fileName.file + (if isLocal then ".js" else ""))


  private def normalizeDir(dir: List[String]): List[String] =
    dir match {
      case "." :: tail => normalizeDir(tail)
      case ".." :: tail => ".." :: normalizeDir(tail)
      case _ :: ".." :: tail => normalizeDir(tail)
      case head :: tail => head :: normalizeDir(tail)
      case Nil => Nil
    }

  private def elementExport(element: ModuleElement[true]): Comp[estree.ExportNamedDeclaration] =
    for
      nextLocalIdRef <- TRef.makeCommit(0)
      localNamesRef <- TSet.empty[String].commit
      localNameMapRef <- TMap.empty[UniqueIdentifier, String].commit
      instanceNameMapRef <- TMap.empty[UniqueIdentifier, String].commit
      parameterNameMapRef <- TMap.empty[(UniqueIdentifier, Int), String].commit
      exprEmitter =
        new ExprEmitter:
          override val context: ModuleEmitter.this.context.type = ModuleEmitter.this.context
          override val tube: ModuleEmitter.this.tube.type = ModuleEmitter.this.tube
          override val options: JSOptions[context.Env, context.Error] = ModuleEmitter.this.options
          override val adapter: PluginContextAdapter.Aux[context.type, JSPlugin.type] = ModuleEmitter.this.adapter

          override val imports: TMap[ImportSpecifier, String] = ModuleEmitter.this.imports
          override val additionalImports: TMap[ModuleName, TSet[String]] = ModuleEmitter.this.additionalImports
          override val module: ModuleEmitter.this.module.type = ModuleEmitter.this.module

          override val nextLocalId: TRef[RuntimeFlags] = nextLocalIdRef
          override val newLocalNames: TSet[String] = localNamesRef
          override val localNameMap: TMap[UniqueIdentifier, String] = localNameMapRef
          override val instanceNameMap: TMap[UniqueIdentifier, String] = instanceNameMapRef
          override val parameterNameMap: TMap[(UniqueIdentifier, RuntimeFlags), String] = parameterNameMapRef
          override protected val vtableBuilder: VTableBuilder[context.type] = ModuleEmitter.this.vtableBuilder
        end new

      declaration <-
        element match
          case ModuleElementC.ClassElement(arClass) => exprEmitter.classExport(arClass)
          case ModuleElementC.TraitElement(arTrait) => exprEmitter.traitExport(arTrait)
          case ModuleElementC.FunctionElement(func) => exprEmitter.functionExport(func)
        end match
    yield declaration



}
