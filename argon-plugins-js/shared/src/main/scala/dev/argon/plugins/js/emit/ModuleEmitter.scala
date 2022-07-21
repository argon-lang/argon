package dev.argon.plugins.js.emit

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.compiler.signature.ErasedSignatureType.Erased
import dev.argon.compiler.signature.{ErasedSignature, ErasedSignatureNoParams, ErasedSignatureType, ErasedSignatureWithResult, ImportSpecifier}
import dev.argon.plugins.js.*
import dev.argon.compiler.tube.TubeName
import dev.argon.parser.IdentifierExpr
import dev.argon.util.*
import zio.*
import zio.stm.*

import scala.collection.mutable

private[emit] trait ModuleEmitter extends EmitModuleCommon {

  def program: Comp[estree.Program] =
    for
      argonExports <- module.allExports()
      jsExports <- ZIO.foreach(argonExports)(elementExport)

      argonImports <- imports.toMap.commit
      jsImports <-
        ZIO.foreach(
          argonImports
            .toSeq
            .groupBy { case (specifier, _) => specifier.moduleName }
            .toSeq
        )(createJSImport)

    yield estree.Program(
      sourceType = "module",
      body = jsImports ++ jsExports
    )

  private def getTubeImportPath(tubeName: TubeName): Comp[String] =
    options.tubes.map.get(tubeName) match {
      case Some(tubeOptions) => ZIO.succeed(tubeOptions.import_path)
      case None => ZIO.fail(ImportPathNotSpecified(tubeName))
    }

  private def createJSImport(moduleName: ModuleName, identifiers: Seq[(ImportSpecifier, String)]): Comp[estree.ImportDeclaration] =
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
          case _ => normalizeDir((tubeImportPathParts ++ fileName.dir).toList)
        }
    yield estree.ImportDeclaration(
      specifiers = identifiers.map { case (specifier, identifier) =>
        estree.ImportSpecifier(
          local = estree.Identifier(name = identifier),
          imported = estree.Identifier(name = getOverloadExportName(specifier.name, specifier.signature))
        )
      },
      source = estree.Literal(
        value = Nullable(dir.mkString("/") + (if isLocal then ".js" else ""))
      )
    )


  private def normalizeDir(dir: List[String]): List[String] =
    dir match {
      case "." :: tail => normalizeDir(tail)
      case ".." :: tail => ".." :: normalizeDir(tail)
      case _ :: ".." :: tail => normalizeDir(tail)
      case head :: tail => head :: normalizeDir(tail)
      case Nil => Nil
    }

  private def elementExport(element: ModuleElement): Comp[estree.ExportNamedDeclaration] =
    for
      nextLocalIdRef <- TRef.makeCommit(0)
      localNamesRef <- TSet.empty[String].commit
      exprEmitter =
        new ExprEmitter:
          override val context: ModuleEmitter.this.context.type = ModuleEmitter.this.context
          override val tube: ModuleEmitter.this.tube.type = ModuleEmitter.this.tube
          override val options: JSOptions[context.Env, context.Error] = ModuleEmitter.this.options

          override val imports: TMap[ImportSpecifier, String] = ModuleEmitter.this.imports
          override val module: ModuleEmitter.this.module.type = ModuleEmitter.this.module

          override val nextLocalId: TRef[RuntimeFlags] = nextLocalIdRef
          override val localNames: TSet[String] = localNamesRef
        end new

      declaration <-
        element match
          case ModuleElementC.ClassElement(arClass) => exprEmitter.classExport(arClass)
          case ModuleElementC.TraitElement(arTrait) => exprEmitter.traitExport(arTrait)
          case ModuleElementC.FunctionElement(func) => exprEmitter.functionExport(func)
        end match
    yield declaration



}
