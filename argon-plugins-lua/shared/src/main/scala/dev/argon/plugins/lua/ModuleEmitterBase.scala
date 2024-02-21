package dev.argon.plugins.lua

import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.signature.{ErasedSignature, ErasedSignatureNoResult, ErasedSignatureType, ErasedSignatureWithResult, ImportSpecifier}
import dev.argon.parser.IdentifierExpr

trait ModuleEmitterBase[R <: LuaEnv, E >: LuaError] extends TubeEmitterBase[R, E]  {
  val currentModule: ArModule & HasImplementation[true]


  protected def getIdentifierKeyExpr(id: Option[IdentifierExpr]): AST.Exp =
    id match {
      case Some(IdentifierExpr.Named(name)) =>
        AST.StringLiteral(name)

      case Some(IdentifierExpr.OperatorIdentifier(op)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("operator")),
          AST.Field.NamedFixed("operator", AST.StringLiteral(op.symbol)),
        ))

      case Some(IdentifierExpr.Extension(inner)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("extension")),
          AST.Field.NamedFixed("inner", getIdentifierKeyExpr(Some(inner))),
        ))

      case Some(IdentifierExpr.Inverse(inner)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("inverse")),
          AST.Field.NamedFixed("inner", getIdentifierKeyExpr(Some(inner))),
        ))

      case Some(IdentifierExpr.Update(inner)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("update")),
          AST.Field.NamedFixed("inner", getIdentifierKeyExpr(Some(inner))),
        ))

      case Some(IdentifierExpr.FunctionResultValue) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("function-result-value")),
        ))

      case None =>
        AST.FalseLiteral
    }

  protected def getIdentifierKeyExprMemo(id: Option[IdentifierExpr]): AST.Exp =
    AST.MethodCall(
      AST.NameExp("rt"),
      "intern_name",
      Seq(getIdentifierKeyExpr(id)),
    )


  protected def getErasedSigKeyExpr(sig: ErasedSignature): AST.Exp =
    sig match
      case ErasedSignatureWithResult(params, result) =>
        val paramsExprs = params.map(getErasedTypeExpr)
        val resExpr = getErasedTypeExpr(result)
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "parameters",
            AST.TableConstructor(paramsExprs.map(AST.Field.Positional.apply)),
          ),
          AST.Field.NamedFixed(
            "result",
            resExpr
          ),
        ))

      case ErasedSignatureNoResult(params) =>
        val paramsExprs = params.map(getErasedTypeExpr)
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "parameters",
            AST.TableConstructor(paramsExprs.map(AST.Field.Positional.apply)),
          ),
        ))
    end match

  protected def getErasedSigKeyExprMemo(sig: ErasedSignature): AST.Exp =
    AST.MethodCall(
      AST.NameExp("rt"),
      "intern_signature",
      Seq(getErasedSigKeyExpr(sig)),
    )

  protected def getImportSpecifierExpr(importSpecifier: ImportSpecifier): AST.Exp =
    AST.TableConstructor(Seq(
      AST.Field.NamedFixed(
        "tube",
        getTubeNameExprMemo(importSpecifier.tube),
      ),
      AST.Field.NamedFixed(
        "module",
        getModulePathExprMemo(importSpecifier.module),
      ),
      AST.Field.NamedFixed(
        "name",
        getIdentifierKeyExprMemo(importSpecifier.name),
      ),
      AST.Field.NamedFixed(
        "signature",
        getErasedSigKeyExprMemo(importSpecifier.signature)
      ),
    ))

  protected def getErasedTypeExpr(t: ErasedSignatureType): AST.Exp =
    t match
      case ErasedSignatureType.Class(classImport, args) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("class"),
          ),
          AST.Field.NamedFixed(
            "import",
            getImportSpecifierExpr(classImport),
          ),
          AST.Field.NamedFixed(
            "args",
            toArrayExp(args.map(getErasedTypeExpr)),
          ),
        ))

      case ErasedSignatureType.Trait(traitImport, args) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("trait"),
          ),
          AST.Field.NamedFixed(
            "import",
            getImportSpecifierExpr(traitImport),
          ),
          AST.Field.NamedFixed(
            "args",
            toArrayExp(args.map(getErasedTypeExpr)),
          ),
        ))
      case ErasedSignatureType.Function(input, output) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("function"),
          ),
          AST.Field.NamedFixed(
            "input",
            getErasedTypeExpr(input),
          ),
          AST.Field.NamedFixed(
            "args",
            getErasedTypeExpr(output),
          ),
        ))

      case ErasedSignatureType.Tuple(elements) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("tuple"),
          ),
          AST.Field.NamedFixed(
            "elements",
            toArrayExp(elements.map(getErasedTypeExpr)),
          ),
        ))
      case ErasedSignatureType.Erased =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("erased"),
          ),
        ))
    end match
}
