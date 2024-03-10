package dev.argon.plugins.lua

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.expr.{BinaryBuiltin, NullaryBuiltin}

trait EmitBase {
  val plugin: LuaPlugin

  protected def toArrayExp(values: Seq[AST.Exp]): AST.Exp =
    AST.TableConstructor(values.map(AST.Field.Positional.apply))


  def getTubePath(tubeName: TubeName): String =
    tubeName.encode.replace(".", "_").nn

  protected def getIdentifierKeyExpr(id: Option[IdentifierExpr]): AST.Exp =
    id match {
      case Some(IdentifierExpr.Named(name)) =>
        AST.StringLiteral(name)

      case Some(IdentifierExpr.Op(op: ast.UnaryOperator)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("unary-operator")),
          AST.Field.NamedFixed("operator", AST.StringLiteral(op.symbol)),
        ))

      case Some(IdentifierExpr.Op(op: ast.BinaryOperator)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("binary-operator")),
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

      case None =>
        AST.FalseLiteral
    }

  def getIdentifierKeyExprMemo(id: Option[IdentifierExpr]): AST.Exp =
    AST.MethodCall(
      AST.NameExp("ArgonRuntime"),
      "intern_name",
      Seq(getIdentifierKeyExpr(id)),
    )


  protected def getErasedSigKeyExpr(sig: ErasedSignature): AST.Exp =
    val paramsExprs = sig.params.map(getErasedTypeExpr)
    val resExpr = getErasedTypeExpr(sig.result)
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

  def getErasedSigKeyExprMemo(sig: ErasedSignature): AST.Exp =
    AST.MethodCall(
      AST.NameExp("ArgonRuntime"),
      "intern_signature",
      Seq(getErasedSigKeyExpr(sig)),
    )

  protected def getErasedTypeExpr(t: ErasedSignatureType): AST.Exp =
    t match
      case ErasedSignatureType.Builtin(builtin, args) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("builtin"),
          ),
          AST.Field.NamedFixed(
            "builtin",
            AST.StringLiteral(builtin match {
              case NullaryBuiltin.IntType => "int"
              case NullaryBuiltin.BoolType => "bool"
              case NullaryBuiltin.StringType => "string"
              case NullaryBuiltin.NeverType => "never"
              case BinaryBuiltin.ConjunctionType => "/\\"
              case BinaryBuiltin.DisjunctionType => "\\/"
            }),
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
