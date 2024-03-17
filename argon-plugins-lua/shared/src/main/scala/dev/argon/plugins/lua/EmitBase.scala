package dev.argon.plugins.lua

import cats.data.NonEmptySeq
import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.TubeName as CTubeName
import dev.argon.expr.{BinaryBuiltin, NullaryBuiltin}
import dev.argon.plugin.vm.*

trait EmitBase {
  protected def toArrayExp(values: Seq[AST.Exp]): AST.Exp =
    AST.TableConstructor(values.map(AST.Field.Positional.apply))


  def getTubePath(tubeName: CTubeName): String =
    tubeName.encode.replace(".", "_").nn

  protected def getIdentifierKeyExpr(id: Option[Identifier]): AST.Exp =
    id match {
      case Some(Identifier.Named(name)) =>
        AST.StringLiteral(name)

      case Some(Identifier.UnaryOperator(op)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("unary-operator")),
          AST.Field.NamedFixed("operator", AST.StringLiteral(op match {
            case UnaryOperatorName.Plus => "+"
            case UnaryOperatorName.Minus => "-"
            case UnaryOperatorName.BitNot => "~~~"
            case UnaryOperatorName.LogicalNot => "!"
          })),
        ))

      case Some(Identifier.BinaryOperator(op)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("binary-operator")),
          AST.Field.NamedFixed("operator", AST.StringLiteral(op match {
            case BinaryOperatorName.Plus => "+"
            case BinaryOperatorName.Minus => "-"
            case BinaryOperatorName.Mul => "*"
            case BinaryOperatorName.Div => "/"
            case BinaryOperatorName.Equal => "="
            case BinaryOperatorName.NotEqual => "!="
            case BinaryOperatorName.LessThan => "<"
            case BinaryOperatorName.LessThanEq => "<="
            case BinaryOperatorName.GreaterThan => ">"
            case BinaryOperatorName.GreaterThanEq => ">="
            case BinaryOperatorName.BitAnd => "&&&"
            case BinaryOperatorName.BitOr => "|||"
            case BinaryOperatorName.BitXor => "^^^"
            case BinaryOperatorName.ShiftLeft => "<<"
            case BinaryOperatorName.ShiftRight => ">>"
            case BinaryOperatorName.Concat => "++"
          })),
        ))

      case Some(Identifier.Extension(inner)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("extension")),
          AST.Field.NamedFixed("inner", getIdentifierKeyExpr(Some(inner))),
        ))

      case Some(Identifier.Inverse(inner)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("inverse")),
          AST.Field.NamedFixed("inner", getIdentifierKeyExpr(Some(inner))),
        ))

      case Some(Identifier.Update(inner)) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed("type", AST.StringLiteral("update")),
          AST.Field.NamedFixed("inner", getIdentifierKeyExpr(Some(inner))),
        ))

      case None =>
        AST.FalseLiteral
    }

  def getIdentifierKeyExprMemo(id: Option[Identifier]): AST.Exp =
    AST.MethodCall(
      AST.NameExp("ArgonRuntime"),
      "intern_name",
      Seq(getIdentifierKeyExpr(id)),
    )


  protected def getErasedSigKeyExpr(sig: ErasedSignature): AST.Exp =
    val paramsExprs = sig.parameters.map(getErasedTypeExpr)
    val resExpr = getErasedTypeExpr(sig.returnType)
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

  protected def getErasedTypeExpr(t: ErasedType): AST.Exp =
    t match
      case ErasedType.Builtin(builtin, args*) =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("builtin"),
          ),
          AST.Field.NamedFixed(
            "builtin",
            AST.StringLiteral(builtin match {
              case Builtin.IntType => "int"
              case Builtin.BoolType => "bool"
              case Builtin.StringType => "string"
              case Builtin.NeverType => "never"
              case Builtin.ConjunctionType => "/\\"
              case Builtin.DisjunctionType => "\\/"
              case _ => ???
            }),
          ),
          AST.Field.NamedFixed(
            "args",
            toArrayExp(args.map(getErasedTypeExpr)),
          ),
        ))

      case ErasedType.Function(input, output) =>
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

      case ErasedType.Tuple(elements*) =>
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
      case ErasedType.Erased =>
        AST.TableConstructor(Seq(
          AST.Field.NamedFixed(
            "type",
            AST.StringLiteral("erased"),
          ),
        ))
    end match

}
