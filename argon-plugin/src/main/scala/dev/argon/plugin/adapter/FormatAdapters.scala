package dev.argon.plugin.adapter

import dev.argon.compiler.*
import dev.argon.tube
import dev.argon.ast.{IdentifierExpr, BinaryOperator, UnaryOperator}
import dev.argon.ast.Operator.ValidIdentifier
import dev.argon.expr.BuiltinType
import dev.argon.expr.NullaryBuiltin
import dev.argon.expr.BinaryBuiltin

object FormatAdapters {
  def createImportSpecifier(di: DefinitionInfo): tube.ImportSpecifier =
    di match {
      case DefinitionInfo.Global(tubeName, modulePath, name, sig) =>
        tube.ImportSpecifier(
          createTubeName(tubeName),
          createModulePath(modulePath),
          name.map(createName),
          createErasedSig(sig)
        )
    }

  def createImportSpecifier(specifier: ImportSpecifier): tube.ImportSpecifier =
    tube.ImportSpecifier(
      createTubeName(specifier.tube),
      createModulePath(specifier.module),
      specifier.name.map(createName),
      createErasedSig(specifier.signature)
    )

  def createTubeName(tubeName: TubeName): tube.TubeName =
    tube.TubeName(tubeName.parts.head, tubeName.parts.tail)

  def createModulePath(modulePath: ModulePath): tube.ModulePath =
    tube.ModulePath(modulePath.parts)

  def createName(name: IdentifierExpr): tube.Identifier =
    name match {
      case IdentifierExpr.Named(s) => tube.Identifier.Named(s)
      case IdentifierExpr.Op(op) =>
        def createBinOp(op: ValidIdentifier & BinaryOperator): tube.BinaryOperator =
          op match {
            case BinaryOperator.Plus => tube.BinaryOperator.Plus() 
            case BinaryOperator.Minus => tube.BinaryOperator.Minus()
            case BinaryOperator.Mul => tube.BinaryOperator.Mul()
            case BinaryOperator.Div => tube.BinaryOperator.Div()
            case BinaryOperator.Equal => tube.BinaryOperator.Equal()
            case BinaryOperator.NotEqual => tube.BinaryOperator.NotEqual()
            case BinaryOperator.LessThan => tube.BinaryOperator.LessThan()
            case BinaryOperator.LessThanEq => tube.BinaryOperator.LessThanEq()
            case BinaryOperator.GreaterThan => tube.BinaryOperator.GreaterThan()
            case BinaryOperator.GreaterThanEq => tube.BinaryOperator.GreaterThanEq()
            case BinaryOperator.BitOr => tube.BinaryOperator.BitOr()
            case BinaryOperator.BitXOr => tube.BinaryOperator.BitXor()
            case BinaryOperator.BitAnd => tube.BinaryOperator.BitAnd()
            case BinaryOperator.ShiftLeft => tube.BinaryOperator.ShiftLeft()
            case BinaryOperator.ShiftRight => tube.BinaryOperator.ShiftRight()
            case BinaryOperator.Concat => tube.BinaryOperator.Concat()
          }
          

        def createUnOp(op: ValidIdentifier & UnaryOperator): tube.UnaryOperator =
          op match {
            case UnaryOperator.Plus => tube.UnaryOperator.Plus()
            case UnaryOperator.Minus => tube.UnaryOperator.Minus()
            case UnaryOperator.BitNot => tube.UnaryOperator.BitNot()
            case UnaryOperator.LogicalNot => tube.UnaryOperator.LogicalNot()
          }
          

        op match {
          case op: BinaryOperator => tube.Identifier.BinOp(createBinOp(op))
          case op: UnaryOperator => tube.Identifier.UnOp(createUnOp(op))
        }

      case IdentifierExpr.Extension(inner) => tube.Identifier.Extension(createName(inner))
      case IdentifierExpr.Inverse(inner) => tube.Identifier.Inverse(createName(inner))
      case IdentifierExpr.Update(inner) => tube.Identifier.Update(createName(inner))
    }
    
  def createErasedSig(sig: ErasedSignature): tube.ErasedSignature =
    tube.ErasedSignature(sig.params.map(createErasedSigType), createErasedSigType(sig.result))

  def createErasedSigType(t: ErasedSignatureType): tube.ErasedSignatureType =
    t match {
      case ErasedSignatureType.Builtin(builtin, args) =>
        val b = createBuiltinType(builtin)
        tube.ErasedSignatureType.Builtin(b, args.map(createErasedSigType))

      case ErasedSignatureType.Function(input, output) =>
        tube.ErasedSignatureType.Function(createErasedSigType(input), createErasedSigType(output))

      case ErasedSignatureType.Record(recordImport, args) =>
        tube.ErasedSignatureType.Record(createImportSpecifier(recordImport), args.map(createErasedSigType))

      case ErasedSignatureType.Tuple(elements) =>
        tube.ErasedSignatureType.Tuple(elements.map(createErasedSigType))

      case ErasedSignatureType.Erased =>
        tube.ErasedSignatureType.Erased()
    }
    
  def createBuiltinType(builtin: BuiltinType): tube.BuiltinType =
    builtin match {
      case NullaryBuiltin.IntType => tube.BuiltinType.Int()
      case NullaryBuiltin.BoolType => tube.BuiltinType.Bool()
      case NullaryBuiltin.StringType => tube.BuiltinType.String()
      case NullaryBuiltin.NeverType => tube.BuiltinType.Never()
      case BinaryBuiltin.ConjunctionType => tube.BuiltinType.Conjunction()
      case BinaryBuiltin.DisjunctionType => tube.BuiltinType.Disjunction()
    }

}
