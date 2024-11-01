package dev.argon.plugins.scheme

import dev.argon.tube.*
import java.nio.charset.StandardCharsets

private[scheme] object TubeExportEncoding {


  def buildTubeExportId(modulePath: ModulePath, name: Option[Identifier], sig: ErasedSignature): String =
    val sb = new StringBuilder()
    buildTubeExportId(modulePath, name, sig, sb)
    sb.toString
  end buildTubeExportId


  def buildTubeLibName(tubeName: TubeName): Seq[String] =
    (tubeName.head +: tubeName.tail).map(escapeTubeLibNameSegment)
  
  def recordConstructorExportId(recordId: String): String =
    recordId + "~make"
  
  def recordTestExportId(recordId: String): String =
    recordId + "~test?"
  
  def recordFieldAccessorExportId(recordId: String, fieldName: Identifier): String =
    val sb = new StringBuilder(recordId)
    sb.append("~get~")
    escapeIdentifier(fieldName, sb)
    sb.toString()
  end recordFieldAccessorExportId

  def recordFieldId(fieldName: Identifier): String =
    val sb = new StringBuilder()
    escapeIdentifier(fieldName, sb)
    sb.toString()
  end recordFieldId

  

  private def reservedChars: String =
    "#/@$%^&*~|[](){}<>_+=:;.,\\"

  private def escapeIdStr(name: String, sb: StringBuilder): Unit =
    for c <- name do
      if reservedChars.indexOf(c) >= 0 then
        sb.append('#')

      sb.append(c)
    end for

  private def escapeIdentifier(name: Identifier, sb: StringBuilder): Unit =
    name match {
      case Identifier.Named(s) => escapeIdStr(s, sb)
      case Identifier.BinOp(op) =>
        val s = op match {
          case BinaryOperator.Plus() => "@+;"
          case BinaryOperator.Minus() => "@-;"
          case BinaryOperator.Mul() => "@×;"
          case BinaryOperator.Div() => "@÷;"
          case BinaryOperator.Equal() => "@=;"
          case BinaryOperator.NotEqual() => "@≠;"
          case BinaryOperator.LessThan() => "@<;"
          case BinaryOperator.LessThanEq() => "@≤;"
          case BinaryOperator.GreaterThan() => "@>;"
          case BinaryOperator.GreaterThanEq() => "@≥;"
          case BinaryOperator.BitOr() => "@|||;"
          case BinaryOperator.BitXor() => "@^^^;"
          case BinaryOperator.BitAnd() => "@&&&;"
          case BinaryOperator.ShiftLeft() => "@<<<;"
          case BinaryOperator.ShiftRight() => "@>>>;"
          case BinaryOperator.Concat() => "@++;"
        }

        sb.append(s)
        
      case Identifier.UnOp(op) =>
        val s = op match {
          case UnaryOperator.Plus() => ":+;"
          case UnaryOperator.Minus() => ":-;"
          case UnaryOperator.BitNot() => ":~~~;"
          case UnaryOperator.LogicalNot() => ":!;"
        }
        
        sb.append(s)

      case Identifier.Extension(inner) =>
        escapeIdentifier(inner, sb)
        sb.append("$extension")

      case Identifier.Inverse(inner) =>
        escapeIdentifier(inner, sb)
        sb.append("$inverse")

      case Identifier.Update(inner) =>
        escapeIdentifier(inner, sb)
        sb.append("$update")
    }
    

  private def escapeIdentifierOpt(name: Option[Identifier], sb: StringBuilder): Unit =
    name match {
      case Some(value) => escapeIdentifier(value, sb)
      case None => 
    }
    
  private def writeFunctionArgs(args: Seq[ErasedSignatureType], sb: StringBuilder): Unit =
    var needsComma = false
    for arg <- args do
      if needsComma then
        sb.append(", ")

      escapeType(arg, sb)

      needsComma = true
    end for
  end writeFunctionArgs


  private def buildTubeExportId(modulePath: ModulePath, name: Option[Identifier], sig: ErasedSignature, sb: StringBuilder): Unit =
    for part <- modulePath.path do
      escapeIdStr(part, sb)
      sb.append('/')
    end for

    escapeIdentifierOpt(name, sb)

    writeFunctionArgs(sig.params, sb)
    escapeType(sig.result, sb)
  end buildTubeExportId

  private def escapeType(t: ErasedSignatureType, sb: StringBuilder): Unit =
    t match {
      case ErasedSignatureType.Builtin(b, args) =>
        val s = b match {
          case BuiltinType.Int() => "%int"
          case BuiltinType.Bool() => "%bool"
          case BuiltinType.String() => "%string"
          case BuiltinType.Never() => "%never"
          case BuiltinType.Conjunction() => "%∧"
          case BuiltinType.Disjunction() => "%∨"
        }

        sb.append(s)
        writeFunctionArgs(args, sb)

      case ErasedSignatureType.Function(input, output) =>
        sb.append("%function")
        writeFunctionArgs(Seq(input, output), sb)

      case ErasedSignatureType.Record(recordImport, args) =>
        buildFullTubeExportId(recordImport, sb)
        writeFunctionArgs(args, sb)

      case ErasedSignatureType.Tuple(elements) =>
        sb.append("%tuple")
        writeFunctionArgs(elements, sb)

      case ErasedSignatureType.Erased() =>
        sb.append("%erased")
    }
    
  private def buildFullTubeExportId(specifier: ImportSpecifier, sb: StringBuilder): Unit =
    for (part, i) <- (specifier.tube.head +: specifier.tube.tail).zipWithIndex do
      escapeIdStr(part, sb)

      if i < specifier.tube.tail.size then
        sb.append(".")
      else
        sb.append("/")
    end for

    buildTubeExportId(specifier.modulePath, specifier.name, specifier.sig, sb)
  end buildFullTubeExportId


  private def escapeTubeLibNameSegment(s: String): String =
    val sb = new StringBuilder()

    val bytes = s.getBytes(StandardCharsets.UTF_8)

    for b <- bytes do
      val unreserved =
        (b >= 'a' && b <= 'z') ||
          (b >= 'A' && b <= 'Z') ||
          (b >= '0' && b <= '9') ||
          b == '-' || b == '.' || b == '_' || b == '~'

      if unreserved then
        sb.append(b.toChar)
      else
        sb.append('%')
        sb.append(java.lang.String.format("%02x", b))
      end if
    end for

    sb.toString
  end escapeTubeLibNameSegment

}
