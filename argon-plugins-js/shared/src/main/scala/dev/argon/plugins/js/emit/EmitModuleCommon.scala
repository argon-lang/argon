package dev.argon.plugins.js.emit

import dev.argon.compiler.signature.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.module.ModuleName
import dev.argon.util.{*, given}
import dev.argon.parser.IdentifierExpr
import zio.*
import zio.stm.*

trait EmitModuleCommon extends EmitTubeCommon {
  val imports: TMap[ImportSpecifier, String]
  val additionalImports: TMap[ModuleName, TSet[String]]
  val module: ArModule & HasImplementation[true]


  protected def getEscapedName(name: IdentifierExpr): String =
    name match
      case IdentifierExpr.Named(name) =>
        def isValidStart(ch: Int): Boolean =
          ch == '_' || ch == '$' || Character.isAlphabetic(ch)

        def isValidPart(ch: Int): Boolean =
          isValidStart(ch) || Character.isDigit(ch)

        val sb = new StringBuilder()
        var i = 0
        while i < name.length do
          val ch = name.codePointAt(i)

          if ch == '$' then
            sb.append("$$")
          else if isValidPart(ch) then
            if i == 0 && !isValidStart(ch) then
              sb.append("$b")
            end if
            sb.underlying.appendCodePoint(ch)
          else if Character.charCount(ch) == 1 then
            sb.append(String.format("$u%04X", ch))
          else
            sb.append(String.format("$U%08X", ch))
          end if

          i += Character.charCount(ch)
        end while

        sb.toString()


      case IdentifierExpr.OperatorIdentifier(op) => "$o" + getEscapedName(IdentifierExpr.Named(op.symbol))
      case IdentifierExpr.Extension(inner) => "$e" + getEscapedName(inner)
      case IdentifierExpr.Inverse(inner) => "$i" + getEscapedName(inner)
      case IdentifierExpr.Update(inner) => "$m" + getEscapedName(inner)
      case IdentifierExpr.FunctionResultValue => ???
    end match

  // Escape Sequences
  // $a - argument to a type
  // $b - prepended to an identifier if it is not a valid JS start character
  // $c - indicates a class type will follow
  // $d - indicates a tube name segment will follow
  // $e - prepended to an extension identifier
  // $f - indicates a function type will follow
  // $g - indicates a module name segment will follow
  // $i - prepended to an inverse identifier
  // $m - prepended to an update identifier
  // $n - indicates that a name will follow
  // $o - prepended to an operator name
  // $p - indicates a signature parameter type
  // $r - indicates a result type
  // $t - indicates a trait type will follow
  // $uXXXX and $UXXXXXXXX - unicode escape
  // $v - indicates a signature without a result type
  // $z - indicates a tuple type will follow
  // $_ - indicates an erased type
  // $$ - escaped $
  protected def getOverloadExportName(name: Option[IdentifierExpr], signature: ErasedSignature): String =
    def sigTypePart(t: ErasedSignatureType): String =
      t match
        case ErasedSignatureType.Class(classImport, args) => "$c" + importSpecifierPart(classImport) + argParts(args)
        case ErasedSignatureType.Trait(traitImport, args) => "$t" + importSpecifierPart(traitImport) + argParts(args)
        case ErasedSignatureType.Function(input, output) => "$f" + sigTypePart(input) + "$r" + sigTypePart(output)
        case ErasedSignatureType.Tuple(elements) => "$z" + argParts(elements)
        case ErasedSignatureType.Erased => "$_"
      end match

    def importSpecifierPart(specifier: ImportSpecifier): String =
      specifier.tube.name.toList.map { part => "$d" + getEscapedName(IdentifierExpr.Named(part)) }.mkString +
        specifier.module.ids.map { part => "$g" + getEscapedName(IdentifierExpr.Named(part)) }.mkString +
        "$n" + getOverloadExportName(specifier.name, specifier.signature)

    def argParts(args: Seq[ErasedSignatureType]): String =
      args.map(arg => "$a" + sigTypePart(arg)).mkString

    val sigPart =
      signature match
        case ErasedSignatureWithResult(params, result) => params.map { param => "$p" + sigTypePart(param) }.mkString + "$r" + sigTypePart(result)
        case ErasedSignatureNoResult(params) => params.map { param => "$p" + sigTypePart(param) }.mkString + "$v"
      end match

    name.fold("$_")(getEscapedName) + sigPart
  end getOverloadExportName

  protected def getImportName(specifier: ImportSpecifier): UIO[String] =
    if specifier.moduleName == module.moduleName then
      ZIO.succeed(getOverloadExportName(specifier.name, specifier.signature))
    else
      imports.get(specifier).flatMap {
        case Some(name) => STM.succeed(name)
        case None =>
          for
            size <- imports.size
            name = s"arimport$size"
            _ <- imports.put(specifier, name)
          yield name
      }.commit

  protected def ensureRawImportName(module: ModuleName, name: String): UIO[Unit] =
    additionalImports.get(module)
      .flatMap {
        case Some(names) => ZSTM.succeed(names)
        case None =>
          for
            names <- TSet.empty[String]
            _ <- additionalImports.put(module, names)
          yield names
      }
      .flatMap { names =>
        names.put(name)
      }
      .commit


  protected val runtimeImportName: String = "argonRuntime"
}
