package dev.argon.compiler

import java.nio.charset.StandardCharsets

import dev.argon.compiler.core.GlobalName
import dev.argon.compiler.lookup.LookupDescription
import dev.argon.grammar.GrammarError
import dev.argon.parser
import dev.argon.parser.{CharacterCategory, SyntaxError, SyntaxErrorData, Token, TokenCategory}
import dev.argon.util._
import dev.argon.module
import dev.argon.compiler.core._
import dev.argon.compiler.types.TypeSystem
import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler.loaders.ResourceIndicator

sealed trait CompilationMessage {
  val source: CompilationMessageSource
  def message: String

  override def toString: String =
    s"${source.formatted}: ${message}"
}
trait CompilationError extends CompilationMessage
trait CompilationMessageNonFatal extends CompilationMessage

object CompilationError {
  final case class SyntaxCompilerError(syntaxError: SyntaxErrorData) extends CompilationError {
    override val source: CompilationMessageSource = CompilationMessageSource.SourceFile(syntaxError.fileSpec, syntaxError.syntaxError.location)

    override def message: String = syntaxError.syntaxError match {
      case SyntaxError.InvalidSurrogatePairs(ch, _) => s"Invalid surrogate: ${ch.toInt.toHexString}"
      case SyntaxError.UnexpectedCombingCharacter(cp, _) => s"Unexpected combing character: ${(cp.toLong & 0xFFFFFFFF).toHexString}"
      case SyntaxError.LexerError(error) => convertGrammarError("character", formatCharacter, formatCharacterCategory, error)
      case SyntaxError.ParserError(error) => convertGrammarError("token", formatToken, formatTokenCategory, error)
      case SyntaxError.AmbiguousParse(_) => "Parse was ambiguous"
    }

    private def convertGrammarError[TToken, TTokenCategory]
    (
      tokenName: String,
      tokenFormatter: TToken => String,
      categoryFormatter: TTokenCategory => String,
      grammarError: GrammarError[TToken, TTokenCategory]
    ): String =
      grammarError match {
        case GrammarError.InfiniteRecursion(_) => "The parser detected an infinitely recursive rule."
        case GrammarError.ExpectedEndOfFile(WithSource(token, _)) => s"Expected end of file, but found ${tokenFormatter(token)}"
        case GrammarError.UnexpectedEndOfFile(tokenCategory, _) => s"Expected ${categoryFormatter(tokenCategory)}, but found end of file"
        case GrammarError.UnexpectedToken(expectedCategory, WithSource(token, _)) => s"Expected ${categoryFormatter(expectedCategory)}, but found ${tokenFormatter(token)}"
      }

    private def formatToken(token: Token) =
      formatTokenCategory(token.category)

    private def formatCharacter(ch: String): String = {
      val utf8 = ch.getBytes(StandardCharsets.UTF_8).map { b => f"${b & 0xFF}%02X" }.mkString(" ")

      val cp = ch.codePointAt(0)

      if(Character.isISOControl(cp) || Character.isWhitespace(cp))
        "UTF-8: " + utf8
      else
        "\"" + ch + "\" (UTF-8: " + utf8 + ")"
    }

    private def formatTokenCategory(category: TokenCategory) = category match {
      case TokenCategory.StringToken => "string literal"
      case TokenCategory.IntToken => "int literal"

      case TokenCategory.Identifier => "identifier"


      case TokenCategory.NewLine => "new line"
      case TokenCategory.Semicolon => "semicolon"

      case TokenCategory.KW_DEF => "def"
      case TokenCategory.KW_PROC => "proc"
      case TokenCategory.KW_INSTANCE => "instance"
      case TokenCategory.KW_CONSTRUCTOR => "constructor"
      case TokenCategory.KW_END => "end"
      case TokenCategory.KW_DO => "do"
      case TokenCategory.KW_VAR => "var"
      case TokenCategory.KW_VAL => "val"
      case TokenCategory.KW_TRUE => "true"
      case TokenCategory.KW_FALSE => "false"
      case TokenCategory.KW_AS => "as"
      case TokenCategory.KW_NAMESPACE => "namespace"
      case TokenCategory.KW_IMPORT => "import"
      case TokenCategory.KW_TRAIT => "trait"
      case TokenCategory.KW_STATIC => "static"
      case TokenCategory.KW_DATA => "data"
      case TokenCategory.KW_PUBLIC => "public"
      case TokenCategory.KW_PROTECTED => "protected"
      case TokenCategory.KW_PRIVATE => "private"
      case TokenCategory.KW_INTERNAL => "internal"
      case TokenCategory.KW_BASE => "base"
      case TokenCategory.KW_IF => "if"
      case TokenCategory.KW_THEN => "then"
      case TokenCategory.KW_ELSE => "else"
      case TokenCategory.KW_ELSIF => "elsif"
      case TokenCategory.KW_OPEN => "open"
      case TokenCategory.KW_SEALED => "sealed"
      case TokenCategory.KW_VIRTUAL => "virtual"
      case TokenCategory.KW_ABSTRACT => "abstract"
      case TokenCategory.KW_OVERRIDE => "override"
      case TokenCategory.KW_FINAL => "final"
      case TokenCategory.KW_TYPE => "type"
      case TokenCategory.KW_MATCH => "match"
      case TokenCategory.KW_CASE => "case"
      case TokenCategory.KW_CLASS => "class"
      case TokenCategory.KW_NEW => "new"
      case TokenCategory.KW_FIELD => "field"
      case TokenCategory.KW_INITIALIZE => "initialize"
      case TokenCategory.KW_UNDERSCORE => "_"
      case TokenCategory.KW_GC => "gc"
      case TokenCategory.KW_STRUCT => "struct"
      case TokenCategory.KW_STACK => "stack"
      case TokenCategory.KW_ANY => "any"
      case TokenCategory.KW_VALUETYPE => "valuetype"
      case TokenCategory.KW_EXTERN => "extern"
      case TokenCategory.KW_RAISE => "raise"
      case TokenCategory.KW_BEGIN => "begin"
      case TokenCategory.KW_RESCUE => "rescue"
      case TokenCategory.KW_ENSURE => "ensure"

      case TokenCategory.OP_BOOLAND => "&&"
      case TokenCategory.OP_BOOLOR => "||"
      case TokenCategory.OP_EQUALS => "="
      case TokenCategory.OP_NOTEQUALS => "≠ (or !=)"
      case TokenCategory.OP_LESSTHANEQ => "≤ (or <=)"
      case TokenCategory.OP_GREATERTHANEQ => "≥ (or >=)"
      case TokenCategory.OP_SHIFTLEFT => "<<"
      case TokenCategory.OP_SHIFTRIGHT => ">>"
      case TokenCategory.OP_ASSIGN => ":="
      case TokenCategory.OP_DOT => "."
      case TokenCategory.OP_COMMA => ","
      case TokenCategory.OP_OPENPAREN => "("
      case TokenCategory.OP_CLOSEPAREN => ")"
      case TokenCategory.OP_OPENBRACKET => "["
      case TokenCategory.OP_CLOSEBRACKET => "]"
      case TokenCategory.OP_OPENCURLY => "{"
      case TokenCategory.OP_CLOSECURLY => "}"
      case TokenCategory.OP_BOOLNOT => "!"
      case TokenCategory.OP_ADD => "+"
      case TokenCategory.OP_SUB => "-"
      case TokenCategory.OP_MUL => "× (or *)"
      case TokenCategory.OP_DIV => "÷ (or /)"
      case TokenCategory.OP_BITAND => "&&&"
      case TokenCategory.OP_BITOR => "|||"
      case TokenCategory.OP_BITXOR => "^^^"
      case TokenCategory.OP_BITNOT => "~"
      case TokenCategory.OP_LESSTHAN => "<"
      case TokenCategory.OP_GREATERTHAN => ">"
      case TokenCategory.OP_COLON => ":"
      case TokenCategory.OP_SUBTYPE => "<:"
      case TokenCategory.OP_SUPERTYPE => ">:"
      case TokenCategory.OP_LAMBDA_TYPE => "->"
      case TokenCategory.OP_LAMBDA => "=>"
      case TokenCategory.OP_UNION => "|"
      case TokenCategory.OP_INTERSECTION => "&"
    }

    private def formatCharacterCategory(category: CharacterCategory) = category match {
      case CharacterCategory.SingleQuoteStringChar => "single quote string character"
      case CharacterCategory.CR => "\\r"
      case CharacterCategory.LF => "\\n"
      case CharacterCategory.Whitespace => "whitespace"
      case CharacterCategory.SingleQuote => "'"
      case CharacterCategory.NumberDigit => "digit"
      case CharacterCategory.NonZeroDigit => "non-zero digit"
      case CharacterCategory.Zero => "0"
      case CharacterCategory.Digit => "unicode digit"
      case CharacterCategory.BaseSpecifier => "base specifier"
      case CharacterCategory.Letter => "letter"
      case CharacterCategory.Underscore => "_"

      case CharacterCategory.QMark => "?"
      case CharacterCategory.Exclaim => "!"

      case CharacterCategory.And => "&"
      case CharacterCategory.Or => "|"
      case CharacterCategory.LessThan => "<"
      case CharacterCategory.GreaterThan => ">"
      case CharacterCategory.Equals => "="
      case CharacterCategory.Colon => ":"
      case CharacterCategory.Plus => "+"
      case CharacterCategory.Minus => "-"

      case CharacterCategory.NotEquals => "≠"
      case CharacterCategory.LessThanEq => "≤"
      case CharacterCategory.GreaterThanEq => "≥"

      case CharacterCategory.Dot => "."
      case CharacterCategory.Comma => ","
      case CharacterCategory.Semicolon => ";"

      case CharacterCategory.OpenParen => "("
      case CharacterCategory.CloseParen => ")"
      case CharacterCategory.OpenSquare => "["
      case CharacterCategory.CloseSquare => "]"
      case CharacterCategory.OpenCurly => "{"
      case CharacterCategory.CloseCurly => "}"

      case CharacterCategory.Star => "*"
      case CharacterCategory.Times => "×"
      case CharacterCategory.Slash => "/"
      case CharacterCategory.Divide => "÷"

      case CharacterCategory.Caret => "^"
      case CharacterCategory.Tilde => "~"
        
    }
  }

  final case class LookupFailedError(description: LookupDescription, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Could not find identifier"
  }

  final case class AmbiguousLookupError(alternatives: NonEmptyVector[Callable], source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Lookup is ambiguous"
  }

  final case class OverloadedLookupFailed(alternatives: NonEmptyVector[(Callable, NonEmptyList[CompilationError])], source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Overloaded lookup failed"
  }

  final case class NamespaceUsedAsValueError(description: LookupDescription, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Namespace used as a value"
  }

  final case class UnsupportedModuleFormatVersion(version: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Unsupported module format version ${version.toString}"
  }

  final case class ReferencedModuleNotFound(ref: module.ModuleReference, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Could not find referenced module '${ref.name}'"
  }

  final case class MissingModuleName(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Missing module name"
  }

  final case class ModuleFormatInvalid(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Module format is invalid"
  }

  final case class NamespaceElementNotFound(module: ModuleId, namespacePath: NamespacePath, name: GlobalName, source: CompilationMessageSource) extends CompilationError {
    private def nameStr: String =
      name match {
        case GlobalName.Normal(name) => s"'$name'"
        case GlobalName.Unnamed => "[unnamed]"
      }

    override def message: String = s"Could not find $nameStr under namespace '${formatNamespace(namespacePath)}' in module '${module.name}'"
  }

  final case class ExpressionNotTypeError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Expression is not a type"
  }

  final case class MutableVariableNotPureError(name: VariableName, source: CompilationMessageSource) extends CompilationError {

    private def nameStr: String =
      name match {
        case VariableName.Normal(name) => s"'$name'"
        case VariableName.Unnamed => "[unnamed]"
      }

    override def message: String = s"Declaring mutable variable $nameStr does not meet purity requirements"
  }

  final case class ImpureFunctionCalledError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"An invoked function does not meet purity requirements"
  }

  sealed trait ModuleObjectType {
    def name: String
  }
  case object ModuleObjectTrait extends ModuleObjectType {
    override def name: String = "trait"
  }
  case object ModuleObjectClass extends ModuleObjectType {
    override def name: String = "class"
  }
  case object ModuleObjectDataConstructor extends ModuleObjectType {
    override def name: String = "data constructor"
  }
  case object ModuleObjectFunction extends ModuleObjectType {
    override def name: String = "function"
  }
  case object ModuleObjectMethod extends ModuleObjectType {
    override def name: String = "method"
  }
  case object ModuleObjectClassConstructor extends ModuleObjectType {
    override def name: String = "class constructor"
  }
  case object ModuleObjectReferencedModule extends ModuleObjectType {
    override def name: String = "referenced module"
  }

  final case class ModuleObjectModuleNotLoaded(objectType: ModuleObjectType, id: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Reference is in unloaded module for ${objectType.name} #${id.toString}."
  }

  final case class ModuleObjectNotFound(objectType: ModuleObjectType, id: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"${objectType.name} #${id.toString} could not be found in specified module."
  }

  final case class ModuleObjectUndefined(objectType: ModuleObjectType, id: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"${objectType.name} #${id.toString} is undefined."
  }

  final case class ModuleObjectInvalidId(objectType: ModuleObjectType, id: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"${objectType.name} ID #${id.toString} is invalid."
  }

  final case class ModuleObjectMustBeDefinition(objectType: ModuleObjectType, id: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"${objectType.name} #${id.toString} was expected to be a definition."
  }

  final case class MetaClassNotSpecified(objectType: ModuleObjectType, id: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Meta class for ${objectType.name} #${id.toString} was not specified."
  }

  final case class CouldNotFindCompatibleModuleLoader(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "A compatible module loader could not be found."
  }

  final case class ModuleDependencyNotFound(missingDependency: ModuleId, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"A module requires a dependency (${missingDependency.name}) that was not found."
  }

  final case class CircularDependencyLoadingModule(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "A circular dependency was detected among modules."
  }

  final case class InvalidExternFunction(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "An external function is invalid."
  }

  final case class AmbiguousExtern(name: String, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Extern \"" + name + "\" is ambiguous."
  }

  final case class InvalidAccessModifierCombination(accessModifier1: parser.AccessModifier, accessModifier2: parser.AccessModifier, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"The access modifier '${formatParserAccessModifier(accessModifier1)}' cannot be combined with '${formatParserAccessModifier(accessModifier2)}'"
  }

  final case class AccessModifierNotAllowedForGlobal(accessModifier: AccessModifier, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"The access modifier '${formatAccessModifier(accessModifier)}' is not valid on global declarations."
  }

  final case class ParameterTypeAnnotationRequired(paramName: String, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Parameter '$paramName' is missing a type annotation."
  }

  final case class UnexpectedStatement(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Unexpected statement."
  }

  final case class FieldMustHaveName(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Field must have name."
  }

  final case class NonAbstractMethodNotImplemented(descriptor: AbsRef[_ <: Context with Singleton, ArMethod], source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Non abstract method must have an implementation."
  }

  final case class InvalidBaseType(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Type is not valid as a base type."
  }

  final case class MultipleBaseClasses(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Class has multiple base classes."
  }

  final case class FieldNotFound(name: String, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"""No field "$name" was found."""
  }

  final case class BaseConstructorNotCalled(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Base constructor was not called."
  }

  final case class InvalidBaseConstructorCall(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Invalid base constructor call."
  }

  final case class InvalidGlobal(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "An invalid global declaration was found."
  }

  final case class MethodMustHaveOwner(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Method must have owner."
  }

  final case class UnknownExternImplementation(name: String, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Extern implementation $name is unknown."
  }

  final case class AbstractClassConstructorCalledError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Cannot call constructor of abstract class."
  }

  final case class NonOpenClassExtendedError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Cannot extend non-open class."
  }

  final case class SealedClassExtendedError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "A sealed class may only be extend in the same file."
  }

  final case class SealedTraitExtendedError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "A sealed trait may only be extend in the same file."
  }

  final case class AbstractMethodNotImplementedError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "An abstract method was not implemented."
  }

  final case class FieldReinitializedError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "An initialized field cannot be reinitialized."
  }

  final case class FieldNotInitializedError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "A field was not initialized."
  }

  final case class ArgumentToSignatureDependencyNotPureError(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "The argument to a parameter used in a dependent manner must be pure."
  }

  final case class InvalidProtocolBufferMessage(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Invalid protocol buffer message."
  }

  final case class ResourceIOError(source: CompilationMessageSource.ThrownException) extends CompilationError {
    override def message: String = "An IO error occurred."
  }

  final case class InvalidLValue(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Invalid LValue"
  }

  final case class ElseClauseWithoutRescue(source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Else without rescue is useless."
  }

  sealed trait CouldNotConvertType extends CompilationError {
    val typeSystem: TypeSystem
    val fromType: typeSystem.TType
    val toType: typeSystem.TType
  }

  object CouldNotConvertType {
    def apply(ts: TypeSystem)(fromT: ts.TType, toT: ts.TType)(src: CompilationMessageSource): CouldNotConvertType =
      new CouldNotConvertType {
        override val typeSystem: ts.type = ts
        override val fromType: typeSystem.TType = fromT
        override val toType: typeSystem.TType = toT
        override val source: CompilationMessageSource = src

        override def message: String = "Could not convert types."
      }
  }

  private def formatNamespace(namespacePath: NamespacePath): String =
    namespacePath.ns.mkString(".")

  private def formatParserAccessModifier(accessModifier: parser.AccessModifier): String =
    accessModifier match {
      case parser.PublicModifier => "public"
      case parser.ProtectedModifier => "protected"
      case parser.PrivateModifier => "private"
      case parser.InternalModifier => "internal"
    }

  private def formatAccessModifier(accessModifier: AccessModifier): String =
    accessModifier match {
      case AccessModifier.Public => "public"
      case AccessModifier.Protected => "protected"
      case AccessModifier.ProtectedInternal => "protected internal"
      case AccessModifier.Private => "private"
      case AccessModifier.PrivateInternal => "private internal"
      case AccessModifier.Internal => "internal"
    }
}

sealed trait CompilationMessageSource {
  def formatted: String
  override def toString: String = formatted
}
object CompilationMessageSource {

  final case class SourceFile(file: FileSpec, location: SourceLocation) extends CompilationMessageSource {
    override def formatted: String =
      s"${file.name} ${location.start.line.toString}.${location.start.position.toString}-${location.end.line.toString}.${location.end.position.toString}"
  }

  final case class ReferencedModule(moduleDescriptor: ModuleId) extends CompilationMessageSource {
    override def formatted: String = s"module ${moduleDescriptor.name}"
  }

  final case class ResourceIdentifier(id: ResourceIndicator) extends CompilationMessageSource {
    override def formatted: String = id.show
  }

  final case class ThrownException(ex: Exception) extends CompilationMessageSource {
    override def formatted: String = ex.toString
  }

}
