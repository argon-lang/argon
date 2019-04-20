package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util._
import dev.argon.parser
import scalaz._
import Scalaz._

trait AccessModifierHelpers {

  protected def getAccessModifiers(modifiers: Vector[WithSource[parser.Modifier]]): List[WithSource[parser.AccessModifier]] =
    modifiers
      .collect {
        case WithSource(modifier: parser.AccessModifier, loc) => WithSource(modifier, loc)
      }
      .toList

  protected def parseGlobalAccessModifier[TComp[+_] : Compilation](fileSpec: FileSpec, stmtLocation: SourceLocation, access: List[WithSource[parser.AccessModifier]]): TComp[AccessModifierGlobal] =
    parseAccessModifier[TComp](fileSpec, stmtLocation, access).flatMap {
      case accessModifier: AccessModifierGlobal => accessModifier.point[TComp]
      case AccessModifier.Private => AccessModifier.PrivateInternal.point[TComp]
      case accessModifier =>
        val loc = access.headOption.map(_.location).getOrElse(stmtLocation)
        Compilation[TComp].forErrors(CompilationError.AccessModifierNotAllowedForGlobal(accessModifier, CompilationMessageSource.SourceFile(fileSpec, loc)))
    }

  protected def parseAccessModifier[TComp[+_] : Compilation](fileSpec: FileSpec, stmtLocation: SourceLocation, access: List[WithSource[parser.AccessModifier]]): TComp[AccessModifier] =
    access match {
      case WithSource(parser.PublicModifier, _) :: Nil =>
        AccessModifier.Public.point[TComp]

      case WithSource(a @ parser.PublicModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.ProtectedModifier, _) :: Nil =>
        AccessModifier.Protected.point[TComp]

      case WithSource(parser.ProtectedModifier, _) :: WithSource(parser.InternalModifier, _) :: Nil =>
        AccessModifier.ProtectedInternal.point[TComp]

      case WithSource(a @ parser.ProtectedModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.PrivateModifier, _) :: Nil =>
        AccessModifier.Protected.point[TComp]

      case WithSource(parser.PrivateModifier, _) :: WithSource(parser.InternalModifier, _) :: Nil =>
        AccessModifier.PrivateInternal.point[TComp]

      case WithSource(a @ parser.PrivateModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.InternalModifier, _) :: Nil =>
        AccessModifier.Protected.point[TComp]

      case WithSource(parser.InternalModifier, _) :: WithSource(parser.ProtectedModifier, _) :: Nil =>
        AccessModifier.ProtectedInternal.point[TComp]

      case WithSource(parser.InternalModifier, _) :: WithSource(parser.PrivateModifier, _) :: Nil =>
        AccessModifier.PrivateInternal.point[TComp]

      case WithSource(a @ parser.InternalModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation[TComp].forErrors(CompilationError.InvalidAccessModifierCombination(a, b, CompilationMessageSource.SourceFile(fileSpec, loc)))

      case Nil =>
        AccessModifier.Private.point[TComp]
    }

}
