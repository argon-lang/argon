package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util._
import dev.argon.parser
import zio.IO

object AccessModifierHelpers {

  def getAccessModifiers(modifiers: Vector[WithSource[parser.Modifier]]): List[WithSource[parser.AccessModifier]] =
    modifiers
      .collect {
        case WithSource(modifier: parser.AccessModifier, loc) => WithSource(modifier, loc)
      }
      .toList

  def parseGlobalAccessModifier(fileSpec: FileSpec, stmtLocation: SourceLocation, access: List[WithSource[parser.AccessModifier]]): Comp[AccessModifierGlobal] =
    parseAccessModifier(fileSpec, access).flatMap {
      case accessModifier: AccessModifierGlobal => IO.succeed(accessModifier)
      case AccessModifier.Private => IO.succeed(AccessModifier.PrivateInternal)
      case accessModifier =>
        val loc = access.headOption.map(_.location).getOrElse(stmtLocation)
        Compilation.forErrors(DiagnosticError.AccessModifierNotAllowedForGlobal(accessModifier, DiagnosticSource.SourceFile(fileSpec, loc)))
    }

  def parseAccessModifier(fileSpec: FileSpec, access: List[WithSource[parser.AccessModifier]]): Comp[AccessModifier] =
    access match {
      case WithSource(parser.PublicModifier, _) :: Nil =>
        IO.succeed(AccessModifier.Public)

      case WithSource(a @ parser.PublicModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation.forErrors(DiagnosticError.InvalidAccessModifierCombination(a, b, DiagnosticSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.ProtectedModifier, _) :: Nil =>
        IO.succeed(AccessModifier.Protected)

      case WithSource(parser.ProtectedModifier, _) :: WithSource(parser.InternalModifier, _) :: Nil =>
        IO.succeed(AccessModifier.ProtectedInternal)

      case WithSource(a @ parser.ProtectedModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation.forErrors(DiagnosticError.InvalidAccessModifierCombination(a, b, DiagnosticSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.PrivateModifier, _) :: Nil =>
        IO.succeed(AccessModifier.Private)

      case WithSource(parser.PrivateModifier, _) :: WithSource(parser.InternalModifier, _) :: Nil =>
        IO.succeed(AccessModifier.PrivateInternal)

      case WithSource(a @ parser.PrivateModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation.forErrors(DiagnosticError.InvalidAccessModifierCombination(a, b, DiagnosticSource.SourceFile(fileSpec, loc)))

      case WithSource(parser.InternalModifier, _) :: Nil =>
        IO.succeed(AccessModifier.Internal)

      case WithSource(parser.InternalModifier, _) :: WithSource(parser.ProtectedModifier, _) :: Nil =>
        IO.succeed(AccessModifier.ProtectedInternal)

      case WithSource(parser.InternalModifier, _) :: WithSource(parser.PrivateModifier, _) :: Nil =>
        IO.succeed(AccessModifier.PrivateInternal)

      case WithSource(a @ parser.InternalModifier, loc) :: WithSource(b, _) :: _ =>
        Compilation.forErrors(DiagnosticError.InvalidAccessModifierCombination(a, b, DiagnosticSource.SourceFile(fileSpec, loc)))

      case Nil =>
        IO.succeed(AccessModifier.Private)
    }

}
