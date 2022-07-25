package dev.argon.plugins.source

import dev.argon.parser
import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.{*, given}
import zio.*

object AccessUtil {
  def parse(modifiers: Seq[WithSource[parser.Modifier]]): IO[CompError, AccessModifier] =
    modifiers.foldLeftM(Option.empty[AccessModifier]) {
      case (None, WithSource(parser.PublicModifier, _)) => ZIO.some(AccessModifier.Public)
      case (None, WithSource(parser.ProtectedModifier, _)) => ZIO.some(AccessModifier.Protected)
      case (None, WithSource(parser.PrivateModifier, _)) => ZIO.some(AccessModifier.Private)
      case (None, WithSource(parser.InternalModifier, _)) => ZIO.some(AccessModifier.TubePrivate)

      case (Some(AccessModifier.TubePrivate), WithSource(parser.PrivateModifier, _)) | (Some(AccessModifier.Private), WithSource(parser.InternalModifier, _)) => ZIO.some(AccessModifier.FilePrivate)
      case (Some(AccessModifier.Private), WithSource(parser.ProtectedModifier, _)) | (Some(AccessModifier.Protected), WithSource(parser.PrivateModifier, _)) => ZIO.some(AccessModifier.TubeAndProtected)
      case (Some(AccessModifier.TubePrivate), WithSource(parser.ProtectedModifier, _)) | (Some(AccessModifier.Protected), WithSource(parser.InternalModifier, _)) => ZIO.some(AccessModifier.TubeOrProtected)

      case (_, WithSource(_: AccessModifier, _)) => ZIO.fail(DiagnosticError.InvalidAccessModifierCombination())
      case (prev, _) => ZIO.succeed(prev)
    }.map { _.getOrElse(AccessModifier.Private) }

  def parseGlobal(modifiers: Seq[WithSource[parser.Modifier]]): IO[CompError, AccessModifierGlobal] =
    parse(modifiers).flatMap {
      case global: AccessModifierGlobal => ZIO.succeed(global)
      case AccessModifier.Private => ZIO.succeed(AccessModifier.FilePrivate)
      case modifier => ZIO.fail(DiagnosticError.InvalidGlobalAccessModifier(modifier))
    }
}
