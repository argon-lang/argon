package dev.argon.plugins.source

import dev.argon.parser
import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.{*, given}
import zio.*

object AccessUtil {
  def parse(modifiers: Seq[WithSource[parser.Modifier]]): IO[CompError, AccessModifier] =
    modifiers.foldLeftM(Option.empty[AccessModifier]) {
      case (None, WithLocation(parser.PublicModifier, _)) => ZIO.some(AccessModifier.Public)
      case (None, WithLocation(parser.ProtectedModifier, _)) => ZIO.some(AccessModifier.Protected)
      case (None, WithLocation(parser.PrivateModifier, _)) => ZIO.some(AccessModifier.Private)
      case (None, WithLocation(parser.InternalModifier, _)) => ZIO.some(AccessModifier.TubePrivate)

      case (Some(AccessModifier.TubePrivate), WithLocation(parser.PrivateModifier, _)) | (Some(AccessModifier.Private), WithLocation(parser.InternalModifier, _)) => ZIO.some(AccessModifier.ModulePrivate)
      case (Some(AccessModifier.Private), WithLocation(parser.ProtectedModifier, _)) | (Some(AccessModifier.Protected), WithLocation(parser.PrivateModifier, _)) => ZIO.some(AccessModifier.TubeAndProtected)
      case (Some(AccessModifier.TubePrivate), WithLocation(parser.ProtectedModifier, _)) | (Some(AccessModifier.Protected), WithLocation(parser.InternalModifier, _)) => ZIO.some(AccessModifier.TubeOrProtected)

      case (_, WithLocation(_: AccessModifier, _)) => ZIO.fail(DiagnosticError.InvalidAccessModifierCombination())
      case (prev, _) => ZIO.succeed(prev)
    }.map { _.getOrElse(AccessModifier.Private) }

  def parseGlobal(modifiers: Seq[WithSource[parser.Modifier]]): IO[CompError, AccessModifierGlobal] =
    parse(modifiers).flatMap {
      case global: AccessModifierGlobal => ZIO.succeed(global)
      case AccessModifier.Private => ZIO.succeed(AccessModifier.ModulePrivate)
      case modifier => ZIO.fail(DiagnosticError.InvalidGlobalAccessModifier(modifier))
    }
}
