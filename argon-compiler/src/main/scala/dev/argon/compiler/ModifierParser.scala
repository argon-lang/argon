package dev.argon.compiler

import dev.argon.util.{*, given}
import dev.argon.ast.Modifier
import dev.argon.util.{FilePosition, Location}
import zio.*


final case class ModifierParser(
  isErased: Option[Ref[Boolean]] = None,
  isProof: Option[Ref[Boolean]] = None,
) {
  def withErased(r: Ref[Boolean]): ModifierParser =
    copy(isErased = Some(r))

  def withProof(r: Ref[Boolean]): ModifierParser =
    copy(isProof = Some(r))

  def parse(modifiers: Seq[WithSource[Modifier]]): URIO[ErrorLog, Unit] =
    ZIO.foreachDiscard(modifiers) { modifier =>
      def parseFlag(r: Option[Ref[Boolean]]): URIO[ErrorLog, Unit] =
        r match {
          case Some(r) =>
            ErrorLog.logError(CompilerError.DuplicateModifier(modifier.location, modifier.value))
              .whenZIODiscard(r.getAndSet(true))

          case None =>
            ErrorLog.logError(CompilerError.InvalidModifier(modifier.location, modifier.value))
        }

      modifier.value match {
        case Modifier.Erased => parseFlag(isErased)
        case Modifier.Proof => parseFlag(isProof)
        case _ => ???
      }
    }
}
