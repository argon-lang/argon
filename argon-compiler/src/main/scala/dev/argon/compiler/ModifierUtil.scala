package dev.argon.compiler

import dev.argon.util.{*, given}
import dev.argon.ast.Modifier
import dev.argon.util.{FilePosition, Location}
import zio.*

sealed trait ModifierUtil extends UsingContext {
  val context: Context
  import context.{ErrorLog, CompilerError}

  final case class ModifierParser(
    isErased: Option[Ref[Boolean]] = None,
    isProof: Option[Ref[Boolean]] = None,
  ) {
    def withErased(r: Ref[Boolean]): ModifierParser =
      copy(isErased = Some(r))

    def withProof(r: Ref[Boolean]): ModifierParser =
      copy(isProof = Some(r))

    def parse(modifiers: Seq[WithSource[Modifier]]): Comp[Unit] =
      ZIO.foreachDiscard(modifiers) { modifier =>
        def parseFlag(r: Option[Ref[Boolean]]): Comp[Unit] =
          r match {
            case Some(r) =>
              ErrorLog.logError(CompilerError.DuplicateModifier(modifier.location, modifier.value))
                .whenZIO(r.getAndSet(true))
                .unit

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

}

object ModifierUtil {
  def apply(ctx: Context): ModifierUtil & HasContext[ctx.type] =
    new ModifierUtil {
      override val context: ctx.type = ctx
    }
}
