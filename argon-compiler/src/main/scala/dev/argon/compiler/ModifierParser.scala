package dev.argon.compiler

import cats.syntax.group
import dev.argon.util.{SourceLocation, WithSource}
import dev.argon.ast.Modifier
import dev.argon.expr.ErasureMode
import zio.*
import zio.prelude.{NonEmptyMap, NonEmptySet}

trait ModifierParser {
  def parse[A](group: NonEmptyMap[Set[Modifier], A]): URIO[ErrorLog, A]
  def done: URIO[ErrorLog, Unit]
}

object ModifierParser {
  def make(modifiers: Seq[WithSource[Modifier]], fallbackLocation: SourceLocation): URIO[ErrorLog, ModifierParser] =
    for
      groupedModifiers = modifiers.groupBy(_.value)
      modifierGroup <- Ref.make(groupedModifiers.view.mapValues(_.head.location).toMap)

      _ <- ZIO.foreachDiscard(groupedModifiers) { (k, v) =>
        ErrorLog.logError(CompilerError.DuplicateModifier(v.head.location, v.head.value))
          .whenDiscard(v.size > 1)
      }

    yield new ModifierParser {
      override def parse[A](group: NonEmptyMap[Set[Modifier], A]): URIO[ErrorLog, A] =
        for
          validGroupModifiers = group.keySet.toSet.flatten
          modifiers <- modifierGroup.modify { modifiers =>
            val relevantModifiers = modifiers.view.filterKeys(validGroupModifiers.contains).toMap
            val remainingModifiers = modifiers -- validGroupModifiers
            (relevantModifiers, remainingModifiers)
          }

          parsedGroup <- parseGroup(modifiers, fallbackLocation, group)
        yield parsedGroup

      override def done: URIO[ErrorLog, Unit] =
        for
          modifiers <- modifierGroup.getAndSet(Map.empty)
          _ <- ErrorLog.logError(CompilerError.InvalidModifier(
              modifiers.headOption.map(_._2).getOrElse(fallbackLocation),
              modifiers.keySet,
              NonEmptyChunk(Set()),
          )).whenDiscard(modifiers.nonEmpty)
        yield ()
    }
  
  // Assumes that modifiers is a subset of those in the group
  private def parseGroup[A](modifiers: Map[Modifier, SourceLocation], fallbackLocation: SourceLocation, group: NonEmptyMap[Set[Modifier], A]): URIO[ErrorLog, A] = {
    val modifierSet = modifiers.keySet
    group.get(modifierSet)
      .map(ZIO.succeed(_))
      .getOrElse {
        // Couldn't find an exact match.
        // Give an error and pick the best match.
        val bestValue = group
          .maxBy { (caseModifiers, _) =>
            val commonModifiers = caseModifiers.intersect(modifierSet)
            (
              commonModifiers.size,
              caseModifiers.size - commonModifiers.size
            )
          }
          ._2

        ErrorLog.logError(CompilerError.InvalidModifier(
          modifiers.headOption.map(_._2).getOrElse(fallbackLocation),
          modifierSet,
          group.keySet.toNonEmptyChunk
        )).as(bestValue)
      }
  }

  val accessModifierGlobal: NonEmptyMap[Set[Modifier], AccessModifier.Global] = NonEmptyMap(
    Set() -> AccessModifier.ModulePrivate,
    Set(Modifier.Public) -> AccessModifier.Public,
    Set(Modifier.Internal) -> AccessModifier.Internal,
    Set(Modifier.Private, Modifier.Internal) -> AccessModifier.ModulePrivate,
  )

  val accessModifierMember: NonEmptyMap[Set[Modifier], AccessModifier] = NonEmptyMap(
    Set() -> AccessModifier.Private,
    Set(Modifier.Public) -> AccessModifier.Public,
    Set(Modifier.Private) -> AccessModifier.Private,
    Set(Modifier.Protected) -> AccessModifier.Protected,
    Set(Modifier.Internal) -> AccessModifier.Internal,
    Set(Modifier.Protected, Modifier.Internal) -> AccessModifier.ProtectedOrInternal,
    Set(Modifier.Private, Modifier.Protected) -> AccessModifier.ProtectedAndInternal,
    Set(Modifier.Private, Modifier.Internal) -> AccessModifier.ModulePrivate,
  )

  val erasureModeWithToken: NonEmptyMap[Set[Modifier], ErasureMode.Declared] = NonEmptyMap(
    Set() -> ErasureMode.Concrete,
    Set(Modifier.Erased) -> ErasureMode.Erased,
    Set(Modifier.Token) -> ErasureMode.Token,
  )
  
  val erasureModeWithoutToken: NonEmptyMap[Set[Modifier], ErasureMode.DeclaredNonToken] = NonEmptyMap(
    Set() -> ErasureMode.Concrete,
    Set(Modifier.Erased) -> ErasureMode.Erased,
  )

  val isWitness: NonEmptyMap[Set[Modifier], Boolean] = NonEmptyMap(
    Set() -> false,
    Set(Modifier.Witness) -> true,
  )

  val isInline: NonEmptyMap[Set[Modifier], Boolean] = NonEmptyMap(
    Set() -> false,
    Set(Modifier.Inline) -> true,
  )

  val methodSlotConcrete: NonEmptyMap[Set[Modifier], MethodSlot] = NonEmptyMap(
    Set() -> MethodSlot.Final,
    Set(Modifier.Final) -> MethodSlot.Final,
    Set(Modifier.Override) -> MethodSlot.Override,
    Set(Modifier.Final, Modifier.Override) -> MethodSlot.FinalOverride,
    Set(Modifier.Virtual) -> MethodSlot.Virtual,
  )

  val methodSlotAbstract: NonEmptyMap[Set[Modifier], MethodSlot] = NonEmptyMap(
    Set() -> MethodSlot.Abstract,
    Set(Modifier.Override) -> MethodSlot.AbstractOverride,
  )
  
  

}
