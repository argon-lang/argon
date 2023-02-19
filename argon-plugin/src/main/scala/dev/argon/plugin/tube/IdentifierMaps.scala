package dev.argon.plugin.tube

import dev.argon.compiler.definitions.{Definition, HasImplementation}
import zio.*
import zio.stm.*
import zio.direct.*

private[tube] final case class IdentifierMaps[A <: Definition, IsImplementation <: Boolean]
(
  referenceIds: TMap[A, BigInt],
  references: TRef[Seq[A]],
  definitionIds: TMap[A, BigInt],
  definitionLookup: TMap[BigInt, A & HasImplementation[IsImplementation]],
)

private[tube] object IdentifierMaps {
  def make[A <: Definition, IsImplementation <: Boolean]: UIO[IdentifierMaps[A, IsImplementation]] =
    defer {
      IdentifierMaps(
        referenceIds = TMap.empty[A, BigInt].commit.run,
        references = TRef.make(Seq.empty[A]).commit.run,
        definitionIds = TMap.empty[A, BigInt].commit.run,
        definitionLookup = TMap.empty[BigInt, A & HasImplementation[IsImplementation]].commit.run,
      )
    }
}
