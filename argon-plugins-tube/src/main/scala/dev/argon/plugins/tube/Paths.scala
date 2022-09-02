package dev.argon.plugins.tube

import dev.argon.tube as t
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

private[tube] object Paths {
  def get[A <: GeneratedMessage: GeneratedMessageCompanion]: String =
    val path = t.TubeProto.zipentry.get(t.ClassDefinition.scalaDescriptor.getOptions).get.path
    assert(path.nonEmpty)
    path
  end get
}
