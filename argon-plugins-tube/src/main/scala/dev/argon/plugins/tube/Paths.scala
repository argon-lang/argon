package dev.argon.plugins.tube

import dev.argon.tube as t
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

private[tube] object Paths {
  def get[A <: GeneratedMessage: GeneratedMessageCompanion](args: Any*): String =
    val path = t.TubeProto.zipentry.get(summon[GeneratedMessageCompanion[A]].scalaDescriptor.getOptions).get.path
    assert(path.nonEmpty)
    path.format(args*)
  end get
}
