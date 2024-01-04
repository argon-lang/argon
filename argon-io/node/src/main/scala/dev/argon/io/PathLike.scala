package dev.argon.io

import dev.argon.io.jstypes.node.NodePath

type PathLike = String

object PathLike {
  def fromString(s: String): PathLike = s
  def join(a: PathLike, b: String): PathLike = NodePath.join(a, b)
}
