package dev.argon.io

import org.scalajs.dom.URL

type PathLike = URL

object PathLike {
  def fromString(s: String): PathLike = new URL(s, org.scalajs.dom.window.location.href)
  def join(a: PathLike, b: String): PathLike = new URL(b, a.toString)
}
