package dev.argon.util

import cats._
import cats.implicits._

final case class FileID(id: Int)

object FileID {

  implicit val eqInstance: Eq[FileID] = derived.semi.eq

}
