package dev.argon.util

import cats._
import cats.implicits._
import scalaz.deriving
import scalaz.std.AllInstances._

@deriving(scalaz.Equal)
final case class FileID(id: Int)

object FileID {

  implicit val eqInstance: Eq[FileID] = derived.semi.eq

}
