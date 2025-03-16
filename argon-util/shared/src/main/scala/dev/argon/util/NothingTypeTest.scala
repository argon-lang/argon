package dev.argon.util

import scala.reflect.TypeTest

object NothingTypeTest {
  given TypeTest[Any, Nothing]:
    override def unapply(x: Any): Option[x.type & Nothing] = None
  end given
}
