package dev.argon.util

import scala.annotation.unused
import scala.util.NotGiven

given [A, B](using CanEqual[A, B]): CanEqual[Option[A], Option[B]] = CanEqual.canEqualAny

given [A]: CanEqual[None.type, Option[A]] = CanEqual.canEqualAny
given [A]: CanEqual[Option[A], None.type] = CanEqual.canEqualAny

given [A, B](using CanEqual[A, B]): CanEqual[List[A], List[B]] = CanEqual.canEqualAny

given given_Nil_List[A]: CanEqual[List[Nothing], List[A]] = CanEqual.canEqualAny
given given_List_Nil[A]: CanEqual[List[A], List[Nothing]] = CanEqual.canEqualAny

given canEqualNullableNull[A]: CanEqual[A | Null, Null] = CanEqual.derived
given canEqualNullNullable[A]: CanEqual[Null, A | Null] = CanEqual.derived

given canEqualNullableNullable[A, B](using CanEqual[A, B]): CanEqual[A | Null, B | Null] = CanEqual.derived

@unused
private def dummy(): Unit =
  summon[NotGiven[CanEqual[Option[String], String]]]
  ()

