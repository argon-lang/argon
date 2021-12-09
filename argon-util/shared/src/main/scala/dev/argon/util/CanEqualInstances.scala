package dev.argon.util

given [A, B](using CanEqual[A, B]): CanEqual[Option[A], Option[B]] = CanEqual.canEqualAny

given [A]: CanEqual[None.type, Option[A]] = CanEqual.canEqualAny
given [A]: CanEqual[Option[A], None.type] = CanEqual.canEqualAny
