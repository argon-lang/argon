package dev.argon.util

given [A, B](using CanEqual[A, B]): CanEqual[Option[A], Option[B]] = CanEqual.canEqualAny

given [A]: CanEqual[None.type, Option[A]] = CanEqual.canEqualAny
given [A]: CanEqual[Option[A], None.type] = CanEqual.canEqualAny

given [A, B](using CanEqual[A, B]): CanEqual[List[A], List[B]] = CanEqual.canEqualAny

given given_Nil_List[A]: CanEqual[List[Nothing], List[A]] = CanEqual.canEqualAny
given given_List_Nil[A]: CanEqual[List[A], List[Nothing]] = CanEqual.canEqualAny


given [A >: Null]: CanEqual[A, Null] = CanEqual.canEqualAny
given [A >: Null]: CanEqual[Null, A] = CanEqual.canEqualAny
