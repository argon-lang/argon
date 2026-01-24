package dev.argon.util

import scala.annotation.unused
import scala.util.NotGiven
import zio.Cause


given canEqualNullableNull: [A] => CanEqual[A | Null, Null] = CanEqual.derived
given canEqualNullNullable: [A] => CanEqual[Null, A | Null] = CanEqual.derived

given canEqualNullableNullable: [A, B] => CanEqual[A, B] => CanEqual[A | Null, B | Null] = CanEqual.derived

