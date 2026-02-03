package dev.argon.util

given canEqualNullableNull: [A] => CanEqual[A | Null, Null] = CanEqual.derived
given canEqualNullNullable: [A] => CanEqual[Null, A | Null] = CanEqual.derived

given canEqualNullableNullable: [A, B] => CanEqual[A, B] => CanEqual[A | Null, B | Null] = CanEqual.derived

