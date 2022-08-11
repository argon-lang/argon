package dev.argon.tube

import scalapb.{GeneratedMessage, GeneratedSealedOneof, GeneratedOneof}

given canEqualMessage[A <: GeneratedMessage]: CanEqual[A, A] = CanEqual.derived
given canEqualSealedOneof[A <: GeneratedSealedOneof]: CanEqual[A, A] = CanEqual.derived
given canEqualOneof[A <: GeneratedOneof]: CanEqual[A, A] = CanEqual.derived
