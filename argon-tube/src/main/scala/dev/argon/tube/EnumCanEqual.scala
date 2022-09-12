package dev.argon.tube

import scalapb.{GeneratedEnum, GeneratedEnumCompanion}

given enumCanEqual[T <: GeneratedEnum](using GeneratedEnumCompanion[T]): CanEqual[T, T] = CanEqual.derived 
