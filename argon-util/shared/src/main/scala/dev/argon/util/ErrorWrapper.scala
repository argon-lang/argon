package dev.argon.util

import zio.*

trait ErrorWrapper[E, EX <: Exception] {
  def wrap(error: Cause[E]): EX
  def unwrap(ex: EX): Cause[E]
}


