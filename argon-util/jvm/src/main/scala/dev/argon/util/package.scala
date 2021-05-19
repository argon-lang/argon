package dev.argon

import zio.blocking.Blocking

package object util {
  type Id[+A] = A
  type MaybeBlocking = Blocking
}
