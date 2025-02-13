package dev.argon.io

import zio.*
import esexpr.ESExpr

trait ESExprResource[+E] extends Resource[E] {
  def expr: IO[E, ESExpr]
}
