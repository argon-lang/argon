package dev.argon.io

import zio.stream.*
import esexpr.ESExpr

trait ESExprStreamResource[+E] extends Resource[E] {
  def expr: Stream[E, ESExpr]
}
