package dev.argon.compiler.core

import dev.argon.compiler.DiagnosticSource

sealed trait MethodImplementation[+TExtern, +TExpr]
object MethodImplementation {
  case object Abstract extends MethodImplementation[Nothing, Nothing]
  final case class Extern[+TExtern](source: DiagnosticSource, extern: TExtern) extends MethodImplementation[TExtern, Nothing]
  final case class Expression[+TExpr](expr: TExpr) extends MethodImplementation[Nothing, TExpr]
}

