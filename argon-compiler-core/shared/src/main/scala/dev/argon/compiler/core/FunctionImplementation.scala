package dev.argon.compiler.core

import dev.argon.compiler.DiagnosticSource

sealed trait FunctionImplementation[+TExtern, +TExpr]
object FunctionImplementation {
  final case class Extern[+TExtern](source: DiagnosticSource, extern: TExtern) extends FunctionImplementation[TExtern, Nothing]
  final case class Expression[+TExpr](expr: TExpr) extends FunctionImplementation[Nothing, TExpr]
}
