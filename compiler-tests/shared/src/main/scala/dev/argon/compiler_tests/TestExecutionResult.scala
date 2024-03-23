package dev.argon.compiler_tests

import dev.argon.compiler.CompilerError
import java.io.StringWriter
import java.io.PrintWriter

enum TestExecutionResult {
  case Output(s: String)
  case CompileErrors(errors: Seq[CompilerError])
  case ExecutionErrors(ex: Throwable)

  override def toString(): String =
    this match {
      case Output(s) => "Output:\n" + s

      case CompileErrors(errors) =>
        "Compile Errors: " + errors.toString()

      case ExecutionErrors(ex) =>
        val sw = new StringWriter()
        ex.printStackTrace(new PrintWriter(sw))
        "ExecutionErrors:\n" + sw.toString()
    }
}
