package dev.argon.backend.jvm

import dev.argon.compiler.{CompilationError, CompilationMessageSource}

object JVMBackendErrors {

  final case class JarNotModule(jarPath: String, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Jar \"" + jarPath + "\" cannot be used as a module and must be on the classpath."
  }

}
