package dev.argon.backend.jvm

import dev.argon.compiler.loaders.ResourceIndicator

sealed trait JVMResourceIndicator[I <: ResourceIndicator] extends ResourceIndicator
object JVMResourceIndicator {
  final case class Wrapped[I <: ResourceIndicator](inner: I) extends JVMResourceIndicator[I] {
    override def extension: String = inner.extension

    override def show: String = inner.show
  }
  final case class SystemLibrary[I <: ResourceIndicator](moduleName: String) extends JVMResourceIndicator[I] {
    override def extension: String = ""
    override def show: String = "Java system module: " + moduleName
  }
  final case class ClassPath[I <: ResourceIndicator]() extends JVMResourceIndicator[I] {
    override def extension: String = ""
    override def show: String = "Java classpath"
  }
}
