package dev.argon.backend.jvm.jdkloader

import java.io.InputStream
import java.nio.file.Path

import dev.argon.compiler.ErrorList
import dev.argon.compiler.core.ModuleId
import zio._
import zio.blocking.Blocking


sealed trait JavaLibrary {

  def findClass(pkg: String, name: String): ZManaged[Blocking, Option[ErrorList], InputStream]
}

abstract class JavaModule extends JavaLibrary {
  val moduleName: String
}

abstract class ClassPath extends JavaLibrary

object JavaLibrary {

  def idFromModuleName(name: String): ModuleId = ModuleId("java:" + name)
  val classpathModuleId: ModuleId = ModuleId("java!classpath")

}
