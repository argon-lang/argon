package dev.argon.plugin.tube

import dev.argon.tube.*
import dev.argon.io.FileSystemResource
import zio.*

trait SerializedTube[R, E] {
  def version: ZIO[R, E, TubeFormatVersion]

  def metadata: ZIO[R, E, Metadata]

  def getResource(id: String): ZIO[R, E, FileSystemResource[R, E]]

  def getModule(modulePath: ModulePath): ZIO[R, E, ModuleDefinition]
  def getClass(id: BigInt): ZIO[R, E, ClassDefinition]
  def getTrait(id: BigInt): ZIO[R, E, TraitDefinition]
  def getFunction(id: BigInt): ZIO[R, E, FunctionDefinition]
  def getMethod(id: BigInt): ZIO[R, E, MethodDefinition]
  def getClassConstructor(id: BigInt): ZIO[R, E, ClassConstructorDefinition]
}
