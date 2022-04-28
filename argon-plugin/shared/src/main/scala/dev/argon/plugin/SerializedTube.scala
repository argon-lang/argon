package dev.argon.plugin

import zio.*
import zio.stream.*

trait SerializedTube[E] {

  def metadata(): IO[E, dev.argon.tube.Metadata.V1]

  def moduleDeclaration(id: BigInt): IO[E, dev.argon.tube.ModuleDeclaration.V1]
  def moduleDefinition(id: BigInt): IO[E, dev.argon.tube.ModuleDefinition.V1]


  def traitRef(id: BigInt): IO[E, dev.argon.tube.TraitReference.V1]
  def traitDef(id: BigInt): IO[E, dev.argon.tube.TraitDefinition.V1]

  def classRef(id: BigInt): IO[E, dev.argon.tube.ClassReference.V1]
  def classDef(id: BigInt): IO[E, dev.argon.tube.ClassDefinition.V1]

  def functionRef(id: BigInt): IO[E, dev.argon.tube.FunctionReference.V1]
  def functionDef(id: BigInt): IO[E, dev.argon.tube.FunctionDefinition.V1]
  def externFunctionImplementation(id: BigInt, platform: Platform[E]): IO[E, platform.ExternFunction]

  def methodRef(id: BigInt): IO[E, dev.argon.tube.MethodReference.V1]
  def methodDef(id: BigInt): IO[E, dev.argon.tube.MethodDefinition.V1]
  def externMethodImplementation(id: BigInt, platform: Platform[E]): IO[E, platform.ExternMethod]

  def classConstructorRef(id: BigInt): IO[E, dev.argon.tube.ClassConstructorReference.V1]
  def classConstructorDef(id: BigInt): IO[E, dev.argon.tube.ClassConstructorDefinition.V1]
  def externClassConstructorImplementation(id: BigInt, platform: Platform[E]): IO[E, platform.ExternClassConstructor]
}
