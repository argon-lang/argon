package com.mi3software.argon.compiler

import java.io.File

import scalaz.effect.IO

trait ModuleLoader {

  type ModuleData

  def loadFile(file: File): IO[Option[ModuleData]]
  def dataDescriptor(data: ModuleData): Option[ModuleDescriptor]
  def dataReferencedModules(data: ModuleData): Vector[ModuleDescriptor]
  def loadModuleReference(context: Context)(data: ModuleData)(referencedModules: Vector[ArModule[context.type]]): context.Comp[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]

}
