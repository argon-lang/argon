package dev.argon.plugin.jsapi

import scalajs.js

trait SerializedTube extends js.Any {
  def version(): js.Promise[proto.TubeFormatVersion]
  def metadata(): js.Promise[proto.Metadata]

  def getResource(id: String): js.Promise[FileSystemResource]

  def getModule(modulePath: proto.ModulePath): js.Promise[proto.ModuleDefinition]
  def getClass(id: js.BigInt): js.Promise[proto.ClassDefinition]
  def getTrait(id: js.BigInt): js.Promise[proto.TraitDefinition]
  def getFunction(id: js.BigInt): js.Promise[proto.FunctionDefinition]
  def getMethod(id: js.BigInt): js.Promise[proto.MethodDefinition]
  def getClassConstructor(id: js.BigInt): js.Promise[proto.ClassConstructorDefinition]

  def close(): js.Promise[Unit]
}
