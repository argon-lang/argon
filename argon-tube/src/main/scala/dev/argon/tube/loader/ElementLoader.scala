package dev.argon.tube.loader

import dev.argon.compiler.UsingContext
import dev.argon.tube.ImportSpecifier
import dev.argon.util.UniqueIdentifier

private[loader] trait ElementLoader extends UsingContext {
  def getTube(id: BigInt): Comp[ArTube]
  def getModule(id: BigInt): Comp[ArModule]
  def getFunction(id: BigInt): Comp[ArFunc]
  def getRecord(id: BigInt): Comp[ArRecord]
  def getRecordField(id: BigInt): Comp[RecordField]
  def getEnum(id: BigInt): Comp[ArEnum]
  def getEnumVariant(id: BigInt): Comp[EnumVariant]
  def getTrait(id: BigInt): Comp[ArTrait]
  def getMethod(id: BigInt): Comp[ArMethod]
  def getInstance(id: BigInt): Comp[ArInstance]

  def getLocalImportId(localImport: ImportSpecifier.Local): Comp[UniqueIdentifier]
  def recordLocalImport(localImport: ImportSpecifier.Local, moduleExport: ModuleExport): Comp[Unit]
  def resolveLocalImport(localImport: ImportSpecifier.Local): Comp[ModuleExport]
}
