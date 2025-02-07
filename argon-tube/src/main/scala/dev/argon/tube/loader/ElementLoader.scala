package dev.argon.tube.loader

import dev.argon.compiler.UsingContext

private[loader] trait ElementLoader extends UsingContext {
  
  def getTube(id: BigInt): Comp[ArTube]
  def getModule(id: BigInt): Comp[ArModule]
  def getFunction(id: BigInt): Comp[ArFunc]
  def getRecord(id: BigInt): Comp[ArRecord]
  def getRecordField(id: BigInt): Comp[RecordField]

}
