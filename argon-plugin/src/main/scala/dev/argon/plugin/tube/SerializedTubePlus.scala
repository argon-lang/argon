package dev.argon.plugin.tube

import dev.argon.tube.*
import zio.*

trait SerializedTubePlus[R, E, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation] extends SerializedTube[R, E] {
  def getExternMethodImplementation(id: BigInt): ZIO[R, E, ExternMethodImplementation]
  def getExternFunctionImplementation(id: BigInt): ZIO[R, E, ExternFunctionImplementation]
  def getExternClassConstructorImplementation(id: BigInt): ZIO[R, E, ExternClassConstructorImplementation]
  def getVTableDiff(id: BigInt): ZIO[R, E, VTable] 
}
