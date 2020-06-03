package dev.argon.backend.js

import dev.argon.compiler.core._
import zio.UIO

object IdProvider {

  trait Service {
    def getClassId(id: ClassId): UIO[Int]
    def getTraitId(id: TraitId): UIO[Int]
    def getDataConstructorId(id: DataConstructorId): UIO[Int]
    def getFunctionId(id: FunctionId): UIO[Int]
  }

}
