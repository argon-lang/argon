package com.mi3software.argon.compiler
import scalaz._

object HoleToArgonTypeSystemConverter {

  def apply(context: Context)(holeTypeSystem: HoleTypeSystem[context.type]): TypeSystemConverter[holeTypeSystem.type, context.typeSystem.type] =
    new TypeSystemConverter[holeTypeSystem.type, context.typeSystem.type] {

      override def convertType[TComp[+ _] : Monad : Compilation]
      (t: holeTypeSystem.TType)
      : TComp[context.typeSystem.TType] =
        ???

    }

}
