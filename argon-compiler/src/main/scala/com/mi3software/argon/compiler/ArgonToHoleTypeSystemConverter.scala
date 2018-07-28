package com.mi3software.argon.compiler
import scalaz._

object ArgonToHoleTypeSystemConverter {

  def apply(context: Context)(holeTypeSystem: HoleTypeSystem[context.type]): TypeSystemConverter[context.typeSystem.type, holeTypeSystem.type] =
    new TypeSystemConverter[context.typeSystem.type, holeTypeSystem.type] {

      override def convertType[TComp[+ _] : Monad : Compilation]
      (t: context.typeSystem.TType)
      : TComp[holeTypeSystem.TType] =
        ???

    }

}
