package com.mi3software.argon.compiler
import scalaz._

object ArgonToHoleTypeSystemConverter {

  def apply(context: Context)(holeTypeSystem: HoleTypeSystem[context.type]): TypeSystemConverterTotal[context.typeSystem.type, holeTypeSystem.type] =
    new TypeSystemConverterTotal[context.typeSystem.type, holeTypeSystem.type] {

      override def convertTypeTotal(t: TypeBase[context.typeSystem.type]): \/[TypeBase[holeTypeSystem.type], TypeHole] =
        ???
    }

}
