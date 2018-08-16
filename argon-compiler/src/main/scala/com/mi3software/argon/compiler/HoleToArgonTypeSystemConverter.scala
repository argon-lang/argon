package com.mi3software.argon.compiler

object HoleToArgonTypeSystemConverter {

  def apply(context: Context)(holeTypeSystem: HoleTypeSystem[context.type]): TypeSystemConverter[holeTypeSystem.type, context.typeSystem.type] =
    new TypeSystemConverter[holeTypeSystem.type, context.typeSystem.type] {

      override def convertType[TComp[+ _] : Compilation]
      (t: holeTypeSystem.TType)
      : TComp[context.typeSystem.TType] =
        ???

    }

}
