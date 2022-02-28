package dev.argon.vm

final case class VMClass(
  baseClass: Option[ClassId],
  vtableSlots: Seq[FunctionId],
  vtable: Map[FunctionId, FunctionId],
  fields: Seq[VMType],
  arrayType: Option[VMType],
)

