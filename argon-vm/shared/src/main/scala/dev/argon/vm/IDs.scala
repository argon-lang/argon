package dev.argon.vm


final case class FunctionId(id: Int)
final case class LocalId(id: Int)
final case class ClassId(id: Int)
final case class FieldId(classId: Int, id: Int)
final case class LabelId(id: Int)

