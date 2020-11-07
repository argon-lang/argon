package dev.argon.module

object ModulePaths {
  val metadata = "argon_module"
  def elementDef(typeName: String, id: Int): String = s"$typeName/${id.abs.toString}"
  def elementRef(typeName: String, id: Int): String = s"$typeName-ref/${id.abs.toString}"

  val traitTypeName = "trait"
  val classTypeName = "class"
  val dataCtorTypeName = "dataCtor"
  val funcTypeName = "func"
  val methodTypeName = "method"
  val classCtorTypeName = "classCtor"

}
