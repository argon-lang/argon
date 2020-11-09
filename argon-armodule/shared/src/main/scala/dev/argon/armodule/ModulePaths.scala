package dev.argon.armodule

object ModulePaths {
  val metadata = "argon_module"
  val referencedModules = "references"
  val namespaceIndex = "namespaces/index"
  def namespaceContent(id: Int): String = s"namespaces/${id.toString}"

  def elementDef(typeName: String, id: Int): String = s"$typeName/${id.abs.toString}"
  def elementRef(typeName: String, id: Int): String = s"$typeName-ref/${id.abs.toString}"

  val traitTypeName = "trait"
  val classTypeName = "class"
  val dataCtorTypeName = "dataCtor"
  val funcTypeName = "func"
  val methodTypeName = "method"
  val classCtorTypeName = "classCtor"

}
