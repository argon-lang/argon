package dev.argon.loaders.armodule

object ModulePaths {
  val metadata = "argon_module"
  def traitDef(id: Int): String = s"trait/${id.abs.toString}"
  def traitRef(id: Int): String = s"trait-ref/${id.abs.toString}"
  def classDef(id: Int): String = s"class/${id.abs.toString}"
  def classRef(id: Int): String = s"class-ref/${id.abs.toString}"
  def dataCtorDef(id: Int): String = s"dataCtor/${id.abs.toString}"
  def dataCtorRef(id: Int): String = s"dataCtor-ref/${id.abs.toString}"
  def funcDef(id: Int): String = s"func/${id.abs.toString}"
  def funcRef(id: Int): String = s"func-ref/${id.abs.toString}"
  def methodDef(id: Int): String = s"method/${id.abs.toString}"
  def methodRef(id: Int): String = s"method-ref/${id.abs.toString}"
  def classCtorDef(id: Int): String = s"classCtor/${id.abs.toString}"
  def classCtorRef(id: Int): String = s"classCtor-ref/${id.abs.toString}"
}
