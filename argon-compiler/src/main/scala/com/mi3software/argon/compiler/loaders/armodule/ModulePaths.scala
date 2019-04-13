package com.mi3software.argon.compiler.loaders.armodule

object ModulePaths {
  val metadata = "argon_module"
  def traitDef(id: Int): String = s"trait/${id.abs}"
  def traitRef(id: Int): String = s"trait-ref/${id.abs}"
  def classDef(id: Int): String = s"class/${id.abs}"
  def classRef(id: Int): String = s"class-ref/${id.abs}"
  def dataCtorDef(id: Int): String = s"dataCtor/${id.abs}"
  def dataCtorRef(id: Int): String = s"dataCtor-ref/${id.abs}"
  def funcDef(id: Int): String = s"func/${id.abs}"
  def funcRef(id: Int): String = s"func-ref/${id.abs}"
  def methodDef(id: Int): String = s"method/${id.abs}"
  def methodRef(id: Int): String = s"method-ref/${id.abs}"
  def classCtorDef(id: Int): String = s"classCtor/${id.abs}"
  def classCtorRef(id: Int): String = s"classCtor-ref/${id.abs}"
}
