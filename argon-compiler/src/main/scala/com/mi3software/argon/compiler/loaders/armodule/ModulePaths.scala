package com.mi3software.argon.compiler.loaders.armodule

object ModulePaths {
  val metadata = "argon_module"
  def traitDef(id: Int): String = s"trait/$id"
  def traitRef(id: Int): String = s"trait-ref/$id"
  def classDef(id: Int): String = s"class/$id"
  def classRef(id: Int): String = s"class-ref/$id"
  def dataCtorDef(id: Int): String = s"dataCtor/$id"
  def dataCtorRef(id: Int): String = s"dataCtor-ref/$id"
  def funcDef(id: Int): String = s"func/$id"
  def funcRef(id: Int): String = s"func-ref/$id"
  def methodDef(id: Int): String = s"method/$id"
  def methodRef(id: Int): String = s"method-ref/$id"
  def classCtorDef(id: Int): String = s"classCtor/$id"
  def classCtorRef(id: Int): String = s"classCtor-ref/$id"
}
