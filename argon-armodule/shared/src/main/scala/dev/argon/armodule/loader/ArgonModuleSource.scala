package dev.argon.armodule.loader

import dev.argon.{module => ArgonModule}
import dev.argon.compiler._

trait ArgonModuleSource {

  val metadata: ArgonModule.Metadata
  def references: Comp[ArgonModule.ModuleReferencesList]

  def namespaces: CompStream[ArgonModule.NamespaceDeclaration]
  def namespaceElements(id: Int): CompStream[ArgonModule.GlobalDeclarationElement]

  def getTraitDef(id: Int): Comp[ArgonModule.TraitDefinition]
  def getTraitRef(id: Int): Comp[ArgonModule.TraitReference]

  def getClassDef(id: Int): Comp[ArgonModule.ClassDefinition]
  def getClassRef(id: Int): Comp[ArgonModule.ClassReference]

  def getDataConstructorDef(id: Int): Comp[ArgonModule.DataConstructorDefinition]
  def getDataConstructorRef(id: Int): Comp[ArgonModule.DataConstructorReference]

  def getFunctionDef(id: Int): Comp[ArgonModule.FunctionDefinition]
  def getFunctionRef(id: Int): Comp[ArgonModule.FunctionReference]

  def getMethodDef(id: Int): Comp[ArgonModule.MethodDefinition]
  def getMethodRef(id: Int): Comp[ArgonModule.MethodReference]

  def getClassConstructorDef(id: Int): Comp[ArgonModule.ClassConstructorDefinition]
  def getClassConstructorRef(id: Int): Comp[ArgonModule.ClassConstructorReference]

}
