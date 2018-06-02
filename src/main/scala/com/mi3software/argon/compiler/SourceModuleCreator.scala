package com.mi3software.argon.compiler

import com.mi3software.argon.parser.SourceAST
import com.mi3software.argon.util.Compilation
import scalaz._
import Scalaz._
import scalaz.effect.IO

private[compiler] object SourceModuleCreator {


  def createModule[TComp[+_] : Monad : Compilation]
  (context: Context with ({ type Comp[+A] = TComp[A] }))
  (input: CompilerInput)
  : IO[TComp[ArModule[context.type]]] =
    loadReferenceModules[TComp](context)(input)
      .map { _.flatMap { refModules => createModuleWithRefs[TComp](context)(input)(refModules) } }

  private def createModuleWithRefs[TComp[+_] : Monad : Compilation]
  (context2: Context with ({ type Comp[+A] = TComp[A] }))
  (input: CompilerInput)
  (referencedModules: Vector[ArModuleWithPayload[context2.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
    : TComp[ArModuleWithPayload[context2.type, PayloadSpecifiers.ReferencePayloadSpecifier]] =
    for {
      moduleElements <- input.source.traverseU { ast => createNamespaceElementFromAST[TComp](context2)(referencedModules)(ast) }

    } yield new ArModuleWithPayload[context2.type, PayloadSpecifiers.ReferencePayloadSpecifier] {
      override val context: context2.type = context2
      override val descriptor: ModuleDescriptor = input.options.moduleDescriptor
      override val globalNamespace: Namespace[ScopeValue[context.ContextScopeTypes]] =
        NamespaceBuilder.createNamespace(moduleElements)
    }


  private def loadReferenceModules[TComp[+_] : Monad : Compilation]
  (context: Context with ({ type Comp[+A] = TComp[A] }))
  (input: CompilerInput)
  : IO[TComp[Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]]] =
    ModuleLoader.loadReferencedModules(context)(input.references)

  private def createNamespaceElementFromAST[TComp[+_] : Monad : Compilation]
  (context: Context with ({ type Comp[+A] = TComp[A] }))
  (referencedModules: Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]])
  (sourceAST: SourceAST)
  : TComp[ModuleElement[ScopeValue[context.ContextScopeTypes]]] =
    ???
}
