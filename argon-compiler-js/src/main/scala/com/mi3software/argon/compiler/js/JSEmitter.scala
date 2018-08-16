package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import scalaz._
import Scalaz._

final class JSEmitter {

  private val moduleVarName = JSIdentifier("modules")
  private val traitsVarName = JSIdentifier("traits")
  private val funcsVarName = JSIdentifier("funcs")

  private val create_empty_obj = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("create")), Vector(JSNull))
  private def freeze_obj(varName: JSIdentifier) = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("freeze")), Vector(varName))

  def emitModule[TComp[+_] : Compilation](context: JSContext[TComp])(module: ArModule[context.type]): TComp[JSModule] = {

    val modulePairs = module.referencedModules
      .zipWithIndex
      .map { case (refModule, i) => (refModule, JSIdentifier(s"module_$i")) }


    for {
      topLevelStmts <- allNamespaceElements(context)(module.globalNamespace).toVector.traverse(createObjectsForScopeValue(context))
    } yield JSModule(
      Vector(
        modulePairs.map { case (refModule, importId) =>
          JSImportAllStatement(None, importId, refModule.descriptor.name)
        },

        Vector(
          JSConst(NonEmptyList(
            JSBindValue(moduleVarName, create_empty_obj)
          )),

          JSConst(NonEmptyList(
            JSBindValue(funcsVarName, create_empty_obj)
          )),

          JSConst(NonEmptyList(
            JSBindValue(traitsVarName, create_empty_obj)
          )),
        ),

        modulePairs.map { case (refModule, importId) =>
          JSAssignment(
            JSPropertyAccessBracket(moduleVarName, JSString(refModule.descriptor.name)),
            importId
          )
        },

        topLevelStmts,

        Vector(
          freeze_obj(moduleVarName),
          freeze_obj(funcsVarName),
          freeze_obj(traitsVarName),
        ),

      ).flatten
    )
  }

  private def allNamespaceElements(context: Context)(namespace: Namespace[ScopeValue[context.ContextScopeTypes]]): Iterator[NonNamespaceScopeValue[context.ContextScopeTypes]] =
    namespace.bindings.iterator.flatMap {
      case NamespaceBinding(_, _, NamespaceScopeValue(ns)) => allNamespaceElements(context)(ns)
      case NamespaceBinding(_, _, scopeValue: NonNamespaceScopeValue[context.ContextScopeTypes]) => Vector(scopeValue)
    }

  private def createObjectsForScopeValue[TComp[+_] : Compilation](context: ContextComp[TComp])(value: NonNamespaceScopeValue[context.ContextScopeTypes]): TComp[JSStatement] = {
    val sigTypeSystem = new SignatureTypeSystem[context.type]
    val tsConverter = ArgonToSignatureTypeSystemConverter(context)(sigTypeSystem)

    value match {
      case VariableScopeValue(_) => ???
      case FunctionScopeValue(func) =>
        for {
          sig <- func.signature
        } yield JSAssignment(
            JSPropertyAccessBracket(funcsVarName, JSString(DescriptorId.forFunc(func.descriptor, sig.convertTypeSystem(tsConverter)))),
            JSObjectLiteral(Vector(
              JSObjectProperty("impl", JSNull)
            ))
          )

      case TraitScopeValue(arTrait) =>
        JSAssignment(
          JSPropertyAccessBracket(traitsVarName, JSString(DescriptorId.forTrait(arTrait.descriptor))),
          JSObjectLiteral(Vector(
            JSObjectProperty("symbol", JSFunctionCall(JSIdentifier("Symbol"), Vector()))
          ))
        ).point[TComp]

      case ClassScopeValue(_) => ???
      case DataConstructorScopeValue(_) => ???
    }
  }



}
