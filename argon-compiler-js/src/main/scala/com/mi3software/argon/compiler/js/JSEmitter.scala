package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.PayloadSpecifiers._
import com.mi3software.argon.compiler.core.{GlobalBinding, _}

final class JSEmitter {

  private val moduleVarName = JSIdentifier("modules")
  private val traitsVarName = JSIdentifier("traits")
  private val funcsVarName = JSIdentifier("funcs")

  private val create_empty_obj = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("create")), Vector(JSNull))
  private def freeze_obj(varName: JSIdentifier) = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("freeze")), Vector(varName))

  def emitModule[TComp[+_] : Compilation](context: JSContext[TComp])(module: ArModule[context.type, DeclarationPayloadSpecifier]): TComp[JSModule] = {

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
            JSDeclareInit(JSBindingIdentifier(moduleVarName), create_empty_obj)
          )),

          JSConst(NonEmptyList(
            JSDeclareInit(JSBindingIdentifier(funcsVarName), create_empty_obj)
          )),

          JSConst(NonEmptyList(
            JSDeclareInit(JSBindingIdentifier(traitsVarName), create_empty_obj)
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

  private def allNamespaceElements(context: Context)(namespace: Namespace[context.type, DeclarationPayloadSpecifier]): Iterator[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
    namespace.bindings.iterator.flatMap {
      case GlobalBinding.NestedNamespace(_, ns) => allNamespaceElements(context)(ns)
      case binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] => Vector(binding)
    }

  private def createObjectsForScopeValue[TComp[+_] : Compilation](context: JSContext[TComp])(value: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): TComp[JSStatement] =
    value match {
      case GlobalBinding.GlobalFunction(_, _, func) =>
        for {
          sig <- func.signature
          impl <- func.payload : TComp[context.JSImpl.Function]
        } yield JSAssignment(
            JSPropertyAccessBracket(funcsVarName, JSString(DescriptorId.forFunc(func.descriptor, ErasedSignature.fromSignature(context)(sig)))),
            JSObjectLiteral(Vector(
              JSObjectProperty("impl", impl match {
                case context.JSImpl.Function.ExpressionBody(expr) => createExpressionImpl(context)(sig)(expr)
              })
            ))
          )

      case GlobalBinding.GlobalTrait(_, _, arTrait) =>
        JSAssignment(
          JSPropertyAccessBracket(traitsVarName, JSString(DescriptorId.forTrait(arTrait.descriptor))),
          JSObjectLiteral(Vector(
            JSObjectProperty("symbol", JSFunctionCall(JSIdentifier("Symbol"), Vector()))
          ))
        ).point[TComp]

      case GlobalBinding.GlobalClass(_, _, _) => ???
      case GlobalBinding.GlobalDataConstructor(_, _, _) => ???
    }

  private def createExpressionImpl[TComp[+_] : Compilation](context: JSContext[TComp])(sig: context.signatureContext.Signature[FunctionResultInfo])(expr: context.typeSystem.ArExpr): JSExpression =
    JSFunctionExpression(
      None,
      createParameterList(context)(sig),
      convertStmt(context)(expr)
    )

  private def createParameterList[TComp[+_] : Compilation](context: JSContext[TComp])(sig: context.signatureContext.Signature[FunctionResultInfo]): JSFunctionParameterList =
    sig.unsubstitutedParameters.foldRight[JSFunctionParameterList](JSFunctionEmptyParameterList) { (param, list) =>
      JSFunctionParameter(
        JSArrayDestructBinding(
          param.tupleVars.map { tupleVar =>
            JSBindingIdentifier(JSIdentifier(s"param_${tupleVar.descriptor.index}_${tupleVar.descriptor.tupleIndex}"))
          }
        ),
        list
      )
    }

  private def convertStmt[TComp[+_] : Compilation](context: JSContext[TComp])(expr: context.typeSystem.ArExpr): Vector[JSStatement] = expr match {
    case _ => Vector(convertExpr(context)(expr))
  }

  private def convertExpr[TComp[+_] : Compilation](context: JSContext[TComp])(expr: context.typeSystem.ArExpr): JSExpression = ???

}
