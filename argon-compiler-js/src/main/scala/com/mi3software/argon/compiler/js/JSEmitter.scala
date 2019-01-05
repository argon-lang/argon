package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler.{js, _}
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.PayloadSpecifiers._
import com.mi3software.argon.compiler.core.{GlobalBinding, _}
import com.mi3software.argon.compiler.lookup.LookupNames

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
          body <- impl match {
            case context.JSImpl.Function.ExpressionBody(expr) => createExpressionImpl(context)(sig)(expr)
          }
        } yield JSAssignment(
            JSPropertyAccessBracket(funcsVarName, JSString(DescriptorId.forFunc(func.descriptor, ErasedSignature.fromSignature(context)(sig)))),
            JSObjectLiteral(Vector(
              JSObjectProperty("impl", body)
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

  private def createExpressionImpl[TComp[+_] : Compilation](context: JSContext[TComp])(sig: context.signatureContext.Signature[FunctionResultInfo])(expr: context.typeSystem.ArExpr): TComp[JSExpression] =
    for {
      body <- convertStmt(context)(expr)
    } yield JSFunctionExpression(
      None,
      createParameterList(context)(sig),
      body
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

  private def convertStmt[TComp[+_] : Compilation](context: JSContext[TComp])(expr: context.typeSystem.ArExpr): TComp[Vector[JSStatement]] = expr match {
    case _ => convertExpr(context)(expr).map(Vector(_))
  }

  private def convertExpr[TComp[+_] : Compilation](context: JSContext[TComp])(expr: context.typeSystem.ArExpr): TComp[JSExpression] = {
    import context.typeSystem. { context => _, _ }
    expr match {
      case FunctionCall(func, args, _) =>
        for {
          sig <- func.value.signature

          funcExpr = func.value.descriptor match {
            case FuncDescriptor.InNamespace(moduleDesc, ns, name, _) =>
              JSPropertyAccessBracket(
                JSPropertyAccessDot(
                  JSPropertyAccessBracket(moduleVarName, JSString(moduleDesc.name)),
                  funcsVarName
                ),
                JSString(DescriptorId.forFunc(func.value.descriptor, ErasedSignature.fromSignature(context)(sig)))
              )
          }

          argExprs <- args.traverse(convertExpr(context)(_))

        } yield JSFunctionCall(funcExpr, argExprs)

      case LoadConstantString(str, _) =>
        JSFunctionCall(
          JSPropertyAccessDot(
            JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
            JSIdentifier("createString")
          ),
          Vector(JSString(str))
        ).point[TComp]

      case expr: LoadTuple =>
        for {
          values <- expr.values.toVector.traverse { elem => convertExpr(context)(elem.value) }
        } yield JSArrayLiteral(values)

      case _ => ???
    }
  }

}
