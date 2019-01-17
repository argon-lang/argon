package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.PayloadSpecifiers._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup.LookupNames

final class JSEmitter {

  private val moduleVarName = JSIdentifier("modules")
  private val traitsVarName = JSIdentifier("traits")
  private val classesVarName = JSIdentifier("classes")
  private val dataCtorsVarName = JSIdentifier("dataConstructors")
  private val funcsVarName = JSIdentifier("functions")
  private val methodsPropName = JSIdentifier("methods")

  private val create_empty_obj = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("create")), Vector(JSNull))
  private def freeze_obj(varName: JSIdentifier) = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("freeze")), Vector(varName))

  def emitModule[TComp[+_] : Compilation](context: JSContext[TComp])(module: ArModule[context.type, DeclarationPayloadSpecifier]): TComp[JSModule] = {

    val modulePairs = module.referencedModules
      .zipWithIndex
      .map { case (refModule, i) => (refModule, JSIdentifier(s"module_$i")) }


    for {
      globalNamespace <- module.globalNamespace
      topLevelStmts <- allNamespaceElements(context)(globalNamespace).toVector.traverse(createObjectsForScopeValue(context))
    } yield JSModule(
      Vector(
        modulePairs.map { case (refModule, importId) =>
          JSImportAllStatement(None, importId, refModule.descriptor.name)
        },

        Vector(
          JSConst(NonEmptyList(
            JSDeclareInit(JSBindingIdentifier(moduleVarName), create_empty_obj)
          )),

          JSExportDeclaration(JSConst(NonEmptyList(
            JSDeclareInit(JSBindingIdentifier(funcsVarName), create_empty_obj)
          ))),

          JSExportDeclaration(JSConst(NonEmptyList(
            JSDeclareInit(JSBindingIdentifier(traitsVarName), create_empty_obj)
          ))),
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
            case context.JSImpl.Function.ExpressionBody(expr) => createExpressionImpl(context)(func.descriptor)(sig)(expr)
          }
        } yield JSAssignment(
            JSPropertyAccessBracket(funcsVarName, JSString(DescriptorId.forFunc(func.descriptor, ErasedSignature.fromSignature(context)(sig)))),
            JSObjectLiteral(Vector(
              JSObjectProperty("value", body)
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

  private def createExpressionImpl[TComp[+_] : Compilation](context: JSContext[TComp])(owner: ParameterOwnerDescriptor)(sig: context.signatureContext.Signature[FunctionResultInfo])(expr: context.typeSystem.ArExpr): TComp[JSExpression] = {

    val parameterList: JSFunctionParameterList =
      sig.unsubstitutedParameters.zipWithIndex.foldRight[JSFunctionParameterList](JSFunctionEmptyParameterList) { case ((param, paramIndex), list) =>
        JSFunctionParameter(
          if(param.tupleVars.nonEmpty)
            JSArrayDestructBinding(
              param.tupleVars.map { tupleVar =>
                JSBindingIdentifier(getVariableName(tupleVar.descriptor))
              }
            )
          else
            JSBindingIdentifier(getVariableName(ParameterDescriptor(owner, paramIndex))),

          list
        )
      }

    def convertStmt(expr: context.typeSystem.ArExpr): TComp[Vector[JSStatement]] = expr match {
      case context.typeSystem.LetBinding(variable, value, next) =>
        for {
          valueExpr <- convertExpr(value)
          nextStmts <- convertStmt(next)
          decl = JSDeclareInit(JSBindingIdentifier(getVariableName(variable.descriptor)), valueExpr)
          declStmt = variable.mutability match {
            case Mutability.Mutable => JSLet(NonEmptyList(decl))
            case Mutability.NonMutable => JSConst(NonEmptyList(decl))
          }
        } yield declStmt +: nextStmts

      case _ => convertExpr(expr).map(jsExpr => Vector(JSReturn(jsExpr)))
    }

    def convertExpr(expr: context.typeSystem.ArExpr): TComp[JSExpression] = {
      import context.typeSystem. { context => _, _ }
      expr match {
        case FunctionCall(func, args, _) =>
          for {
            sig <- func.value.signature

            funcExpr = func.value.descriptor match {
              case FuncDescriptor.InNamespace(moduleDesc, ns, name, _) =>
                val funcsObject =
                  if(moduleDesc === getParamOwnerModule(owner))
                    funcsVarName
                  else
                    JSPropertyAccessDot(
                      JSPropertyAccessBracket(moduleVarName, JSString(moduleDesc.name)),
                      funcsVarName
                    )

                JSPropertyAccessDot(
                  JSPropertyAccessBracket(
                    funcsObject,
                    JSString(DescriptorId.forFunc(func.value.descriptor, ErasedSignature.fromSignature(context)(sig)))
                  ),
                  JSIdentifier("value")
                )
            }

            argExprs <- args.traverse(convertExpr(_))

          } yield JSFunctionCall(funcExpr, argExprs)

        case FunctionObjectCall(funcExpr, arg, _) =>
          for {
            jsFunc <- convertExpr(funcExpr)
            jsArg <- convertExpr(arg)
          } yield JSFunctionCall(jsFunc, Vector(jsArg))

        case LetBinding(_, _, _) =>
          wrapStatement(expr)

        case LoadConstantInt(i, _) =>
          JSFunctionCall(
            JSPropertyAccessDot(
              JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
              JSIdentifier("createInt")
            ),
            Vector(JSBigInt(i))
          ).point[TComp]

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
            values <- expr.values.toVector.traverse { elem => convertExpr(elem.value) }
          } yield JSArrayLiteral(values)

        case LoadUnit(_) =>
          JSPropertyAccessDot(
            JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
            JSIdentifier("unitValue")
          ).point[TComp]

        case LoadVariable(variable) =>
          getVariableName(variable.descriptor).point[TComp]

        case MethodCall(method, instance, args, _) =>
          for {
            sig <- method.value.signature

            instanceExpr <- convertExpr(instance)

            methodSymbol = {
              val descriptor = method.value.descriptor
              val ownerObj = getClassLikeJSObject(getParamOwnerModule(owner), descriptor.typeDescriptor)

              JSPropertyAccessDot(
                JSPropertyAccessBracket(
                  JSPropertyAccessDot(
                    ownerObj,
                    methodsPropName
                  ),
                  JSString(DescriptorId.forMethod(method.value.descriptor, ErasedSignature.fromSignature(context)(sig)))
                ),
                JSIdentifier("symbol")
              )
            }

            methodExpr = JSPropertyAccessBracket(instanceExpr, methodSymbol)

            argExprs <- args.traverse(convertExpr(_))

          } yield JSFunctionCall(methodExpr, argExprs)


        case PrimitiveOp(PrimitiveOperation.AddInt, left, right, _) =>
          for {
            leftExpr <- convertExpr(left)
            rightExpr <- convertExpr(right)
          } yield JSFunctionCall(
            JSPropertyAccessDot(
              JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
              JSIdentifier("addInt")
            ),
            Vector(leftExpr, rightExpr)
          )

        case _ => ???
      }
    }

    def wrapStatement(expr: context.typeSystem.ArExpr): TComp[JSExpression] =
      for {
        stmts <- convertStmt(expr)
      } yield JSFunctionCall(JSFunctionExpression(None, JSFunctionEmptyParameterList, stmts), Vector())




    for {
      body <- convertStmt(expr)
    } yield JSFunctionExpression(
      None,
      parameterList,
      body
    )
  }

  private def getVariableName(descriptor: VariableLikeDescriptor): JSIdentifier = JSIdentifier(descriptor match {
    case ParameterDescriptor(_, index) => s"param_$index"
    case DeconstructedParameterDescriptor(_, index, tupleIndex) => s"param_${index}_$tupleIndex"
    case VariableDescriptor(_, index) => s"local_$index"
  })

  private def getClassJSObject(moduleDescriptor: ModuleDescriptor, descriptor: ClassDescriptor): JSExpression = {
    val classModule = getParamOwnerModule(descriptor)
    val classesObj =
      if(moduleDescriptor === classModule)
        classesVarName
      else
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(classModule.name)),
          classesVarName
        )

    JSPropertyAccessBracket(classesObj, JSString(DescriptorId.forClass(descriptor)))
  }

  private def getTraitJSObject(moduleDescriptor: ModuleDescriptor, descriptor: TraitDescriptor): JSExpression = {
    val traitModule = getParamOwnerModule(descriptor)
    val traitsObj =
      if(moduleDescriptor === traitModule)
        traitsVarName
      else
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(traitModule.name)),
          traitsVarName
        )

    JSPropertyAccessBracket(traitsObj, JSString(DescriptorId.forTrait(descriptor)))
  }


  private def getDataCtorJSObject(moduleDescriptor: ModuleDescriptor, descriptor: DataConstructorDescriptor): JSExpression = {
    val dataCtorModule = getParamOwnerModule(descriptor)
    val dataCtorsObj =
      if(moduleDescriptor === dataCtorModule)
        dataCtorsVarName
      else
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(dataCtorModule.name)),
          dataCtorsVarName
        )

    JSPropertyAccessBracket(dataCtorsObj, JSString(DescriptorId.forDataConstructor(descriptor)))
  }

  private def getClassLikeJSObject(moduleDescriptor: ModuleDescriptor, descriptor: ClassLikeDescriptor): JSExpression =
    descriptor match {
      case descriptor: ClassDescriptor => getClassJSObject(moduleDescriptor, descriptor)
      case descriptor: TraitDescriptor => getTraitJSObject(moduleDescriptor, descriptor)
      case descriptor: DataConstructorDescriptor => getDataCtorJSObject(moduleDescriptor, descriptor)
    }

  private def getParamOwnerModule(descriptor: ParameterOwnerDescriptor): ModuleDescriptor =
    descriptor match {
      case FuncDescriptor.InNamespace(module, _, _, _) => module
      case MethodDescriptor(typeDescriptor, _, _) => getParamOwnerModule(typeDescriptor)
      case ClassDescriptor.InNamespace(module, _, _, _) => module
      case ClassDescriptor.MetaClass(ownerClass) => getParamOwnerModule(ownerClass)
      case ClassDescriptor.TraitMetaClass(ownerTrait) => getParamOwnerModule(ownerTrait)
      case TraitDescriptor.InNamespace(module, _, _, _) => module
      case DataConstructorDescriptor.InNamespace(module, _, _, _) => module
      case ClassConstructorDescriptor(ownerClass, _) => getParamOwnerModule(ownerClass)
    }
}
