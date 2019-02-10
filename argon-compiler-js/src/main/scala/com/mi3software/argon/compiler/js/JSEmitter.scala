package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler.core.PayloadSpecifiers._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup.LookupNames
import com.mi3software.argon.compiler.types.TypeSystem

final class JSEmitter[TComp[+_] : Compilation, TContext <: JSContext[TComp] with Singleton](context: TContext) {

  private val moduleVarName = JSIdentifier("modules")
  private val traitsVarName = JSIdentifier("traits")
  private val classesVarName = JSIdentifier("classes")
  private val dataCtorsVarName = JSIdentifier("dataConstructors")
  private val funcsVarName = JSIdentifier("functions")
  private val methodsPropName = JSIdentifier("methods")
  private val constructorsPropName = JSIdentifier("constructors")

  private val create_empty_obj = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("create")), Vector(JSNull))
  private def freeze_obj(varName: JSIdentifier) = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("freeze")), Vector(varName))

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): TComp[JSModule] = {

    val modulePairs = module.referencedModules
      .zipWithIndex
      .map { case (refModule, i) => (refModule, JSIdentifier(s"module_$i")) }


    for {
      globalNamespace <- module.globalNamespace
      topLevelStmts <- allNamespaceElements(globalNamespace).toVector.traverse(createObjectsForScopeValue)
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

          JSExportDeclaration(JSConst(NonEmptyList(
            JSDeclareInit(JSBindingIdentifier(classesVarName), create_empty_obj)
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
          freeze_obj(classesVarName),
        ),

      ).flatten
    )
  }

  private def allNamespaceElements(namespace: Namespace[context.type, DeclarationPayloadSpecifier]): Iterator[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
    namespace.bindings.iterator.flatMap {
      case GlobalBinding.NestedNamespace(_, ns) => allNamespaceElements(ns)
      case binding: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier] => Vector(binding)
    }

  private def createObjectsForScopeValue(value: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): TComp[JSStatement] =
    value match {
      case GlobalBinding.GlobalFunction(_, _, func) =>
        for {
          sig <- func.signature
          impl <- func.payload : TComp[context.JSImpl.Function]
          body <- impl match {
            case context.JSImpl.Function.ExpressionBody(expr) => createExpressionImpl(func.descriptor)(sig)(expr)
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

      case GlobalBinding.GlobalClass(_, _, arClass) =>
        for {
          sig <- arClass.signature
          fields <- arClass.fields
          methods <- arClass.methods
          staticMethods <- arClass.staticMethods
          classConstructors <- arClass.classConstructors

          fieldObjects = fields.map { field =>
            JSObjectLiteral(Vector(
              JSObjectProperty("name", JSString(field.descriptor.name))
            ))
          }
          methodObjects <- methods.traverse(createMethodObject(_))
          staticMethodObjects <- staticMethods.traverse(createMethodObject(_))
          ctorObjects <- classConstructors.traverse(createClassCtorObject(_))

          classSpec = JSObjectLiteral(Vector(

            sig.unsubstitutedResult.baseTypes.baseClass.map { baseClass =>
              JSObjectGetProperty(
                "baseClass",
                Vector(
                  JSReturn(getClassJSObject(getParamOwnerModule(arClass.descriptor), baseClass.arClass.value.descriptor))
                )
              )
            }.toVector,

            Vector(JSObjectGetProperty(
              "baseTraits",
              Vector(
                JSReturn(JSArrayLiteral(
                  sig.unsubstitutedResult.baseTypes.baseTraits.map { baseTrait =>
                    getTraitJSObject(getParamOwnerModule(arClass.descriptor), baseTrait.arTrait.value.descriptor)
                  }
                ))
              )
            )),

            Vector(
              JSObjectProperty("fields", JSArrayLiteral(fieldObjects)),
              JSObjectProperty("methods", JSArrayLiteral(methodObjects)),
              JSObjectProperty("staticMethods", JSArrayLiteral(staticMethodObjects)),
              JSObjectProperty("constructors", JSArrayLiteral(ctorObjects)),
            ),

          ).flatten)

        } yield JSAssignment(
          JSPropertyAccessBracket(classesVarName, JSString(DescriptorId.forClass(arClass.descriptor))),
          JSFunctionCall(
            JSPropertyAccessDot(
              JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
              JSIdentifier("createClass")
            ),
            Vector(classSpec)
          )
        )


      case GlobalBinding.GlobalDataConstructor(_, _, _) => ???
    }

  private def createMethodObject(method: ArMethod[context.type, PayloadSpecifiers.DeclarationPayloadSpecifier]): TComp[JSExpression] =
    for {
      sig <- method.signature
      impl <- method.payload : TComp[context.JSImpl.Method]
      body <- impl match {
        case context.JSImpl.Method.ExpressionBody(expr) => createExpressionImpl(method.descriptor)(sig)(expr)
        case context.JSImpl.Method.Abstract => JSNull.point[TComp]
      }
    } yield JSObjectLiteral(Vector(
      JSObjectProperty("descriptor", JSString(DescriptorId.forMethod(method.descriptor, ErasedSignature.fromSignature(context)(sig)))),
      JSObjectProperty("value", body),
    ))

  private def createClassCtorObject(ctor: ClassConstructor[context.type, PayloadSpecifiers.DeclarationPayloadSpecifier]): TComp[JSExpression] =
    for {
      sig <- ctor.signature
      descriptorId = DescriptorId.forClassConstructor(ctor.descriptor)
      impl <- ctor.payload : TComp[context.JSImpl.ClassConstructor]
      body <- impl match {
        case context.JSImpl.ClassConstructor.StatementBody(body) =>
          for {
            initStmts <- body.initStatements.traverseM {
              case context.typeSystem.ClassConstructorStatementExpr(expr) => convertStmt(ctor.descriptor)(useReturn = false)(expr)
              case context.typeSystem.InitializeFieldStatement(field, value) =>
                for {
                  valueExpr <- convertExpr(ctor.descriptor)(value)
                } yield Vector(JSAssignment(
                  getVariableExpr(getParamOwnerModule(ctor.descriptor), field.descriptor),
                  valueExpr
                ))
            }

            baseCall <- body.baseConstructorCall.traverse { baseCallExpr =>
              val descriptor = baseCallExpr.classCtor.value.descriptor
              val ownerObj = getClassJSObject(getParamOwnerModule(ctor.descriptor), descriptor.ownerClass)

              val baseCtorSymbol = JSPropertyAccessDot(
                JSPropertyAccessBracket(
                  JSPropertyAccessDot(
                    ownerObj,
                    constructorsPropName
                  ),
                  JSString(descriptorId)
                ),
                JSIdentifier("symbol")
              )

              baseCallExpr.args.traverse(convertExpr(ctor.descriptor)(_)).map { argExprs =>
                JSFunctionCall(
                  JSPropertyAccessDot(
                    JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")),
                    JSIdentifier("call")
                  ),
                  JSThis +: baseCtorSymbol +: argExprs
                )
              }
            }

            endExpr <- convertStmt(ctor.descriptor)(useReturn = false)(body.endExpr)
          } yield initStmts ++ baseCall.toVector ++ endExpr

      }

      func = JSFunctionExpression(
        None,
        createParameterList(ctor.descriptor)(sig),
        body
      )

    } yield JSObjectLiteral(Vector(
      JSObjectProperty("descriptor", JSString(descriptorId)),
      JSObjectProperty("value", func),
    ))


  private def createExpressionImpl(owner: ParameterOwnerDescriptor)(sig: context.signatureContext.Signature[FunctionResultInfo])(expr: context.typeSystem.ArExpr): TComp[JSExpression] =
    for {
      body <- convertStmt(owner)(useReturn = true)(expr)
    } yield JSFunctionExpression(
      None,
      createParameterList(owner)(sig),
      body
    )

  def createParameterList[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](owner: ParameterOwnerDescriptor)(sig: context.signatureContext.Signature[TResult]): JSFunctionParameterList =
    sig.unsubstitutedParameters.zipWithIndex.foldRight[JSFunctionParameterList](JSFunctionEmptyParameterList) { case ((param, paramIndex), list) =>
      JSFunctionParameter(
        if(param.tupleVars.nonEmpty)
          JSArrayDestructBinding(
            param.tupleVars.map { tupleVar =>
              JSBindingIdentifier(getDeconstructedParameterName(tupleVar.descriptor))
            }
          )
        else
          JSBindingIdentifier(getParameterName(ParameterDescriptor(owner, paramIndex))),

        list
      )
    }

  def convertStmt(owner: ParameterOwnerDescriptor)(useReturn: Boolean)(expr: context.typeSystem.ArExpr): TComp[Vector[JSStatement]] = expr match {
    case context.typeSystem.LetBinding(variable, value, next) =>
      for {
        valueExpr <- convertExpr(owner)(value)
        nextStmts <- convertStmt(owner)(useReturn)(next)
        decl = JSDeclareInit(JSBindingIdentifier(getVariableName(variable.descriptor)), valueExpr)
        declStmt = variable.mutability match {
          case Mutability.Mutable => JSLet(NonEmptyList(decl))
          case Mutability.NonMutable => JSConst(NonEmptyList(decl))
        }
      } yield declStmt +: nextStmts

    case context.typeSystem.IfElse(condition, ifBody, elseBody) =>
      for {
        condExpr <- convertExpr(owner)(condition)
        ifBodyStmts <- convertStmt(owner)(useReturn)(ifBody)
        elseBodyStmts <- convertStmt(owner)(useReturn)(elseBody)

        accessNativeBool = JSPropertyAccessBracket(
          condExpr,
          JSPropertyAccessDot(
            JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
            JSIdentifier("boolValueSymbol")
          )
        )
      } yield Vector(JSIfElseStatement(accessNativeBool, ifBodyStmts, elseBodyStmts))


    case context.typeSystem.Sequence(first, second) =>
      for {
        convFirst <- wrapStatement(owner)(first)
        convSecond <- convertStmt(owner)(useReturn)(second)
      } yield convFirst +: convSecond

    case _ =>
      if(useReturn) convertExpr(owner)(expr).map { jsExpr => Vector(JSReturn(jsExpr)) }
      else convertExpr(owner)(expr).map { jsExpr => Vector(jsExpr) }
  }

  def convertExpr(owner: ParameterOwnerDescriptor)(expr: context.typeSystem.ArExpr): TComp[JSExpression] = {
    import context.typeSystem. { context => _, _ }
    expr match {
      case ClassConstructorCall(classType, ctor, args) =>
        for {
          sig <- ctor.value.signature

          ownerObj = getClassJSObject(getParamOwnerModule(owner), ctor.value.descriptor.ownerClass)
          descriptorId = DescriptorId.forClassConstructor(ctor.value.descriptor)

          baseCtorSymbol = JSPropertyAccessDot(
            JSPropertyAccessBracket(
              JSPropertyAccessDot(
                ownerObj,
                constructorsPropName
              ),
              JSString(descriptorId)
            ),
            JSIdentifier("symbol")
          )

          argExprs <- args.traverse(convertExpr(ctor.value.descriptor)(_))

        } yield JSNewCall(JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")), baseCtorSymbol +: argExprs)


      case FunctionCall(func, args, _) =>
        for {
          sig <- func.value.signature

          funcExpr = func.value.descriptor match {
            case FuncDescriptor.InNamespace(moduleDesc, _, _, ns, name, _) =>
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

          argExprs <- args.traverse(convertExpr(owner)(_))

        } yield JSFunctionCall(funcExpr, argExprs)

      case FunctionObjectCall(funcExpr, arg, _) =>
        for {
          jsFunc <- convertExpr(owner)(funcExpr)
          jsArg <- convertExpr(owner)(arg)
        } yield JSFunctionCall(jsFunc, Vector(jsArg))

      case IfElse(_, _, _) =>
        wrapStatement(owner)(expr)

      case LetBinding(_, _, _) =>
        wrapStatement(owner)(expr)

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

      case LoadLambda(argVariable, body) =>
        for {
          bodyExpr <- convertExpr(owner)(body)
        } yield JSArrowFunctionExpr(
          JSFunctionParameter(JSBindingIdentifier(getVariableName(argVariable.descriptor)), JSFunctionEmptyParameterList),
          bodyExpr
        )

      case expr: LoadTuple =>
        for {
          values <- expr.values.toVector.traverse { elem => convertExpr(owner)(elem.value) }
        } yield JSArrayLiteral(values)

      case LoadUnit(_) =>
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
          JSIdentifier("unitValue")
        ).point[TComp]

      case LoadVariable(variable) =>
        getVariableExpr(getParamOwnerModule(owner), variable.descriptor).point[TComp]

      case MethodCall(method, instance, args, _) =>
        for {
          sig <- method.value.signature

          instanceExpr <- convertExpr(owner)(instance)

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

          argExprs <- args.traverse(convertExpr(owner)(_))

        } yield JSFunctionCall(methodExpr, argExprs)


      case PrimitiveOp(op, left, right, _) =>
        for {
          leftExpr <- convertExpr(owner)(left)
          rightExpr <- convertExpr(owner)(right)
        } yield JSFunctionCall(
          JSPropertyAccessDot(
            JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
            JSIdentifier(op match {
              case PrimitiveOperation.AddInt => "addInt"
              case PrimitiveOperation.SubInt => "subInt"
              case PrimitiveOperation.MulInt => "mulInt"
              case PrimitiveOperation.IntEqual => "intEqual"
            })
          ),
          Vector(leftExpr, rightExpr)
        )

      case e @ Sequence(_, _) =>
        wrapStatement(owner)(e)

      case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented")
    }
  }

  def wrapStatement(owner: ParameterOwnerDescriptor)(expr: context.typeSystem.ArExpr): TComp[JSExpression] =
    for {
      stmts <- convertStmt(owner)(useReturn = true)(expr)
    } yield JSFunctionCall(JSArrowFunctionStmts(JSFunctionEmptyParameterList, stmts), Vector())


  private def getParameterName(descriptor: ParameterDescriptor): JSIdentifier =
    JSIdentifier(s"param_${descriptor.index}")

  private def getDeconstructedParameterName(descriptor: DeconstructedParameterDescriptor): JSIdentifier =
    JSIdentifier(s"param_${descriptor.index}_${descriptor.tupleIndex}")

  private def getVariableName(descriptor: VariableDescriptor): JSIdentifier =
    JSIdentifier(s"local_${descriptor.index}")

  private def getVariableExpr(moduleDescriptor: ModuleDescriptor, descriptor: VariableLikeDescriptor): JSExpression = descriptor match {
    case descriptor: ParameterDescriptor => getParameterName(descriptor)
    case descriptor: DeconstructedParameterDescriptor => getDeconstructedParameterName(descriptor)
    case descriptor: VariableDescriptor => getVariableName(descriptor)
    case FieldDescriptor(owner, name) =>
      JSPropertyAccessBracket(
        JSThis,
        JSPropertyAccessDot(
          JSPropertyAccessBracket(
            JSPropertyAccessDot(
              getClassJSObject(moduleDescriptor, owner),
              JSIdentifier("fields")
            ),
            JSString(name)
          ),
          JSIdentifier("symbol")
        )
      )
  }

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

  private def getClassLikeJSObject(moduleDescriptor: ModuleDescriptor, descriptor: MethodOwnerDescriptor): JSExpression =
    descriptor match {
      case descriptor: TraitDescriptor => getTraitJSObject(moduleDescriptor, descriptor)
      case TraitObjectDescriptor(traitDescriptor) => JSPropertyAccessDot(getTraitJSObject(moduleDescriptor, traitDescriptor), JSIdentifier("static"))
      case descriptor: ClassDescriptor => getClassJSObject(moduleDescriptor, descriptor)
      case ClassObjectDescriptor(classDescriptor) => JSPropertyAccessDot(getClassJSObject(moduleDescriptor, classDescriptor), JSIdentifier("static"))
      case descriptor: DataConstructorDescriptor => getDataCtorJSObject(moduleDescriptor, descriptor)
    }

  private def getParamOwnerModule(descriptor: ParameterOwnerDescriptor): ModuleDescriptor =
    descriptor.moduleDescriptor
}
