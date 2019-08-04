package dev.argon.backend.js

import dev.argon.compiler._
import cats._
import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.core._
import dev.argon.compiler.lookup.LookupNames
import dev.argon.compiler.types.TypeSystem
import dev.argon.compiler.types.TypeSystem.PrimitiveOperation
import dev.argon.compiler.vtable._

final class JSEmitter[CompRE[-_, +_, +_], R, TContext <: JSContext[CompRE, R, _] with Singleton](val context: TContext, inject: JSInjectCode[Id]) {

  import context._
  import context.signatureContext.{ context => _, typeSystem => _, _ }

  private val moduleVarName = JSIdentifier("modules")
  private val traitsVarName = JSIdentifier("traits")
  private val classesVarName = JSIdentifier("classes")
  private val dataCtorsVarName = JSIdentifier("dataConstructors")
  private val funcsVarName = JSIdentifier("functions")
  private val methodsPropName = JSIdentifier("methods")
  private val constructorsPropName = JSIdentifier("constructors")
  private val classCreateMethodName = JSIdentifier("create")
  private val traitCreateMethodName = JSIdentifier("create")

  private val create_empty_obj = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("create")), Vector(JSNull))
  private def freeze_obj(varName: JSIdentifier) = JSFunctionCall(JSPropertyAccessDot(JSIdentifier("Object"), JSIdentifier("freeze")), Vector(varName))

  private type VarMap = Map[VariableLikeDescriptor, JSExpression]

  private final case class EmitParams(owner: ParameterOwnerDescriptor, varMapping: VarMap)

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): Comp[JSModule] = {


    val modulePairs = module.referencedModules
      .zipWithIndex
      .map { case (refModule, i) => (refModule, JSIdentifier(s"module_${i.toString}")) }


    for {
      globalNamespace <- module.globalNamespace
      vtableBuilder <- VTableBuilder(context)
      topLevelStmts <- allNamespaceElements(globalNamespace).toVector.traverse(createObjectsForScopeValue(vtableBuilder))
    } yield JSModule(
      Vector(
        modulePairs.map { case (refModule, importId) =>
          JSImportAllStatement(None, importId, refModule.descriptor.name)
        },

        inject.before.map(JSModuleRaw.apply).toList.toVector,

        Vector(
          JSConst(NonEmptyList.of(
            JSDeclareInit(JSBindingIdentifier(moduleVarName), create_empty_obj)
          )),

          JSExportDeclaration(JSConst(NonEmptyList.of(
            JSDeclareInit(JSBindingIdentifier(funcsVarName), create_empty_obj)
          ))),

          JSExportDeclaration(JSConst(NonEmptyList.of(
            JSDeclareInit(JSBindingIdentifier(traitsVarName), create_empty_obj)
          ))),

          JSExportDeclaration(JSConst(NonEmptyList.of(
            JSDeclareInit(JSBindingIdentifier(classesVarName), create_empty_obj)
          ))),

          JSExportDeclaration(JSConst(NonEmptyList.of(
            JSDeclareInit(JSBindingIdentifier(dataCtorsVarName), create_empty_obj)
          ))),
        ),

        modulePairs.map { case (refModule, importId) =>
          JSAssignment(
            JSPropertyAccessBracket(moduleVarName, JSString(refModule.descriptor.name)),
            importId
          )
        },

        topLevelStmts,

        inject.after.map(JSModuleRaw.apply).toList.toVector,

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

  private def coreLibExport(moduleDescriptor: ModuleDescriptor, name: String): JSExpression =
    if(moduleDescriptor.name === LookupNames.argonCoreLib)
      JSIdentifier(name)
    else
      JSPropertyAccessDot(
        JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
        JSIdentifier(name)
      )

  private def parameterVarMapping[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](owner: ParameterOwnerDescriptor)(sig: Signature[TResult]): Map[VariableLikeDescriptor, JSIdentifier] =
    sig.unsubstitutedParameters.map { param =>
      val desc = param.paramVar.descriptor
      desc -> getParameterName(desc)
    }.toMap



  private def createObjectsForScopeValue(vtableBuilder: VTableBuilder.Aux[context.type])(value: GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]): Comp[JSStatement] =
    value match {
      case GlobalBinding.GlobalFunction(_, _, func) =>
        for {
          sig <- func.signature
          impl <- func.payload : Comp[context.JSImpl.Function]
          body <- impl match {
            case context.JSImpl.Function.JSExpressionBody(expr) => expr.pure[Comp]
            case context.JSImpl.Function.ExpressionBody(expr) =>
              createExpressionImpl(EmitParams(
                owner = func.descriptor,
                varMapping = parameterVarMapping(func.descriptor)(sig),
              ))(sig)(expr)
          }
        } yield JSAssignment(
            JSPropertyAccessBracket(funcsVarName, JSString(DescriptorId.forFunc(func.descriptor, ErasedSignature.fromSignature(context)(sig)))),
            JSObjectLiteral(Vector(
              JSObjectProperty("value", body)
            ))
          )

      case GlobalBinding.GlobalTrait(_, _, arTrait) =>
        for {
          sig <- arTrait.signature
          methods <- arTrait.methods
          staticMethods <- arTrait.staticMethods
          methodObjects <- methods.traverse { method => createMethodObject(Map.empty)(method.method) }
          staticMethodObjects <- staticMethods.traverse { method => createMethodObject(Map.empty)(method.method) }

        } yield JSAssignment(
          JSPropertyAccessBracket(traitsVarName, JSString(DescriptorId.forTrait(arTrait.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)))),
          JSFunctionCall(
            coreLibExport(arTrait.descriptor.moduleDescriptor, "createTrait"),
            Vector(
              JSObjectLiteral(Vector(
                JSObjectProperty("symbol", JSFunctionCall(JSIdentifier("Symbol"), Vector())),
                JSObjectProperty("methods", JSArrayLiteral(methodObjects)),
                JSObjectProperty("staticMethods", JSArrayLiteral(staticMethodObjects)),
              ))
            )
          )
        )

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
          methodObjects <- methods.traverse { method =>
            fields
              .traverse { field =>
                getFieldVariableExpr(arClass.descriptor.moduleDescriptor, field).map { fieldExpr =>
                  field.descriptor -> fieldExpr
                }
              }
              .flatMap { fieldVarMap =>
                createMethodObject(fieldVarMap.toMap)(method.method)
              }
          }
          staticMethodObjects <- staticMethods.traverse { method => createMethodObject(Map.empty)(method.method) }
          ctorObjects <- classConstructors.traverse { ctor => createClassCtorObject(ctor.ctor) }

          vtable <- vtableBuilder.fromClass(arClass)
          vtableObject <- createVTableObject(vtableBuilder.vtableContext)(vtable, arClass.descriptor)

          baseClassExpr <- sig.unsubstitutedResult.baseTypes.baseClass.traverse { baseClass =>
            baseClass.arClass.value.signature.map { sig =>
              val erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)

              JSObjectGetProperty(
                "baseClass",
                Vector(
                  JSReturn(getClassJSObject(getParamOwnerModule(arClass.descriptor), baseClass.arClass.value.descriptor, erasedSig))
                )
              )
            }
          }

          baseTraitExprs <- sig.unsubstitutedResult.baseTypes.baseTraits.traverse { baseTrait =>
            baseTrait.arTrait.value.signature.map { sig =>
              val erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
              getTraitJSObject(getParamOwnerModule(arClass.descriptor), baseTrait.arTrait.value.descriptor, erasedSig)
            }
          }

          classSpec = JSObjectLiteral(Vector(

            baseClassExpr.toList.toVector,

            Vector(JSObjectGetProperty(
              "baseTraits",
              Vector(
                JSReturn(JSArrayLiteral(
                  baseTraitExprs
                ))
              )
            )),

            Vector(
              JSObjectProperty("fields", JSArrayLiteral(fieldObjects)),
              JSObjectProperty("methods", JSArrayLiteral(methodObjects)),
              JSObjectProperty("staticMethods", JSArrayLiteral(staticMethodObjects)),
              JSObjectProperty("constructors", JSArrayLiteral(ctorObjects)),
              JSObjectGetProperty("vtable", Vector(JSReturn(vtableObject))),
            ),

          ).flatten)

        } yield JSAssignment(
          JSPropertyAccessBracket(classesVarName, JSString(DescriptorId.forClass(arClass.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)))),
          JSFunctionCall(
            coreLibExport(arClass.descriptor.moduleDescriptor, "createClass"),
            Vector(classSpec)
          )
        )


      case GlobalBinding.GlobalDataConstructor(_, _, ctor) =>
        for {
          sig <- ctor.signature
          methods <- ctor.methods
          ctorImpl <- ctor.payload : Comp[context.JSImpl.DataConstructor]

          descString = JSString(DescriptorId.forDataConstructor(ctor.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)))

          sigVarMapping = parameterVarMapping(ctor.descriptor)(sig)

          sigVarMemberMapping = sigVarMapping.view.mapValues(StatementConverterDataCtorFieldBinding(descString).fieldVarExpr).toMap

          (ctorFunc, methodVarMap) <- createDataCtorBody(ctor, sig, ctorImpl, sigVarMapping, sigVarMemberMapping, StatementConverterDataCtorFieldBinding(descString))

          methodObjects <- methods.traverse { method =>
              createMethodObject(methodVarMap)(method.method)
          }

          vtable <- vtableBuilder.fromDataConstructor(ctor)
          vtableObject <- createVTableObject(vtableBuilder.vtableContext)(vtable, ctor.descriptor)

          instanceTypeSig <- sig.unsubstitutedResult.instanceType.arTrait.value.signature
          instanceTypeExpr = getTraitJSObject(
            getParamOwnerModule(ctor.descriptor),
            sig.unsubstitutedResult.instanceType.arTrait.value.descriptor,
            ErasedSignature.fromSignatureParameters(context)(instanceTypeSig)
          )

          propObjects = methodVarMap.keys
            .collect {
              case desc: ParameterDescriptor => getParameterName(desc)
              case desc: VariableDescriptor => getVariableName(desc)
            }
            .map { case JSIdentifier(id) =>
              JSObjectLiteral(Vector(
                JSObjectProperty("name", JSString(id))
              ))
            }
            .toVector

          classSpec = JSObjectLiteral(Vector(
            JSObjectGetProperty("instanceType", Vector(JSReturn(instanceTypeExpr))),
            JSObjectProperty("properties", JSArrayLiteral(propObjects)),
            JSObjectProperty("methods", JSArrayLiteral(methodObjects)),
            JSObjectProperty("constructor", ctorFunc),
            JSObjectGetProperty("vtable", Vector(JSReturn(vtableObject))),
          ))

        } yield JSAssignment(
          JSPropertyAccessBracket(dataCtorsVarName, descString),
          JSFunctionCall(
            coreLibExport(ctor.descriptor.moduleDescriptor, "createDataConstructor"),
            Vector(classSpec)
          )
        )



    }

  private def createMethodObject(ownerVarMapping: VarMap)(method: ArMethod[context.type, PayloadSpecifiers.DeclarationPayloadSpecifier]): Comp[JSExpression] =
    for {
      sig <- method.signatureUnsubstituted
      impl <- method.payload : Comp[context.JSImpl.Method]
      body <- impl match {
        case context.JSImpl.Method.JSExpressionBody(expr) => expr.pure[Comp]
        case context.JSImpl.Method.ExpressionBody(expr) =>
          createExpressionImpl(EmitParams(
            owner = method.descriptor,
            varMapping = ownerVarMapping ++ parameterVarMapping(method.descriptor)(sig),
          ))(sig)(expr)
        case context.JSImpl.Method.Abstract => JSNull.pure[Comp]
      }
    } yield JSObjectLiteral(Vector(
      JSObjectProperty("descriptor", JSString(DescriptorId.forMethod(method.descriptor, ErasedSignature.fromSignature(context)(sig)))),
      JSObjectProperty("value", body),
    ))

  private def createClassCtorObject(ctor: ClassConstructor[context.type, PayloadSpecifiers.DeclarationPayloadSpecifier]): Comp[JSExpression] =
    for {
      sig <- ctor.signature
      descriptorId = DescriptorId.forClassConstructor(ErasedSignature.fromSignatureParameters(context)(sig))
      impl <- ctor.payload : Comp[context.JSImpl.ClassConstructor]
      body <- impl match {
        case context.JSImpl.ClassConstructor.StatementBody(body) =>
          val paramVarMapping: VarMap = parameterVarMapping(ctor.descriptor)(sig)

          for {
            (initStmts, initVarMapping) <- body.initStatements.foldLeftM((Vector.empty[JSStatement], paramVarMapping)) {
              case ((acc, varMapping), context.typeSystem.ClassConstructorStatementExpr(expr)) =>
                for {
                  (newStmts, newMapping) <- StatementConverterLocalBinding.convertStmt(EmitParams(
                    owner = ctor.descriptor,
                    varMapping = varMapping
                  ))(useReturn = false)(expr)
                } yield (acc ++ newStmts, newMapping)

              case ((acc, varMapping), context.typeSystem.InitializeFieldStatement(field, value)) =>
                for {
                  varExpr <- getFieldVariableExpr(getParamOwnerModule(ctor.descriptor), field)
                  valueExpr <- convertExpr(EmitParams(
                    owner = ctor.descriptor,
                    varMapping = varMapping,
                  ))(value)

                  newMapping = varMapping + (field.descriptor -> varExpr)
                } yield (acc ++ Vector(JSAssignment(
                  varExpr,
                  valueExpr
                )), newMapping)
            }

            baseCall <- body.baseConstructorCall.traverse { baseCallExpr =>
              val ownerClass = baseCallExpr.classCtor.value.ownerClass
              baseCallExpr.classCtor.value.ownerClass.signature.flatMap { ownerClassSig =>
                val ownerClassErasedSig = ErasedSignature.fromSignatureParameters(context)(ownerClassSig)
                val ownerObj = getClassJSObject(getParamOwnerModule(ctor.descriptor), ownerClass.descriptor, ownerClassErasedSig)

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

                val emitParams = EmitParams(
                  owner = ctor.descriptor,
                  varMapping = initVarMapping,
                )

                baseCallExpr.args.traverse(convertExpr(emitParams)(_)).map { argExprs =>
                  JSFunctionCall(
                    JSPropertyAccessDot(
                      JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")),
                      JSIdentifier("call")
                    ),
                    JSThis +: baseCtorSymbol +: argExprs
                  )
                }
              }
            }

            endExpr <- convertStmt(EmitParams(
              owner = ctor.descriptor,
              varMapping = initVarMapping,
            ))(useReturn = false)(body.endExpr)
          } yield initStmts ++ baseCall.toList.toVector ++ endExpr

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

  private def createDataCtorBody(ctor: DataConstructor[context.type, DeclarationPayloadSpecifier], sig: Signature[DataConstructor.ResultInfo], ctorImpl: context.JSImpl.DataConstructor, sigVarMapping: VarMap, sigVarMemberMapping: VarMap, statementConverter: StatementConverter): Comp[(JSExpression, VarMap)] =
    ctorImpl match {
      case context.JSImpl.DataConstructor.ExpressionBody(expr) =>

        val paramMemberInitStmts =
          sigVarMemberMapping.toVector.map {
            case (descriptor, memberExpr) =>
              JSAssignment(memberExpr, sigVarMapping(descriptor))
          }

        for {
          (body, varMap) <- statementConverter.convertStmt(EmitParams(
            owner = ctor.descriptor,
            varMapping = sigVarMemberMapping,
          ))(useReturn = true)(expr)

          ctorFunc = JSFunctionExpression(
            None,
            createParameterList(ctor.descriptor)(sig),
            paramMemberInitStmts ++ body
          )
        } yield (ctorFunc, varMap)
    }

  private def createVTableObject(vtableContext: VTableContext.Aux[context.type])(vtable: vtableContext.VTable, descriptor: MethodOwnerDescriptor): Comp[JSExpression] =
    vtable.methodMap.toVector
      .traverse {
        case (slotMethod, vtableContext.VTableEntry(_, _, vtableContext.VTableEntryMethod(method))) =>
          for {
            slotSym <- getMethodSymbol(getParamOwnerModule(descriptor))(slotMethod)
            implObj <- getMethodObject(getParamOwnerModule(descriptor))(method)
          } yield JSObjectLiteral(Vector(
            JSObjectProperty("symbol", slotSym),
            JSObjectProperty("value", JSPropertyAccessDot(
              implObj,
              JSIdentifier("value")
            )),
          ))

        case (slotMethod, _) =>
          for {
            slotSym <- getMethodSymbol(getParamOwnerModule(descriptor))(slotMethod)
          } yield JSObjectLiteral(Vector(
            JSObjectProperty("symbol", slotSym),
            JSObjectProperty("value", JSNull),
          ))
      }
    .map(JSArrayLiteral(_))


  private def createExpressionImpl(params: EmitParams)(sig: context.signatureContext.Signature[FunctionResultInfo])(expr: context.typeSystem.ArExpr): Comp[JSExpression] =
    for {
      body <- convertStmt(params)(useReturn = true)(expr)
    } yield JSFunctionExpression(
      None,
      createParameterList(params.owner)(sig),
      body
    )

  def createParameterList[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](owner: ParameterOwnerDescriptor)(sig: context.signatureContext.Signature[TResult]): JSFunctionParameterList =
    sig.unsubstitutedParameters.foldRight[JSFunctionParameterList](JSFunctionEmptyParameterList) { case (param, list) =>
      JSFunctionParameter(
        JSBindingIdentifier(getParameterName(param.paramVar.descriptor)),
        list
      )
    }

  private trait StatementConverter {

    protected def initializeLetBinding(variable: context.typeSystem.LocalVariable, valueExpr: JSExpression): (Vector[JSStatement], JSExpression)

    final def wrapStatement(params: EmitParams)(expr: context.typeSystem.ArExpr): Comp[JSExpression] =
      for {
        (stmts, _) <- convertStmt(params)(useReturn = true)(expr)
      } yield JSFunctionCall(JSArrowFunctionStmts(JSFunctionEmptyParameterList, stmts), Vector())

    final def convertStmt(params: EmitParams)(useReturn: Boolean)(expr: context.typeSystem.ArExpr): Comp[(Vector[JSStatement], VarMap)] = expr match {
      case context.typeSystem.LetBinding(variable, value, next) =>
        for {
          valueExpr <- convertExpr(params)(value)
          bindingResult = initializeLetBinding(variable, valueExpr)
          declStmts = bindingResult._1
          varExpr = bindingResult._2
          (nextStmts, endMapping) <- convertStmt(params.copy(varMapping = params.varMapping + (variable.descriptor -> varExpr)))(useReturn)(next)
        } yield (declStmts ++ nextStmts, endMapping)

      case context.typeSystem.IfElse(condition, ifBody, elseBody) =>
        for {
          condExpr <- convertExpr(params)(condition)
          (ifBodyStmts, _) <- convertStmt(params)(useReturn)(ifBody)
          (elseBodyStmts, _) <- convertStmt(params)(useReturn)(elseBody)

          accessNativeBool = JSPropertyAccessBracket(
            condExpr,
            coreLibExport(params.owner.moduleDescriptor, "boolValueSymbol")
          )
        } yield (Vector(JSIfElseStatement(accessNativeBool, ifBodyStmts, elseBodyStmts)), params.varMapping)


      case context.typeSystem.Sequence(first, second) =>
        for {
          convFirst <- wrapStatement(params)(first)
          (convSecond, endMapping) <- convertStmt(params)(useReturn)(second)
        } yield (convFirst +: convSecond, endMapping)

      case _ =>
        if(useReturn) convertExpr(params)(expr).map { jsExpr => (Vector(JSReturn(jsExpr)), params.varMapping) }
        else convertExpr(params)(expr).map { jsExpr => (Vector(jsExpr), params.varMapping) }
    }

  }

  private object StatementConverterLocalBinding extends StatementConverter {
    override def initializeLetBinding(variable: context.typeSystem.LocalVariable, valueExpr: JSExpression): (Vector[JSStatement], JSExpression) = {
      val varName = getVariableName(variable.descriptor)
      val decl = JSDeclareInit(JSBindingIdentifier(varName), valueExpr)
      val declStmt = variable.mutability match {
        case Mutability.Mutable => JSLet(NonEmptyList.of(decl))
        case Mutability.NonMutable => JSConst(NonEmptyList.of(decl))
      }

      (Vector(declStmt), varName)
    }
  }

  def convertStmt(params: EmitParams)(useReturn: Boolean)(expr: context.typeSystem.ArExpr): Comp[Vector[JSStatement]] =
    StatementConverterLocalBinding.convertStmt(params)(useReturn)(expr).map { case (convBody, _) => convBody }

  private trait StatementConverterFieldBinding extends StatementConverter {
    def fieldVarExpr(id: JSIdentifier): JSExpression

    override protected final def initializeLetBinding(variable: context.typeSystem.LocalVariable, valueExpr: JSExpression): (Vector[JSStatement], JSExpression) = {
      val varName = getVariableName(variable.descriptor)
      val fieldExpr = fieldVarExpr(varName)
      val assign = JSAssignment(fieldExpr, valueExpr)

      (Vector(assign), fieldExpr)
    }
  }

  private final case class StatementConverterDataCtorFieldBinding(descString: JSString) extends StatementConverterFieldBinding {

    override def fieldVarExpr(id: JSIdentifier): JSExpression =
      JSPropertyAccessBracket(
        JSThis,
        JSPropertyAccessDot(
          JSPropertyAccessBracket(
            JSPropertyAccessDot(
              JSPropertyAccessBracket(dataCtorsVarName, descString),
              JSIdentifier("properties")
            ),
            JSString(id.id)
          ),
          JSIdentifier("symbol")
        )
      )

  }


  def convertExpr(params: EmitParams)(expr: context.typeSystem.ArExpr): Comp[JSExpression] = {
    import context.typeSystem. { context => _, _ }
    expr match {
      case ClassConstructorCall(classType, ctor, args) =>
        for {
          sig <- ctor.value.signature
          ownerClassSig <- ctor.value.ownerClass.signature

          ownerObj = getClassJSObject(getParamOwnerModule(params.owner), ctor.value.descriptor.ownerClass, ErasedSignature.fromSignatureParameters(context)(ownerClassSig))
          descriptorId = DescriptorId.forClassConstructor(ErasedSignature.fromSignatureParameters(context)(sig))

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

          argExprs <- args.traverse(convertExpr(params)(_))

        } yield JSNewCall(JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")), baseCtorSymbol +: argExprs)

      case DataConstructorCall(dataCtorInstanceType, args) =>
        for {
          sig <- dataCtorInstanceType.ctor.value.signature

          ownerObj = getDataCtorJSObject(getParamOwnerModule(params.owner), dataCtorInstanceType.ctor.value.descriptor, ErasedSignature.fromSignatureParameters(context)(sig))

          argExprs <- args.traverse(convertExpr(params)(_))
        } yield JSNewCall(JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")), argExprs)

      case FunctionCall(func, args, _) =>
        for {
          sig <- func.value.signature

          funcExpr = func.value.descriptor match {
            case FuncDescriptor.InNamespace(moduleDesc, _, ns, name) =>
              val funcsObject =
                if(moduleDesc === getParamOwnerModule(params.owner))
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

          argExprs <- args.traverse(convertExpr(params)(_))

        } yield JSFunctionCall(funcExpr, argExprs)

      case FunctionObjectCall(funcExpr, arg, _) =>
        for {
          jsFunc <- convertExpr(params)(funcExpr)
          jsArg <- convertExpr(params)(arg)
        } yield JSFunctionCall(jsFunc, Vector(jsArg))

      case IfElse(_, _, _) =>
        StatementConverterLocalBinding.wrapStatement(params)(expr)

      case LetBinding(_, _, _) =>
        StatementConverterLocalBinding.wrapStatement(params)(expr)

      case LoadConstantInt(i, _) =>
        JSFunctionCall(
          coreLibExport(params.owner.moduleDescriptor, "createInt"),
          Vector(JSBigInt(i))
        ).pure[Comp]

      case LoadConstantString(str, _) =>
        JSFunctionCall(
          coreLibExport(params.owner.moduleDescriptor, "createString"),
          Vector(JSString(str))
        ).pure[Comp]

      case LoadLambda(argVariable, body) =>
        val varName = getVariableName(argVariable.descriptor)
        for {
          bodyExpr <- convertExpr(params.copy(varMapping = params.varMapping + (argVariable.descriptor -> varName)))(body)
        } yield JSArrowFunctionExpr(
          JSFunctionParameter(JSBindingIdentifier(varName), JSFunctionEmptyParameterList),
          bodyExpr
        )

      case LoadTuple(NonEmptyList(TupleElement(value), Nil)) =>
        convertExpr(params)(value)

      case expr: LoadTuple =>
        for {
          values <- expr.values.toList.toVector.traverse { elem => convertExpr(params)(elem.value) }
        } yield JSArrayLiteral(values)

      case LoadTupleElement(tupleValue, _, index) =>
        for {
          tuple <- convertExpr(params)(tupleValue)
        } yield JSPropertyAccessBracket(tuple, JSBigInt(index))

      case LoadUnit(_) =>
        coreLibExport(params.owner.moduleDescriptor, "unitValue").pure[Comp]

      case LoadVariable(variable) =>
        params.varMapping(variable.descriptor).pure[Comp]

      case MethodCall(method, instance, args, _) =>
        for {
          instanceExpr <- convertExpr(params)(instance)

          methodSymbol <- getMethodSymbol(getParamOwnerModule(params.owner))(method)
          methodExpr = JSPropertyAccessBracket(instanceExpr, methodSymbol)

          argExprs <- args.traverse(convertExpr(params)(_))

        } yield JSFunctionCall(methodExpr, argExprs)


      case PrimitiveOp(op, left, right, _) =>
        for {
          leftExpr <- convertExpr(params)(left)
          rightExpr <- convertExpr(params)(right)
        } yield JSFunctionCall(
          coreLibExport(params.owner.moduleDescriptor,
            op match {
              case PrimitiveOperation.AddInt => "addInt"
              case PrimitiveOperation.SubInt => "subInt"
              case PrimitiveOperation.MulInt => "mulInt"
              case PrimitiveOperation.IntEqual => "intEqual"
            }
          ),
          Vector(leftExpr, rightExpr)
        )

      case e @ Sequence(_, _) =>
        StatementConverterLocalBinding.wrapStatement(params)(e)

      case ClassType(arClass, args, _) =>
        for {
          sig <- arClass.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          classObj = getClassJSObject(getParamOwnerModule(params.owner), arClass.value.descriptor, erasedSig)

          argExprs <- args.traverse(convertTypeArg(params)(_))
        } yield JSFunctionCall(
          JSPropertyAccessDot(classObj, classCreateMethodName),
          argExprs
        )

      case TraitType(arTrait, args, _) =>
        for {
          sig <- arTrait.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          traitObj = getTraitJSObject(getParamOwnerModule(params.owner), arTrait.value.descriptor, erasedSig)

          argExprs <- args.traverse(convertTypeArg(params)(_))
        } yield JSFunctionCall(
          JSPropertyAccessDot(traitObj, traitCreateMethodName),
          argExprs
        )

      case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented")
    }
  }

  def convertTypeArg(params: EmitParams)(arg: context.typeSystem.TypeArgument): Comp[JSExpression] =
    arg match {
      case context.typeSystem.TypeArgument.Expr(expr) => convertExpr(params)(expr)
      case context.typeSystem.TypeArgument.Wildcard => JSNull.pure[Comp]
    }

  def getMethodObject(moduleDescriptor: ModuleDescriptor)(method: AbsRef[context.type, ArMethod]): Comp[JSExpression] = for {
    sig <- method.value.signatureUnsubstituted
    ownerObj <- getClassLikeJSObject(moduleDescriptor, method.value.owner)
  } yield JSPropertyAccessBracket(
    JSPropertyAccessDot(
      ownerObj,
      methodsPropName
    ),
    JSString(DescriptorId.forMethod(method.value.descriptor, ErasedSignature.fromSignature(context)(sig)))
  )

  def getMethodSymbol(moduleDescriptor: ModuleDescriptor)(method: AbsRef[context.type, ArMethod]): Comp[JSExpression] = for {
    methodObj <- getMethodObject(moduleDescriptor)(method)
  } yield JSPropertyAccessDot(
    methodObj,
    JSIdentifier("symbol")
  )


  private def getParameterName(descriptor: ParameterDescriptor): JSIdentifier =
    JSIdentifier(s"param_${descriptor.index.toString}")

  private def getVariableName(descriptor: VariableDescriptor): JSIdentifier =
    JSIdentifier(s"local_${descriptor.index.toString}")

  private def getFieldVariableExpr(moduleDescriptor: ModuleDescriptor, variable: context.typeSystem.FieldVariable): Comp[JSExpression] = for {
    sig <- variable.ownerClass.value.signature
    erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
  } yield JSPropertyAccessBracket(
    JSThis,
    JSPropertyAccessDot(
      JSPropertyAccessBracket(
        JSPropertyAccessDot(
          getClassJSObject(moduleDescriptor, variable.descriptor.owner, erasedSig),
          JSIdentifier("fields")
        ),
        JSString(variable.descriptor.name)
      ),
      JSIdentifier("symbol")
    )
  )

  private def getClassJSObject(moduleDescriptor: ModuleDescriptor, descriptor: ClassDescriptor, sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = {
    val classModule = getParamOwnerModule(descriptor)
    val classesObj =
      if(moduleDescriptor === classModule)
        classesVarName
      else
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(classModule.name)),
          classesVarName
        )

    JSPropertyAccessBracket(classesObj, JSString(DescriptorId.forClass(descriptor, sig)))
  }

  private def getTraitJSObject(moduleDescriptor: ModuleDescriptor, descriptor: TraitDescriptor, sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = {
    val traitModule = getParamOwnerModule(descriptor)
    val traitsObj =
      if(moduleDescriptor === traitModule)
        traitsVarName
      else
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(traitModule.name)),
          traitsVarName
        )

    JSPropertyAccessBracket(traitsObj, JSString(DescriptorId.forTrait(descriptor, sig)))
  }


  private def getDataCtorJSObject(moduleDescriptor: ModuleDescriptor, descriptor: DataConstructorDescriptor, sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = {
    val dataCtorModule = getParamOwnerModule(descriptor)
    val dataCtorsObj =
      if(moduleDescriptor === dataCtorModule)
        dataCtorsVarName
      else
        JSPropertyAccessDot(
          JSPropertyAccessBracket(moduleVarName, JSString(dataCtorModule.name)),
          dataCtorsVarName
        )

    JSPropertyAccessBracket(dataCtorsObj, JSString(DescriptorId.forDataConstructor(descriptor, sig)))
  }

  private def getClassLikeJSObject[TPayloadSpec[_, _]](moduleDescriptor: ModuleDescriptor, methodOwner: ArMethod.Owner[context.type, TPayloadSpec]): Comp[JSExpression] =
    methodOwner match {
      case ArMethod.TraitOwner(ownerTrait) => ownerTrait.signature.map { sig => getTraitJSObject(moduleDescriptor, ownerTrait.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)) }
      case ArMethod.TraitObjectOwner(ownerTrait) => ownerTrait.signature.map { sig => JSPropertyAccessDot(getTraitJSObject(moduleDescriptor, ownerTrait.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)), JSIdentifier("static")) }
      case ArMethod.ClassOwner(ownerClass) => ownerClass.signature.map { sig => getClassJSObject(moduleDescriptor, ownerClass.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)) }
      case ArMethod.ClassObjectOwner(ownerClass) => ownerClass.signature.map { sig => JSPropertyAccessDot(getClassJSObject(moduleDescriptor, ownerClass.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)), JSIdentifier("static")) }
      case ArMethod.DataCtorOwner(dataCtor) => dataCtor.signature.map { sig => getDataCtorJSObject(moduleDescriptor, dataCtor.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)) }
    }

  private def getParamOwnerModule(descriptor: ParameterOwnerDescriptor): ModuleDescriptor =
    descriptor.moduleDescriptor
}
