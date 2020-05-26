package dev.argon.backend.js

import dev.argon.compiler._
import cats.{Id => _, _}
import cats.implicits._
import cats.data.NonEmptyList
import shapeless.Id
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.lookup.LookupNames
import dev.argon.compiler.types.TypeSystem
import dev.argon.compiler.vtable._
import zio._
import zio.interop.catz.core._

final class JSEmitter[TContext <: JSContext with Singleton, I <: ResourceIndicator: Tag] private(val context: TContext, inject: JSInjectCode[Id, I], localVariableIdMapping: Ref[Map[VariableOwnerDescriptor, Seq[UniqueIdentifier]]]) {

  import context._
  import context.signatureContext.{ context => _, _ }

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

  def emitModule(module: ArModule[context.type, DeclarationPayloadSpecifier]): RComp[ResourceReader[I], JSModule] = {


    val modulePairs = module.referencedModules
      .zipWithIndex
      .map { case (refModule, i) => (refModule, JSIdentifier(s"module_${i.toString}")) }


    for {
      injectBefore <- ZIO.foreach(inject.before) { singleFile =>
        ZIO.accessM[ResourceReader[I]](_.get.readTextFileAsString(singleFile.file))
      }

      injectAfter <- ZIO.foreach(inject.after) { singleFile =>
        ZIO.accessM[ResourceReader[I]](_.get.readTextFileAsString(singleFile.file))
      }

      globalNamespace <- module.globalNamespace
      vtableBuilder <- VTableBuilder(context)
      topLevelStmts <- allNamespaceElements(globalNamespace).toVector.traverse(createObjectsForScopeValue(vtableBuilder))
    } yield JSModule(
      Vector(
        modulePairs.map { case (refModule, importId) =>
          JSImportAllStatement(None, importId, refModule.id.name)
        },

        injectBefore.map(JSModuleRaw.apply).toList.toVector,

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
            JSPropertyAccessBracket(moduleVarName, JSString(refModule.id.name)),
            importId
          )
        },

        topLevelStmts,

        injectAfter.map(JSModuleRaw.apply).toList.toVector,

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

  private def coreLibExport(moduleDescriptor: ModuleId, name: String): JSExpression =
    if(moduleDescriptor.name === LookupNames.argonCoreLib)
      JSIdentifier(name)
    else
      JSPropertyAccessDot(
        JSPropertyAccessBracket(moduleVarName, JSString(LookupNames.argonCoreLib)),
        JSIdentifier(name)
      )

  private def parameterVarMapping[TResult[_ <: Context with Singleton, Wrap[+_]]](owner: ParameterOwnerDescriptor)(sig: Signature[TResult, _]): Map[VariableLikeDescriptor, JSIdentifier] =
    sig.unsubstitutedParameters.unsized.map { param =>
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
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)

          traitObj = getTraitJSObject(getParamOwnerModule(arTrait.descriptor), arTrait.descriptor, erasedSig)
          instanceParamVarMap = sig.unsubstitutedParameters
            .zipWithIndex
            .map {
              case (param, i) =>
                param.paramVar.descriptor -> JSPropertyAccessBracket(
                  JSPropertyAccessDot(
                    JSFunctionCall(
                      JSPropertyAccessBracket(
                        JSThis,
                        JSPropertyAccessDot(
                          traitObj,
                          JSIdentifier("marker")
                        )
                      ),
                      Vector.empty
                    ),
                    JSIdentifier("typeArguments")
                  ),
                  JSBigInt(i)
                )
            }
            .toMap
            : VarMap
          staticParamVarMap = sig.unsubstitutedParameters
            .zipWithIndex
            .map {
              case (param, i) =>
                param.paramVar.descriptor -> JSPropertyAccessBracket(
                  JSPropertyAccessDot(
                    JSThis,
                    JSIdentifier("typeArguments")
                  ),
                  JSBigInt(i)
                )
            }
            .toMap
            : VarMap

          methods <- arTrait.methods
          staticMethods <- arTrait.staticMethods
          methodObjects <- methods.traverse { method => createMethodObject(instanceParamVarMap)(method.method) }
          staticMethodObjects <- staticMethods.traverse { method => createMethodObject(staticParamVarMap)(method.method) }

          baseTypes <- sig.unsubstitutedResult.baseTypes
          baseTraitExprs <- baseTypes.baseTraits.traverse(createBaseTraitObject(arTrait.descriptor)(sig)(_))

        } yield JSAssignment(
          JSPropertyAccessBracket(traitsVarName, JSString(DescriptorId.forTrait(arTrait.descriptor, erasedSig))),
          JSFunctionCall(
            coreLibExport(arTrait.descriptor.moduleDescriptor, "createTrait"),
            Vector(
              JSObjectLiteral(Vector(
                JSObjectProperty("methods", JSArrayLiteral(methodObjects)),
                JSObjectProperty("staticMethods", JSArrayLiteral(staticMethodObjects)),
                JSObjectProperty("baseTraits", JSArrayLiteral(baseTraitExprs)),
              ))
            )
          )
        )

      case GlobalBinding.GlobalClass(_, _, arClass) =>
        for {
          sig <- arClass.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          fields <- arClass.fields
          methods <- arClass.methods
          staticMethods <- arClass.staticMethods
          classConstructors <- arClass.classConstructors

          classObj = getClassJSObject(getParamOwnerModule(arClass.descriptor), arClass.descriptor, erasedSig)
          instanceParamVarMap = sig.unsubstitutedParameters
            .zipWithIndex
            .map {
              case (param, i) =>
                param.paramVar.descriptor -> JSPropertyAccessBracket(
                  JSPropertyAccessDot(
                    JSFunctionCall(
                      JSPropertyAccessBracket(
                        JSThis,
                        JSPropertyAccessDot(
                          classObj,
                          JSIdentifier("marker")
                        )
                      ),
                      Vector.empty
                    ),
                    JSIdentifier("typeArguments")
                  ),
                  JSBigInt(i)
                )
            }
            .toMap
            : VarMap

          staticParamVarMap = sig.unsubstitutedParameters
            .zipWithIndex
            .map {
              case (param, i) =>
                param.paramVar.descriptor -> JSPropertyAccessBracket(
                  JSPropertyAccessDot(
                    JSThis,
                    JSIdentifier("typeArguments")
                  ),
                  JSBigInt(i)
                )
            }
            .toMap
            : VarMap

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
                createMethodObject(instanceParamVarMap ++ fieldVarMap.toMap)(method.method)
              }
          }
          staticMethodObjects <- staticMethods.traverse { method => createMethodObject(staticParamVarMap)(method.method) }
          ctorObjects <- classConstructors.traverse { ctor => createClassCtorObject(ctor.ctor) }

          vtable <- vtableBuilder.fromClass(arClass)
          vtableObject <- createVTableObject(vtableBuilder.vtableContext)(vtable, arClass.descriptor)

          baseTypes <- sig.unsubstitutedResult.baseTypes
          baseClassExpr <- baseTypes.baseClass.traverse { baseClass =>
            for {
              baseSig <- baseClass.arClass.value.signature
              erasedBaseSig = ErasedSignature.fromSignatureParameters(context)(baseSig)

              varMap = sig.unsubstitutedParameters
                .zipWithIndex
                .map {
                  case (param, i) =>
                    param.paramVar.descriptor -> JSPropertyAccessBracket(JSPropertyAccessDot(JSIdentifier("classObj"), JSIdentifier("typeArguments")), JSBigInt(i))
                }
                .toMap
                : VarMap

              baseClassObjExpr <- convertStmt(EmitParams(arClass.descriptor, varMap))(useReturn = true)(baseClass)

            } yield JSObjectGetProperty(
              "baseClass",
              Vector(
                JSReturn(
                  JSObjectLiteral(Vector(
                    JSObjectProperty(
                      "class",
                      getClassJSObject(getParamOwnerModule(arClass.descriptor), baseClass.arClass.value.descriptor, erasedBaseSig)
                    ),
                    JSObjectProperty(
                      "createClassObj",
                      JSFunctionExpression(
                        None,
                        JSFunctionParameter(JSBindingIdentifier(JSIdentifier("classObj")), JSFunctionEmptyParameterList),
                        baseClassObjExpr
                      )
                    ),
                  ))
                )
              )
            )
          }

          baseTraits <- sig.unsubstitutedResult.baseTypes
          baseTraitExprs <- baseTypes.baseTraits.traverse(createBaseTraitObject(arClass.descriptor)(sig)(_))

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
          JSPropertyAccessBracket(classesVarName, JSString(DescriptorId.forClass(arClass.descriptor, erasedSig))),
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

          instanceTypeExpr <- createBaseTraitObject(ctor.descriptor)(sig)(sig.unsubstitutedResult.instanceType)

          propObjects <-
            ZIO.foreach(
              methodVarMap.keys
                .collect {
                  case desc: ParameterDescriptor => IO.succeed(getParameterName(desc))
                  case desc: VariableDescriptor => getVariableName(desc)
                }
            ) {
              _.map {
                case JSIdentifier(id) =>
                  JSObjectLiteral(Vector(
                    JSObjectProperty("name", JSString(id))
                  ))
              }
            }

          classSpec = JSObjectLiteral(Vector(
            JSObjectGetProperty("instanceType", Vector(JSReturn(instanceTypeExpr))),
            JSObjectProperty("properties", JSArrayLiteral(propObjects.toVector)),
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

  private def createBaseTraitObject[TResult[_ <: Context with Singleton, Wrap[+_]]]
  (ownerDescriptor: ParameterOwnerDescriptor)
  (ownerSig: Signature[TResult, _])
  (baseTrait: typeSystem.TTraitType)
  : Comp[JSExpression] =
    for {
      baseSig <- baseTrait.arTrait.value.signature
      erasedBaseSig = ErasedSignature.fromSignatureParameters(context)(baseSig)

      varMap = ownerSig.unsubstitutedParameters
        .zipWithIndex
        .map {
          case (param, i) =>
            param.paramVar.descriptor -> JSPropertyAccessBracket(JSPropertyAccessDot(JSIdentifier("traitObj"), JSIdentifier("typeArguments")), JSBigInt(i))
        }
        .toMap
        : VarMap

      baseTraitObjExpr <- convertStmt(EmitParams(ownerDescriptor, varMap))(useReturn = true)(baseTrait)

    } yield JSObjectLiteral(Vector(
      JSObjectProperty(
        "trait",
        getTraitJSObject(getParamOwnerModule(ownerDescriptor), baseTrait.arTrait.value.descriptor, erasedBaseSig)
      ),
      JSObjectProperty(
        "createTraitObj",
        JSFunctionExpression(
          None,
          JSFunctionParameter(JSBindingIdentifier(JSIdentifier("traitObj")), JSFunctionEmptyParameterList),
          baseTraitObjExpr
        )
      ),
    ))


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
      sig <- ctor.signatureUnsubstituted
      descriptorId = DescriptorId.forClassConstructor(ErasedSignature.fromSignatureParameters(context)(sig))
      impl <- ctor.payload : Comp[context.JSImpl.ClassConstructor]
      body <- impl match {
        case context.JSImpl.ClassConstructor.StatementBody(body) =>
          val paramVarMapping: VarMap = parameterVarMapping(ctor.descriptor)(sig)

          for {
            (initStmts, initVarMapping) <- body.initStatements.foldLeftM((Vector.empty[JSStatement], paramVarMapping)) {
              case ((acc, varMapping), ClassConstructorStatementExpr(expr)) =>
                for {
                  (newStmts, newMapping) <- StatementConverterLocalBinding.convertStmt(EmitParams(
                    owner = ctor.descriptor,
                    varMapping = varMapping
                  ))(useReturn = false)(expr)
                } yield (acc ++ newStmts, newMapping)

              case ((acc, varMapping), InitializeFieldStatement(field, value)) =>
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

              for {
                ownerClassSig <- ownerClass.signature
                baseCtorSig <- baseCallExpr.classCtor.value.signatureUnsubstituted

                ownerClassErasedSig = ErasedSignature.fromSignatureParameters(context)(ownerClassSig)
                baseCtorDescriptorId = DescriptorId.forClassConstructor(ErasedSignature.fromSignatureParameters(context)(baseCtorSig))
                ownerObj = getClassJSObject(getParamOwnerModule(ctor.descriptor), ownerClass.descriptor, ownerClassErasedSig)

                baseCtorFunc = JSPropertyAccessDot(
                  JSPropertyAccessBracket(
                    JSPropertyAccessDot(
                      ownerObj,
                      constructorsPropName
                    ),
                    JSString(baseCtorDescriptorId)
                  ),
                  JSIdentifier("function")
                )

                emitParams = EmitParams(
                  owner = ctor.descriptor,
                  varMapping = initVarMapping,
                )

                argExprs <- baseCallExpr.args.traverse(convertExpr(emitParams)(_))
              } yield JSFunctionCall(
                JSPropertyAccessDot(
                  baseCtorFunc,
                  JSIdentifier("call")
                ),
                JSThis +: argExprs
              )
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

  private def createDataCtorBody(ctor: DataConstructor[context.type, DeclarationPayloadSpecifier], sig: Signature[DataConstructor.ResultInfo, _], ctorImpl: context.JSImpl.DataConstructor, sigVarMapping: VarMap, sigVarMemberMapping: VarMap, statementConverter: StatementConverter): Comp[(JSExpression, VarMap)] =
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


  private def createExpressionImpl(params: EmitParams)(sig: context.signatureContext.Signature[FunctionResultInfo, _])(expr: context.typeSystem.SimpleExpr): Comp[JSExpression] =
    for {
      body <- convertStmt(params)(useReturn = true)(expr)
    } yield JSFunctionExpression(
      None,
      createParameterList(params.owner)(sig),
      body
    )

  def createParameterList[TResult[_ <: Context with Singleton, Wrap[+_]]](owner: ParameterOwnerDescriptor)(sig: context.signatureContext.Signature[TResult, _]): JSFunctionParameterList =
    sig.unsubstitutedParameters.foldRight[JSFunctionParameterList](JSFunctionEmptyParameterList) { case (param, list) =>
      JSFunctionParameter(
        JSBindingIdentifier(getParameterName(param.paramVar.descriptor)),
        list
      )
    }

  private trait StatementConverter {

    protected def initializeLetBinding(variable: LocalVariable[context.type, Id], valueExpr: JSExpression): UIO[(Vector[JSStatement], JSExpression)]

    final def wrapStatement(params: EmitParams)(expr: context.typeSystem.SimpleExpr): Comp[JSExpression] =
      for {
        (stmts, _) <- convertStmt(params)(useReturn = true)(expr)
      } yield JSFunctionCall(JSArrowFunctionStmts(JSFunctionEmptyParameterList, stmts), Vector())

    final def convertStmt(params: EmitParams)(useReturn: Boolean)(expr: context.typeSystem.SimpleExpr): Comp[(Vector[JSStatement], VarMap)] = expr match {
      case LetBinding(variable, value, next) =>
        for {
          valueExpr <- convertExpr(params)(value)
          (declStmts, varExpr) <- initializeLetBinding(variable, valueExpr)
          (nextStmts, endMapping) <- convertStmt(params.copy(varMapping = params.varMapping + (variable.descriptor -> varExpr)))(useReturn)(next)
        } yield (declStmts ++ nextStmts, endMapping)

      case IfElse(condition, ifBody, elseBody) =>
        for {
          condExpr <- convertExpr(params)(condition)
          (ifBodyStmts, _) <- convertStmt(params)(useReturn)(ifBody)
          (elseBodyStmts, _) <- convertStmt(params)(useReturn)(elseBody)

          accessNativeBool = JSPropertyAccessBracket(
            condExpr,
            coreLibExport(params.owner.moduleDescriptor, "boolValueSymbol")
          )
        } yield (Vector(JSIfElseStatement(accessNativeBool, ifBodyStmts, elseBodyStmts)), params.varMapping)


      case Sequence(first, second) =>
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
    override def initializeLetBinding(variable: LocalVariable[context.type, Id], valueExpr: JSExpression): UIO[(Vector[JSStatement], JSExpression)] = for {
      varName <- getVariableName(variable.descriptor)
      decl = JSDeclareInit(JSBindingIdentifier(varName), valueExpr)
      declStmt = variable.mutability match {
        case Mutability.Mutable => JSLet(NonEmptyList.of(decl))
        case Mutability.NonMutable => JSConst(NonEmptyList.of(decl))
      }
    } yield (Vector(declStmt), varName)
  }

  def convertStmt(params: EmitParams)(useReturn: Boolean)(expr: context.typeSystem.SimpleExpr): Comp[Vector[JSStatement]] =
    StatementConverterLocalBinding.convertStmt(params)(useReturn)(expr).map { case (convBody, _) => convBody }

  private trait StatementConverterFieldBinding extends StatementConverter {
    def fieldVarExpr(id: JSIdentifier): JSExpression

    override protected final def initializeLetBinding(variable: LocalVariable[context.type, Id], valueExpr: JSExpression): UIO[(Vector[JSStatement], JSExpression)] = for {
      varName <- getVariableName(variable.descriptor)
      fieldExpr = fieldVarExpr(varName)
      assign = JSAssignment(fieldExpr, valueExpr)
    } yield (Vector(assign), fieldExpr)
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


  def convertExpr(params: EmitParams)(expr: context.typeSystem.SimpleExpr): Comp[JSExpression] = {
    import context.typeSystem. { context => _, _ }
    expr match {
      case ClassConstructorCall(classType, ctor, args) =>
        for {
          sig <- ctor.value.signatureUnsubstituted
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

          classTypeObj <- convertExpr(params)(classType)

        } yield JSNewCall(JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")), classTypeObj +: baseCtorSymbol +: argExprs)

      case DataConstructorCall(dataCtorInstanceType, args) =>
        for {
          sig <- dataCtorInstanceType.ctor.value.signature

          ownerObj = getDataCtorJSObject(getParamOwnerModule(params.owner), dataCtorInstanceType.ctor.value.descriptor, ErasedSignature.fromSignatureParameters(context)(sig))

          argExprs <- args.traverse(convertExpr(params)(_))
        } yield JSNewCall(JSPropertyAccessDot(ownerObj, JSIdentifier("constructor")), argExprs)

      case EnsureExecuted(body, ensuring) =>
        for {
          jsBody <- convertStmt(params)(useReturn = true)(body)
          jsEnsuring <- convertStmt(params)(useReturn = false)(ensuring)
        } yield JSFunctionCall(JSFunctionExpression(None, JSFunctionEmptyParameterList, Vector(JSTryStatement(jsBody, None, Some(jsEnsuring)))), Vector())

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
        for {
          varName <- getVariableName(argVariable.descriptor)
          bodyExpr <- convertExpr(params.copy(varMapping = params.varMapping + (argVariable.descriptor -> varName)))(body)
        } yield JSArrowFunctionExpr(
          JSFunctionParameter(JSBindingIdentifier(varName), JSFunctionEmptyParameterList),
          bodyExpr
        )

      case LoadTuple(NonEmptyList(TupleElement(value), Nil)) =>
        convertExpr(params)(value)

      case expr @ LoadTuple(_) =>
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

      case PatternMatch(expr, cases) =>

        def convertPattern(params: EmitParams)(patternValue: JSExpression)(pattern: PatternExpr[context.type, Id]): Comp[(Vector[JSStatement] => Vector[JSStatement], EmitParams)] =
          pattern match {
            case PatternExpr.DataDeconstructor(ctor, args) =>  ???
            case PatternExpr.Binding(variable) =>
              for {
                (bindStmts, varExpr) <- StatementConverterLocalBinding.initializeLetBinding(variable, patternValue)
                params2 = params.copy(varMapping = params.varMapping + (variable.descriptor -> varExpr))
              } yield ((body: Vector[JSStatement]) => bindStmts ++ body, params2)

            case PatternExpr.CastBinding(variable) =>
              for {
                (bindStmts, varExpr) <- StatementConverterLocalBinding.initializeLetBinding(variable, patternValue)
                params2 = params.copy(varMapping = params.varMapping + (variable.descriptor -> varExpr))
                typeExpr <- convertExpr(params)(variable.varType)
              } yield ((body: Vector[JSStatement]) => Vector(
                JSIfElseStatement(
                  JSFunctionCall(
                    coreLibExport(params.owner.moduleDescriptor, "isInstanceOf"),
                    Vector(patternValue, typeExpr),
                  ),
                  bindStmts ++ body,
                  Vector(),
                )
              ), params2)
          }

        val matchValueIdentifier = JSIdentifier("matchValue")

        for {
          jsExpr <- convertExpr(params)(expr)
          casesBody <- cases.traverse {
            case PatternCase(pattern, body) =>
              for {
                (patternStmtsFunc, params2) <- convertPattern(params)(matchValueIdentifier)(pattern)
                (bodyStmts, _) <- StatementConverterLocalBinding.convertStmt(params2)(useReturn = true)(body)
              } yield JSBlockStatement(patternStmtsFunc(bodyStmts))
          }
        } yield JSFunctionCall(
          JSArrowFunctionStmts(
            JSFunctionParameter(JSBindingIdentifier(matchValueIdentifier), JSFunctionEmptyParameterList),
            casesBody.toList.toVector
          ),
          Vector(jsExpr)
        )

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

      case StoreVariable(variable, value, _) =>
        for {
          jsValue <- convertExpr(params)(value)
        } yield JSAssignment(
          params.varMapping(variable.descriptor),
          jsValue
        )

      case ClassType(arClass, args) =>
        for {
          sig <- arClass.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          classObj = getClassJSObject(getParamOwnerModule(params.owner), arClass.value.descriptor, erasedSig)

          argExprs <- args.traverse(convertExpr(params)(_))
        } yield JSFunctionCall(
          JSPropertyAccessDot(classObj, classCreateMethodName),
          argExprs
        )

      case TraitType(arTrait, args) =>
        for {
          sig <- arTrait.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          traitObj = getTraitJSObject(getParamOwnerModule(params.owner), arTrait.value.descriptor, erasedSig)

          argExprs <- args.traverse(convertExpr(params)(_))
        } yield JSFunctionCall(
          JSPropertyAccessDot(traitObj, traitCreateMethodName),
          argExprs
        )

      case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented")
    }
  }

  def getMethodObject(moduleDescriptor: ModuleId)(method: AbsRef[context.type, ArMethod]): Comp[JSExpression] = for {
    sig <- method.value.signatureUnsubstituted
    ownerObj <- getClassLikeJSObject(moduleDescriptor, method.value.owner)
  } yield JSPropertyAccessBracket(
    JSPropertyAccessDot(
      ownerObj,
      methodsPropName
    ),
    JSString(DescriptorId.forMethod(method.value.descriptor, ErasedSignature.fromSignature(context)(sig)))
  )

  def getMethodSymbol(moduleDescriptor: ModuleId)(method: AbsRef[context.type, ArMethod]): Comp[JSExpression] = for {
    methodObj <- getMethodObject(moduleDescriptor)(method)
  } yield JSPropertyAccessDot(
    methodObj,
    JSIdentifier("symbol")
  )


  private def getParameterName(descriptor: ParameterDescriptor): JSIdentifier =
    JSIdentifier(s"param_${descriptor.index.toString}")

  private def getVariableName(descriptor: VariableDescriptor): UIO[JSIdentifier] =
    localVariableIdMapping.modify { mapping =>
      val ids = mapping.getOrElse(descriptor.owner, Seq.empty)
      val index = ids.indexOf(descriptor.id)
      if(index < 0)
        (ids.size, mapping.updated(descriptor.owner, ids :+ descriptor.id))
      else
        (index, mapping)
    }
      .map { index => JSIdentifier(s"local_${index.toString}") }

  private def getFieldVariableExpr(moduleDescriptor: ModuleId, variable: context.typeSystem.TFieldVariable): Comp[JSExpression] = for {
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

  private def getClassJSObject(moduleDescriptor: ModuleId, descriptor: ClassDescriptor, sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = {
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

  private def getTraitJSObject(moduleDescriptor: ModuleId, descriptor: TraitDescriptor, sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = {
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


  private def getDataCtorJSObject(moduleDescriptor: ModuleId, descriptor: DataConstructorDescriptor, sig: ErasedSignature.ParameterOnlySignature[context.type]): JSExpression = {
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

  private def getClassLikeJSObject[TPayloadSpec[_, _]](moduleDescriptor: ModuleId, methodOwner: ArMethod.Owner[context.type, TPayloadSpec]): Comp[JSExpression] =
    methodOwner match {
      case ArMethod.TraitOwner(ownerTrait) => ownerTrait.signature.map { sig => getTraitJSObject(moduleDescriptor, ownerTrait.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)) }
      case ArMethod.TraitObjectOwner(ownerTrait) => ownerTrait.signature.map { sig => JSPropertyAccessDot(getTraitJSObject(moduleDescriptor, ownerTrait.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)), JSIdentifier("static")) }
      case ArMethod.ClassOwner(ownerClass) => ownerClass.signature.map { sig => getClassJSObject(moduleDescriptor, ownerClass.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)) }
      case ArMethod.ClassObjectOwner(ownerClass) => ownerClass.signature.map { sig => JSPropertyAccessDot(getClassJSObject(moduleDescriptor, ownerClass.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)), JSIdentifier("static")) }
      case ArMethod.DataCtorOwner(dataCtor) => dataCtor.signature.map { sig => getDataCtorJSObject(moduleDescriptor, dataCtor.descriptor, ErasedSignature.fromSignatureParameters(context)(sig)) }
    }

  private def getParamOwnerModule(descriptor: ParameterOwnerDescriptor): ModuleId =
    descriptor.moduleDescriptor
}

object JSEmitter {

  def make[I <: ResourceIndicator: Tag](context: JSContext)(inject: JSInjectCode[Id, I]): UIO[JSEmitter[context.type, I]] =
    for {
      localVariableIdMapping <- Ref.make(Map.empty[VariableOwnerDescriptor, Seq[UniqueIdentifier]])
    } yield new JSEmitter[context.type, I](context, inject, localVariableIdMapping)

}
