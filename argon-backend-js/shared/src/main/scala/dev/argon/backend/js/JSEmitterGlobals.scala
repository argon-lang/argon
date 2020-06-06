package dev.argon.backend.js

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler.expr._
import dev.argon.compiler.vtable.{VTableBuilder, VTableContext}
import zio._
import zio.interop.catz.core._
import cats.implicits._
import shapeless.Id

private[js] trait JSEmitterGlobals extends JSEmitterExpressions {
  import JSDSL._

  import context.typeSystem
  import context.signatureContext.{ context => _, _ }

  def createGlobalFunction(func: ArFunc[context.type, DeclarationPayloadSpecifier]): Emit[JSExpression] =
    for {
      sig <- func.signature
      impl <- func.payload : Comp[context.JSImpl.Function]
      body <- impl match {
        case context.JSImpl.Function.JSExpressionBody(expr) => expr.pure[Comp]
        case context.JSImpl.Function.ExpressionBody(expr) =>
          for {
            (paramList, paramMap) <- createParameterList(sig)
            jsBody <- addToVarMap(paramMap.map { case (variable, name) => variable.id -> new LocalVariableLoader(variable, JSIdentifier(name)) }: _*)(convertStmt(useReturn = true)(expr))
          } yield JSFunctionExpression(None, paramList, jsBody)
      }

      createFunction <- coreLibExport("createFunction")
    } yield (
      createFunction(jsobj(
        "implementation" -> body
      ))
      )

  private def loadTypeParameter(paramVar: ParameterVariable[context.type, Id], typeInfo: JSExpression, thisExpr: JSExpression): VariableLoader =
    new VariableLoader {
      override def loadVariable: JSExpression =
        typeInfo.prop(id"getParameter")(thisExpr, JSNumberInt(paramVar.index))

      override def storeVariable(value: JSExpression): Option[JSExpression] = None
      override def initializeVariable(value: JSExpression): Option[JSStatement] = None
    }

  private def loadTypeParametersFromSig[TResult[_ <: Context with Singleton, Wrap[+_]]](sig: Signature[TResult, _], typeInfo: JSExpression, thisExpr: JSExpression): VarMap =
    sig.unsubstitutedParameters.unsized.map { param =>
      param.paramVar.id -> loadTypeParameter(param.paramVar, typeInfo, thisExpr)
    }.toMap

  def createGlobalTrait(arTrait: ArTrait[context.type, DeclarationPayloadSpecifier]): Emit[JSExpression] =
    for {
      sig <- arTrait.signature
      erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)

      traitObj <- getTraitJSObject(arTrait, erasedSig)

      instanceVarMap = (instance: JSExpression) =>
        loadTypeParametersFromSig(sig, traitObj, instance)

      methods <- arTrait.methods
      staticMethods <- arTrait.staticMethods
      methodObjects <- methods.traverse { method =>
        createMethodObject(method.method, instanceVarMap)
      }
      staticMethodObjects <- staticMethods.traverse { method =>
        createMethodObject(method.method, instanceVarMap)
      }

      baseTypes <- sig.unsubstitutedResult.baseTypes
      baseTraitExprs <- baseTypes.baseTraits.traverse(createBaseTraitObject(sig)(_))

      createTrait <- coreLibExport("createTrait")

    } yield createTrait(
      jsobj(
        get("baseTraits")(
          JSReturn(JSArrayLiteral(baseTraitExprs))
        ),

        "methods" -> JSArrayLiteral(methodObjects),
        "staticMethods" -> JSArrayLiteral(staticMethodObjects),
      )
    )

  private final class FieldVariableLoader(thisExpr: JSExpression, classObj: JSExpression, fieldName: String) extends VariableLoader {
    private val fieldObj = classObj.prop(id"field")(JSString(fieldName))

    override def loadVariable: JSExpression =
      fieldObj.prop(id"read")(thisExpr)

    override def storeVariable(value: JSExpression): Option[JSExpression] =
      Some(fieldObj.prop(id"write")(thisExpr, value))

    override def initializeVariable(value: JSExpression): Option[JSStatement] =
      Some(fieldObj.prop(id"initialize")(thisExpr, value))
  }

  def createGlobalClass(vtableBuilder: VTableBuilder.Aux[context.type])(arClass: ArClass[context.type, DeclarationPayloadSpecifier]): Emit[JSExpression] =
    for {
      sig <- arClass.signature
      erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)

      classObj <- getClassJSObject(arClass, erasedSig)

      fields <- arClass.fields
      fieldObjects = fields.map { field =>
        jsobj(
          "name" -> JSString(field.name.name),
          "mutability" -> (field.mutability match {
            case Mutability.Mutable => JSString("mutable")
            case Mutability.NonMutable => JSString("non-mutable")
          }),
        )
      }

      typeParamsVarMap = (instance: JSExpression) =>
        loadTypeParametersFromSig(sig, classObj, instance)

      instanceVarMap = (instance: JSExpression) =>
        typeParamsVarMap(instance) ++
          fields.map { field =>
            field.id -> new FieldVariableLoader(instance, classObj, field.name.name)
          }

      methods <- arClass.methods
      methodObjects <- methods.traverse { method =>
        createMethodObject(method.method, instanceVarMap)
      }

      staticMethods <- arClass.staticMethods
      staticMethodObjects <- staticMethods.traverse { method =>
        createMethodObject(method.method, typeParamsVarMap)
      }

      classConstructors <- arClass.classConstructors
      ctorObjects <- classConstructors.traverse { ctor => createClassCtorObject(ctor.ctor) }

      vtable <- vtableBuilder.fromClass(arClass)
      vtableObject <- createVTableObject(vtableBuilder.vtableContext)(vtable)

      baseTypes <- sig.unsubstitutedResult.baseTypes
      baseClassExpr <- baseTypes.baseClass.traverse { baseClass =>
        for {
          baseSig <- baseClass.arClass.value.signature
          erasedBaseSig = ErasedSignature.fromSignatureParameters(context)(baseSig)

          baseClassObjExpr <- getClassJSObject(baseClass.arClass.value, erasedBaseSig)

          paramVarNames <- ZIO.access[EmitEnv] { emitEnv =>
            sig.unsubstitutedParameters
              .zipWithIndex
              .map {
                case (param, i) =>
                  param.paramVar.id -> id"param_${emitEnv.varMap.size + i}"
              }
          }

          classArgs <- baseClass.args.traverse(convertExpr).provideSome[EmitEnv] { emitEnv =>

            val paramVarMap: VarMap = paramVarNames
              .map {
                case (id, name) =>
                  id -> VariableLoader.fromExpr(name)
              }
              .toMap

            emitEnv.copy(varMap = emitEnv.varMap ++ paramVarMap)
          }

        } yield jsobj(
          "baseType" -> baseClassObjExpr,
          "parameterMapping" -> jsfunction(None)(paramVarNames.map { case (_, name) => name }: _*)(
            JSReturn(JSArrayLiteral(classArgs))
          )
        )
      }

      baseTraitExprs <- baseTypes.baseTraits.traverse(createBaseTraitObject(sig)(_))

      createClass <- coreLibExport("createClass")

    } yield createClass(JSObjectLiteral(Vector[Vector[JSObjectMember]](

      baseClassExpr.map[JSObjectMember] { "baseClass" -> _ }.toList.toVector,

      Vector(
        get("baseTraits")(
          JSReturn(JSArrayLiteral(
            baseTraitExprs
          )),
        ),
        "fields" -> JSArrayLiteral(fieldObjects),
        "methods" -> JSArrayLiteral(methodObjects),
        "staticMethods" -> JSArrayLiteral(staticMethodObjects),
        "constructors" -> JSArrayLiteral(ctorObjects),
        "loadVTable" -> vtableObject,
      ),
    ).flatten))

  def createGlobalDataConstructor(vtableBuilder: VTableBuilder.Aux[context.type])(ctor: DataConstructor[context.type, DeclarationPayloadSpecifier]): Emit[Seq[JSStatement]] =
    for {
      sig <- ctor.signature
      erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)

      ctorObj <- getDataCtorJSObject(ctor, erasedSig)
      localFieldMappingRef <- Ref.make(Map.empty[LocalVariableId, JSIdentifier])


      ctorImpl <- ctor.payload : Comp[context.JSImpl.DataConstructor]
      (ctorFunc, paramMapping) <- ctorImpl match {
        case context.JSImpl.DataConstructor.ExpressionBody(expr) =>
          for {
            thisVarId <- getNextSymbolId
            thisVarName = id"this_$thisVarId"

            statementConverter = StatementConverterDataCtorFieldBinding(thisVarName, localFieldMappingRef)

            (paramListNoThis, paramMapping) <- createParameterList(sig)
            paramList = JSFunctionParameter(JSBindingIdentifier(thisVarName), paramListNoThis)

            initObjectExprs = paramMapping
              .map { case (paramVar, varName) =>
                paramVar.mutability match {
                  case Mutability.Mutable => JSThis.cprop(id"${varName}_sym") := JSIdentifier(varName)
                  case Mutability.NonMutable =>
                    defineProperty(thisVarName, id"${varName}_sym", JSIdentifier(varName))
                }
              }
              .toVector

            body <- addToVarMap(paramMapping.map { case (paramVar, varName) => paramVar.id -> VariableLoader.fromExpr(thisVarName.cprop(id"${varName}_sym")) }: _*)(
              statementConverter.convertStmt(useReturn = true)(expr)
            )
          } yield (JSFunctionExpression(None, paramList, initObjectExprs ++ body), paramMapping)
      }

      localFieldMapping <- localFieldMappingRef.get

      instanceVarMap = (instance: JSExpression) =>
        (paramMapping.map { case (paramVar, varName) => paramVar.id -> VariableLoader.fromExpr(instance.cprop(id"${varName}_sym")) }.toMap : VarMap) ++
        (localFieldMapping.map { case (id, varName) =>
          id -> new VariableLoader {
            override def loadVariable: JSExpression =
              instance.cprop(varName)

            override def storeVariable(value: JSExpression): Option[JSExpression] =
              Some(instance.cprop(varName) := value)

            override def initializeVariable(value: JSExpression): Option[JSStatement] =
              None
          }
        }: VarMap)

      methods <- ctor.methods
      methodObjects <- methods.traverse { method =>
        createMethodObject(method.method, instanceVarMap)
      }

      vtable <- vtableBuilder.fromDataConstructor(ctor)
      vtableObject <- createVTableObject(vtableBuilder.vtableContext)(vtable)

      instanceTypeExpr <- createBaseTraitObject(sig)(sig.unsubstitutedResult.instanceType)

      createDataConstructor <- coreLibExport("createDataConstructor")

      paramSymbolDecls = paramMapping.map { case (_, varName) =>
        const(id"${varName}_sym" ::= id"Symbol"())
      }

      localSymbolDecls = localFieldMapping.map { case (_, varName) =>
        const(varName ::= id"Symbol"())
      }

      dataCtorObj = createDataConstructor(jsobj(
        get("instanceTrait")(
          JSReturn(instanceTypeExpr),
        ),
        "constructor" -> ctorFunc,

        "methods" -> JSArrayLiteral(methodObjects),
        "loadVTable" -> vtableObject,
      ))

    } yield paramSymbolDecls ++ localSymbolDecls :+ JSReturn(dataCtorObj)

  private def createMethodObject(method: ArMethod[context.type, PayloadSpecifiers.DeclarationPayloadSpecifier], instanceVarMap: JSExpression => VarMap): Emit[JSExpression] =
    for {
      sig <- method.signatureUnsubstituted
      impl <- method.payload : Comp[context.JSImpl.Method]
      jsImpl <- impl match {
        case context.JSImpl.Method.JSExpressionBody(expr) => expr.pure[Comp]
        case context.JSImpl.Method.ExpressionBody(expr) =>
          for {
            thisVarNum <- getNextSymbolId
            thisVarName = id"this_$thisVarNum"

            (paramListNoThis, paramMap) <- createParameterList(sig)
            paramList = JSFunctionParameter(JSBindingIdentifier(thisVarName), paramListNoThis)

            body <- addToVarMap(
              instanceVarMap(thisVarName).toSeq ++ paramMap.map { case (variable, name) => variable.id -> VariableLoader.fromExpr(JSIdentifier(name)) }
            : _*)(convertStmt(useReturn = true)(expr))
          } yield JSFunctionExpression(
            None,
            paramList,
            body
          )

        case context.JSImpl.Method.Abstract => JSNull.pure[Comp]
      }

      createMethod <- coreLibExport("createMethod")
      convMethodName <- getMethodName(method.name)
      convSig <- convertSignature(ErasedSignature.fromSignature(context)(sig))
    } yield jsobj(
      "name" -> convMethodName,
      get("sig")(
        JSReturn(convSig),
      ),
      "create" -> jsfunction(None)()(
        JSReturn(createMethod(jsobj(
          "implementation" -> jsImpl,
        )))
      )
    )

  private def createBaseTraitObject[TResult[_ <: Context with Singleton, Wrap[+_]]]
  (ownerSig: Signature[TResult, _])
  (baseTrait: typeSystem.TTraitType)
  : Emit[JSExpression] =
    for {
      baseSig <- baseTrait.arTrait.value.signature
      erasedBaseSig = ErasedSignature.fromSignatureParameters(context)(baseSig)

      baseTraitObjExpr <- getTraitJSObject(baseTrait.arTrait.value, erasedBaseSig)

      paramVarNames <- ZIO.access[EmitEnv] { emitEnv =>
        ownerSig.unsubstitutedParameters
          .zipWithIndex
          .map {
            case (param, i) =>
              param.paramVar.id -> id"param_${emitEnv.varMap.size + i}"
          }
      }

      traitArgs <- baseTrait.args.traverse(convertExpr).provideSome[EmitEnv] { emitEnv =>

        val paramVarMap: VarMap = paramVarNames
          .map {
            case (id, name) =>
              id -> VariableLoader.fromExpr(name)
          }
          .toMap

        emitEnv.copy(varMap = emitEnv.varMap ++ paramVarMap)
      }

    } yield jsobj(
      "baseType" -> baseTraitObjExpr,
      "parameterMapping" -> jsfunction(None)(paramVarNames.map { case (_, name) => name }: _*)(
        JSReturn(JSArrayLiteral(traitArgs))
      )
    )



  private def createClassCtorObject(ctor: ClassConstructor[context.type, PayloadSpecifiers.DeclarationPayloadSpecifier]): Emit[JSExpression] =
    for {
      sig <- ctor.signatureUnsubstituted
      impl <- ctor.payload : Comp[context.JSImpl.ClassConstructor]

      ownerClassSig <- ctor.ownerClass.signature
      ownerClassObj <- getClassJSObject(ctor.ownerClass, ErasedSignature.fromSignatureParameters(context)(ownerClassSig))

      func <- impl match {
        case context.JSImpl.ClassConstructor.StatementBody(body) =>
          for {
            thisVarNum <- getNextSymbolId
            thisVarName = id"this_$thisVarNum"

            (paramListNoThis, paramMap) <- createParameterList(sig)
            paramList = JSFunctionParameter(JSBindingIdentifier(thisVarName), paramListNoThis)
            paramVarMapping = paramMap
              .map { case (variable, name) =>
                variable.id -> VariableLoader.fromExpr(JSIdentifier(name))
              }

            (initStmts, initVarMapping) <- body.initStatements.foldLeftM[Emit, (Vector[JSStatement], Seq[(VariableId, VariableLoader)])]((Vector.empty[JSStatement], paramVarMapping)) {
              case ((acc, varMapping), ClassConstructorStatementExpr(expr)) =>
                for {
                  localVarMapRef <- Ref.make[VarMap](Map.empty)
                  newStmts <- addToVarMap(varMapping: _*)(
                    StatementConverterBindingRecorder(StatementConverterLocalBinding, localVarMapRef)
                      .convertStmt(useReturn = false)(expr)
                  )
                  localVarMap <- localVarMapRef.get
                } yield (acc ++ newStmts, varMapping ++ localVarMap)

              case ((acc, varMapping), InitializeFieldStatement(field, value)) =>
                val loader = new FieldVariableLoader(thisVarName, ownerClassObj, field.name.name)
                for {
                  valueExpr <- addToVarMap(varMapping: _*)(convertExpr(value))
                  newMapping = varMapping :+ (field.id -> loader)

                  initFieldStatement <- IO.fromEither(
                    loader.initializeVariable(valueExpr)
                      .toRight { NonEmptyList.of(CompilationError.EmitError(CompilationMessageSource.EmitPhase())) }
                  )

                } yield (acc :+ initFieldStatement, newMapping)
            }

            baseCall <- body.baseConstructorCall.traverse { baseCallExpr =>
              val baseClass = baseCallExpr.classCtor.value.ownerClass

              for {
                baseClassSig <- baseClass.signature
                baseCtorSig <- baseCallExpr.classCtor.value.signatureUnsubstituted

                baseClassErasedSig = ErasedSignature.fromSignatureParameters(context)(baseClassSig)
                baseClassObj <- getClassJSObject(baseClass, baseClassErasedSig)

                convBaseCtorSig <- convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(baseCtorSig))

                baseCtorObj = baseClassObj.prop(id"constructor")(convBaseCtorSig)

                argExprs <- addToVarMap(initVarMapping: _*)(
                  baseCallExpr.args.traverse(convertExpr(_))
                )
              } yield baseCtorObj.prop(id"initializeInstance")(thisVarName +: argExprs :_*)
            }

            postInitVarMapping = initVarMapping :+ (ThisParameterVariableId(ctor.id) -> VariableLoader.fromExpr(thisVarName))

            endExpr <- addToVarMap(postInitVarMapping: _*)(convertStmt(useReturn = false)(body.endExpr))
          } yield JSFunctionExpression(None, paramList, initStmts ++ baseCall.toList.toVector ++ endExpr)

      }

      convSig <- convertParameterOnlySignature(ErasedSignature.fromSignatureParameters(context)(sig))

      createClassConstructor <- coreLibExport("createClassConstructor")

    } yield jsobj(
      get("sig")(
        JSReturn(convSig),
      ),
      "create" -> jsfunction(None)(id"instanceClass")(
        JSReturn(createClassConstructor(
          jsobj(
          "implementation" -> func
          ),
          id"instanceClass",
        )),
      ),
    )

  private def createVTableObject(vtableContext: VTableContext.Aux[context.type])(vtable: vtableContext.VTable): Emit[JSExpression] = {
    val protoObjName = id"proto"

    vtable.methodMap.toVector
      .flatTraverse {
        case (slotMethod, vtableContext.VTableEntry(_, _, vtableContext.VTableEntryMethod(method))) =>
          for {
            slotMethodObj <- getMethodJSObject(slotMethod.value)
            implObj <- getMethodJSObject(method.value)
          } yield Vector(slotMethodObj.prop(id"override")(protoObjName, implObj.prop(id"invokeNonVirtual")))

        case (_, _) =>
          IO.succeed(Vector.empty)

      }
      .map { stmts =>
        jsfunction(None)(protoObjName)(stmts: _*)
      }
  }

  def createParameterList[TResult[_ <: Context with Singleton, Wrap[+_]]](sig: context.signatureContext.Signature[TResult, _]): UEmit[(JSFunctionParameterList, Seq[(ParameterVariable[context.type, Id], String)])] =
    ZIO.foldRight(sig.unsubstitutedParameters.unsized)((JSFunctionEmptyParameterList : JSFunctionParameterList, Seq.empty[(ParameterVariable[context.type, Id], String)])) {
      case (param, (list, map)) =>
        for {
          varNum <- getNextSymbolId
          varName = s"param_${varNum.toString}"

        } yield (
          JSFunctionParameter(
            JSBindingIdentifier(JSIdentifier(varName)),
            list
          ),
          map :+ (param.paramVar -> varName)
        )
    }
}
