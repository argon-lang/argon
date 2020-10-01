package dev.argon.backend.js

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import shapeless.{Id, Nat}
import zio._
import zio.interop.catz.core._
import cats.implicits._

private[js] trait JSEmitterExpressions extends JSEmitterReferenceLoader {
  import JSDSL._

  trait StatementConverter {

    def declareLocalVariable(variable: LocalVariable[context.type, Id]): Emit[VariableLoader]

    final def wrapStatement(expr: context.typeSystem.SimpleExpr): Emit[JSExpression] =
      for {
        stmts <- convertStmt(useReturn = true)(expr)
      } yield JSFunctionCall(JSArrowFunctionStmts(JSFunctionEmptyParameterList, stmts), Vector())

    final def convertStmt(useReturn: Boolean)(expr: context.typeSystem.SimpleExpr): Emit[Vector[JSStatement]] = expr match {
      case LetBinding(variable, value, next) =>
        for {
          valueExpr <- convertExpr(value)
          loader <- declareLocalVariable(variable)
          declStmt <- loader.initializeVariable(valueExpr) match {
            case Some(initExpr) => IO.succeed(initExpr)
            case None => Compilation.forErrors(DiagnosticError.EmitError(DiagnosticSource.EmitPhase()))
          }
          nextStmts <- convertStmt(useReturn)(next).provideSome[EmitEnv] { emitEnv =>
            emitEnv.copy(varMap = emitEnv.varMap + (variable.id -> loader))
          }
        } yield declStmt +: nextStmts

      case IfElse(condition, ifBody, elseBody) =>
        for {
          condExpr <- convertExpr(condition)
          ifBodyStmts <- convertStmt(useReturn)(ifBody)
          elseBodyStmts <- convertStmt(useReturn)(elseBody)

          boolValueSymbol <- coreLibExport("boolValueSymbol")
          accessNativeBool = condExpr.cprop(boolValueSymbol)
        } yield Vector(JSIfElseStatement(accessNativeBool, ifBodyStmts, elseBodyStmts))


      case Sequence(first, second) =>
        for {
          convFirst <- wrapStatement(first)
          convSecond <- convertStmt(useReturn)(second)
        } yield convFirst +: convSecond

      case _ =>
        if(useReturn) convertExpr(expr).map { jsExpr => Vector(JSReturn(jsExpr)) }
        else convertExpr(expr).map { jsExpr => Vector(jsExpr) }
    }

  }

  final case class StatementConverterBindingRecorder(converter: StatementConverter, localMap: Ref[VarMap]) extends StatementConverter {
    override def declareLocalVariable(variable: LocalVariable[context.type, Id]): Emit[VariableLoader] = for {
      loader <- converter.declareLocalVariable(variable)
      _ <- localMap.update { _ + (variable.id -> loader) }
    } yield loader
  }

  final class LocalVariableLoader(variable: Variable[context.type, Id], varName: JSIdentifier) extends VariableLoader {
    override def loadVariable: JSExpression = varName

    override def storeVariable(value: JSExpression): Option[JSExpression] =
      Some(varName := value)

    override def initializeVariable(value: JSExpression): Option[JSStatement] =
      variable.mutability match {
        case Mutability.Mutable => Some(let(varName ::= value))
        case Mutability.NonMutable => Some(const(varName ::= value))
      }
  }

  object StatementConverterLocalBinding extends StatementConverter {
    override def declareLocalVariable(variable: LocalVariable[context.type, Id]): Emit[VariableLoader] = for {
      varNum <- getNextSymbolId
      varName = id"local_$varNum"
    } yield new LocalVariableLoader(variable, varName)
  }

  def convertStmt(useReturn: Boolean)(expr: context.typeSystem.SimpleExpr): Emit[Vector[JSStatement]] =
    StatementConverterLocalBinding.convertStmt(useReturn)(expr)

  final case class StatementConverterDataCtorFieldBinding(instance: JSExpression, fieldMap: Ref[Map[LocalVariableId, JSIdentifier]]) extends StatementConverter {
    override def declareLocalVariable(variable: LocalVariable[context.type, Id]): Emit[VariableLoader] = for {
      symVarNum <- getNextSymbolId
      symVarName = id"local_sym_$symVarNum"

      _ <- fieldMap.update { _ + (variable.id -> symVarName) }
    } yield new VariableLoader {
      override def loadVariable: JSExpression = instance.cprop(symVarName)

      override def storeVariable(value: JSExpression): Option[JSExpression] =
        Some(instance.cprop(symVarName) := value)

      override def initializeVariable(value: JSExpression): Option[JSStatement] =
        variable.mutability match {
          case Mutability.Mutable => storeVariable(value)
          case Mutability.NonMutable => Some(defineProperty(instance, symVarName, value))
        }
    }
  }

  def convertArgs[TResult[_ <: Context with Singleton, _[+_]]](sigComp: Comp[context.signatureContext.Signature[TResult, _ <: Nat]])(args: Vector[context.typeSystem.WrapExpr]): Emit[Vector[JSExpression]] =
    sigComp.flatMap { sig =>
      sig.unsubstitutedParameters.zip(args)
        .collect { case (parameter, arg) if !parameter.paramVar.isErased => arg }
        .traverse(convertExpr(_))
    }


  def convertExpr(expr: context.typeSystem.SimpleExpr): Emit[JSExpression] = {
    import context.typeSystem. { context => _, _ }
    expr match {
      case ClassConstructorCall(classType, ctor, args) =>
        for {
          ctorObj <- getClassConstructorJSObject(ctor.value)

          classTypeObj <- convertExpr(classType)
          argExprs <- convertArgs(ctor.value.signatureUnsubstituted)(args)

        } yield ctorObj.prop(id"invoke")(classTypeObj +: argExprs: _*)

      case DataConstructorCall(dataCtorInstanceType, args) =>
        for {
          sig <- dataCtorInstanceType.ctor.value.signature

          ownerObj <- getDataCtorJSObject(dataCtorInstanceType.ctor.value, ErasedSignature.fromSignatureParameters(context)(sig))

          instTypeExpr <- convertExpr(dataCtorInstanceType)

          argExprs <- convertArgs(dataCtorInstanceType.ctor.value.signature)(args)
        } yield ownerObj.prop(id"createInstance")(instTypeExpr +: argExprs: _*)

      case EnsureExecuted(body, ensuring) =>
        for {
          jsBody <- convertStmt(useReturn = true)(body)
          jsEnsuring <- convertStmt(useReturn = false)(ensuring)
        } yield JSFunctionCall(JSFunctionExpression(None, JSFunctionEmptyParameterList, Vector(JSTryStatement(jsBody, None, Some(jsEnsuring)))), Vector())

      case FunctionCall(func, args, _) =>
        for {
          funcExpr <- getFunctionJSObject(func.value)
          argExprs <- convertArgs(func.value.signature)(args)
        } yield funcExpr.prop(id"invoke")(argExprs: _*)

      case FunctionObjectCall(funcExpr, arg, _) =>
        for {
          jsFunc <- convertExpr(funcExpr)
          jsArg <- convertExpr(arg)
        } yield JSFunctionCall(jsFunc, Vector(jsArg))

      case IfElse(_, _, _) =>
        StatementConverterLocalBinding.wrapStatement(expr)

      case LetBinding(_, _, _) =>
        StatementConverterLocalBinding.wrapStatement(expr)

      case LoadConstantBool(b, _) =>
        for {
          createBool <- coreLibExport("createBool")
        } yield createBool(JSBoolean(b))

      case LoadConstantInt(i, _) =>
        for {
          createInt <- coreLibExport("createInt")
        } yield createInt(JSBigInt(i))

      case LoadConstantString(str, _) =>
        for {
          createInt <- coreLibExport("createString")
        } yield createInt(JSString(str))

      case LoadLambda(argVariable, body) =>
        for {
          varNum <- getNextSymbolId
          varName = id"local_$varNum"
          bodyExpr <- addToVarMap(argVariable.id -> new LocalVariableLoader(argVariable, varName))(convertExpr(body))
        } yield JSArrowFunctionExpr(
          JSFunctionParameter(JSBindingIdentifier(varName), JSFunctionEmptyParameterList),
          bodyExpr
        )

      case LoadTuple(NonEmptyList(TupleElement(value), Nil)) =>
        convertExpr(value)

      case expr @ LoadTuple(_) =>
        for {
          values <- expr.values.toList.toVector.traverse { elem => convertExpr(elem.value) }
        } yield JSArrayLiteral(values)

      case LoadTupleElement(tupleValue, _, index) =>
        for {
          tuple <- convertExpr(tupleValue)
        } yield JSPropertyAccessBracket(tuple, JSBigInt(index))

      case LoadUnit(_) =>
        coreLibExport("unitValue")

      case LoadVariable(variable) =>
        ZIO.access[EmitEnv] { emitEnv =>
          emitEnv.varMap(variable.id).loadVariable
        }

      case MethodCall(method, instance, _, args, _) =>
        for {
          instanceExpr <- convertExpr(instance)
          methodObj <- getMethodJSObject(method.value)
          argExprs <- convertArgs(method.value.signatureUnsubstituted)(args)
        } yield methodObj.prop(id"invoke")(instanceExpr +: argExprs: _*)

      case PatternMatch(expr, cases) =>

        def convertPattern(patternValue: JSExpression)(pattern: PatternExpr[context.type, Id])(bodyEmitter: Emit[Vector[JSStatement]]): Emit[Vector[JSStatement]] =
          pattern match {
            case PatternExpr.DataDeconstructor(ctor, args) =>  ???
            case PatternExpr.Binding(variable) =>
              for {
                loader <- StatementConverterLocalBinding.declareLocalVariable(variable)
                initStmt <- ZIO.fromOption(loader.initializeVariable(patternValue))
                  .mapError { _ => DiagnosticError.EmitError(DiagnosticSource.EmitPhase()) }

                body <- bodyEmitter.provideSome[EmitEnv] { emitEnv =>
                  emitEnv.copy(varMap = emitEnv.varMap + (variable.id -> loader))
                }

              } yield initStmt +: body

            case PatternExpr.CastBinding(variable) =>
              for {
                loader <- StatementConverterLocalBinding.declareLocalVariable(variable)
                initStmt <- ZIO.fromOption(loader.initializeVariable(patternValue))
                  .mapError { _ => DiagnosticError.EmitError(DiagnosticSource.EmitPhase()) }

                body <- bodyEmitter.provideSome[EmitEnv] { emitEnv =>
                  emitEnv.copy(varMap = emitEnv.varMap + (variable.id -> loader))
                }

              } yield ???
          }

        val matchValueIdentifier = JSIdentifier("matchValue")

        for {
          jsExpr <- convertExpr(expr)
          casesBody <- cases.traverse {
            case PatternCase(pattern, body) =>
              convertPattern(matchValueIdentifier)(pattern)(StatementConverterLocalBinding.convertStmt(useReturn = true)(body))
                .map { bodyStmts =>
                  JSBlockStatement(bodyStmts)
                }
          }
        } yield JSFunctionCall(
          JSArrowFunctionStmts(
            JSFunctionParameter(JSBindingIdentifier(matchValueIdentifier), JSFunctionEmptyParameterList),
            casesBody.toList.toVector
          ),
          Vector(jsExpr)
        )

      case e @ Sequence(_, _) =>
        StatementConverterLocalBinding.wrapStatement(e)

      case StoreVariable(variable, value, _) =>
        for {
          jsValue <- convertExpr(value)
          loader <- getVariableLoader(variable.id)
          expr <- requireSome(loader.storeVariable(jsValue))
        } yield expr

      case ClassType(arClass, args) =>
        for {
          sig <- arClass.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          classObj <- getClassJSObject(arClass.value, erasedSig)

          argExprs <- convertArgs(arClass.value.signature)(args)
        } yield classObj.prop(id"createClassObject")(argExprs: _*)

      case TraitType(arTrait, args) =>
        for {
          sig <- arTrait.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          traitObj <- getTraitJSObject(arTrait.value, erasedSig)

          argExprs <- convertArgs(arTrait.value.signature)(args)
        } yield traitObj.prop(id"createTraitObject")(argExprs: _*)

      case DataConstructorType(ctor, args, _) =>
        for {
          sig <- ctor.value.signature
          erasedSig = ErasedSignature.fromSignatureParameters(context)(sig)
          ctorObj <- getDataCtorJSObject(ctor.value, erasedSig)

          argExprs <- convertArgs(ctor.value.signature)(args)
        } yield ctorObj.prop(id"createTypeObject")(argExprs: _*)


      case e => throw new NotImplementedError(s"Expression type ${e.getClass.getName} is not yet implemented")
    }
  }

  def getNextSymbolId: UEmit[Int] =
    ZIO.accessM[EmitEnv] { emitEnv =>
      emitEnv.nextSymbolId.modify { x => (x, x + 1) }
    }

  def defineProperty(obj: JSExpression, name: JSExpression, value: JSExpression): JSExpression =
    id"Object".prop(id"defineProperty")(obj, name, jsobj(
      "configurable" -> JSBoolean(false),
      "writable" -> JSBoolean(false),
      "value" -> value,
    ))

}
