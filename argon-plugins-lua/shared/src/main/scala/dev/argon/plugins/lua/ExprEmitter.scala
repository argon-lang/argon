package dev.argon.plugins.lua

import dev.argon.compiler.HasContext
import dev.argon.compiler.signature.{ImportSpecifier, Signature, SignatureEraser}
import dev.argon.expr.ArgonBuiltin
import dev.argon.plugins.lua.ExprEmitter.OutputMode
import dev.argon.util.{*, given}
import zio.*
import zio.stm.*

trait ExprEmitter[R <: LuaEnv, E >: LuaError] extends ModuleEmitterBase[R, E] {
  import context.Comp
  import context.ExprContext.*

  private lazy val eraser: SignatureEraser & HasContext[context.type] = SignatureEraser(context)
  val nextLocalIndex: TRef[Int]
  val varMapping: TMap[LocalVariable, String]
  val instanceVarMapping: Map[InstanceVariable, AST.Var]
  val parameterVarMapping: Map[ParameterVariable, AST.Var]
  val memberVarMapping: Map[MemberVariable, AST.Var]
  
  private def getNextLocalSTM: USTM[String] =
    nextLocalIndex.getAndUpdate(_ + 1).map { i => s"local$i" }
    
  private def getNextLocal: UIO[String] =
    getNextLocalSTM.commit
    
  private def getLocalName(v: LocalVariable): UIO[String] =
    varMapping.getOrElseSTM(v, getNextLocalSTM).commit

  private def getVarExp(v: Variable): UIO[AST.Var] =
    v match
      case v: LocalVariable => getLocalName(v).map(AST.NameExp.apply)
      case v: InstanceVariable => ZIO.fromEither(instanceVarMapping.get(v).toRight { RuntimeException("Unknown variable was referenced") }).orDie
      case v: MemberVariable => ZIO.fromEither(memberVarMapping.get(v).toRight { RuntimeException("Unknown variable was referenced") }).orDie
      case v: ParameterVariable => ZIO.fromEither(parameterVarMapping.get(v).toRight { RuntimeException("Unknown variable was referenced") }).orDie
      case _: FunctionResultVariable => ZIO.die(RuntimeException("Cannot emit FunctionResultVariable because it is always erased"))
    end match

  def emit(outputMode: OutputMode)(e: WrapExpr): Comp[AST.Block] =
    emitWrap(outputMode)(e).map(AST.Block.apply)

  private def emitWrap(outputMode: OutputMode)(e: WrapExpr): Comp[Seq[AST.Stat]] =
    e match
      case WrapExpr.OfExpr(expr) => emitArExpr(outputMode)(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitArExpr(outputMode: OutputMode)(e: ArExpr[ExprConstructor]): Comp[Seq[AST.Stat]] =
    e.constructor match
      case ctor: (e.constructor.type & ExprConstructor.BindVariable) =>
        for
          varName <- getLocalName(ctor.variable)
          value <- emitWrap(OutputMode.assignVar(varName))(e.getArgs(ctor))
        yield declareVar(varName) ++ value ++ unitResult(outputMode)

      case ctor: (e.constructor.type & ExprConstructor.FunctionCall) =>
        val f = ctor.function
        for
          sig <- f.signature
          erasedSig <- eraser.erasedWithResult(sig)
          func = getImport("functions", ImportSpecifier(f.owner.module.tube.tubeName, f.owner.module.moduleName.path, f.owner.ownedName, erasedSig))
          (argStats, argVars) <- emitArgs(sig, e.getArgs(ctor))

        yield argStats ++ funcResult(outputMode)(AST.SimpleFunctionCall(func, argVars))

      case ctor: (e.constructor.type & ExprConstructor.FunctionObjectCall.type) =>
        val (f, a) = e.getArgs(ctor)
        for
          fName <- getNextLocal
          fStat <- emitWrap(OutputMode.assignVar(fName))(f)
          aName <- getNextLocal
          aStat <- emitWrap(OutputMode.assignVar(aName))(f)
        yield declareVar(fName) ++ fStat ++ declareVar(aName) ++ aStat ++
          funcResult(outputMode)(AST.SimpleFunctionCall(AST.NameExp(fName), Seq(AST.NameExp(aName))))

      case ctor: (e.constructor.type & ExprConstructor.IfElse) =>
        val (cond, trueBody, falseBody) = e.getArgs(ctor)
        for
          condVar <- getNextLocal
          condStmts <- emitWrap(OutputMode.assignVar(condVar))(cond)
          trueStmts <- emitWrap(outputMode)(trueBody)
          falseStmts <- emitWrap(outputMode)(falseBody)
        yield (declareVar(condVar) ++ condStmts) :+ AST.If(
          AST.NameExp(condVar),
          AST.Block(trueStmts),
          Seq.empty,
          if falseStmts.isEmpty then
            None
          else
            Some(AST.Block(falseStmts))
        )

      case ctor: (e.constructor.type & ExprConstructor.LoadConstantBool) =>
        ZIO.succeed(
          expResult(outputMode)(if ctor.b then AST.TrueLiteral else AST.FalseLiteral)
        )

      case ctor: (e.constructor.type & ExprConstructor.LoadConstantInt) =>
        ZIO.succeed(
          expResult(outputMode)(AST.IntegerLiteral(ctor.i.toLong))
        )

      case ctor: (e.constructor.type & ExprConstructor.LoadConstantString) =>
        ZIO.succeed(
          expResult(outputMode)(AST.StringLiteral(ctor.s))
        )

      case ctor: (e.constructor.type & ExprConstructor.LoadLambda) =>
        for
          argName <- getLocalName(ctor.argVariable)
          body <- emitWrap(OutputMode.ReturnValue)(e.getArgs(ctor))
        yield expResult(outputMode)(AST.FunctionDefinitionExp(Seq(argName), hasRest = false, AST.Block(body)))

      case ctor: (e.constructor.type & ExprConstructor.LoadTuple.type) =>
        for
          (stats, elements) <- emitMany(e.getArgs(ctor))
        yield stats ++ expResult(outputMode)(toArrayExp(elements))

      case ctor: (e.constructor.type & ExprConstructor.LoadTupleElement) =>
        for
          varName <- getNextLocal
          stats <- emitWrap(OutputMode.assignVar(varName))(e.getArgs(ctor))
        yield declareVar(varName) ++ stats ++ expResult(outputMode)(
          AST.MemberAccessIndex(
            AST.NameExp(varName),
            AST.IntegerLiteral(ctor.index.toLong),
          )
        )

      case ctor: (e.constructor.type & ExprConstructor.LoadVariable) =>
        getVarExp(ctor.variable).map(expResult(outputMode))

      case ctor: (e.constructor.type & ExprConstructor.Proving) =>
        ZIO.suspendSucceed { emitWrap(outputMode)(e.getArgs(ctor)) }

      case ctor: (e.constructor.type & ExprConstructor.Sequence.type) =>
        val seqBody: NonEmptyList[WrapExpr] = e.getArgs(ctor)

        for
          initStmts <- ZIO.foreach(seqBody.init)(emitWrap(OutputMode.Ignore))
          resStmts <- emitWrap(outputMode)(seqBody.last)
        yield initStmts.flatten ++ resStmts

      case ctor: (e.constructor.type & ExprConstructor.StoreVariable) =>
        for
          vExp <- getVarExp(ctor.variable)
          storeStats <- emitWrap(OutputMode.Assignment(vExp))(e.getArgs(ctor))
        yield storeStats ++ unitResult(outputMode)

      case ctor: (e.constructor.type & ExprConstructor.Builtin[?]) =>
        ctor.builtin match
          case value: ArgonBuiltin.SimpleValue =>
            value match
              case ArgonBuiltin.IntType =>
                ZIO.succeed(expResult(outputMode)(AST.TableConstructor(Seq(
                  AST.Field.NamedFixed("type", AST.StringLiteral("int"))
                ))))

              case ArgonBuiltin.BoolType =>
                ZIO.succeed(expResult(outputMode)(AST.TableConstructor(Seq(
                  AST.Field.NamedFixed("type", AST.StringLiteral("bool"))
                ))))

              case ArgonBuiltin.StringType =>
                ZIO.succeed(expResult(outputMode)(AST.TableConstructor(Seq(
                  AST.Field.NamedFixed("type", AST.StringLiteral("string"))
                ))))

              case ArgonBuiltin.NeverType =>
                ZIO.succeed(expResult(outputMode)(AST.TableConstructor(Seq(
                  AST.Field.NamedFixed("type", AST.StringLiteral("never"))
                ))))
            end match

          case operation: ArgonBuiltin.UnaryOperation =>
            val a = e.getArgs(ctor).head
            for
              aVar <- getNextLocal
              aStat <- emitWrap(OutputMode.assignVar(aVar))(a)

              resExp =
                operation match
                  case ArgonBuiltin.IntNegate => AST.UnOpExp(AST.UnOp.Minus, AST.NameExp(aVar))
                end match
              
            yield declareVar(aVar) ++ aStat ++ expResult(outputMode)(resExp)

          case operation: ArgonBuiltin.BinaryOperation =>
            val args = e.getArgs(ctor)
            val a = args.head
            val b = args.tail.head
            
            for
              aVar <- getNextLocal
              aStat <- emitWrap(OutputMode.assignVar(aVar))(a)
              aExp = AST.NameExp(aVar)

              bVar <- getNextLocal
              bStat <- emitWrap(OutputMode.assignVar(bVar))(b)
              bExp = AST.NameExp(bVar)

              resExp =
                operation match
                  case ArgonBuiltin.IntAdd => AST.BinOpExp(AST.BinOp.Add, aExp, bExp)
                  case ArgonBuiltin.IntSub => AST.BinOpExp(AST.BinOp.Sub, aExp, bExp)
                  case ArgonBuiltin.IntMul => AST.BinOpExp(AST.BinOp.Mul, aExp, bExp)
                  case ArgonBuiltin.IntEQ => AST.BinOpExp(AST.BinOp.EQ, aExp, bExp)
                  case ArgonBuiltin.IntNE => AST.BinOpExp(AST.BinOp.NE, aExp, bExp)
                  case ArgonBuiltin.IntLT => AST.BinOpExp(AST.BinOp.LT, aExp, bExp)
                  case ArgonBuiltin.IntLE => AST.BinOpExp(AST.BinOp.LE, aExp, bExp)
                  case ArgonBuiltin.IntGT => AST.BinOpExp(AST.BinOp.GT, aExp, bExp)
                  case ArgonBuiltin.IntGE => AST.BinOpExp(AST.BinOp.GE, aExp, bExp)
                  case ArgonBuiltin.StringConcat => AST.BinOpExp(AST.BinOp.Concat, aExp, bExp)
                end match

            yield declareVar(aVar) ++ aStat ++ declareVar(bVar) ++ bStat ++ expResult(outputMode)(resExp)

        end match

      case ctor: (e.constructor.type & ExprConstructor.TraitType) =>
        val t = ctor.arTrait
        for
          sig <- t.signature
          erasedSig <- eraser.erasedNoResult(sig)
          func = getImport("functions", ImportSpecifier(t.owner.module.tube.tubeName, t.owner.module.moduleName.path, t.owner.ownedName, erasedSig))
          (argStats, argVars) <- emitArgs(sig, e.getArgs(ctor))

        yield argStats ++ funcResult(outputMode)(AST.SimpleFunctionCall(func, argVars))

      case ctor: (e.constructor.type & ExprConstructor.ClassType) =>
        val t = ctor.arClass
        for
          sig <- t.signature
          erasedSig <- eraser.erasedNoResult(sig)
          func = getImport("functions", ImportSpecifier(t.owner.module.tube.tubeName, t.owner.module.moduleName.path, t.owner.ownedName, erasedSig))
          (argStats, argVars) <- emitArgs(sig, e.getArgs(ctor))

        yield argStats ++ funcResult(outputMode)(AST.SimpleFunctionCall(func, argVars))

      case _ => ???
    end match

  private def emitMany(args: Seq[WrapExpr]): Comp[(Seq[AST.Stat], Seq[AST.Exp])] =
    ZIO.foreach(args) { arg =>
      for
        name <- getNextLocal
        argStats <- emitWrap(OutputMode.assignVar(name))(arg)
      yield (argStats, AST.NameExp(name))
    }.map { results =>
      val allStats = results.flatMap { _._1 }
      val allNames = results.map { _._2 }
      (allStats, allNames)
    }

  private def emitArgs(sig: Signature[WrapExpr, ?], args: Seq[WrapExpr]): Comp[(Seq[AST.Stat], Seq[AST.Exp])] =
    def filterArgs(sig: Signature[WrapExpr, ?], args: Seq[WrapExpr], acc: Seq[WrapExpr]): Seq[WrapExpr] =
      (sig, args) match
        case (sig: Signature.Parameter[WrapExpr, ?], _ +: t) if sig.isErased =>
          filterArgs(sig.next, t, acc)

        case (sig: Signature.Parameter[WrapExpr, ?], h +: t) =>
          filterArgs(sig.next, t, acc :+ h)

        case (sig: Signature.Result[WrapExpr, ?], Seq()) => acc

        case _ => throw new RuntimeException("Argument mismatch")
      end match

    emitMany(filterArgs(sig, args, Seq.empty))
  end emitArgs

  private def declareVar(varName: String): Seq[AST.Stat] =
    Seq(AST.LocalDeclaration(Seq((varName, AST.Attrib.Empty)), Seq()))

  private def expResult(outputMode: OutputMode)(e: AST.Exp): Seq[AST.Stat] =
    outputMode match
      case OutputMode.ReturnValue => Seq(AST.Return(Seq(e)))
      case OutputMode.Ignore => Seq.empty
      case OutputMode.Assignment(varExp) => Seq(AST.Assignment(Seq(varExp), Seq(e)))
    end match

  private def funcResult(outputMode: OutputMode)(e: AST.FunctionCall): Seq[AST.Stat] =
    outputMode match
      case OutputMode.ReturnValue => Seq(AST.Return(Seq(e)))
      case OutputMode.Ignore => Seq(e)
      case OutputMode.Assignment(varExp) => Seq(AST.Assignment(Seq(varExp), Seq(e)))
    end match

  private def unitResult(outputMode: OutputMode): Seq[AST.Stat] =
    expResult(outputMode)(AST.TableConstructor(Seq()))


  private def getImport(importType: "classes" | "traits" | "functions", specifier: ImportSpecifier): AST.Exp =
    AST.MemberAccessIndex(
      AST.MemberAccessIndex(
        AST.MemberAccessName(
          AST.MethodCall(
            AST.MethodCall(
              AST.NameExp("rt"),
              "get_tube",
              Seq(
                getTubeNameExpr(specifier.tube),
              ),
            ),
            "get_module",
            Seq(
              getModulePathExpr(specifier.module),
            ),
          ),
          importType,
        ),
        getIdentifierKeyExprMemo(specifier.name),
      ),
      getErasedSigKeyExprMemo(specifier.signature),
    )

}

object ExprEmitter {
  enum OutputMode derives CanEqual {
    case ReturnValue
    case Ignore
    case Assignment(target: AST.Var)
  }

  object OutputMode {
    def assignVar(name: String): OutputMode =
      OutputMode.Assignment(AST.NameExp(name))
  }
}
