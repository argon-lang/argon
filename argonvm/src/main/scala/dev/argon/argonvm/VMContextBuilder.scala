package dev.argon.argonvm

import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.expr.{BuiltinType, NullaryBuiltin, UnaryBuiltin, BinaryBuiltin}
import dev.argon.util.UniqueIdentifier
import zio.{Ref, ZIO}
import zio.stm.TMap

trait VMContextBuilder {
  val context: Context

  import context.Comp
  import context.DefaultExprContext.{Expr, Builtin, Var, ParameterVar}

  type CompatibleVMContext = VMContext {
    type Env = context.Env
    type Error = context.Error
    val implementations: Implementations {
      type ExternFunctionImplementation = context.implementations.ExternFunctionImplementation
      type FunctionReference = context.implementations.FunctionReference
    }
  }

  val vmContext: CompatibleVMContext
  import vmContext.{
    VMType,
    RegisterType,
    ControlFlowGraph,
    Register,
    RegisterDeclaration,
    InstructionBlock,
    InstructionResult,
    Instruction,
    FunctionCall,
    BranchInstruction,
  }

  def createTube(tube: ArTubeC & HasContext[context.type]): vmContext.VMTube =
    new vmContext.VMTube {
      override def modules: Map[ModulePath, vmContext.VMModule] =
        tube.modules.view.mapValues(createModule).toMap
    }

  def createModule(module: ArModuleC & HasContext[context.type]): vmContext.VMModule =
    new vmContext.VMModule {
      override def exports: Comp[Seq[(Option[IdentifierExpr], ErasedSignature, vmContext.ModuleExport)]] =
        val eraser = SignatureEraser(context)

        for
          exportMap <- module.allExports(Set.empty)
          res <- ZIO.foreach(exportMap.toSeq) { (name, exports) =>
            ZIO.foreach(exports) {
              case ModuleExportC.Function(f) =>
                for
                  sig <- f.signature
                  sig <- eraser.eraseSignature(sig)

                  _ <- if f.isErased then f.implementation.getOrElse(ZIO.unit) else ZIO.unit

                yield if f.isErased then Seq() else Seq((name, sig, createFunc(f)))

              case ModuleExportC.Exported(_) =>
                ZIO.succeed(Seq())
            }
          }
        yield res.flatten.flatten
      end exports
    }

  def createFunc(f: ArFuncC & HasContext[context.type]): vmContext.VMFunction =
    new vmContext.VMFunction {
      override def parameters: Comp[Seq[VMType]] =
        f.signature.flatMap { sig =>
          ZIO.foreach(sig.parameters.filterNot { _.isErased }) { param =>
            exprToType(param.paramType)
          }
        }

      override def returnType: Comp[VMType] =
        f.signature.flatMap(sig => exprToType(sig.returnType))

      override def reference: Comp[vmContext.implementations.FunctionReference] =
        f.reference

      override def implementation: Option[Comp[vmContext.implementations.FunctionImplementation]] =
        f.implementation.map(_.flatMap(convertImpl))

      private def convertImpl(impl: context.implementations.FunctionImplementation): Comp[vmContext.implementations.FunctionImplementation] =
        impl match {
          case context.implementations.FunctionImplementation.Expr(e) =>
            for
              sig <- f.signature
              paramVars =  context.DefaultSignatureContext.SignatureParameter.getParameterVariables(f, sig.parameters)
              cfg <- exprToCFG(e, paramVars.filterNot { _.isErased })
            yield vmContext.implementations.FunctionImplementation.Instructions(cfg)

          case context.implementations.FunctionImplementation.Extern(e) =>
            ZIO.succeed(vmContext.implementations.FunctionImplementation.Extern(e))
        }
    }



  def exprToType(e: Expr): Comp[VMType] =
    ArgonEvaluator(context).normalizeToValue(e, context.Config.fuel).flatMap {
      case Expr.Builtin(Builtin.Nullary(t: BuiltinType)) =>
        ZIO.succeed(VMType.Builtin(t, Seq()))

      case Expr.Builtin(Builtin.Unary(t: BuiltinType, a)) =>
        for
          a <- exprToType(a)
        yield VMType.Builtin(t, Seq(a))

      case Expr.Builtin(Builtin.Binary(t: BuiltinType, a, b)) =>
        for
          a <- exprToType(a)
          b <- exprToType(b)
        yield VMType.Builtin(t, Seq(a, b))

      case Expr.FunctionType(a, b) =>
        for
          a <- exprToType(a)
          b <- exprToType(b)
        yield VMType.Function(a, b)

      case Expr.Tuple(items) =>
        for
          items <- ZIO.foreach(items)(exprToType)
        yield VMType.Tuple(items)

      case _ =>
        println(e.getClass)
        ???
    }

  def exprToRegType(e: Expr): Comp[RegisterType] =
    ArgonEvaluator(context).normalizeToValue(e, context.Config.fuel).flatMap {
      case Expr.Builtin(Builtin.Nullary(t: BuiltinType)) =>
        ZIO.succeed(RegisterType.Builtin(t, Seq()))

      case Expr.Builtin(Builtin.Unary(t: BuiltinType, a)) =>
        for
          a <- exprToRegType(a)
        yield RegisterType.Builtin(t, Seq(a))

      case Expr.Builtin(Builtin.Binary(t: BuiltinType, a, b)) =>
        for
          a <- exprToRegType(a)
          b <- exprToRegType(b)
        yield RegisterType.Builtin(t, Seq(a, b))

      case Expr.FunctionType(a, b) =>
        for
          a <- exprToRegType(a)
          b <- exprToRegType(b)
        yield RegisterType.Function(a, b)

      case Expr.Tuple(items) =>
        for
          items <- ZIO.foreach(items)(exprToRegType)
        yield RegisterType.Tuple(items)

      case _ =>
        println(e.getClass)
        ???
    }

  def vmTypeToRegType(regArgs: Seq[Register])(t: VMType): RegisterType =
    t match {
      case VMType.Builtin(builtin, args) =>
        RegisterType.Builtin(builtin, args.map(vmTypeToRegType(regArgs)))

      case VMType.Function(input, output) =>
        val input2 = vmTypeToRegType(regArgs)(input)
        val output2 = vmTypeToRegType(regArgs)(output)
        RegisterType.Function(input2, output2)

      case VMType.Tuple(args) =>
        RegisterType.Tuple(args.map(vmTypeToRegType(regArgs)))

      case VMType.Type =>
        RegisterType.Type

      case VMType.Param(i) =>
        RegisterType.Reg(regArgs(i))
    }

  def exprToCFG(expr: Expr, params: Seq[ParameterVar]): Comp[ControlFlowGraph] =
    for
      startBlockId <- UniqueIdentifier.make
      blocks <- Ref.make(Map.empty[UniqueIdentifier, InstructionBlock])
      variables <- Ref.make(Map.empty[Var, RegisterDeclaration])
      blockParams <- Ref.make(Seq.empty[RegisterDeclaration])
      currentBlockId <- Ref.make(startBlockId)
      instructions <- Ref.make(Seq.empty[Instruction])

      emitter = CFGEmitter(
        startBlockId = startBlockId,
        blocks = blocks,
        variables = variables,
        blockParams = blockParams,
        currentBlockId = currentBlockId,
        instructions = instructions,
      )

      _ <- emitter.initParams(params)
      _ <- emitter.emitExprTail(expr)
      cfg <- emitter.toCFG
    yield cfg

  final class CFGEmitter(
    startBlockId: UniqueIdentifier,
    blocks: Ref[Map[UniqueIdentifier, InstructionBlock]],
    variables: Ref[Map[Var, RegisterDeclaration]],
    blockParams: Ref[Seq[RegisterDeclaration]],
    currentBlockId: Ref[UniqueIdentifier],
    instructions: Ref[Seq[Instruction]],
  ) {
    def initParams(params: Seq[ParameterVar]): Comp[Unit] =
      ZIO.foreachDiscard(params) { param =>
        for
          t <- exprToRegType(param.varType)
          reg <- makeRegister(t)
          _ <- blockParams.update(_ :+ reg)
          _ <- variables.update(_.updated(param, reg))
        yield ()
      }

    def emitExpr(expr: Expr): Comp[RegisterDeclaration] =
      expr match {
        case Expr.BoolLiteral(b) =>
          for
            reg <- makeRegister(RegisterType.Builtin(NullaryBuiltin.BoolType, Seq()))
            _ <- append(Instruction.LoadBool(reg, b))
          yield reg

        case Expr.Builtin(Builtin.Nullary(builtin)) =>
          val t = builtin match {
            case NullaryBuiltin.IntType |
                 NullaryBuiltin.BoolType |
                 NullaryBuiltin.StringType |
                 NullaryBuiltin.NeverType =>
              RegisterType.Type
          }
          for
            reg <- makeRegister(t)
            _ <- append(Instruction.LoadNullaryBuiltin(reg, builtin))
          yield reg

        case Expr.Builtin(Builtin.Unary(builtin, a)) =>
          val t = builtin match {
            case UnaryBuiltin.IntNegate |
                 UnaryBuiltin.IntBitNot =>
              RegisterType.Builtin(NullaryBuiltin.IntType, Seq())
          }

          for
            aReg <- emitExpr(a)
            reg <- makeRegister(t)
            _ <- append(Instruction.LoadUnaryBuiltin(reg, builtin, aReg.register))
          yield reg

        case Expr.Builtin(Builtin.Binary(builtin, a, b)) =>
          val t = builtin match {
            case BinaryBuiltin.ConjunctionType |
                 BinaryBuiltin.DisjunctionType =>
              ???

            case BinaryBuiltin.IntAdd |
                 BinaryBuiltin.IntSub |
                 BinaryBuiltin.IntMul |
                 BinaryBuiltin.IntBitAnd |
                 BinaryBuiltin.IntBitOr |
                 BinaryBuiltin.IntBitXOr |
                 BinaryBuiltin.IntBitShiftLeft |
                 BinaryBuiltin.IntBitShiftRight =>
              RegisterType.Builtin(NullaryBuiltin.IntType, Seq())


            case BinaryBuiltin.IntEQ |
                 BinaryBuiltin.IntNE |
                 BinaryBuiltin.IntLT |
                 BinaryBuiltin.IntLE |
                 BinaryBuiltin.IntGT |
                 BinaryBuiltin.IntGE |
                 BinaryBuiltin.StringEQ |
                 BinaryBuiltin.StringNE =>
              RegisterType.Builtin(NullaryBuiltin.BoolType, Seq())

            case BinaryBuiltin.StringConcat =>
              RegisterType.Builtin(NullaryBuiltin.StringType, Seq())
          }

          for
            aReg <- emitExpr(a)
            bReg <- emitExpr(b)
            reg <- makeRegister(t)
            _ <- append(Instruction.LoadBinaryBuiltin(reg, builtin, aReg.register, bReg.register))
          yield reg

        case Expr.FunctionCall(f, args) =>
          for
            call <- emitFunctionCall(f, args)

            returnType <- call.f.returnType.map(vmTypeToRegType(call.args))

            reg <- makeRegister(returnType)
            _ <- append(Instruction.Call(InstructionResult.Value(reg), call))
          yield reg

        case Expr.IfElse(_, _, cond, whenTrue, whenFalse) =>
          for
            condReg <- emitExpr(cond)

            savedVars <- variables.get
            savedVarSeq = savedVars.keys.toSeq
            savedVarRegDecls = savedVarSeq.map(v => savedVars(v))
            savedVarRegs = savedVarRegDecls.map(r => r.register)
            
            whenTrueLabel <- UniqueIdentifier.make
            whenFalseLabel <- UniqueIdentifier.make
            endLabel <- UniqueIdentifier.make

            _ <- generateBlock(BranchInstruction.JumpIf(condReg.register, whenTrueLabel, savedVarRegs, whenFalseLabel, savedVarRegs))

            _ <- startBlock(whenTrueLabel, savedVarRegDecls, savedVars)
            trueReg <- emitExpr(whenTrue)
            currentVars <- variables.get
            _ <- generateBlock(BranchInstruction.Jump(endLabel, savedVarSeq.map(v => currentVars(v).register) :+ trueReg.register))

            _ <- startBlock(whenFalseLabel, savedVarRegDecls, savedVars)
            falseReg <- emitExpr(whenFalse)
            currentVars <- variables.get
            _ <- generateBlock(BranchInstruction.Jump(endLabel, savedVarSeq.map(v => currentVars(v).register) :+ falseReg.register))

            regMapping <- startBlock(endLabel, savedVarRegDecls :+ trueReg, savedVars)

          yield regMapping(trueReg.register)

        case Expr.Sequence(stmts, result) =>
          for
            _ <- ZIO.foreach(stmts)(emitExpr)
            r <- emitExpr(result)
          yield r

        case Expr.StringLiteral(s) =>
          for
            reg <- makeRegister(RegisterType.Builtin(NullaryBuiltin.StringType, Seq()))
            _ <- append(Instruction.LoadString(reg, s))
          yield reg


        case Expr.Tuple(items) =>
          for
            itemRegs <- ZIO.foreach(items)(emitExpr)
            reg <- makeRegister(RegisterType.Tuple(itemRegs.map(_.t)))
            _ <- append(Instruction.CreateTuple(reg, itemRegs.map(_.register)))
          yield reg

        case Expr.Variable(v) =>
          for
            vars <- variables.get
          yield vars(v)

        case _ =>
          println(expr.getClass)
          ???
      }

    def emitExprTail(expr: Expr): Comp[Unit] =
      expr match {
        case Expr.FunctionCall(f, args) =>
          tailCall(emitFunctionCall(f, args))

        case Expr.FunctionObjectCall(f, a) =>
          tailCall(emitFunctionObjectCall(f, a).map(_._1))

        case Expr.IfElse(_, _, cond, whenTrue, whenFalse) =>
          for
            condReg <- emitExpr(cond)

            savedVars <- variables.get
            savedVarSeq = savedVars.keys.toSeq
            savedVarRegDecls = savedVarSeq.map(v => savedVars(v))
            savedVarRegs = savedVarRegDecls.map(r => r.register)

            whenTrueLabel <- UniqueIdentifier.make
            whenFalseLabel <- UniqueIdentifier.make

            _ <- generateBlock(BranchInstruction.JumpIf(condReg.register, whenTrueLabel, savedVarRegs, whenFalseLabel, savedVarRegs))

            _ <- startBlock(whenTrueLabel, savedVarRegDecls, savedVars)
            _ <- emitExprTail(whenTrue)

            _ <- startBlock(whenFalseLabel, savedVarRegDecls, savedVars)
            _ <- emitExprTail(whenFalse)
          yield ()

        case Expr.Sequence(stmts, result) =>
          for
            _ <- ZIO.foreach(stmts)(emitExpr)
            _ <- emitExprTail(result)
          yield ()

        case _ =>
          for
            reg <- emitExpr(expr)
            _ <- generateBlock(BranchInstruction.Return(reg.register))
          yield ()
      }

    def toCFG: Comp[ControlFlowGraph] =
      for
        blocks <- blocks.get
      yield ControlFlowGraph(startBlockId, blocks)

    private def makeRegister(t: RegisterType): Comp[RegisterDeclaration] =
      for
        regId <- UniqueIdentifier.make
      yield RegisterDeclaration(Register(regId), t)


    private def append(instruction: Instruction): Comp[Unit] =
      instructions.update(_ :+ instruction)

    private def generateBlock(branch: BranchInstruction): Comp[Unit] =
      for
        blockId <- currentBlockId.get
        params <- blockParams.get
        insns <- instructions.get
        _ <- blocks.update(_.updated(blockId, InstructionBlock(params, insns, branch)))
      yield ()

    private def startBlock(blockId: UniqueIdentifier, scopeVariableRegs: Seq[RegisterDeclaration], scopeVariables: Map[Var, RegisterDeclaration]): Comp[Map[Register, RegisterDeclaration]] =
      for
        _ <- currentBlockId.set(blockId)
        regMappingSeq <- ZIO.foreach(scopeVariableRegs) { r =>
          for
            id <- UniqueIdentifier.make
          yield r.register -> RegisterDeclaration(Register(id), r.t)
        }
        regMapping = regMappingSeq.toMap


        _ <- variables.set(scopeVariables.view.mapValues { r => regMapping(r.register) }.toMap)
        _ <- blockParams.set(regMappingSeq.map(_._2))
        _ <- currentBlockId.set(blockId)
        _ <- instructions.set(Seq.empty)
      yield regMapping

    private def tailCall(call: Comp[FunctionCall]): Comp[Unit] =
      for
        call <- call
        _ <- generateBlock(BranchInstruction.ReturnCall(call))
      yield ()

    private def emitFunctionCall(f: ArFuncC & HasContext[context.type], args: Seq[Expr]): Comp[FunctionCall.Function] =
      for
        argRegs <- ZIO.foreach(args)(emitExpr)
      yield FunctionCall.Function(createFunc(f), argRegs.map(_.register))

    private def emitFunctionObjectCall(f: Expr, a: Expr): Comp[(FunctionCall.FunctionObject, RegisterType)] =
      for
        fReg <- emitExpr(f)
        aReg <- emitExpr(a)

        resType = fReg.t match {
          case RegisterType.Function(_, output) => output
          case _ => ???
        }

      yield (FunctionCall.FunctionObject(fReg.register, aReg.register), resType)
  }


}

object VMContextBuilder {
  def apply(ctx: Context): VMContextBuilder { val context: ctx.type } =
    new VMContextBuilder {
      override val context: ctx.type = ctx
      override val vmContext: CompatibleVMContext =
        new VMContext {
          override type Env = context.Env
          override type Error = context.Error
          override val implementations: Implementations {
            type ExternFunctionImplementation = context.implementations.ExternFunctionImplementation
            type FunctionReference = context.implementations.FunctionReference
          } = new Implementations {
            override type ExternFunctionImplementation = context.implementations.ExternFunctionImplementation
            override type FunctionReference = context.implementations.FunctionReference
          }
        }
    }


}
