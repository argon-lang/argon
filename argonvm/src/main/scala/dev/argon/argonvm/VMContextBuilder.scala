package dev.argon.argonvm

import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.expr.BuiltinType
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
          exportMap <- module.allExports
          res <- ZIO.foreach(exportMap.toSeq) { (name, exports) =>
            ZIO.foreach(exports) {
              case ModuleExportC.Function(f) =>
                for
                  sig <- f.signature
                  sig <- eraser.eraseSignature(sig)
                yield (name, sig, createFunc(f))
            }
          }
        yield res.flatten
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

  def exprToCFG(expr: Expr, params: Seq[ParameterVar]): Comp[ControlFlowGraph] =
    for
      startBlockId <- UniqueIdentifier.make
      blocks <- Ref.make(Map.empty[UniqueIdentifier, InstructionBlock])
      variables <- Ref.make(Map.empty[Var, Register])
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
    variables: Ref[Map[Var, Register]],
    blockParams: Ref[Seq[RegisterDeclaration]],
    currentBlockId: Ref[UniqueIdentifier],
    instructions: Ref[Seq[Instruction]],
  ) {
    def initParams(params: Seq[ParameterVar]): Comp[Unit] =
      ZIO.foreachDiscard(params) { param =>
        for
          reg <- makeRegister

          _ <- param.tupleIndex match {
            case Some(tupleIndex) =>
              for
                paramRegs <- blockParams.get
                paramReg = paramRegs.last
                _ <- append(Instruction.TupleElement(reg, paramReg.register, tupleIndex))
              yield ()

            case None =>
              for
                t <- exprToType(param.varType)
                _ <- blockParams.update(_ :+ RegisterDeclaration(reg, t))
              yield ()
          }

          _ <- variables.update(_.updated(param, reg))
        yield ()
      }

    def emitExpr(expr: Expr): Comp[Register] =
      expr match {
        case Expr.Builtin(Builtin.Nullary(builtin)) =>
          for
            reg <- makeRegister
            _ <- append(Instruction.LoadNullaryBuiltin(reg, builtin))
          yield reg


        case Expr.FunctionCall(f, args) =>
          for
            call <- emitFunctionCall(f, args)
            reg <- makeRegister
            _ <- append(Instruction.Call(InstructionResult.Value(reg), call))
          yield reg

        case Expr.Tuple(items) =>
          for
            itemRegs <- ZIO.foreach(items)(emitExpr)
            reg <- makeRegister
            _ <- append(Instruction.CreateTuple(reg, itemRegs))
          yield reg

        case _ =>
          println(expr.getClass)
          ???
      }

    def emitExprTail(expr: Expr): Comp[Unit] =
      expr match {
        case Expr.FunctionCall(f, args) =>
          tailCall(emitFunctionCall(f, args))

        case Expr.FunctionObjectCall(f, a) =>
          tailCall(emitFunctionObjectCall(f, a))

        case _ =>
          for
            reg <- emitExpr(expr)
            blockId <- currentBlockId.get
            params <- blockParams.get
            insns <- instructions.get
            _ <- blocks.update(_.updated(blockId, InstructionBlock(params, insns, BranchInstruction.Return(reg))))
          yield ()
      }

    def toCFG: Comp[ControlFlowGraph] =
      for
        blocks <- blocks.get
      yield ControlFlowGraph(startBlockId, blocks)

    private def makeRegister: Comp[Register] =
      for
        regId <- UniqueIdentifier.make
      yield Register(regId)


    private def append(instruction: Instruction): Comp[Unit] =
      instructions.update(_ :+ instruction)

    private def tailCall(call: Comp[FunctionCall]): Comp[Unit] =
      for
        call <- call
        blockId <- currentBlockId.get
        params <- blockParams.get
        insns <- instructions.get
        _ <- blocks.update(_.updated(blockId, InstructionBlock(params, insns, BranchInstruction.ReturnCall(call))))
      yield ()

    private def emitFunctionCall(f: ArFuncC & HasContext[context.type], args: Seq[Expr]): Comp[FunctionCall] =
      for
        fRef <- f.reference
        argRegs <- ZIO.foreach(args)(emitExpr)
      yield FunctionCall.Function(fRef, argRegs)

    private def emitFunctionObjectCall(f: Expr, a: Expr): Comp[FunctionCall] =
      for
        fReg <- emitExpr(f)
        aReg <- emitExpr(a)
      yield FunctionCall.FunctionObject(fReg, aReg)
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
