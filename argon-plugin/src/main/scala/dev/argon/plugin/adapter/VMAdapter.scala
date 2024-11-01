package dev.argon.plugin.adapter

import dev.argon.plugin.*
import dev.argon.compiler.*
import dev.argon.tube
import dev.argon.plugin.scalaApi
import dev.argon.plugin.scalaApi.vm
import zio.*
import dev.argon.util.async.ErrorWrapper
import dev.argon.util.*
import dev.argon.expr.{BuiltinType, NullaryBuiltin, UnaryBuiltin, BinaryBuiltin}

sealed abstract class VMAdapter[Externs, E >: PluginError, EX <: Throwable] private(using ErrorWrapper.Aux[E, EX]) {
  val plugin: AdaptedPlatformPlugin[Externs, E, EX, ?]
  val context: plugin.ContextIncluding
  protected val tubeMemo: MemoCacheStore[context.Env, context.Error, ArTubeC & HasContext[context.type], vm.TubeDefinition[Externs, EX]]

  import context.Comp
  import context.DefaultSignatureContext.FunctionSignature
  import context.DefaultExprContext.{Expr, Builtin, Var, LocalVar}



  def getTube(tube: ArTubeC & HasContext[context.type]): Comp[vm.TubeDefinition[Externs, EX]] =
    tubeMemo.usingCreate(tube)(createTube)


  private def createTube(tube: ArTubeC & HasContext[context.type]): Comp[vm.TubeDefinition[Externs, EX]] =
    for
      mods <- ZIO.foreach(tube.modules.values.toSeq)(createModule)
    yield vm.TubeDefinition(
      name = FormatAdapters.createTubeName(tube.name),
      modules = mods,
    )

  private def createModule(module: ArModuleC & HasContext[context.type]): Comp[vm.ModuleDefinition[Externs, EX]] =
    for
      env <- ZIO.environment[context.Env]

      exps <- 
        ErrorWrapper.wrapEffect(
          module.allExports(Set.empty).flatMap { exps =>
            ZIO.foreach(exps.toSeq) { (name, exps) =>
              ZIO.foreach(exps) {
                case ModuleExportC.Function(f) => createFunction(f).asSome
                case ModuleExportC.Record(r) => createRecord(r).asSome
                case ModuleExportC.Exported(_) => ZIO.none
              }.map { exps =>
                vm.ModuleExportEntry(
                  name = name.map(FormatAdapters.createName),
                  exports = exps.flatten,
                )
              }
            }
          }
        )
          .provideEnvironment(env)
          .memoize

    yield new vm.ModuleDefinition[Externs, EX] {
      override def path(): UIO[tube.ModulePath] =
        ZIO.succeed(FormatAdapters.createModulePath(module.path))

      override def exports(): IO[EX, Seq[vm.ModuleExportEntry[Externs, EX]]] = exps
    }


  private def createFunction(f: ArFuncC & HasContext[context.type]): Comp[vm.ModuleExportOverload[Externs, EX]] =
    for
      env <- ZIO.environment[context.Env]

      sig <- f.signature
      erasedSig <- SignatureEraser(context).eraseSignature(sig)
      sig <- 
        ErrorWrapper.wrapEffect(
          createFunctionSignature(sig)
        )
        .provideEnvironment(env)
        .memoize


      impl <-
        ErrorWrapper.wrapEffect(
          ZIO.fromEither(f.implementation.toRight(???))
            .flatten
            .flatMap {
              case context.implementations.FunctionImplementation.Expr(e) =>
                for
                  sig <- f.signature
                  block <- createVmIrFunctionImplementation(context.DefaultExprContext.ParameterOwner.Func(f), sig, e)
                yield vm.FunctionImplementation.VmIr(block)

              case context.implementations.FunctionImplementation.Extern(e) =>
                ZIO.succeed(vm.FunctionImplementation.Extern[Externs](e.get[plugin.FunctionImplementationWrapper].inner))
            }
        )
        .provideEnvironment(env)
        .memoize

    yield vm.ModuleExportOverload(
      sig = FormatAdapters.createErasedSig(erasedSig),
      `export` = vm.ModuleExport.Function(new vm.FunctionDefinition[Externs, EX] {
        override def metadata(): UIO[vm.FunctionMetadata] =
          ZIO.succeed(vm.FunctionMetadata(
            isInline = f.isInline,
            isErased = f.isErased,
          ))

        override def signature(): IO[EX, vm.FunctionSignature[Externs]] = sig

        override def reference(): IO[EX, scalaApi.ExternFunctionRef[Externs]] =
          ErrorWrapper.wrapEffect(f.reference)
            .provideEnvironment(env)
            .map(ref => ref.get[plugin.FunctionReferenceWrapper].inner)

        override def implementation(): IO[EX, vm.FunctionImplementation[Externs]] = impl
      })
    )

  private def createRecord(r: ArRecordC & HasContext[context.type]): Comp[vm.ModuleExportOverload[Externs, EX]] =
    for
      env <- ZIO.environment[context.Env]

      sig <- r.signature
      erasedSig <- SignatureEraser(context).eraseSignature(sig)
      
      recFields <- 
        ErrorWrapper.wrapEffect(
          r.fields.flatMap { fields =>
            ZIO.foreach(fields) { field =>
              for
                t <- createVmType(field.fieldType)
              yield vm.RecordField(
                name = FormatAdapters.createName(field.name),
                fieldType = t,
              )
            }  
          }
        )
        .provideEnvironment(env)
        .memoize

    yield vm.ModuleExportOverload(
      sig = FormatAdapters.createErasedSig(erasedSig),
      `export` = vm.ModuleExport.Record(new vm.RecordDefinition[Externs, EX] {

        override def metadata(): UIO[vm.RecordMetadata] =
          ZIO.succeed(vm.RecordMetadata())

        override def signature(): IO[EX, vm.TypeSignature] =
          ZIO.succeed(vm.TypeSignature())

        override def fields(): IO[EX, Seq[vm.RecordField[Externs]]] = recFields

        override def reference(): IO[EX, scalaApi.ExternRecordRef[Externs]] =
          ErrorWrapper.wrapEffect(r.reference)
            .provideEnvironment(env)
            .map(ref => ref.get[plugin.RecordReferenceWrapper].inner)
      }),
    )

  private def getConcreteArgs(sig: FunctionSignature)(args: Seq[Expr]): Seq[Expr] =
    sig.parameters
      .view
      .zip(args)
      .filterNot { (param, _) => param.isErased }
      .map { (_, arg) => arg }
      .toSeq


  private def createVmType(t: Expr): Comp[vm.VmType[Externs]] =
    t match {
      case Expr.Builtin(Builtin.Nullary(builtin: BuiltinType)) =>
        val b = FormatAdapters.createBuiltinType(builtin)
        ZIO.succeed(vm.VmType.Builtin(b, Seq()))

      case Expr.Builtin(Builtin.Binary(builtin: BuiltinType, x, y)) =>
        val b = FormatAdapters.createBuiltinType(builtin)
        for
          x <- createVmType(x)
          y <- createVmType(y)
        yield vm.VmType.Builtin(b, Seq(x, y))

      case Expr.FunctionType(a, r) =>
        for
          a <- createVmType(a)
          r <- createVmType(r)
        yield vm.VmType.Function(a, r)

      case Expr.RecordType(r, args) =>
        for
          sig <- r.signature
          ref <- r.reference
          args <- ZIO.foreach(getConcreteArgs(sig)(args))(createVmType)
        yield vm.VmType.Record(ref.get[plugin.RecordReferenceWrapper].inner, args)

      case Expr.Tuple(elements) =>
        for
          elements <- ZIO.foreach(elements)(createVmType)
        yield vm.VmType.Tuple(elements)

      case _ => ???
    }

  private def createFunctionSignature(sig: FunctionSignature): Comp[vm.FunctionSignature[Externs]] =
    for
      params <- ZIO.foreach(sig.parameters.filterNot(_.isErased)) { p =>
        for
          paramType <- createVmType(p.paramType)
        yield vm.FunctionParameter(paramType)
      }

      res <- createVmType(sig.returnType)

    yield vm.FunctionSignature(
      parameters = params,
      result = res,
    )

  private def createVmIrFunctionImplementation(
    owner: context.DefaultExprContext.ParameterOwner,
    sig: FunctionSignature,
    body: Expr,
  ): Comp[vm.Block[Externs]] =
    val paramVarMap = sig.parameters
      .view
      .zipWithIndex
      .map { (param, i) => param.asParameterVar(owner, i) : Var }
      .filterNot(_.isErased)
      .zipWithIndex
      .map { (param, i) => (param, vm.RegisterId(i)) }
      .toMap

    val emitState = EmitState(
      variables = paramVarMap,
      nextVarIndex = 0,
      currentScopeVars = Seq(),
      instructions = Seq(),
    )

    createBlock(emitState, ResultOutput.Return)(body)
      .map { (block, _) => block }
  end createVmIrFunctionImplementation


  private final case class EmitState(
    variables: Map[Var, vm.RegisterId],
    nextVarIndex: Int,
    currentScopeVars: Seq[vm.VariableDefinition[Externs]],
    instructions: Seq[vm.Instruction[Externs]],
  )

  sealed trait ResultOutput derives CanEqual {
    type Info
  }

  object ResultOutput {
    case object Return extends ResultOutput {
      override type Info = Unit
    }

    final case class Variable(id: vm.RegisterId) extends ResultOutput {
      override type Info = Unit
    }
    
    case object NewVariable extends ResultOutput {
      override type Info = vm.RegisterId
    }

    case object Discard extends ResultOutput {
      override type Info = Unit
    }
  }

  private def createBlock(emitState: EmitState, resultOutput: ResultOutput)(expr: Expr): Comp[(vm.Block[Externs], resultOutput.Info)] =
    for
      emitStateRef <- Ref.make(emitState.copy(
        currentScopeVars = Seq(),
        instructions = Seq(),
      ))

      (_, info) <- emitExpr(emitStateRef, resultOutput)(expr)

      emitState <- emitStateRef.get

      block = vm.Block(
        variables = emitState.currentScopeVars,
        instructions = emitState.instructions,
      )

    yield (block, info)
  end createBlock

  private def emitExpr(emitState: Ref[EmitState], resultOutput: ResultOutput)(expr: Expr): Comp[(vm.VmType[Externs], resultOutput.Info)] =

    def emit(insn: vm.Instruction[Externs]): Comp[Unit] = ???

    def getVariable(v: Var): Comp[vm.RegisterId] = ???

    def defineVariable(variable: LocalVar): Comp[vm.RegisterId] = ???
    def writeResult(varType: vm.VmType[Externs])(f: vm.RegisterId => Comp[Unit]): Comp[(vm.VmType[Externs], resultOutput.Info)] = ???
    def writeResultPure(varType: vm.VmType[Externs])(f: vm.RegisterId => Comp[Unit]): Comp[(vm.VmType[Externs], resultOutput.Info)] = ???

    def unitResult: Comp[(vm.VmType[Externs], resultOutput.Info)] =
      writeResultPure(vm.VmType.Tuple(Seq()))(index => emit(vm.Instruction.Tuple(index, Seq())))

    def intType: vm.VmType[Externs] = vm.VmType.Builtin(tube.BuiltinType.Int(), Seq())
    def stringType: vm.VmType[Externs] = vm.VmType.Builtin(tube.BuiltinType.String(), Seq())

    expr match {
      case Expr.BindVariable(v, value) =>
        for
          id <- defineVariable(v)
          _ <- emitExpr(emitState, ResultOutput.Variable(id))(value)
          info <- unitResult
        yield info

      case Expr.BoolLiteral(b) =>
        writeResultPure(vm.VmType.Builtin(tube.BuiltinType.Bool(), Seq())) { id =>
          emit(vm.Instruction.ConstBool(id, b))
        }

      case Expr.Builtin(Builtin.Unary(builtin, a)) =>
        val (t, builtin2) = builtin match {
          case UnaryBuiltin.IntNegate => (intType, vm.BuiltinUnaryOp.IntNegate())
          case UnaryBuiltin.IntBitNot => (intType, vm.BuiltinUnaryOp.IntBitNot())
        }

        for
          (_, a) <- emitExpr(emitState, ResultOutput.NewVariable)(a)
          info <- writeResult(t) { id => emit(vm.Instruction.BuiltinUnary(id, builtin2, a)) }
        yield info

      case Expr.Builtin(Builtin.Binary(builtin, a, b)) =>
        val (t, builtin2) = builtin match {
          case BinaryBuiltin.ConjunctionType => ???
          case BinaryBuiltin.DisjunctionType => ???

          case BinaryBuiltin.IntAdd => (intType, vm.BuiltinBinaryOp.IntAdd())
          case BinaryBuiltin.IntSub => (intType, vm.BuiltinBinaryOp.IntSub())
          case BinaryBuiltin.IntMul => (intType, vm.BuiltinBinaryOp.IntMul())
          case BinaryBuiltin.IntBitAnd => (intType, vm.BuiltinBinaryOp.IntBitAnd())
          case BinaryBuiltin.IntBitOr => (intType, vm.BuiltinBinaryOp.IntBitOr())
          case BinaryBuiltin.IntBitXOr => (intType, vm.BuiltinBinaryOp.IntBitXor())
          case BinaryBuiltin.IntBitShiftLeft => (intType, vm.BuiltinBinaryOp.IntBitShiftLeft())
          case BinaryBuiltin.IntBitShiftRight => (intType, vm.BuiltinBinaryOp.IntBitShiftRight())
          case BinaryBuiltin.IntEQ => (intType, vm.BuiltinBinaryOp.IntEq())
          case BinaryBuiltin.IntNE => (intType, vm.BuiltinBinaryOp.IntNe())
          case BinaryBuiltin.IntLT => (intType, vm.BuiltinBinaryOp.IntLt())
          case BinaryBuiltin.IntLE => (intType, vm.BuiltinBinaryOp.IntLe())
          case BinaryBuiltin.IntGT => (intType, vm.BuiltinBinaryOp.IntGt())
          case BinaryBuiltin.IntGE => (intType, vm.BuiltinBinaryOp.IntGe())
          case BinaryBuiltin.StringConcat => (stringType, vm.BuiltinBinaryOp.StringConcat())
          case BinaryBuiltin.StringEQ => (stringType, vm.BuiltinBinaryOp.StringEq())
          case BinaryBuiltin.StringNE => (stringType, vm.BuiltinBinaryOp.StringNe())
        }

        for
          (_, a) <- emitExpr(emitState, ResultOutput.NewVariable)(a)
          (_, b) <- emitExpr(emitState, ResultOutput.NewVariable)(b)
          info <- writeResult(t) { id => emit(vm.Instruction.BuiltinBinary(id, builtin2, a, b)) }
        yield info

      case Expr.FunctionCall(f, args) =>
        for
          sig <- f.signature
          argRegs <- ZIO.foreach(getConcreteArgs(sig)(args))(arg => emitExpr(emitState, ResultOutput.NewVariable)(arg).map { case (_, argReg) => argReg })
          returnType <- createVmType(sig.returnType) // TODO: substitution
          ref <- f.reference
          info <- writeResult(returnType) { id => emit(vm.Instruction.FunctionCall(id, ref.get[plugin.FunctionReferenceWrapper].inner, argRegs)) }
        yield info

      case Expr.IntLiteral(i) =>
        writeResultPure(vm.VmType.Builtin(tube.BuiltinType.Bool(), Seq())) { id =>
          emit(vm.Instruction.ConstInt(id, i))
        }

      case Expr.Sequence(stmts, result) =>
        ZIO.foreachDiscard(stmts)(emitExpr(emitState, ResultOutput.Discard)) *>
          emitExpr(emitState, resultOutput)(result)

      case Expr.StringLiteral(s) =>
        writeResultPure(vm.VmType.Builtin(tube.BuiltinType.Bool(), Seq())) { id =>
          emit(vm.Instruction.ConstString(id, s))
        }

      case Expr.StoreVariable(v, value) =>
        for
          id <- getVariable(v)
          _ <- emitExpr(emitState, ResultOutput.Variable(id))(value)
          info <- unitResult
        yield info

      case Expr.Tuple(elements) =>
        for
          elems <- ZIO.foreach(elements)(emitExpr(emitState, ResultOutput.NewVariable))

          elemTypes = elems.map(_._1)
          elemRegs = elems.map(_._2)

          info <- writeResultPure(vm.VmType.Tuple(elemTypes)) { id => emit(vm.Instruction.Tuple(id, elemRegs)) }
        yield info

      case Expr.TupleElement(index, tuple) =>
        for
          (tupleType, tupleReg) <- emitExpr(emitState, ResultOutput.NewVariable)(tuple)

          elemType <- tupleType match {
            case vm.VmType.Tuple(elems) =>
              ZIO.succeed(elems.lift(index.toInt).getOrElse { ??? })
              
            case _ => ???
          }

          info <- writeResultPure(elemType) { id => emit(vm.Instruction.TupleElement(id, index, tupleReg)) }
        yield info

      case _ => ???
    }
  end emitExpr

}

object VMAdapter {
  type Aux[TPlugin, Ctx <: Context, Externs, E >: PluginError, EX <: Throwable] = VMAdapter[Externs, E, EX] {
    val plugin: TPlugin
    val context: Ctx
  }
  
  def apply[Externs, E >: PluginError, EX <: Throwable]
  (plugin: AdaptedPlatformPlugin[Externs, E, EX, ?])
  (context: plugin.ContextIncluding)
  (using ErrorWrapper.Aux[E, EX])
  : context.Comp[Aux[plugin.type, context.type, Externs, E, EX]] =
    val p2: plugin.type = plugin
    val ctx: context.type = context
    for
      getTubeMemo <- MemoCacheStore.make[context.Env, context.Error, ArTubeC & HasContext[context.type], vm.TubeDefinition[Externs, EX]]
    yield new VMAdapter[Externs, E, EX] {
      override val plugin: p2.type = p2
      val context: ctx.type = ctx
      override protected val tubeMemo: MemoCacheStore[context.Env, context.Error, ArTubeC & HasContext[context.type], vm.TubeDefinition[Externs, EX]] =
        getTubeMemo
    }
  end apply
}
