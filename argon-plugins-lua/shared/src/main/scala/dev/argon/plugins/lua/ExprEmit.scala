package dev.argon.plugins.lua

import dev.argon.compiler.*
import dev.argon.expr.*
import dev.argon.plugins.lua.ExprEmit.OutputMode
import dev.argon.util.{*, given}
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given
import scala.collection.mutable

trait ExprEmit extends ModuleEmitBase {
  val regMapping: TMap[Register, String]
  val blockMapping: TMap[UniqueIdentifier, String]


  private def getRegVar(r: Register): UIO[String] =
    regMapping.get(r).flatMap {
      case Some(v) => STM.succeed(v)
      case None =>
        regMapping.size.flatMap { i =>
          val name = s"r$i"
          regMapping.put(r, name).as(name)
        }
    }.commit

  private def getBlockLabel(id: UniqueIdentifier): UIO[String] =
    blockMapping.get(id).flatMap {
      case Some(v) => STM.succeed(v)
      case None =>
        blockMapping.size.flatMap { i =>
          val name = s"l$i"
          blockMapping.put(id, name).as(name)
        }
    }.commit

  private def getReferenceExp(reference: LuaReference): AST.PrefixExp =
    reference match {
      case LuaReference.Global(importTube, module, name, signature) =>
        AST.MemberAccessIndex(
          AST.MemberAccessIndex(
            AST.MemberAccessIndex(
              AST.SimpleFunctionCall(
                AST.NameExp("require"),
                Seq(
                  AST.StringLiteral(importTube),
                ),
              ),
              AST.StringLiteral(module),
            ),
            name
          ),
          signature
        )
    }

  def emit(cfg: ControlFlowGraph): Comp[AST.FunctionDefinitionExp] =
    for
      emittedBlocks <- TSet.empty[UniqueIdentifier].commit
      paramNames <- ZIO.foreach(cfg.blocks(cfg.startBlockId).parameters)(r => getRegVar(r.register))
      body <- {
        def getSuccBlocks(blockId: UniqueIdentifier): Seq[UniqueIdentifier] =
          cfg.blocks(blockId).branch match {
            case BranchInstruction.Return(_) | BranchInstruction.ReturnCall(_) => Seq()
            case BranchInstruction.Jump(target, _) => Seq(target)
            case BranchInstruction.JumpIf(_, taken, _, notTaken, _) => Seq(taken, notTaken)
          }

        def impl(blockId: UniqueIdentifier): Comp[Seq[AST.Stat]] =
          emittedBlocks.contains(blockId).commit.flatMap {
            case true => ZIO.succeed(Seq.empty)
            case false =>
              for
                stats <- emitBlock(cfg, blockId)
                _ <- emittedBlocks.put(blockId).commit
                succStats <- ZIO.foreach(getSuccBlocks(blockId))(impl)
              yield stats ++ succStats.flatten
          }

        impl(cfg.startBlockId)
      }
      varNames <- regMapping.values.commit
    yield AST.FunctionDefinitionExp(
      params = paramNames,
      hasRest = false,
      body = AST.Block(
        (AST.LocalDeclaration(varNames.map(name => AST.VariableBinding(name, AST.Attrib.Empty)), Seq())) +:
          body
      )
    )

  private def emitBlock(cfg: ControlFlowGraph, id: UniqueIdentifier): Comp[Seq[AST.Stat]] =
    val block = cfg.blocks(id)
    for
      label <- getBlockLabel(id)
      insnStats <- ZIO.foreach(block.instructions)(emitInsn)
      branchStats <- emitBranch(cfg)(block.branch)
    yield Seq(
      AST.Label(label),
      AST.Do(AST.Block(insnStats.flatten ++ branchStats)),
    )
  end emitBlock

  private def emitInsn(insn: Instruction): Comp[Seq[AST.Stat]] =
    insn match {
      case Instruction.Call(InstructionResult.Value(res), call) =>
        for
          v <- getRegVar(res.register)
          callExp <- emitCall(call)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(callExp)))

      case Instruction.Call(InstructionResult.Discard, call) =>
        for
          callExp <- emitCall(call)
        yield Seq(callExp)

      case Instruction.CreateTuple(res, items) =>
        for
          v <- getRegVar(res.register)
          itemVars <- ZIO.foreach(items)(getRegVar)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(toArrayExp(itemVars.map(AST.NameExp.apply)))))

      case Instruction.LoadBool(res, value) =>
        for
          v <- getRegVar(res.register)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(if value then AST.TrueLiteral else AST.FalseLiteral)))

      case Instruction.LoadNullaryBuiltin(res, builtin) =>
        val builtinName = builtin match {
          case NullaryBuiltin.IntType => "int_type"
          case NullaryBuiltin.BoolType => "bool_type"
          case NullaryBuiltin.StringType => "string_type"
          case NullaryBuiltin.NeverType => "never_type"
        }

        for
          v <- getRegVar(res.register)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(AST.MemberAccessName(AST.NameExp("ArgonRuntime"), builtinName))))

      case Instruction.LoadUnaryBuiltin(res, builtin, a) =>
        for
          av <- getRegVar(a)
          expr = builtin match {
            case UnaryBuiltin.IntNegate => AST.UnOpExp(AST.UnOp.Minus, AST.NameExp(av))
            case UnaryBuiltin.IntBitNot => AST.UnOpExp(AST.UnOp.BNot, AST.NameExp(av))
          }

          v <- getRegVar(res.register)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(expr)))

      case Instruction.LoadBinaryBuiltin(res, builtin, a, b) =>
        for
          av <- getRegVar(a)
          bv <- getRegVar(b)
          expr = builtin match {
            case BinaryBuiltin.ConjunctionType => ???
            case BinaryBuiltin.DisjunctionType => ???

            case BinaryBuiltin.IntAdd => AST.BinOpExp(AST.BinOp.Add, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntSub => AST.BinOpExp(AST.BinOp.Sub, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntMul => AST.BinOpExp(AST.BinOp.Mul, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntBitAnd => AST.BinOpExp(AST.BinOp.BAnd, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntBitOr => AST.BinOpExp(AST.BinOp.BOr, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntBitXOr => AST.BinOpExp(AST.BinOp.BXOr, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntBitShiftLeft => AST.BinOpExp(AST.BinOp.ShiftLeft, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntBitShiftRight => AST.BinOpExp(AST.BinOp.ShiftRight, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntEQ | BinaryBuiltin.StringEQ => AST.BinOpExp(AST.BinOp.EQ, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntNE | BinaryBuiltin.StringNE => AST.BinOpExp(AST.BinOp.NE, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntLT => AST.BinOpExp(AST.BinOp.LT, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntLE => AST.BinOpExp(AST.BinOp.LE, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntGT => AST.BinOpExp(AST.BinOp.GT, AST.NameExp(av), AST.NameExp(bv))
            case BinaryBuiltin.IntGE => AST.BinOpExp(AST.BinOp.GE, AST.NameExp(av), AST.NameExp(bv))

            case BinaryBuiltin.StringConcat => AST.BinOpExp(AST.BinOp.Concat, AST.NameExp(av), AST.NameExp(bv))
          }

          v <- getRegVar(res.register)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(expr)))


      case Instruction.LoadString(res, s) =>
        for
          v <- getRegVar(res.register)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(AST.StringLiteral(s))))
        

      case Instruction.TupleElement(res, tuple, index) =>
        for
          v <- getRegVar(res.register)
          t <- getRegVar(tuple)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(AST.MemberAccessIndex(
          AST.NameExp(t),
          AST.IntegerLiteral(index)
        ))))
    }

  private def emitBranch(cfg: ControlFlowGraph)(branch: BranchInstruction): Comp[Seq[AST.Stat]] =
    branch match {
      case BranchInstruction.Return(r) =>
        for
          varName <- getRegVar(r)
        yield Seq(AST.Return(Seq(AST.NameExp(varName))))

      case BranchInstruction.ReturnCall(call) =>
        for
          callExp <- emitCall(call)
        yield Seq(AST.Return(Seq(callExp)))

      case BranchInstruction.Jump(targetId, args) =>
        for
          targetLabel <- getBlockLabel(targetId)
          paramVars <- ZIO.foreach(cfg.blocks(targetId).parameters.map(_.register))(getRegVar)
          argVars <- ZIO.foreach(args)(getRegVar)
        yield Seq(
          AST.Assignment(
            paramVars.map(AST.NameExp.apply),
            argVars.map(AST.NameExp.apply)
          ),
          AST.Goto(targetLabel),
        )

      case BranchInstruction.JumpIf(condition, takenId, takenArgs, notTakenId, notTakenArgs) =>
        for
          condVar <- getRegVar(condition)
          takenJump <- emitBranch(cfg)(BranchInstruction.Jump(takenId, takenArgs))
          notTakenJump <- emitBranch(cfg)(BranchInstruction.Jump(notTakenId, notTakenArgs))
        yield Seq(
          AST.If(
            AST.NameExp(condVar),
            AST.Block(takenJump),
            Seq(),
            Some(AST.Block(notTakenJump)),
          ),
        )

    }

  private def emitCall(call: FunctionCall): Comp[AST.FunctionCall] =
    call match {
      case FunctionCall.Function(f, args) =>
        for
          argVars <- ZIO.foreach(args)(getRegVar)
          fRef <- f.reference
        yield AST.SimpleFunctionCall(
          getReferenceExp(fRef.get[LuaReference]),
          argVars.map(AST.NameExp.apply),
        )

      case FunctionCall.FunctionObject(f, a) =>
        for
          fVar <- getRegVar(f)
          aVar <- getRegVar(a)
        yield AST.SimpleFunctionCall(
          AST.NameExp(fVar),
          Seq(AST.NameExp(aVar))
        )
    }


}

object ExprEmit {
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
