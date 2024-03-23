package dev.argon.plugins.lua

import dev.argon.compiler.*
import dev.argon.expr.*
import dev.argon.plugin.vm.*
import dev.argon.plugins.lua.ExprEmit.OutputMode
import dev.argon.util.{*, given}
import zio.*
import zio.stm.*
import cats.*
import cats.data.NonEmptySeq
import cats.implicits.given
import zio.interop.catz.core.given
import scala.collection.mutable

trait ExprEmit extends ModuleEmitBase {
  val regNames: TSet[String]

  private def getRegVar(r: Register): UIO[String] =
    r match {
      case Register.Reg(n) =>
        val name = s"r$n"
        regNames.put(name).as(name).commit
    }

  private def getBlockLabel(id: BigInt): String =
    s"l$id"

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
      emittedBlocks <- TSet.empty[BigInt].commit
      paramNames <- ZIO.foreach(cfg.blocks.head.parameters)(r => getRegVar(r.register))
      body <- {
        def getSuccBlocks(blockId: BigInt): Seq[BigInt] =
          cfg.blocks(blockId.toInt).branch match {
            case BranchInstruction.Return(_) | BranchInstruction.ReturnCall(_) => Seq()
            case BranchInstruction.Jump(target, _*) => Seq(target)
            case BranchInstruction.JumpIf(_, taken, _, notTaken, _) => Seq(taken, notTaken)
          }

        def impl(blockId: BigInt): Comp[Seq[AST.Stat]] =
          emittedBlocks.contains(blockId).commit.flatMap {
            case true => ZIO.succeed(Seq.empty)
            case false =>
              for
                stats <- emitBlock(cfg, blockId)
                _ <- emittedBlocks.put(blockId).commit
                succStats <- ZIO.foreach(getSuccBlocks(blockId))(impl)
              yield stats ++ succStats.flatten
          }

        impl(0)
      }
      varNames <- regNames.toSet.commit
      nonParamVars = (varNames -- paramNames).toSeq
    yield AST.FunctionDefinitionExp(
      params = paramNames,
      hasRest = false,
      body = AST.Block(
        (
          NonEmptySeq.fromSeq(nonParamVars)
            .map(npv => Seq(AST.LocalDeclaration(npv.map(name => AST.VariableBinding(name, AST.Attrib.Empty)), Seq())))
            .getOrElse(Seq())
        )
          ++ body
      )
    )

  private def emitBlock(cfg: ControlFlowGraph, id: BigInt): Comp[Seq[AST.Stat]] =
    val block = cfg.blocks(id.toInt)
    val label = getBlockLabel(id)
    for
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

      case Instruction.CreateTuple(res, items*) =>
        for
          v <- getRegVar(res.register)
          itemVars <- ZIO.foreach(items)(getRegVar)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(toArrayExp(itemVars.map(AST.NameExp.apply)))))

      case Instruction.LoadBool(res, value) =>
        for
          v <- getRegVar(res.register)
        yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(if value then AST.TrueLiteral else AST.FalseLiteral)))

      case Instruction.LoadBuiltin(res, builtin, args*) =>
        def nullaryRuntimeExport(name: String): Comp[Seq[AST.Stat]] =
          args match {
            case Seq() =>
              for
                v <- getRegVar(res.register)
              yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(AST.MemberAccessName(AST.NameExp("ArgonRuntime"), name))))

            case _ => ???
          }
          
        def unOp(op: AST.UnOp): Comp[Seq[AST.Stat]] =
          args match {
            case Seq(a) =>
              for
                av <- getRegVar(a)
                expr = AST.UnOpExp(op, AST.NameExp(av))

                v <- getRegVar(res.register)
              yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(expr)))

            case _ => ???
          }
        
        def binOp(op: AST.BinOp): Comp[Seq[AST.Stat]] =
          args match {
            case Seq(a, b) =>
              for
                av <- getRegVar(a)
                bv <- getRegVar(b)
                expr = AST.BinOpExp(op, AST.NameExp(av), AST.NameExp(bv))

                v <- getRegVar(res.register)
              yield Seq(AST.Assignment(Seq(AST.NameExp(v)), Seq(expr)))

            case _ => ???
          }
        
        builtin match {
          case Builtin.IntType => nullaryRuntimeExport("int_type")
          case Builtin.BoolType => nullaryRuntimeExport("bool_type")
          case Builtin.StringType => nullaryRuntimeExport("string_type")
          case Builtin.NeverType => nullaryRuntimeExport("never_type")
          
          
          
          case Builtin.IntNegate => unOp(AST.UnOp.Minus)
          case Builtin.IntBitNot => unOp(AST.UnOp.BNot)
          
          
          case Builtin.ConjunctionType => ???
          case Builtin.DisjunctionType => ???

          case Builtin.IntAdd => binOp(AST.BinOp.Add)
          case Builtin.IntSub => binOp(AST.BinOp.Sub)
          case Builtin.IntMul => binOp(AST.BinOp.Mul)
          case Builtin.IntBitAnd => binOp(AST.BinOp.BAnd)
          case Builtin.IntBitOr => binOp(AST.BinOp.BOr)
          case Builtin.IntBitXor => binOp(AST.BinOp.BXOr)
          case Builtin.IntBitShiftLeft => binOp(AST.BinOp.ShiftLeft)
          case Builtin.IntBitShiftRight => binOp(AST.BinOp.ShiftRight)
          case Builtin.IntEq | Builtin.StringEq => binOp(AST.BinOp.EQ)
          case Builtin.IntNe | Builtin.StringNe => binOp(AST.BinOp.NE)
          case Builtin.IntLt => binOp(AST.BinOp.LT)
          case Builtin.IntLe => binOp(AST.BinOp.LE)
          case Builtin.IntGt => binOp(AST.BinOp.GT)
          case Builtin.IntGe => binOp(AST.BinOp.GE)

          case Builtin.StringConcat => binOp(AST.BinOp.Concat)

          case Builtin.EqualTo => ???
        }

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
          AST.IntegerLiteral(index.toLong)
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

      case BranchInstruction.Jump(targetId, args*) =>
        val targetLabel = getBlockLabel(targetId)
        for
          paramVars <- ZIO.foreach(cfg.blocks(targetId.toInt).parameters.map(_.register))(getRegVar)
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
          takenJump <- emitBranch(cfg)(BranchInstruction.Jump(takenId, takenArgs*))
          notTakenJump <- emitBranch(cfg)(BranchInstruction.Jump(notTakenId, notTakenArgs*))
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
      case FunctionCall.Function(f, args*) =>
        for
          argVars <- ZIO.foreach(args)(getRegVar)
          fRef <- currentTube.getFunctionReference(f)
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
