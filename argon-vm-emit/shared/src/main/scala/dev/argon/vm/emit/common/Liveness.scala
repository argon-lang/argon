package dev.argon.vm.emit.common

import dev.argon.vm.*
import dev.argon.util.{*, given}
import scala.collection.immutable.Queue

type LivenessMap = Map[LabelId, Set[LocalId]]
final case class LivenessResult(in: LivenessMap, out: LivenessMap)

trait Liveness {

  type StorageLocation

  def variableLive(cfg: ControlFlowGraph): LivenessResult =
    val preds = getPreds(cfg)
    val (kill, gen) = getKillGen(cfg)

    def iter(queue: Queue[LabelId], in: LivenessMap, out: LivenessMap): LivenessResult =
      queue.dequeueOption match {
        case Some((label, queue)) =>
          val updatedInSet = (out.getOrElse(label, Set.empty) -- kill(label)) ++ gen(label)
          val in2 = in.updated(label, updatedInSet)

          val (out2, queue2) = preds(label).foldLeft((out, queue)) { case ((out, queue), pred) =>
            val outSet = out.getOrElse(pred, Set.empty)
            if updatedInSet.subsetOf(outSet) then
              (out.updated(pred, outSet ++ updatedInSet), queue.enqueue(pred))
            else
              (out, queue)
          }

          iter(queue2, in2, out2)

        case None =>
          LivenessResult(in, out)
      }
      
    iter(Queue.from(findExitNodes(cfg)), Map.empty, Map.empty)
  end variableLive

  def findExitNodes(cfg: ControlFlowGraph): Set[LabelId] =
    cfg.blocks.iterator.flatMap {
      case (label, InstructionBlock(_, JumpInstruction.TailCall(_, _, _) | JumpInstruction.Return(_) | JumpInstruction.Throw(_))) => Seq(label)
      case (label, InstructionBlock(_, _)) => Seq.empty
      case (label, CatchBlock(body, _, _)) => findExitNodes(body)
      case (label, WithLocalReference(_, _, block)) => findExitNodes(block)
    }.toSet


  def getPreds(cfg: ControlFlowGraph): Map[LabelId, Set[LabelId]] =
    def jumpTargets(jump: JumpInstruction): Set[LabelId] =
      jump match {
        case JumpInstruction.TailCall(_, _, _) | JumpInstruction.Return(_) | JumpInstruction.Throw(_) => Set.empty
        case JumpInstruction.Jump(label) => Set(label)
        case JumpInstruction.JumpZero(_, z, nz) => Set(z, nz)
      }

    def impl(cfg: ControlFlowGraph): Iterator[Map[LabelId, Set[LabelId]]] =
      cfg.blocks.iterator.flatMap {
        case (label, InstructionBlock(_, jump)) =>
          jumpTargets(jump).iterator.map { target => Map(target -> Set(label)) }

        case (label, CatchBlock(body, exceptionVariable, handler)) =>
          Seq(Map(
            body.start -> Set(label),
            handler -> Set(label),
          )).iterator ++ impl(body)

        case (label, WithLocalReference(_, _, block)) =>
          Seq(
            Map(block.start -> Set(label)),
          ).iterator ++ impl(block)
      }

    impl(cfg).foldMonoid
  end getPreds

  def getKillGen(cfg: ControlFlowGraph): (LivenessMap, LivenessMap) =
    cfg.blocks.iterator.map[(LivenessMap, LivenessMap)] {
      case (label, InstructionBlock(instructions, jump)) =>
        val (kill, gen) = instructions.foldRight((Set.empty[LocalId], jumpInstructionGen(jump).keySet)) { case (instruction, (kill, gen)) =>
          val ikill = instructionKill(instruction).keySet
          val igen = instructionGen(instruction).keySet

          (kill ++ ikill, (gen -- ikill) ++ igen)
        }

        (Map(label -> kill), Map(label -> gen))

      case (label, CatchBlock(body, exceptionVariable, _)) =>
        val (kill, gen) = getKillGen(body)
        (kill + (label -> Set(exceptionVariable)), gen)

      case (label, WithLocalReference(refHolder, referenced, block)) =>
        val (kill, gen) = getKillGen(block)
        (kill + (label -> Set(refHolder)), gen + (label -> Set(referenced)))
    }.foldMonoid
  end getKillGen

  def instructionKill(insn: Instruction): Map[LocalId, Set[StorageLocation]]
  def instructionGen(insn: Instruction): Map[LocalId, Set[StorageLocation]]

  def jumpInstructionGen(jump: JumpInstruction): Map[LocalId, Set[StorageLocation]]

}
