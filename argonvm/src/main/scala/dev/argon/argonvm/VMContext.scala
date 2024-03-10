package dev.argon.argonvm

import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.{ErasedSignature, ModulePath}
import dev.argon.expr.{BuiltinType, NullaryBuiltin}
import dev.argon.util.UniqueIdentifier
import zio.ZIO

trait VMContext {

  type Env
  type Error
  type Comp[+A] = ZIO[Env, Error, A]



  trait Implementations {
    type ExternFunctionImplementation
    type FunctionReference

    enum FunctionImplementation {
      case Instructions(e: ControlFlowGraph)
      case Extern(e: ExternFunctionImplementation)
    }
  }

  val implementations: Implementations

  enum VMType derives CanEqual {
    case Builtin(builtin: BuiltinType, args: Seq[VMType])
    case Function(input: VMType, output: VMType)
    case Tuple(elements: Seq[VMType])
  }

  trait VMTube {
    def modules: Map[ModulePath, VMModule]
  }

  trait VMModule {
    def exports: Comp[Seq[(Option[IdentifierExpr], ErasedSignature, ModuleExport)]]
  }

  sealed trait ModuleExport

  trait VMFunction extends ModuleExport {
    def parameters: Comp[Seq[VMType]]
    def returnType: Comp[VMType]

    def reference: Comp[implementations.FunctionReference]
    def implementation: Option[Comp[implementations.FunctionImplementation]]
  }

  final case class Register(id: UniqueIdentifier) derives CanEqual
  final case class RegisterDeclaration(register: Register, t: VMType)

  final case class ControlFlowGraph(
    startBlockId: UniqueIdentifier,
    blocks: Map[UniqueIdentifier, InstructionBlock],
  )

  final case class InstructionBlock(
    parameters: Seq[RegisterDeclaration],
    instructions: Seq[Instruction],
    branch: BranchInstruction,
  )
  
  enum InstructionResult derives CanEqual {
    case Value(register: Register)
    case Discard
  }

  enum Instruction {
    case Move(target: Register, source: Register)
    case Call(result: InstructionResult, call: FunctionCall)
    case CreateTuple(result: Register, items: Seq[Register])
    case LoadNullaryBuiltin(result: Register, builtin: NullaryBuiltin)
    case LoadString(result: Register, s: String)
    case TupleElement(result: Register, tuple: Register, index: Int)
  }
  
  enum FunctionCall {
    case Function(f: implementations.FunctionReference, args: Seq[Register])
    case FunctionObject(f: Register, arg: Register)
  }

  enum BranchInstruction {
    case Return(value: Register)
    case ReturnCall(call: FunctionCall)
    case Jump(blockId: UniqueIdentifier, args: Seq[Register])
    case JumpIf(
      condition: Register,
      takenId: UniqueIdentifier,
      takenArgs: Seq[Register],
      notTakenId: UniqueIdentifier,
      notTakenArgs: Seq[Register],
    )
  }

}
