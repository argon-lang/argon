package dev.argon.vm.interpreter

import dev.argon.vm.*
import zio.*
import InterpreterVM.*
import dev.argon.util.{*, given}

object Interpreter {

  private type BlockError = GCObject | Throwable
  private type BlockResult = VMValue | JumpInstruction.TailCall | LabelId

  def executeFunctionVirtual(program: VMProgram)(function: VMFunction)(args: Seq[VMValue]): IO[GCObject | Throwable, VMValue] =
    executeFunction(program)(function)(args)

  def executeFunction(program: VMProgram)(function: VMFunction)(args: Seq[VMValue]): IO[GCObject | Throwable, VMValue] =
    function.body match {
      case Some(body: FunctionBody) =>
        createLocals(body).flatMap { locals =>
          val interpreter = new InterpreterImpl(program, locals)

          interpreter.initParameters(body, args).flatMap { _ =>
            interpreter.runBlock(body.block)
              .flatMap {
                case _: LabelId => IO.fail(new UnknownLabelException())
                case JumpInstruction.TailCall(nextFuncId, argLocals, virtual) =>
                  interpreter.callFunction(nextFuncId, argLocals, virtual)

                case value: VMValue =>
                  IO.succeed(value)
              }
          }
        }

      case Some(func: ExternFunctionBody) => func(args)

      case None => IO.fail(new MissingFunctionBodyException())
    }

  private def createLocals(function: FunctionBody): UIO[Map[LocalId, VMCell]] =
    ZIO.foreach(function.locals) { case (id, t) =>
      createCell(t).map((id, _))
    }

  private def createCell(t: VMType): UIO[VMCell] =
    IO.succeed {
      t match {
        case VMType.I8 => new VMCell.ByteCell(0)
        case VMType.I16 => new VMCell.ShortCell(0)
        case VMType.I32 => new VMCell.IntCell(0)
        case VMType.I64 => new VMCell.LongCell(0)
        case VMType.F32 => new VMCell.FloatCell(0.0f)
        case VMType.F64 => new VMCell.DoubleCell(0.0)
        case VMType.GCRef | VMType.GCPtr | _: VMType.Tuple => new VMCell.RefCell(null)
      }
    }

  private final class InterpreterImpl(program: VMProgram, locals: Map[LocalId, VMCell]) {


    def initParameters(body: FunctionBody, args: Seq[VMValue]): Task[Unit] =
      if body.parameters.size != args.size then
        IO.fail(new ArgumentMismatchException())
      else
        ZIO.foreachDiscard(body.parameters.zip(args))(setLocal(_, _))

    def setLocal(local: LocalId, b: Byte): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.ByteCell) =>
          IO.succeed { cell.value = b }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocal(local: LocalId, s: Short): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.ShortCell) =>
          IO.succeed { cell.value = s }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocal(local: LocalId, i: Int): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.IntCell) =>
          IO.succeed { cell.value = i }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocal(local: LocalId, l: Long): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.LongCell) =>
          IO.succeed { cell.value = l }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocal(local: LocalId, f: Float): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.FloatCell) =>
          IO.succeed { cell.value = f }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocal(local: LocalId, d: Double): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.DoubleCell) =>
          IO.succeed { cell.value = d }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocalRef(local: LocalId, ref: AnyRef | Null): Task[Unit] =
      locals.get(local) match {
        case Some(cell: VMCell.RefCell) =>
          IO.succeed { cell.value = ref }

        case Some(_) => IO.fail(new LocalTypeMismatchException())
        case None => IO.fail(new UnknownLocalException())
      }

    def setLocal(local: LocalId, value: VMValue): Task[Unit] =
      value match {
        case b: Byte => setLocal(local, b)
        case s: Short => setLocal(local, s)
        case i: Int => setLocal(local, i)
        case l: Long => setLocal(local, l)
        case f: Float => setLocal(local, f)
        case d: Double => setLocal(local, d)
        case value: (GCObject | VMCell | VMTuple) =>
          setLocalRef(local, value)
      }

    def withLocal[E >: Throwable, A](local: LocalId)(f: PartialFunction[VMCell, IO[E, A]]): IO[E, A] =
      locals.get(local) match {
        case Some(cell) => f.applyOrElse(cell, _ => IO.fail(new LocalTypeMismatchException()))
        case None => IO.fail(new UnknownLocalException())
      }

    def withLocal[E >: Throwable, A](l1: LocalId, l2: LocalId)(f: PartialFunction[(VMCell, VMCell), IO[E, A]]): IO[E, A] =
      withLocal(l1) { cell1 =>
        withLocal(l2)(new PartialFunction[VMCell, IO[E, A]] {
          override def isDefinedAt(cell2: VMCell): Boolean = f.isDefinedAt((cell1, cell2))
          override def apply(cell2: VMCell): IO[E, A] =
            f((cell1, cell2))
        })
      }

    def runCFG(graph: ControlFlowGraph): IO[BlockError, BlockResult] =
      IO.attempt { graph.blocks.head }
        .flatMap(runCFG(graph, _))

    def runCFG(graph: ControlFlowGraph, block: BasicBlock): IO[BlockError, BlockResult] =
      object LabelTarget {
        def unapply(label: LabelId): Option[Int] =
          graph.labels.get(label)
      }

      runBlock(block).flatMap {
        case LabelTarget(offset) =>
          IO.attempt { graph.blocks(offset) }
            .flatMap(runCFG(graph, _))

        case result => IO.succeed(result)
      }
    end runCFG

    def runCFGNoJump(graph: ControlFlowGraph): IO[BlockError, VMValue] =
      runCFG(graph)
        .flatMap {
          case _: (LabelId | JumpInstruction.TailCall) => IO.fail(new InvalidJumpException())
          case value: VMValue => IO.succeed(value)
        }

    def runBlock(block: BasicBlock): IO[BlockError, BlockResult] =
      block match {
        case InstructionBlock(insns, jump) =>
          ZIO.foreachDiscard(insns)(run) *> runJump(jump)

        case CatchBlock(body, handlers) =>
          runCFG(body)
            .catchSome {
              case e: GCObject =>
                def catchError(handlers: List[ExceptionHandler]): IO[BlockError, BlockResult] =
                  handlers match {
                    case ExceptionHandler(exception, filter, handler) :: t =>
                      setLocal(exception, e).flatMap { _ =>
                        runCFGNoJump(filter).flatMap {
                          case shouldHandle: Int =>
                            if shouldHandle != 0 then
                              runCFG(handler)
                            else
                              catchError(t)

                          case _ => IO.fail(new LocalTypeMismatchException())
                        }
                      }

                    case Nil => IO.fail(e)
                  }

                catchError(handlers.toList)
            }

        case FinallyBlock(body, handler) =>
          runCFG(body)
            .foldZIO(
              failure = e => runCFGNoJump(handler) *> IO.fail(e),
              success = _ => runCFGNoJump(handler)
            )

      }

    def callFunction(id: FunctionId, args: Seq[LocalId], virtual: Boolean): IO[GCObject | Throwable, VMValue] =
      IO.fromOption(program.functions.get(id))
        .mapError { _ => new UnknownFunctionException() }
        .flatMap { nextFunc =>
          ZIO.foreach(args) { local =>
            IO.fromOption(locals.get(local)).mapError { _ => new UnknownLocalException() }
          }
            .flatMap { argValues =>
              if virtual then
                executeFunctionVirtual(program)(nextFunc)(argValues)
              else
                executeFunction(program)(nextFunc)(argValues)
            }
        }

    object UByte {
      val MaxValue: Short = 0xFF
      val MinValue: Byte = 0
    }

    object UShort {
      val MaxValue: Int = 0xFFFF
      val MinValue: Short = 0
    }

    object UInt {
      val MaxValue: Long = 0xFFFFFFFF
      val MinValue: Int = 0
    }

    object ULong {
      val MaxValue: BigInt = (BigInt(1) << 64) - 1
      val MinValue: Long = 0
    }

    def run(insn: Instruction): IO[BlockError, Unit] =
      import Instruction.*
      insn match {
        case ConstI8(out, b) => setLocal(out, b)
        case ConstI16(out, s) => setLocal(out, s)
        case ConstI32(out, i) => setLocal(out, i)
        case ConstI64(out, l) => setLocal(out, l)
        case ConstF32(out, f) => setLocal(out, f)
        case ConstF64(out, d) => setLocal(out, d)
        case ConstInt(out, i) => setLocal(out, i)
        case ConstString(out, s) => setLocal(out, s)




        case ConvertS8(out, in, OverflowMode.None) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b)
            case VMCell.ShortCell(s) => IO.succeed(s.toByte)
            case VMCell.IntCell(i) => IO.succeed(i.toByte)
            case VMCell.LongCell(l) => IO.succeed(l.toByte)
            case VMCell.FloatCell(f) => IO.succeed(f.toByte)
            case VMCell.DoubleCell(d) => IO.succeed(d.toByte)
          }.flatMap(setLocal(out, _))

        case ConvertS8(out, in, OverflowMode.Signed) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b)
            case VMCell.ShortCell(s) =>
              if s.isValidByte then
                IO.succeed(s.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if i.isValidByte then
                IO.succeed(i.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if l.isValidByte then
                IO.succeed(l.toByte)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertS8(out, in, OverflowMode.Unsigned) =>
          withLocal(in) {
            case VMCell.ByteCell(b) =>
              if java.lang.Byte.compareUnsigned(b, Byte.MaxValue) <= 0 then
                IO.succeed(b)
              else
                IO.fail(new ArithmeticException())

            case VMCell.ShortCell(s) =>
              if java.lang.Short.compareUnsigned(s, Byte.MaxValue) <= 0 then
                IO.succeed(s.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if java.lang.Integer.compareUnsigned(i, Byte.MaxValue) <= 0 then
                IO.succeed(i.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if java.lang.Long.compareUnsigned(l, Byte.MaxValue) <= 0 then
                IO.succeed(l.toByte)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))


        case ConvertU8(out, in, OverflowMode.None) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b)
            case VMCell.ShortCell(s) => IO.succeed(s.toByte)
            case VMCell.IntCell(i) => IO.succeed(i.toByte)
            case VMCell.LongCell(l) => IO.succeed(l.toByte)
            case VMCell.FloatCell(f) => IO.succeed(f.toByte)
            case VMCell.DoubleCell(d) => IO.succeed(d.toByte)
          }.flatMap(setLocal(out, _))

        case ConvertU8(out, in, OverflowMode.Signed) =>
          withLocal(in) {
            case VMCell.ByteCell(b) =>
              if b >= UByte.MinValue then
                IO.succeed(b)
              else
                IO.fail(new ArithmeticException())

            case VMCell.ShortCell(s) =>
              if s >= UByte.MinValue && s <= UByte.MaxValue then
                IO.succeed(s.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if i >= UByte.MinValue && i <= UByte.MaxValue then
                IO.succeed(i.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if l >= UByte.MinValue && l <= UByte.MaxValue then
                IO.succeed(l.toByte)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertU8(out, in, OverflowMode.Unsigned) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b)

            case VMCell.ShortCell(s) =>
              if java.lang.Short.compareUnsigned(s, UByte.MaxValue) <= 0 then
                IO.succeed(s.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if java.lang.Integer.compareUnsigned(i, UByte.MaxValue) <= 0 then
                IO.succeed(i.toByte)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if java.lang.Long.compareUnsigned(l, UByte.MaxValue) <= 0 then
                IO.succeed(l.toByte)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertS16(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toShort)
            case VMCell.ShortCell(s) => IO.succeed(s)
            case VMCell.IntCell(i) => IO.succeed(i.toShort)
            case VMCell.LongCell(l) => IO.succeed(l.toShort)
            case VMCell.FloatCell(f) => IO.succeed(f.toShort)
            case VMCell.DoubleCell(d) => IO.succeed(d.toShort)
          }.flatMap(setLocal(out, _))

        case ConvertS16(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toShort)
            case VMCell.ShortCell(s) => IO.succeed(s)

            case VMCell.IntCell(i) =>
              if i.isValidShort then
                IO.succeed(i.toShort)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if l.isValidShort then
                IO.succeed(l.toShort)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertU_S16(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b).toShort)
            case VMCell.ShortCell(s) => IO.succeed(s)
            case VMCell.IntCell(i) => IO.succeed(i.toShort)
            case VMCell.LongCell(l) => IO.succeed(l.toShort)
            case VMCell.FloatCell(f) => IO.succeed(f.toShort)
            case VMCell.DoubleCell(d) => IO.succeed(d.toShort)
          }.flatMap(setLocal(out, _))

        case ConvertU_S16(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b).toShort)

            case VMCell.ShortCell(s) =>
              if java.lang.Short.compareUnsigned(s, Short.MaxValue) <= 0 then
                IO.succeed(s)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if java.lang.Integer.compareUnsigned(i, Short.MaxValue) <= 0 then
                IO.succeed(i.toShort)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if java.lang.Long.compareUnsigned(l, Short.MaxValue) <= 0 then
                IO.succeed(l.toShort)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))


        case ConvertU16(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toShort)
            case VMCell.ShortCell(s) => IO.succeed(s)
            case VMCell.IntCell(i) => IO.succeed(i.toShort)
            case VMCell.LongCell(l) => IO.succeed(l.toShort)
            case VMCell.FloatCell(f) => IO.succeed(f.toShort)
            case VMCell.DoubleCell(d) => IO.succeed(d.toShort)
          }.flatMap(setLocal(out, _))

        case ConvertU16(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) =>
              if b >= UShort.MinValue then
                IO.succeed(b.toShort)
              else
                IO.fail(new ArithmeticException())

            case VMCell.ShortCell(s) =>
              if s >= UShort.MinValue then
                IO.succeed(s)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if i >= UShort.MinValue && i <= UShort.MaxValue then
                IO.succeed(i.toShort)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if l >= UShort.MinValue && l <= UShort.MaxValue then
                IO.succeed(l.toShort)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))


        case ConvertU_U16(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b).toShort)
            case VMCell.ShortCell(s) => IO.succeed(s)
            case VMCell.IntCell(i) => IO.succeed(i.toShort)
            case VMCell.LongCell(l) => IO.succeed(l.toShort)
            case VMCell.FloatCell(f) => IO.succeed(f.toShort)
            case VMCell.DoubleCell(d) => IO.succeed(d.toShort)
          }.flatMap(setLocal(out, _))

        case ConvertU_U16(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b).toShort)
            case VMCell.ShortCell(s) => IO.succeed(s)

            case VMCell.IntCell(i) =>
              if java.lang.Integer.compareUnsigned(i, UShort.MaxValue) <= 0 then
                IO.succeed(i.toShort)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if java.lang.Long.compareUnsigned(l, UShort.MaxValue) <= 0 then
                IO.succeed(l.toShort)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertS32(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toInt)
            case VMCell.ShortCell(s) => IO.succeed(s.toInt)
            case VMCell.IntCell(i) => IO.succeed(i)
            case VMCell.LongCell(l) => IO.succeed(l.toInt)
            case VMCell.FloatCell(f) => IO.succeed(f.toInt)
            case VMCell.DoubleCell(d) => IO.succeed(d.toInt)
          }.flatMap(setLocal(out, _))

        case ConvertS32(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toInt)
            case VMCell.ShortCell(s) => IO.succeed(s.toInt)
            case VMCell.IntCell(i) => IO.succeed(i)
            case VMCell.LongCell(l) => IO.attempt { java.lang.Math.toIntExact(l) }
          }.flatMap(setLocal(out, _))

        case ConvertU_S32(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b))
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedInt(s))
            case VMCell.IntCell(i) => IO.succeed(i)
            case VMCell.LongCell(l) => IO.succeed(l.toInt)
            case VMCell.FloatCell(f) => IO.succeed(f.toInt)
            case VMCell.DoubleCell(d) => IO.succeed(d.toInt)
          }.flatMap(setLocal(out, _))

        case ConvertU_S32(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b))
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedInt(s))

            case VMCell.IntCell(i) =>
              if java.lang.Integer.compareUnsigned(i, Int.MaxValue) <= 0 then
                IO.succeed(i.toInt)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if java.lang.Long.compareUnsigned(l, Int.MaxValue) <= 0 then
                IO.succeed(l.toInt)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))


        case ConvertU32(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toInt)
            case VMCell.ShortCell(s) => IO.succeed(s.toInt)
            case VMCell.IntCell(i) => IO.succeed(i)
            case VMCell.LongCell(l) => IO.succeed(l.toInt)
            case VMCell.FloatCell(f) => IO.succeed(f.toInt)
            case VMCell.DoubleCell(d) => IO.succeed(d.toInt)
          }.flatMap(setLocal(out, _))

        case ConvertU32(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) =>
              if b >= UInt.MinValue then
                IO.succeed(b.toInt)
              else
                IO.fail(new ArithmeticException())

            case VMCell.ShortCell(s) =>
              if s >= UInt.MinValue then
                IO.succeed(s.toInt)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if i >= UInt.MinValue then
                IO.succeed(i)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if l >= UInt.MinValue && l <= UInt.MaxValue then
                IO.succeed(l.toInt)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertU_U32(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b))
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedInt(s))
            case VMCell.IntCell(i) => IO.succeed(i)
            case VMCell.LongCell(l) => IO.succeed(l.toInt)
            case VMCell.FloatCell(f) => IO.succeed(f.toInt)
            case VMCell.DoubleCell(d) => IO.succeed(d.toInt)
          }.flatMap(setLocal(out, _))

        case ConvertU_U32(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b))
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedInt(s))
            case VMCell.IntCell(i) => IO.succeed(i)

            case VMCell.LongCell(l) =>
              if java.lang.Long.compareUnsigned(l, UInt.MaxValue) <= 0 then
                IO.succeed(l.toInt)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertS64(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toLong)
            case VMCell.ShortCell(s) => IO.succeed(s.toLong)
            case VMCell.IntCell(i) => IO.succeed(i.toLong)
            case VMCell.LongCell(l) => IO.succeed(l)
            case VMCell.FloatCell(f) => IO.succeed(f.toLong)
            case VMCell.DoubleCell(d) => IO.succeed(d.toLong)
          }.flatMap(setLocal(out, _))

        case ConvertS64(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toLong)
            case VMCell.ShortCell(s) => IO.succeed(s.toLong)
            case VMCell.IntCell(i) => IO.succeed(i.toLong)
            case VMCell.LongCell(l) => IO.succeed(l)
          }.flatMap(setLocal(out, _))

        case ConvertU_S64(out, in, _) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedLong(b))
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedLong(s))
            case VMCell.IntCell(i) => IO.succeed(java.lang.Integer.toUnsignedLong(i))
            case VMCell.LongCell(l) => IO.succeed(l)
          }.flatMap(setLocal(out, _))


        case ConvertU64(out, in, false) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toLong)
            case VMCell.ShortCell(s) => IO.succeed(s.toLong)
            case VMCell.IntCell(i) => IO.succeed(i.toLong)
            case VMCell.LongCell(l) => IO.succeed(l)
            case VMCell.FloatCell(f) => IO.succeed(f.toLong)
            case VMCell.DoubleCell(d) => IO.succeed(d.toLong)
          }.flatMap(setLocal(out, _))

        case ConvertU64(out, in, true) =>
          withLocal(in) {
            case VMCell.ByteCell(b) =>
              if b >= ULong.MinValue then
                IO.succeed(b.toLong)
              else
                IO.fail(new ArithmeticException())

            case VMCell.ShortCell(s) =>
              if s >= ULong.MinValue then
                IO.succeed(s.toLong)
              else
                IO.fail(new ArithmeticException())

            case VMCell.IntCell(i) =>
              if i >= ULong.MinValue then
                IO.succeed(i.toLong)
              else
                IO.fail(new ArithmeticException())

            case VMCell.LongCell(l) =>
              if l >= ULong.MinValue then
                IO.succeed(l)
              else
                IO.fail(new ArithmeticException())

          }.flatMap(setLocal(out, _))

        case ConvertU_U64(out, in, _) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedLong(b))
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedLong(s))
            case VMCell.IntCell(i) => IO.succeed(java.lang.Integer.toUnsignedLong(i))
            case VMCell.LongCell(l) => IO.succeed(l)
          }.flatMap(setLocal(out, _))

        case ConvertF32(out, in) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toFloat)
            case VMCell.ShortCell(s) => IO.succeed(s.toFloat)
            case VMCell.IntCell(i) => IO.succeed(i.toFloat)
            case VMCell.LongCell(l) => IO.succeed(l.toFloat)
            case VMCell.FloatCell(f) => IO.succeed(f)
            case VMCell.DoubleCell(d) => IO.succeed(d.toFloat)
          }.flatMap(setLocal(out, _))

        case ConvertU_F32(out, in) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b).toFloat)
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedInt(s).toFloat)
            case VMCell.IntCell(i) => IO.succeed(java.lang.Integer.toUnsignedLong(i).toFloat)
            case VMCell.LongCell(l) =>
              if l >= 0 then IO.succeed(l.toFloat)
              else IO.succeed(((BigInt(1) << 64) + l).toFloat)
          }.flatMap(setLocal(out, _))

        case ConvertF64(out, in) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(b.toFloat)
            case VMCell.ShortCell(s) => IO.succeed(s.toFloat)
            case VMCell.IntCell(i) => IO.succeed(i.toFloat)
            case VMCell.LongCell(l) => IO.succeed(l.toFloat)
            case VMCell.FloatCell(f) => IO.succeed(f)
            case VMCell.DoubleCell(d) => IO.succeed(d.toFloat)
          }.flatMap(setLocal(out, _))

        case ConvertU_F64(out, in) =>
          withLocal(in) {
            case VMCell.ByteCell(b) => IO.succeed(java.lang.Byte.toUnsignedInt(b).toDouble)
            case VMCell.ShortCell(s) => IO.succeed(java.lang.Short.toUnsignedInt(s).toDouble)
            case VMCell.IntCell(i) => IO.succeed(java.lang.Integer.toUnsignedLong(i).toDouble)
            case VMCell.LongCell(l) =>
              if l >= 0 then IO.succeed(l.toDouble)
              else IO.succeed(((BigInt(1) << 64) + l).toDouble)
          }.flatMap(setLocal(out, _))



        case Add(out, a, b, OverflowMode.None) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x + y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x + y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x + y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x + y)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, x + y)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, x + y)
          }

        case Add(out, a, b, OverflowMode.Signed) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              val result = x + y
              if result.isValidByte then setLocal(out, result.toByte)
              else IO.fail(new ArithmeticException())

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => 
              val result = x + y
              if result.isValidShort then setLocal(out, result.toShort)
              else IO.fail(new ArithmeticException())

            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, Math.addExact(x, y))
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, Math.addExact(x, y))
          }

        case Add(out, a, b, OverflowMode.Unsigned) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              val result = java.lang.Byte.toUnsignedInt(x) + java.lang.Byte.toUnsignedInt(y)
              if result <= UByte.MaxValue then setLocal(out, result.toByte)
              else IO.fail(new ArithmeticException())

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => 
              val result = java.lang.Short.toUnsignedInt(x) + java.lang.Short.toUnsignedInt(y)
              if result <= UShort.MaxValue then setLocal(out, result.toShort)
              else IO.fail(new ArithmeticException())

            case (VMCell.IntCell(x), VMCell.IntCell(y)) =>
              val result = x + y
              if java.lang.Integer.compareUnsigned(result, x) < 0 || java.lang.Integer.compareUnsigned(result, y) < 0 then
                setLocal(out, result)
              else
                IO.fail(new ArithmeticException())
            case (VMCell.LongCell(x), VMCell.LongCell(y)) =>
              val result = x + y
              if java.lang.Long.compareUnsigned(result, x) < 0 || java.lang.Long.compareUnsigned(result, y) < 0 then
                setLocal(out, result)
              else
                IO.fail(new ArithmeticException())
          }

        case Sub(out, a, b, OverflowMode.None) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x - y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x - y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x - y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x - y)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, x - y)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, x - y)
          }

        case Sub(out, a, b, OverflowMode.Signed) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              val result = x - y
              if result.isValidByte then setLocal(out, result.toByte)
              else IO.fail(new ArithmeticException())

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => 
              val result = x - y
              if result.isValidShort then setLocal(out, result.toShort)
              else IO.fail(new ArithmeticException())

            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, Math.subtractExact(x, y))
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, Math.subtractExact(x, y))
          }

        case Sub(out, a, b, OverflowMode.Unsigned) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              val result = java.lang.Byte.toUnsignedInt(x) - java.lang.Byte.toUnsignedInt(y)
              if result >= 0 then setLocal(out, result.toByte)
              else IO.fail(new ArithmeticException())

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => 
              val result = java.lang.Short.toUnsignedInt(x) - java.lang.Short.toUnsignedInt(y)
              if result >= 0 then setLocal(out, result.toShort)
              else IO.fail(new ArithmeticException())

            case (VMCell.IntCell(x), VMCell.IntCell(y)) =>
              val result = x - y
              if java.lang.Integer.compareUnsigned(result, x) > 0 || java.lang.Integer.compareUnsigned(result, y) > 0 then
                setLocal(out, result)
              else
                IO.fail(new ArithmeticException())

            case (VMCell.LongCell(x), VMCell.LongCell(y)) =>
              val result = x - y
              if java.lang.Long.compareUnsigned(result, x) < 0 || java.lang.Long.compareUnsigned(result, y) > 0 then
                setLocal(out, result)
              else
                IO.fail(new ArithmeticException())
          }

        case Mul(out, a, b, OverflowMode.None) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x * y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x * y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x * y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x * y)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, x * y)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, x * y)
          }

        case Mul(out, a, b, OverflowMode.Signed) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              val result = x * y
              if result.isValidByte then setLocal(out, result.toByte)
              else IO.fail(new ArithmeticException())

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => 
              val result = x * y
              if result.isValidShort then setLocal(out, result.toShort)
              else IO.fail(new ArithmeticException())

            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, Math.multiplyExact(x, y))
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, Math.multiplyExact(x, y))
          }

        case Mul(out, a, b, OverflowMode.Unsigned) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              val result = java.lang.Byte.toUnsignedInt(x) * java.lang.Byte.toUnsignedInt(y)
              if result <= UByte.MaxValue then setLocal(out, result.toByte)
              else IO.fail(new ArithmeticException())

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => 
              val result = java.lang.Short.toUnsignedInt(x) * java.lang.Short.toUnsignedInt(y)
              if result <= UShort.MaxValue then setLocal(out, result.toShort)
              else IO.fail(new ArithmeticException())

            case (VMCell.IntCell(x), VMCell.IntCell(y)) =>
              val result = java.lang.Integer.toUnsignedLong(x) * java.lang.Integer.toUnsignedLong(y)
              if result <= UInt.MaxValue then setLocal(out, result.toInt)
              else IO.fail(new ArithmeticException())

            case (VMCell.LongCell(x), VMCell.LongCell(y)) =>
              // https://stackoverflow.com/a/20060733
              def hasOverflow = x != 0 && y != 0 && {
                val xHalf = x >>> 2
                val yHalf = y >>> 2
                if x >= 0 then Long.MaxValue / y < xHalf
                else if y >= 0 then Long.MaxValue / x < yHalf
                else (Long.MaxValue - yHalf) / y < xHalf
              }

              if hasOverflow then IO.fail(new ArithmeticException())
              else setLocal(out, x * y)
          }

        case DivS(out, a, b, false) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x / y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x / y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x / y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x / y)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, x / y)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, x / y)
          }

        case DivS(out, a, b, true) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              if x == Byte.MinValue && y == -1 then IO.fail(new ArithmeticException())
              else setLocal(out, (x / y).toByte)

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) =>
              if x == Short.MinValue && y == -1 then IO.fail(new ArithmeticException())
              else setLocal(out, (x / y).toShort)
              
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => 
              if x == Int.MinValue && y == -1 then IO.fail(new ArithmeticException())
              else setLocal(out, x / y)

            case (VMCell.LongCell(x), VMCell.LongCell(y)) =>
              if x == Long.MinValue && y == -1 then IO.fail(new ArithmeticException())
              else setLocal(out, x / y)
          }

        case DivU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              setLocal(out, (java.lang.Byte.toUnsignedInt(x) / java.lang.Byte.toUnsignedInt(y)).toByte)

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) =>
              setLocal(out, (java.lang.Short.toUnsignedInt(x) / java.lang.Short.toUnsignedInt(y)).toShort)

            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, java.lang.Integer.divideUnsigned(x, y))
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, java.lang.Long.divideUnsigned(x, y))
          }

        case RemS(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x % y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x % y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x % y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x % y)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, x % y)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, x % y)
          }

        case RemU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) =>
              setLocal(out, (java.lang.Byte.toUnsignedInt(x) % java.lang.Byte.toUnsignedInt(y)).toByte)

            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) =>
              setLocal(out, (java.lang.Short.toUnsignedInt(x) % java.lang.Short.toUnsignedInt(y)).toShort)

            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, java.lang.Integer.remainderUnsigned(x, y))
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, java.lang.Long.remainderUnsigned(x, y))
          }

        case Negate(out, a, false) =>
          withLocal(a) {
            case VMCell.ByteCell(x) => setLocal(out, (-x).toByte)
            case VMCell.ShortCell(x) => setLocal(out, (-x).toShort)
            case VMCell.IntCell(x) => setLocal(out, -x)
            case VMCell.LongCell(x) => setLocal(out, -x)
            case VMCell.FloatCell(x) => setLocal(out, -x)
            case VMCell.DoubleCell(x) => setLocal(out, -x)
          }

        case Negate(out, a, true) =>
          withLocal(a) {
            case VMCell.ByteCell(x) =>
              if x == Byte.MinValue then IO.fail(new ArithmeticException())
              else setLocal(out, (-x).toByte)

            case VMCell.ShortCell(x) =>
              if x == Short.MinValue then IO.fail(new ArithmeticException())
              else setLocal(out, (-x).toShort)
              
            case VMCell.IntCell(x) => setLocal(out, Math.negateExact(x))
            case VMCell.LongCell(x) => setLocal(out, Math.negateExact(x))
          }

        case ShiftLeft(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x << y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x << y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x << y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x << y)
          }

        case ShiftRightS(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x >> y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x >> y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x >> y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x >> y)
          }

        case ShiftRightU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x >>> y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x >>> y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x >>> y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x >>> y)
          }

        case BitAnd(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x & y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x & y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x & y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x & y)
          }

        case BitOr(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x | y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x | y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x | y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x | y)
          }

        case BitXOr(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, (x ^ y).toByte)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, (x ^ y).toShort)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, x ^ y)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, x ^ y)
          }

        case BitNot(out, a) =>
          withLocal(a) {
            case VMCell.ByteCell(x) => setLocal(out, (~x).toByte)
            case VMCell.ShortCell(x) => setLocal(out, (~x).toShort)
            case VMCell.IntCell(x) => setLocal(out, ~x)
            case VMCell.LongCell(x) => setLocal(out, ~x)
          }

        case EqualTo(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if x == y then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if x == y then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if x == y then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if x == y then 1 else 0)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, if x == y then 1 else 0)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, if x == y then 1 else 0)
          }

        case NotEqualTo(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if x != y then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if x != y then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if x != y then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if x != y then 1 else 0)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, if x != y then 1 else 0)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, if x != y then 1 else 0)
          }

        case LessThan(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if x < y then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if x < y then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if x < y then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if x < y then 1 else 0)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, if x < y then 1 else 0)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, if x < y then 1 else 0)
          }

        case LessThanU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if java.lang.Byte.compareUnsigned(x, y) < 0 then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if java.lang.Short.compareUnsigned(x, y) < 0 then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if java.lang.Integer.compareUnsigned(x, y) < 0 then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if java.lang.Long.compareUnsigned(x, y) < 0 then 1 else 0)
          }

        case LessThanEq(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if x <= y then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if x <= y then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if x <= y then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if x <= y then 1 else 0)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, if x <= y then 1 else 0)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, if x <= y then 1 else 0)
          }

        case LessThanEqU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if java.lang.Byte.compareUnsigned(x, y) <= 0 then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if java.lang.Short.compareUnsigned(x, y) <= 0 then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if java.lang.Integer.compareUnsigned(x, y) <= 0 then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if java.lang.Long.compareUnsigned(x, y) <= 0 then 1 else 0)
          }

        case GreaterThan(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if x > y then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if x > y then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if x > y then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if x > y then 1 else 0)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, if x > y then 1 else 0)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, if x > y then 1 else 0)
          }

        case GreaterThanU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if java.lang.Byte.compareUnsigned(x, y) > 0 then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if java.lang.Short.compareUnsigned(x, y) > 0 then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if java.lang.Integer.compareUnsigned(x, y) > 0 then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if java.lang.Long.compareUnsigned(x, y) > 0 then 1 else 0)
          }

        case GreaterThanEq(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if x >= y then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if x >= y then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if x >= y then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if x >= y then 1 else 0)
            case (VMCell.FloatCell(x), VMCell.FloatCell(y)) => setLocal(out, if x >= y then 1 else 0)
            case (VMCell.DoubleCell(x), VMCell.DoubleCell(y)) => setLocal(out, if x >= y then 1 else 0)
          }

        case GreaterThanEqU(out, a, b) =>
          withLocal(a, b) {
            case (VMCell.ByteCell(x), VMCell.ByteCell(y)) => setLocal(out, if java.lang.Byte.compareUnsigned(x, y) >= 0 then 1 else 0)
            case (VMCell.ShortCell(x), VMCell.ShortCell(y)) => setLocal(out, if java.lang.Short.compareUnsigned(x, y) >= 0 then 1 else 0)
            case (VMCell.IntCell(x), VMCell.IntCell(y)) => setLocal(out, if java.lang.Integer.compareUnsigned(x, y) >= 0 then 1 else 0)
            case (VMCell.LongCell(x), VMCell.LongCell(y)) => setLocal(out, if java.lang.Long.compareUnsigned(x, y) >= 0 then 1 else 0)
          }

        case EqualToZero(out, a) =>
          withLocal(a) {
            case VMCell.ByteCell(x) => setLocal(out, if x == 0 then 1 else 0)
            case VMCell.ShortCell(x) => setLocal(out, if x == 0 then 1 else 0)
            case VMCell.IntCell(x) => setLocal(out, if x == 0 then 1 else 0)
            case VMCell.LongCell(x) => setLocal(out, if x == 0 then 1 else 0)
            case VMCell.FloatCell(x) => setLocal(out, if x == 0 then 1 else 0)
            case VMCell.DoubleCell(x) => setLocal(out, if x == 0 then 1 else 0)
          }

        case NotEqualToZero(out, a) =>
          withLocal(a) {
            case VMCell.ByteCell(x) => setLocal(out, if x != 0 then 1 else 0)
            case VMCell.ShortCell(x) => setLocal(out, if x != 0 then 1 else 0)
            case VMCell.IntCell(x) => setLocal(out, if x != 0 then 1 else 0)
            case VMCell.LongCell(x) => setLocal(out, if x != 0 then 1 else 0)
            case VMCell.FloatCell(x) => setLocal(out, if x != 0 then 1 else 0)
            case VMCell.DoubleCell(x) => setLocal(out, if x != 0 then 1 else 0)
          }

        case Call(id, args, result, virtual) =>
          callFunction(id, args, virtual).flatMap(setLocal(result, _))

        case LoadLocalReference(out, id) =>
          locals.get(id) match {
            case Some(cell) => setLocal(out, cell)
            case None => IO.fail(new UnknownLocalException())
          }

        case DerefLoad(out, ptr, elementType, false) =>
          withLocal(ptr) {
            case VMCell.RefCell(cell) =>
              (cell, elementType) match {
                case (VMCell.ByteCell(value), VMType.I8) => setLocal(out, value)
                case (VMCell.ShortCell(value), VMType.I16) => setLocal(out, value)
                case (VMCell.IntCell(value), VMType.I32) => setLocal(out, value)
                case (VMCell.LongCell(value), VMType.I64) => setLocal(out, value)
                case (VMCell.FloatCell(value), VMType.F32) => setLocal(out, value)
                case (VMCell.DoubleCell(value), VMType.F64) => setLocal(out, value)
                case (VMCell.RefCell(value), VMType.GCRef | VMType.GCPtr | VMType.Tuple(_)) => setLocalRef(out, value)
                case _ => IO.fail(new LocalTypeMismatchException())
              }

          }

        case DerefLoad(out, ptr, elementType, true) =>
          withLocal(ptr) {
            case VMCell.RefCell(cell) =>
              (cell, elementType) match {
                case (cell: VMCell.ByteCell, VMType.I8) => setLocal(out, cell.readVolatile)
                case (cell: VMCell.ShortCell, VMType.I16) => setLocal(out, cell.readVolatile)
                case (cell: VMCell.IntCell, VMType.I32) => setLocal(out, cell.readVolatile)
                case (cell: VMCell.LongCell, VMType.I64) => setLocal(out, cell.readVolatile)
                case (cell: VMCell.FloatCell, VMType.F32) => setLocal(out, cell.readVolatile)
                case (cell: VMCell.DoubleCell, VMType.F64) => setLocal(out, cell.readVolatile)
                case (cell: VMCell.RefCell, VMType.GCRef | VMType.GCPtr | VMType.Tuple(_)) => setLocalRef(out, cell.readVolatile)
                case _ => IO.fail(new LocalTypeMismatchException())
              }

          }

        case DerefStore(ptr, src, elementType, false) =>
          withLocal(ptr, src) {
            case (VMCell.RefCell(cell), valueCell) =>
              (cell, valueCell, elementType) match {
                case (cell: VMCell.ByteCell, valueCell: VMCell.ByteCell, VMType.I8) => IO.succeed { cell.value = valueCell.value }
                case (cell: VMCell.ShortCell, valueCell: VMCell.ShortCell, VMType.I16) => IO.succeed { cell.value = valueCell.value }
                case (cell: VMCell.IntCell, valueCell: VMCell.IntCell, VMType.I32) => IO.succeed { cell.value = valueCell.value }
                case (cell: VMCell.LongCell, valueCell: VMCell.LongCell, VMType.I64) => IO.succeed { cell.value = valueCell.value }
                case (cell: VMCell.FloatCell, valueCell: VMCell.FloatCell, VMType.F32) => IO.succeed { cell.value = valueCell.value }
                case (cell: VMCell.DoubleCell, valueCell: VMCell.DoubleCell, VMType.F64) => IO.succeed { cell.value = valueCell.value }
                case (cell: VMCell.RefCell, valueCell: VMCell.RefCell, VMType.GCRef | VMType.GCPtr | VMType.Tuple(_)) => IO.succeed { cell.value = valueCell.value }
                case _ => IO.fail(new LocalTypeMismatchException())
              }
          }

        case DerefStore(ptr, src, elementType, true) =>
          withLocal(ptr, src) {
            case (VMCell.RefCell(cell), valueCell) =>
              (cell, valueCell, elementType) match {
                case (cell: VMCell.ByteCell, valueCell: VMCell.ByteCell, VMType.I8) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case (cell: VMCell.ShortCell, valueCell: VMCell.ShortCell, VMType.I16) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case (cell: VMCell.IntCell, valueCell: VMCell.IntCell, VMType.I32) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case (cell: VMCell.LongCell, valueCell: VMCell.LongCell, VMType.I64) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case (cell: VMCell.FloatCell, valueCell: VMCell.FloatCell, VMType.F32) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case (cell: VMCell.DoubleCell, valueCell: VMCell.DoubleCell, VMType.F64) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case (cell: VMCell.RefCell, valueCell: VMCell.RefCell, VMType.GCRef | VMType.GCPtr | VMType.Tuple(_)) => IO.succeed { cell.writeVolatile(valueCell.value) }
                case _ => IO.fail(new LocalTypeMismatchException())
              }
          }

      }
    end run

    def runJump(insn: JumpInstruction): IO[BlockError, BlockResult] =
      import JumpInstruction.*
      insn match {
        case tail: TailCall =>
          IO.succeed(tail)

        case Return(value) =>
          locals.get(value) match {
            case Some(VMCell.ByteCell(value)) => IO.succeed(value)
            case Some(VMCell.ShortCell(value)) => IO.succeed(value)
            case Some(VMCell.IntCell(value)) => IO.succeed(value)
            case Some(VMCell.LongCell(value)) => IO.succeed(value)
            case Some(VMCell.FloatCell(value)) => IO.succeed(value)
            case Some(VMCell.DoubleCell(value)) => IO.succeed(value)
            case Some(VMCell.RefCell(value: (GCObject | VMCell | VMTuple))) => IO.succeed(value)
            case Some(VMCell.RefCell(_)) => IO.fail(new LocalTypeMismatchException())
            case None => IO.fail(new UnknownLocalException())
          }

        case Jump(label) =>
          IO.succeed(label)

        case JumpZero(value, zTarget, nzTarget) =>
          withLocal(value) {
            case VMCell.IntCell(b) =>
              IO.succeed(
                if b == 0 then zTarget
                else nzTarget
              )
          }

        case Throw(exception) =>
          withLocal(exception) {
            case VMCell.RefCell(value: GCObject) =>
              IO.fail(value)
          }
      }
    end runJump

  }

}
