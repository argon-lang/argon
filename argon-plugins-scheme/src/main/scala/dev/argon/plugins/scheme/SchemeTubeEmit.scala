package dev.argon.plugins.scheme

import zio.*
import zio.stm.*
import zio.stream.*
import dev.argon.tube.{ImportSpecifier, Identifier, ModulePath, ErasedSignature}

import SchemeTubeEmit.*
import dev.argon.plugin.scalaApi.vm.*
import dev.argon.plugins.scheme.SchemeExterns.ExternRefFunction
import dev.argon.plugins.scheme.SchemeExterns.ExternFunction

private[scheme] class SchemeTubeEmit[E <: Throwable](
  exportState: TSet[SchemeExpr],
  importState: TMap[Seq[String], ImportedLibrary],
) {

  def emitTube(tube: TubeDefinition[SchemeExterns, E]): IO[E, Seq[SchemeExpr]] =
    ZStream.fromIterable(tube.modules)
      .flatMap(emitModule)
      .runCollect

  
  private def getImport(importSpec: ImportSpecifier): IO[E, SchemeExpr] =
    getImport(
        TubeExportEncoding.buildTubeLibName(importSpec.tube),
        TubeExportEncoding.buildTubeExportId(importSpec.modulePath, importSpec.name, importSpec.sig),
    )


  private def getImport(lib: Seq[String], name: String): IO[E, SchemeExpr] = (
    for
      importedLib <- importState.get(lib)
      importedLib <- importedLib.fold(
        for
          len <- importState.size
          names <- TMap.empty
        yield ImportedLibrary(len, names)
      )(STM.succeed)
      importIndex <- importedLib.names.get(name)
      importIndex <- importIndex.fold(
        for
          len <- importedLib.names.size
          _ <- importedLib.names.put(name, len)
        yield len
      )(ZSTM.succeed)
    yield SchemeExpr.Symbol(s"import_${importedLib.index}_${importIndex}")
  ).commit

  private def addExport(id: String): IO[E, Unit] =
    exportState.put(SchemeExpr.Symbol(id)).commit

  private def emitModule(module: ModuleDefinition[SchemeExterns, E]): ZStream[Any, E, SchemeExpr] =
    ZStream.fromZIO(module.path()).flatMap { modulePath =>
      ZStream.fromIterableZIO(module.exports())
        .flatMap {
          case ModuleExportEntry(name, exports) => 
            ZStream.fromIterable(exports)
              .mapZIO { overload =>
                overload.`export` match {
                  case ModuleExport.Function(f) => emitFunction(modulePath, name, overload.sig, f)
                  case ModuleExport.Record(r) => emitRecord(modulePath, name, overload.sig, r)
                }
              }
        }
    }

  

  private def emitFunction(module: ModulePath, name: Option[Identifier], erasedSig: ErasedSignature, func: FunctionDefinition[SchemeExterns, E]): IO[E, SchemeExpr] =
    func.implementation().flatMap {
      case FunctionImplementation.VmIr(block) =>
        for
          sig <- func.signature()
          body <- emitBlock(VarScope(varOffset = sig.parameters.size))(block)

          symName = TubeExportEncoding.buildTubeExportId(module, name, erasedSig)
          _ <- addExport(symName)
        yield SchemeExpr.List(Seq(
          SchemeExpr.Symbol("define"),
          SchemeExpr.List(
            SchemeExpr.Symbol(symName) +: sig.parameters.indices.map(i => getRegVar(RegisterId(i))),
            None
          ),
          body,
        ), None)

      case FunctionImplementation.Extern(SchemeExterns.ExternFunction(library, id)) =>
        for
          externName <- getImport(library, id)
        yield SchemeExpr.List(Seq(
          SchemeExpr.Symbol("define"),
          externName,
        ), None)
    }

  private def emitRecord(module: ModulePath, name: Option[Identifier], erasedSig: ErasedSignature, rec: RecordDefinition[SchemeExterns, E]): IO[E, SchemeExpr] =
    val symName = TubeExportEncoding.buildTubeExportId(module, name, erasedSig)
    val ctorName = TubeExportEncoding.recordConstructorExportId(symName)
    val testName = TubeExportEncoding.recordTestExportId(symName)
    for
      _ <- addExport(symName)
      _ <- addExport(ctorName)

      fields <- rec.fields()
      fieldDefs <- ZIO.foreach(fields) { field =>
        val fieldName = TubeExportEncoding.recordFieldId(field.name)
        val accessorName = TubeExportEncoding.recordFieldAccessorExportId(symName, field.name)

        for
          _ <- addExport(accessorName)
        yield SchemeExpr.List(Seq(
          SchemeExpr.Symbol("immutable"),
          SchemeExpr.Symbol(fieldName),
          SchemeExpr.Symbol(accessorName),
        ), None)
      }

    yield SchemeExpr.List(Seq(
      SchemeExpr.Symbol("define-record-type"),
      SchemeExpr.List(Seq(
        SchemeExpr.Symbol(symName),
        SchemeExpr.Symbol(ctorName),
        SchemeExpr.Symbol(testName),
      ), None),
      SchemeExpr.List(Seq(
        SchemeExpr.Symbol("sealed"),
        SchemeExpr.Boolean(true),
      ), None),
      SchemeExpr.List(
        SchemeExpr.Symbol("fields") +: fieldDefs,
        None,
      ),
    ), None)



  private def emitBlock(scope: VarScope)(block: Block[SchemeExterns]): IO[E, SchemeExpr] =
    for
      insns <- ZIO.foreach(block.instructions)(emitInstruction(VarScope(varOffset = scope.varOffset + block.variables.size)))
    yield
      if block.variables.isEmpty then
        SchemeExpr.List(
          SchemeExpr.Symbol("block") +: insns.flatten,
          None
        )
      else
        SchemeExpr.List(
          SchemeExpr.Symbol("let") +:
            SchemeExpr.List(
              block.variables.indices.map(i => 
                SchemeExpr.List(Seq(
                  getRegVar(RegisterId(scope.varOffset + i)),
                  SchemeExpr.List(Seq(), None),
                ), None)
              ),
              None
            ) +:
            insns.flatten,
          None
        )
      end if

  private def emitInstruction(scope: VarScope)(insn: Instruction[SchemeExterns]): IO[E, Seq[SchemeExpr]] =
    insn match {
      case Instruction.BuiltinUnary(dest, op, a) =>
        val ra = getRegVar(a)
        val value = op match {
          case BuiltinUnaryOp.IntNegate() => SchemeExpr.call("-", ra)
          case BuiltinUnaryOp.IntBitNot() => SchemeExpr.call("bitwise-not", ra)
        }

        ZIO.succeed(Seq(set(dest, value)))
        

      case Instruction.BuiltinBinary(dest, op, a, b) =>
        val ra = getRegVar(a)
        val rb = getRegVar(b)
        val value = op match {
          case BuiltinBinaryOp.IntAdd() => SchemeExpr.call("+", ra, rb)
          case BuiltinBinaryOp.IntSub() => SchemeExpr.call("-", ra, rb)
          case BuiltinBinaryOp.IntMul() => SchemeExpr.call("*", ra, rb)
          case BuiltinBinaryOp.IntBitAnd() => SchemeExpr.call("bitwise-and", ra, rb)
          case BuiltinBinaryOp.IntBitOr() => SchemeExpr.call("bitwise-or", ra, rb)
          case BuiltinBinaryOp.IntBitXor() => SchemeExpr.call("bitwise-xor", ra, rb)
          case BuiltinBinaryOp.IntBitShiftLeft() => SchemeExpr.call("bitwise-arithmetic-shift", ra, rb)
          case BuiltinBinaryOp.IntBitShiftRight() => SchemeExpr.call("bitwise-arithmetic-shift", ra, SchemeExpr.call("-", rb))
          case BuiltinBinaryOp.IntEq() => SchemeExpr.call("=", ra, rb)
          case BuiltinBinaryOp.IntNe() => SchemeExpr.call("not", SchemeExpr.call("=", ra, rb))
          case BuiltinBinaryOp.IntLt() => SchemeExpr.call("<", ra, rb)
          case BuiltinBinaryOp.IntLe() => SchemeExpr.call("<=", ra, rb)
          case BuiltinBinaryOp.IntGt() => SchemeExpr.call(">", ra, rb)
          case BuiltinBinaryOp.IntGe() => SchemeExpr.call(">=", ra, rb)
          case BuiltinBinaryOp.StringConcat() => SchemeExpr.call("string-concat", ra, rb)
          case BuiltinBinaryOp.StringEq() => SchemeExpr.call("string=?", ra, rb)
          case BuiltinBinaryOp.StringNe() => SchemeExpr.call("not", SchemeExpr.call("string=?", ra, rb))
        }
        

        ZIO.succeed(Seq(set(dest, value)))


      case Instruction.ConstBool(dest, value) => ZIO.succeed(Seq(set(dest, SchemeExpr.Boolean(value))))
      case Instruction.ConstInt(dest, value) => ZIO.succeed(Seq(set(dest, SchemeExpr.Num(SchemeExpr.Number.Integer(value)))))
      case Instruction.ConstString(dest, value) => ZIO.succeed(Seq(set(dest, SchemeExpr.String(value))))
      case Instruction.FunctionCall(dest, function, args) =>
        for
          funcExpr <- function match {
            case ExternRefFunction(library, id) => 
              getImport(library, id)
          }
          
        yield Seq(
          set(
            dest,
            SchemeExpr.List(
              funcExpr +: args.map(getRegVar),
              None
            ),
          ),
        )

      case Instruction.Move(dest, src) => ZIO.succeed(Seq(set(dest, getRegVar(src))))
      case Instruction.Tuple(dest, values) =>
        ZIO.succeed(Seq(
          set(
            dest,
            SchemeExpr.Vector(values.map(getRegVar))
          )
        ))

      case Instruction.TupleElement(dest, elementIndex, src) =>
        ZIO.succeed(Seq(
          set(
            dest,
            SchemeExpr.call(
              "vector-ref",
              getRegVar(src),
              SchemeExpr.Num(SchemeExpr.Number.Integer(elementIndex)),
            ),
          ),
        ))

      case Instruction.Return(src) =>
        ZIO.succeed(Seq(
          SchemeExpr.call("return", getRegVar(src)),
        ))
    }
    
  private def getRegVar(reg: RegisterId): SchemeExpr =
    SchemeExpr.Symbol(s"local_${reg.id}")

  private def set(dest: RegisterId, value: SchemeExpr): SchemeExpr =
    SchemeExpr.call("set!", getRegVar(dest), value)

}

private[scheme] object SchemeTubeEmit {
  final case class ImportedLibrary(index: Int, names: TMap[String, Int])
  private final case class VarScope(varOffset: Int)
}
