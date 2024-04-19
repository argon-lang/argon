package dev.argon.plugin.vm

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}
import dev.argon.expr
import dev.argon.compiler.{
  ErasedSignature as CErasedSignature,
  ErasedSignatureType as CErasedSignatureType,
  ImportSpecifier as CImportSpecifier,
  ModulePath as CModulePath,
  TubeName as CTubeName,
  *
}
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given

import scala.collection.immutable.SortedMap

object VMTubeImpl {
  def make(context: Context)(tube: ArTubeC & HasContext[context.type])
  : UIO[VmTube[context.Env, context.Error, [EI <: ExternalImplementation] =>> EI match {
    case "function" => context.implementations.ExternFunctionImplementation
    case "function-reference" => context.implementations.FunctionReference
      case "record-reference" => context.implementations.RecordReference
  }]] =
    type Externs[EI <: ExternalImplementation] = EI match {
      case "function" => context.implementations.ExternFunctionImplementation
      case "function-reference" => context.implementations.FunctionReference
      case "record-reference" => context.implementations.RecordReference
    }

    import context.{Comp, Env, Error}
    import context.DefaultExprContext.{Expr, Builtin as EBuiltin, ParameterVar, Var, LocalVar}

    final case class Lookup[A, B](
      ids: TMap[A, BigInt],
      lookup: TMap[BigInt, A],
      cache: MemoCacheStore[Env, Error, A, B],
    ) {
      def getId(a: A): UIO[BigInt] =
        ids.getOrElseSTM(a,
          for
            id0 <- ids.size
            id = id0 : BigInt
            _ <- ids.put(a, id)
            _ <- lookup.put(id, a)
          yield id
        ).commit

      def fromId(id: BigInt): UIO[A] =
        lookup.get(id).commit.map(_.get)
    }

    def makeLookup[A, B]: UIO[Lookup[A, B]] =
      for
        ids <- TMap.empty[A, BigInt].commit
        lookup <- TMap.empty[BigInt, A].commit
        cache <- MemoCacheStore.make[Env, Error, A, B]
      yield Lookup(ids, lookup, cache)


    for
      metadataCache <- MemoCell.make[Env, Error, VmTubeMetadata]
      moduleCache <- MemoCacheStore.make[Env, Error, ModulePath, VmModule]
      funcs <- makeLookup[ArFuncC & HasContext[context.type], VmFunction]
      records <- makeLookup[ArRecordC & HasContext[context.type], VmRecord]
    yield new VmTube[Env, Error, Externs] {

      override def metadata(): Comp[VmTubeMetadata] =
        metadataCache.get(ZIO.succeed(VmTubeMetadata(
          modules = tube.modules.keysIterator.map(getModulePath).toSeq,
        )))



      override def getModule(module: ModulePath): Comp[VmModule] =
        moduleCache.usingCreate(module) { modulePath =>
          val module = tube.modules(CModulePath(modulePath.path))
          val eraser = SignatureEraser(context)
          for
            allExports <- module.allExports(Set.empty)
            exports <- ZIO.foreach(allExports.toSeq) { (name, exports) =>
              ZIO.foreach(exports) {
                case ModuleExportC.Function(f) if f.isErased =>
                  f.implementation.sequence.as(Seq.empty)

                case ModuleExportC.Function(f) =>
                  for
                    sig <- f.signature
                    sig <- eraser.eraseSignature(sig)

                    id <- funcs.getId(f)
                  yield Seq(VmModuleExportEntry(
                    name.map(getIdentifier),
                    getErasedSignature(sig),
                    VmModuleExport.Function(id)
                  ))

                case ModuleExportC.Record(r) =>
                  for
                    sig <- r.signature
                    sig <- eraser.eraseSignature(sig)

                    id <- records.getId(r)
                  yield Seq(VmModuleExportEntry(
                    name.map(getIdentifier),
                    getErasedSignature(sig),
                    VmModuleExport.Function(id)
                  ))

                case ModuleExportC.Exported(_) =>
                  ZIO.succeed(Seq())
              }
            }
          yield VmModule(
            exports = exports.flatten.flatten,
          )
        }

      override def getFunctionDefinition(id: BigInt): Comp[VmFunction] =
        funcs.fromId(id).flatMap { func =>
          funcs.cache.usingCreate(func)(createFunc(id))
        }

      override def getFunctionExternImplementation(id: BigInt): Comp[Externs["function"]] =
        funcs.fromId(id)
          .flatMap { func => func.implementation.get }
          .map {
            case context.implementations.FunctionImplementation.Extern(e) => e
            case _ => ???
          }

      override def getFunctionReference(id: BigInt): Comp[Externs["function-reference"]] =
        funcs.fromId(id)
          .flatMap { func => func.reference }

      override def getRecordDefinition(id: BigInt): Comp[VmRecord] =
        records.fromId(id).flatMap { rec =>
          records.cache.usingCreate(rec)(createRecord(id))
        }

      override def getRecordReference(id: BigInt): Comp[Externs["record-reference"]] =
        records.fromId(id)
          .flatMap { rec => rec.reference }


      private def createFunc(id: BigInt)(f: ArFuncC & HasContext[context.type]): Comp[VmFunction] =
        for
          sig <- f.signature
          params <- ZIO.foreach(sig.parameters.filterNot { _.isErased }) { param =>
            exprToType(param.paramType)
          }

          returnType <- exprToType(sig.returnType)

          impl <- f.implementation.sequence
          funcImpl <- ZIO.foreach(impl)(convertImpl(f)(id))

        yield VmFunction(
          parameters = params,
          returnType = returnType,
          implementation = funcImpl,
        )

      private def createRecord(id: BigInt)(r: ArRecordC & HasContext[context.type]): Comp[VmRecord] =
        for
          sig <- r.signature

          params <- ZIO.foreach(sig.parameters.filterNot { _.isErased }) { param =>
            exprToType(param.paramType)
          }

          returnType <- exprToType(sig.returnType)

          fields <- r.fields
          fields <- ZIO.foreach(fields) { field =>
            for
              t <- exprToType(field.fieldType)
            yield VmRecordField(getIdentifier(field.name), t)
          }

        yield VmRecord(
          parameters = params,
          returnType = returnType,
          fields = fields*,
        )

      private def convertImpl(f: ArFuncC & HasContext[context.type])(id: BigInt)(impl: context.implementations.FunctionImplementation): Comp[VmFunctionImplementation] =
        impl match {
          case context.implementations.FunctionImplementation.Expr(e) =>
            for
              sig <- f.signature
              paramVars = context.DefaultSignatureContext.SignatureParameter.getParameterVariables(context.DefaultExprContext.ParameterOwner.Func(f), sig.parameters)
              cfg <- exprToCFG(e, paramVars.filterNot { _.isErased })
            yield VmFunctionImplementation.Instructions(cfg)

          case context.implementations.FunctionImplementation.Extern(_) =>
            ZIO.succeed(VmFunctionImplementation.Extern)
        }


      private def exprToType(e: Expr): Comp[VmType] =
        ArgonEvaluator(context).normalizeToValue(e, context.Config.evaluatorFuel).flatMap {
          case Expr.Builtin(EBuiltin.Nullary(t: expr.BuiltinType)) =>
            ZIO.succeed(VmType.Builtin(getBuiltin(t)))

          case Expr.Builtin(EBuiltin.Unary(t: expr.BuiltinType, a)) =>
            for
              a <- exprToType(a)
            yield VmType.Builtin(getBuiltin(t), a)

          case Expr.Builtin(EBuiltin.Binary(t: expr.BuiltinType, a, b)) =>
            for
              a <- exprToType(a)
              b <- exprToType(b)
            yield VmType.Builtin(getBuiltin(t), a, b)

          case Expr.FunctionType(a, b) =>
            for
              a <- exprToType(a)
              b <- exprToType(b)
            yield VmType.Function(a, b)

          case Expr.RecordType(rec, args) =>
            for
              recId <- records.getId(rec)
              args <- ZIO.foreach(args)(exprToType)
            yield VmType.Record(recId, args*)

          case Expr.Tuple(items) =>
            for
              items <- ZIO.foreach(items)(exprToType)
            yield VmType.Tuple(items*)

          case Expr.TypeN(_) | Expr.TypeBigN(_) =>
            ZIO.succeed(VmType.Type)

          case _ =>
            println(e.getClass)
            ???
        }

      private final case class RegisterWithType(r: Register, t: RegisterType)

      private def exprToRegType(e: Expr): Comp[RegisterType] =
        ArgonEvaluator(context).normalizeToValue(e, context.Config.evaluatorFuel).flatMap {
          case Expr.Builtin(EBuiltin.Nullary(t: expr.BuiltinType)) =>
            ZIO.succeed(RegisterType.Builtin(getBuiltin(t)))

          case Expr.Builtin(EBuiltin.Unary(t: expr.BuiltinType, a)) =>
            for
              a <- exprToRegType(a)
            yield RegisterType.Builtin(getBuiltin(t), a)

          case Expr.Builtin(EBuiltin.Binary(t: expr.BuiltinType, a, b)) =>
            for
              a <- exprToRegType(a)
              b <- exprToRegType(b)
            yield RegisterType.Builtin(getBuiltin(t), a, b)

          case Expr.FunctionType(a, b) =>
            for
              a <- exprToRegType(a)
              b <- exprToRegType(b)
            yield RegisterType.Function(a, b)

          case Expr.RecordType(rec, args) =>
            for
              recId <- records.getId(rec)
              args <- ZIO.foreach(args)(exprToRegType)
            yield RegisterType.Record(recId, args*)


          case Expr.Tuple(items) =>
            for
              items <- ZIO.foreach(items)(exprToRegType)
            yield RegisterType.Tuple(items*)

          case Expr.TypeN(_) | Expr.TypeBigN(_) =>
            ZIO.succeed(RegisterType.Type)

          case _ =>
            println(e.getClass)
            ???
        }

      private def vmTypeToRegType(regArgs: Seq[Register])(t: VmType): RegisterType =
        t match {
          case VmType.Builtin(builtin, args*) =>
            RegisterType.Builtin(builtin, args.map(vmTypeToRegType(regArgs))*)

          case VmType.Record(regId, args*) =>
            RegisterType.Record(regId, args.map(vmTypeToRegType(regArgs))*)

          case VmType.Function(input, output) =>
            val input2 = vmTypeToRegType(regArgs)(input)
            val output2 = vmTypeToRegType(regArgs)(output)
            RegisterType.Function(input2, output2)

          case VmType.Tuple(args*) =>
            RegisterType.Tuple(args.map(vmTypeToRegType(regArgs))*)

          case VmType.Type =>
            RegisterType.Type

          case VmType.Param(i) =>
            assert(i >= Int.MinValue && i <= Int.MaxValue)

            RegisterType.Reg(regArgs(i.toInt))
        }

      private def exprToCFG(expr: Expr, params: Seq[ParameterVar]): Comp[ControlFlowGraph] =
        for
          nextBlockId <- Ref.make[BigInt](1)
          blocks <- Ref.make(SortedMap.empty[BigInt, InstructionBlock])
          nextRegId <- Ref.make[BigInt](0)
          variables <- Ref.make(Map.empty[Var, Register])
          registers <- Ref.make(Seq.empty[RegisterDeclaration])
          currentBlockId <- Ref.make[BigInt](0)
          instructions <- Ref.make(Seq.empty[Instruction])

          emitter = CFGEmitter(
            nextBlockId = nextBlockId,
            blocks = blocks,
            nextRegId = nextRegId,
            variables = variables,
            registers = registers,
            currentBlockId = currentBlockId,
            instructions = instructions,
          )

          _ <- emitter.initParams(params)
          _ <- emitter.emitExprTail(expr)
          cfg <- emitter.toCFG
        yield cfg

      private final class CFGEmitter(
        nextBlockId: Ref[BigInt],
        blocks: Ref[SortedMap[BigInt, InstructionBlock]],
        nextRegId: Ref[BigInt],
        variables: Ref[Map[Var, Register]],
        registers: Ref[Seq[RegisterDeclaration]],
        currentBlockId: Ref[BigInt],
        instructions: Ref[Seq[Instruction]],
      ) {
        def initParams(params: Seq[ParameterVar]): Comp[Unit] =
          ZIO.foreachDiscard(params) { param =>
            for
              t <- exprToRegType(param.varType)
              regId <- nextRegId.getAndUpdate(_ + 1)
              
              _ <- variables.update(_.updated(param, Register.Reg(regId)))
            yield ()
          }

        def emitExpr(e: Expr): Comp[RegisterWithType] =
          e match {
            case Expr.BindVariable(v, value) =>
              for
                varReg <- emitExpr(value)
                _ <- variables.update(_.updated(v, varReg.r))

                t = RegisterType.Tuple()
                reg <- makeRegister(t)
                _ <- append(Instruction.CreateTuple(reg))
              yield RegisterWithType(reg, t)

            case Expr.BoolLiteral(b) =>
              val t = RegisterType.Builtin(Builtin.BoolType)
              for
                reg <- makeRegister(t)
                _ <- append(Instruction.LoadBool(reg, b))
              yield RegisterWithType(reg, t)

            case Expr.Builtin(EBuiltin.Nullary(builtin)) =>
              val t = builtin match {
                case expr.NullaryBuiltin.IntType |
                     expr.NullaryBuiltin.BoolType |
                     expr.NullaryBuiltin.StringType |
                     expr.NullaryBuiltin.NeverType =>
                  RegisterType.Type
              }
              for
                reg <- makeRegister(t)
                _ <- append(Instruction.LoadBuiltin(reg, getBuiltin(builtin)))
              yield RegisterWithType(reg, t)

            case Expr.Builtin(EBuiltin.Unary(builtin, a)) =>
              val t = builtin match {
                case expr.UnaryBuiltin.IntNegate |
                     expr.UnaryBuiltin.IntBitNot =>
                  RegisterType.Builtin(Builtin.IntType)
              }

              for
                aReg <- emitExpr(a)
                reg <- makeRegister(t)
                _ <- append(Instruction.LoadBuiltin(reg, getBuiltin(builtin), aReg.r))
              yield RegisterWithType(reg, t)

            case Expr.Builtin(EBuiltin.Binary(builtin, a, b)) =>
              val t = builtin match {
                case expr.BinaryBuiltin.ConjunctionType |
                     expr.BinaryBuiltin.DisjunctionType =>
                  ???

                case expr.BinaryBuiltin.IntAdd |
                     expr.BinaryBuiltin.IntSub |
                     expr.BinaryBuiltin.IntMul |
                     expr.BinaryBuiltin.IntBitAnd |
                     expr.BinaryBuiltin.IntBitOr |
                     expr.BinaryBuiltin.IntBitXOr |
                     expr.BinaryBuiltin.IntBitShiftLeft |
                     expr.BinaryBuiltin.IntBitShiftRight =>
                  RegisterType.Builtin(Builtin.IntType)


                case expr.BinaryBuiltin.IntEQ |
                     expr.BinaryBuiltin.IntNE |
                     expr.BinaryBuiltin.IntLT |
                     expr.BinaryBuiltin.IntLE |
                     expr.BinaryBuiltin.IntGT |
                     expr.BinaryBuiltin.IntGE |
                     expr.BinaryBuiltin.StringEQ |
                     expr.BinaryBuiltin.StringNE =>
                  RegisterType.Builtin(Builtin.BoolType)

                case expr.BinaryBuiltin.StringConcat =>
                  RegisterType.Builtin(Builtin.StringType)
              }

              for
                aReg <- emitExpr(a)
                bReg <- emitExpr(b)
                reg <- makeRegister(t)
                _ <- append(Instruction.LoadBuiltin(reg, getBuiltin(builtin), aReg.r, bReg.r))
              yield RegisterWithType(reg, t)

            case Expr.FunctionCall(f, args) =>
              for
                call <- emitFunctionCall(f, args)
                sig <- f.signature
                returnType <- exprToType(sig.returnType)

                t = vmTypeToRegType(call.args)(returnType)
                reg <- makeRegister(t)
                _ <- append(Instruction.Call(InstructionResult.Value(reg), call))
              yield RegisterWithType(reg, t)

            case Expr.IfElse(_, _, cond, whenTrue, whenFalse) =>
              for
                condReg <- emitExpr(cond)

                whenTrueLabel <- nextLabel
                whenFalseLabel <- nextLabel
                endLabel <- nextLabel

                _ <- generateBlock(BranchInstruction.JumpIf(condReg.r, whenTrueLabel, whenFalseLabel))

                _ <- startBlock(whenTrueLabel)
                trueReg <- emitExpr(whenTrue)
                _ <- generateBlock(BranchInstruction.Jump(endLabel))

                _ <- startBlock(whenFalseLabel)
                falseReg <- emitExpr(whenFalse)
                _ <- append(Instruction.Move(trueReg.r, falseReg.r))
                _ <- generateBlock(BranchInstruction.Jump(endLabel))

                _ <- startBlock(endLabel)

              yield trueReg

            case Expr.IntLiteral(i) =>
              val t = RegisterType.Builtin(Builtin.IntType)
              for
                reg <- makeRegister(t)
                _ <- append(Instruction.LoadInt(reg, i))
              yield RegisterWithType(reg, t)

            case Expr.RecordLiteral(rec, fields) =>
              for
                recordId <- records.getId(rec.record)
                regType <- exprToRegType(rec)
                recValue <- emitExpr(rec)
                reg <- makeRegister(regType)

                recFields <- rec.record.fields

                fieldValues <- ZIO.foreach(recFields.map(recField => fields.find(_.name == recField.name).get)) { fieldLit =>
                  emitExpr(fieldLit.value).map(_.r)
                }

                _ <- append(Instruction.CreateRecord(reg, recordId, recValue.r, fieldValues*))
              yield RegisterWithType(reg, regType)

            case Expr.RecordFieldLoad(record, field, recordValue) =>
              for
                recordId <- records.getId(record.record)
                recordType <- emitExpr(record)

                fields <- record.record.fields
                fieldIndex = fields.indexOf(field)

                recordValue <- emitExpr(recordValue)

                fieldSig <- context.DefaultSignatureContext.recordFieldSig(record, field)
                fieldType <- exprToRegType(fieldSig.returnType)
                res <- makeRegister(fieldType)
                _ <- append(Instruction.LoadRecordField(res, recordId, recordType.r, fieldIndex, recordValue.r))

              yield RegisterWithType(res, fieldType)

            case Expr.RecordType(rec, args) =>
              for
                recId <- records.getId(rec)
                args <- ZIO.foreach(args)(emitExpr(_).map { _.r })

                reg <- makeRegister(RegisterType.Type)
                _ <- append(Instruction.RecordType(reg, recId, args*))

              yield RegisterWithType(reg, RegisterType.Type)

            case Expr.Sequence(stmts, result) =>
              for
                _ <- ZIO.foreach(stmts)(emitExpr)
                r <- emitExpr(result)
              yield r

            case Expr.StringLiteral(s) =>
              val t = RegisterType.Builtin(Builtin.StringType)
              for
                reg <- makeRegister(t)
                _ <- append(Instruction.LoadString(reg, s))
              yield RegisterWithType(reg, t)

            case Expr.Tuple(items) =>
              for
                itemRegs <- ZIO.foreach(items)(emitExpr)
                tupleType = RegisterType.Tuple(itemRegs.map(_.t)*)
                reg <- makeRegister(tupleType)
                _ <- append(Instruction.CreateTuple(reg, itemRegs.map(_.r)*))
              yield RegisterWithType(reg, tupleType)

            case Expr.TupleElement(index, tuple) =>
              for
                tupleReg <- emitExpr(tuple)
                itemType = tupleReg.t match {
                  case dev.argon.plugin.vm.RegisterType.Tuple(itemTypes*) =>
                    itemTypes(index)
                    
                  case _ => ???
                }

                reg <- makeRegister(itemType)
                _ <- append(Instruction.TupleElement(reg, tupleReg.r, index))
              yield RegisterWithType(reg, itemType)

            case Expr.Variable(v) =>
              for
                vars <- variables.get
                t <- exprToRegType(v.varType)
              yield RegisterWithType(vars(v), t) 

            case _ =>
              println(e.getClass)
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

                whenTrueLabel <- nextLabel
                whenFalseLabel <- nextLabel

                _ <- generateBlock(BranchInstruction.JumpIf(condReg.r, whenTrueLabel, whenFalseLabel))

                _ <- startBlock(whenTrueLabel)
                _ <- emitExprTail(whenTrue)

                _ <- startBlock(whenFalseLabel)
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
                _ <- generateBlock(BranchInstruction.Return(reg.r))
              yield ()
          }

        def toCFG: Comp[ControlFlowGraph] =
          for
            registers <- registers.get
            blocks <- blocks.get
          yield ControlFlowGraph(registers, blocks.values.toSeq)

        private def makeRegister(t: RegisterType): Comp[Register] =
          for
            regId <- nextRegId.getAndUpdate(_ + 1)
            _ <- registers.update(_ :+ RegisterDeclaration(t))
          yield Register.Reg(regId)

        private def nextLabel: UIO[BigInt] =
          nextBlockId.getAndUpdate(_ + 1)

        private def append(instruction: Instruction): Comp[Unit] =
          instructions.update(_ :+ instruction)

        private def generateBlock(branch: BranchInstruction): Comp[Unit] =
          for
            blockId <- currentBlockId.get
            insns <- instructions.get
            _ <- blocks.update(_.updated(blockId, InstructionBlock(insns, branch)))
          yield ()

        private def startBlock(blockId: BigInt): Comp[Unit] =
          for
            _ <- currentBlockId.set(blockId)
            _ <- instructions.set(Seq.empty)
          yield ()

        private def tailCall(call: Comp[FunctionCall]): Comp[Unit] =
          for
            call <- call
            _ <- generateBlock(BranchInstruction.ReturnCall(call))
          yield ()

        private def emitFunctionCall(f: ArFuncC & HasContext[context.type], args: Seq[Expr]): Comp[FunctionCall.Function] =
          for
            id <- funcs.getId(f)
            argRegs <- ZIO.foreach(args)(emitExpr)
          yield FunctionCall.Function(id, argRegs.map(_.r)*)

        private def emitFunctionObjectCall(f: Expr, a: Expr): Comp[(FunctionCall.FunctionObject, RegisterType)] =
          for
            fReg <- emitExpr(f)
            aReg <- emitExpr(a)

            resType = fReg.t match {
              case RegisterType.Function(_, output) => output
              case _ => ???
            }

          yield (FunctionCall.FunctionObject(fReg.r, aReg.r), resType)
      }


    }
  end make


  def getIdentifier(id: IdentifierExpr): Identifier =
    id match {
      case IdentifierExpr.Named(name) => Identifier.Named(name)
      case IdentifierExpr.Op(op: ast.UnaryOperator) =>
        val op2 = op match {
          case ast.UnaryOperator.Plus => UnaryOperatorName.Plus
          case ast.UnaryOperator.Minus => UnaryOperatorName.Minus
          case ast.UnaryOperator.BitNot => UnaryOperatorName.BitNot
          case ast.UnaryOperator.LogicalNot => UnaryOperatorName.LogicalNot
        }

        Identifier.UnaryOperator(op2)

      case IdentifierExpr.Op(op: ast.BinaryOperator) =>
        val op2 = op match {
          case ast.BinaryOperator.Plus => BinaryOperatorName.Plus
          case ast.BinaryOperator.Minus => BinaryOperatorName.Minus
          case ast.BinaryOperator.Mul => BinaryOperatorName.Mul
          case ast.BinaryOperator.Div => BinaryOperatorName.Div
          case ast.BinaryOperator.Equal => BinaryOperatorName.Equal
          case ast.BinaryOperator.NotEqual => BinaryOperatorName.NotEqual
          case ast.BinaryOperator.LessThan => BinaryOperatorName.LessThan
          case ast.BinaryOperator.LessThanEq => BinaryOperatorName.LessThanEq
          case ast.BinaryOperator.GreaterThan => BinaryOperatorName.GreaterThan
          case ast.BinaryOperator.GreaterThanEq => BinaryOperatorName.GreaterThanEq
          case ast.BinaryOperator.BitOr => BinaryOperatorName.BitOr
          case ast.BinaryOperator.BitXOr => BinaryOperatorName.BitXor
          case ast.BinaryOperator.BitAnd => BinaryOperatorName.BitAnd
          case ast.BinaryOperator.ShiftLeft => BinaryOperatorName.ShiftLeft
          case ast.BinaryOperator.ShiftRight => BinaryOperatorName.ShiftRight
          case ast.BinaryOperator.Concat => BinaryOperatorName.Concat
        }

        Identifier.BinaryOperator(op2)

      case IdentifierExpr.Extension(inner) =>
        Identifier.Extension(getIdentifier(inner))

      case IdentifierExpr.Inverse(inner) =>
        Identifier.Inverse(getIdentifier(inner))

      case IdentifierExpr.Update(inner) =>
        Identifier.Update(getIdentifier(inner))

    }

  def getTubeName(name: CTubeName): TubeName =
    TubeName(name.parts.head, name.parts.tail*)

  def getModulePath(path: CModulePath): ModulePath =
    ModulePath(path.parts*)

  def getErasedSignature(sig: CErasedSignature): ErasedSignature =
    ErasedSignature(sig.params.map(getErasedSignatureType), getErasedSignatureType(sig.result))

  def getErasedSignatureType(t: CErasedSignatureType): ErasedType =
    t match {
      case CErasedSignatureType.Builtin(builtin, args) =>
        ErasedType.Builtin(getBuiltin(builtin), args.map(getErasedSignatureType)*)

      case CErasedSignatureType.Function(input, output) =>
        ErasedType.Function(getErasedSignatureType(input), getErasedSignatureType(output))

      case CErasedSignatureType.Record(recordImport, args) =>
        ErasedType.Record(getImportSpecifier(recordImport), args.map(getErasedSignatureType)*)

      case CErasedSignatureType.Tuple(items) =>
        ErasedType.Tuple(items.map(getErasedSignatureType) *)

      case CErasedSignatureType.Erased =>
        ErasedType.Erased
    }

  def getImportSpecifier(imp: CImportSpecifier): ImportSpecifier =
    ImportSpecifier(
      tube = getTubeName(imp.tube),
      module = getModulePath(imp.module),
      name = imp.name.map(getIdentifier),
      signature = getErasedSignature(imp.signature), 
    )
  
  def getBuiltin(e: expr.BuiltinBase): Builtin =
    e match {
      case expr.NullaryBuiltin.IntType => Builtin.IntType
      case expr.NullaryBuiltin.BoolType => Builtin.BoolType
      case expr.NullaryBuiltin.StringType => Builtin.StringType
      case expr.NullaryBuiltin.NeverType => Builtin.NeverType

      case expr.UnaryBuiltin.IntNegate => Builtin.IntNegate
      case expr.UnaryBuiltin.IntBitNot => Builtin.IntBitNot

      case expr.BinaryBuiltin.ConjunctionType => Builtin.ConjunctionType
      case expr.BinaryBuiltin.DisjunctionType => Builtin.DisjunctionType
      case expr.BinaryBuiltin.IntAdd => Builtin.IntAdd
      case expr.BinaryBuiltin.IntSub => Builtin.IntSub
      case expr.BinaryBuiltin.IntMul => Builtin.IntMul
      case expr.BinaryBuiltin.IntBitAnd => Builtin.IntBitAnd
      case expr.BinaryBuiltin.IntBitOr => Builtin.IntBitOr
      case expr.BinaryBuiltin.IntBitXOr => Builtin.IntBitXor
      case expr.BinaryBuiltin.IntBitShiftLeft => Builtin.IntBitShiftLeft
      case expr.BinaryBuiltin.IntBitShiftRight => Builtin.IntBitShiftRight
      case expr.BinaryBuiltin.IntEQ => Builtin.IntEq
      case expr.BinaryBuiltin.IntNE => Builtin.IntNe
      case expr.BinaryBuiltin.IntLT => Builtin.IntLt
      case expr.BinaryBuiltin.IntLE => Builtin.IntLe
      case expr.BinaryBuiltin.IntGT => Builtin.IntGt
      case expr.BinaryBuiltin.IntGE => Builtin.IntGe
      case expr.BinaryBuiltin.StringConcat => Builtin.StringConcat
      case expr.BinaryBuiltin.StringEQ => Builtin.StringEq
      case expr.BinaryBuiltin.StringNE => Builtin.StringNe
    }
  
}