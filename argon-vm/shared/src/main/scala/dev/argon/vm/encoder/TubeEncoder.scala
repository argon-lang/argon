package dev.argon.vm.encoder

import dev.argon.compiler as c

import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.encoder.TubeEncoderBase
import dev.argon.vm.*


import zio.*
import zio.stream.*
import zio.stm.{ZSTM, USTM, TMap, TRef}
import dev.argon.ast
import dev.argon.compiler.{SignatureEraser, HasContext, ArgonEvaluator}
import dev.argon.expr.{NullaryBuiltin, UnaryBuiltin, BinaryBuiltin}


private[vm] class TubeEncoder(platformId: String) extends TubeEncoderBase[TubeFileEntry] {
  override def createEmitter(state: EncodeState): state.Emitter =
    new state.Emitter {
      import state.*
      import context.DefaultExprContext.Expr as ArExpr
  
      def emitTube: Comp[Unit] =
        val orderedTubes = tube.referencedTubes.toSeq
        val orderedModules = tube.modules.values.toSeq

        val header =
          TubeHeader(
            formatVersionMajor = 0,
            formatVersionMinor = 0,
          )

        def emitMetadata: Comp[TubeMetadata] =
          for
            modules <- emitModules(orderedModules)
          yield TubeMetadata(
            name = encodeTubeName(tube.name),
            referencedTubes = orderedTubes.map(encodeTubeName),
            platformMetadata = tube.metadata._2.get(platformId),
            modules = modules,
          )
        
        emitEntryBuilder(ZIO.succeed(TubeFileEntry.Header(header))) *>
          ZIO.foreachDiscard(orderedTubes.zipWithIndex) { (name, tubeIndex) =>
              assignTubeId(name, tubeIndex + 1)
          } *>
          ZIO.foreachDiscard(orderedModules.zipWithIndex) { (module, moduleIndex) =>
              assignModuleId(c.ModuleName(tube.name, module.path), moduleIndex)
          } *>
          emitEntryBuilder(emitMetadata.map(TubeFileEntry.Metadata.apply))
      end emitTube

      private def encodeTubeName(name: c.TubeName): TubeName =
        TubeName(name.parts.head, name.parts.tail)

      private def encodeModulePath(path: c.ModulePath): ModulePath =
        ModulePath(path.parts)

      private def encodeIdentifier(id: ast.IdentifierExpr): Identifier =
        id match {
          case ast.IdentifierExpr.Named(s) => Identifier.Named(s)
          case ast.IdentifierExpr.Op(op: (ast.BinaryOperator & ast.Operator.ValidIdentifier)) =>
            val op2 = op match {
              case ast.BinaryOperator.Plus => BinaryOperator.Plus
              case ast.BinaryOperator.Minus => BinaryOperator.Minus
              case ast.BinaryOperator.Mul => BinaryOperator.Mul
              case ast.BinaryOperator.Div => BinaryOperator.Div
              case ast.BinaryOperator.Equal => BinaryOperator.Equal
              case ast.BinaryOperator.NotEqual => BinaryOperator.NotEqual
              case ast.BinaryOperator.LessThan => BinaryOperator.LessThan
              case ast.BinaryOperator.LessThanEq => BinaryOperator.LessThanEq
              case ast.BinaryOperator.GreaterThan => BinaryOperator.GreaterThan
              case ast.BinaryOperator.GreaterThanEq => BinaryOperator.GreaterThanEq
              case ast.BinaryOperator.BitOr => BinaryOperator.BitOr
              case ast.BinaryOperator.BitXOr => BinaryOperator.BitXor
              case ast.BinaryOperator.BitAnd => BinaryOperator.BitAnd
              case ast.BinaryOperator.ShiftLeft => BinaryOperator.ShiftLeft
              case ast.BinaryOperator.ShiftRight => BinaryOperator.ShiftRight
              case ast.BinaryOperator.Concat => BinaryOperator.Concat
            }

            Identifier.BinOp(op2)
            
          case ast.IdentifierExpr.Op(op: ast.UnaryOperator) => 
            val op2 = op match {
              case ast.UnaryOperator.Plus => UnaryOperator.Plus
              case ast.UnaryOperator.Minus => UnaryOperator.Minus
              case ast.UnaryOperator.BitNot => UnaryOperator.BitNot
              case ast.UnaryOperator.LogicalNot => UnaryOperator.LogicalNot
            }

            Identifier.UnOp(op2)

          case ast.IdentifierExpr.Extension(inner) =>
            Identifier.Extension(encodeIdentifier(inner))

          case ast.IdentifierExpr.Inverse(inner) =>
            Identifier.Inverse(encodeIdentifier(inner))

          case ast.IdentifierExpr.Update(inner) =>
            Identifier.Update(encodeIdentifier(inner))
        }

      private def encodeImportSpecifier(specifier: c.ImportSpecifier): UIO[ImportSpecifier] =
        for
          moduleId <- getModuleId(c.ModuleName(specifier.tube, specifier.module))
          sig <- encodeErasedSignature(specifier.signature)
        yield ImportSpecifier(
          moduleId = moduleId,
          name = specifier.name.map(encodeIdentifier),
          sig = sig
        )

      private def encodeErasedSignature(sig: c.ErasedSignature): UIO[ErasedSignature] =
        for
          paramTypes <- ZIO.foreach(sig.params)(encodeErasedSignatureType)
          resultType <- encodeErasedSignatureType(sig.result)
        yield ErasedSignature(paramTypes, resultType)

      private def encodeErasedSignatureType(t: c.ErasedSignatureType): UIO[ErasedSignatureType] =
        t match {
          case c.ErasedSignatureType.Builtin(builtin, args) =>
            val builtin2 = builtin match
              case dev.argon.expr.NullaryBuiltin.IntType => BuiltinType.Int()
              case dev.argon.expr.NullaryBuiltin.BoolType => BuiltinType.Bool()
              case dev.argon.expr.NullaryBuiltin.StringType => BuiltinType.String()
              case dev.argon.expr.NullaryBuiltin.NeverType => BuiltinType.Never()
              case dev.argon.expr.BinaryBuiltin.ConjunctionType => BuiltinType.Conjunction()
              case dev.argon.expr.BinaryBuiltin.DisjunctionType => BuiltinType.Disjunction()

            for
              args <- ZIO.foreach(args)(encodeErasedSignatureType)
            yield ErasedSignatureType.Builtin(builtin2, args)

            

          case c.ErasedSignatureType.Function(input, output) =>
            for
              input <- encodeErasedSignatureType(input)
              output <- encodeErasedSignatureType(output)
            yield ErasedSignatureType.Function(input, output)

          case c.ErasedSignatureType.Record(recordImport, args) =>
            for
              recordImport <- encodeImportSpecifier(recordImport)
              args <- ZIO.foreach(args)(encodeErasedSignatureType)
            yield ErasedSignatureType.Record(recordImport, args)

          case c.ErasedSignatureType.Tuple(elements) =>
            for
              elements <- ZIO.foreach(elements)(encodeErasedSignatureType)
            yield ErasedSignatureType.Tuple(elements)

          case c.ErasedSignatureType.Erased =>
            ZIO.succeed(ErasedSignatureType.Erased())
        }
        

      private def emitModules(orderedModules: Seq[ArModule]): Comp[Seq[Module]] =
        ZIO.foreach(orderedModules.zipWithIndex) {
          case (module, index) =>
            for
              modExports <- module.allExports(Set.empty)
              _ <- ZIO.foreachDiscard(modExports.toSeq) { (name, items) =>
                ZIO.foreachDiscard(items)(emitModuleExport)
              }
            yield Module(
              path = encodeModulePath(module.path),
            )
        }

      private def emitModuleExport(exp: ModuleExport): Comp[Unit] =
        exp match {
          case c.ModuleExportC.Function(f) =>
            getFunctionId(f).unit.whenDiscard(!f.isErased)

          case c.ModuleExportC.Record(r) =>
            getRecordId(r).unit

          case c.ModuleExportC.Exported(exp) =>
            ZIO.unit
        }

      override def emitModuleReference(moduleName: c.ModuleName, id: BigInt): Comp[TubeFileEntry] =
        for
          tubeId <- getTubeId(moduleName.tubeName)
        yield TubeFileEntry.ModuleReference(
          moduleId = id,
          tubeId = tubeId,
          path = encodeModulePath(moduleName.path),
        )
          
      private def importOrDefine[A]
      (value: A, makeSpecifier: Comp[c.ImportSpecifier])
      (emitDef: A => Comp[TubeFileEntry], emitRef: ImportSpecifier => Comp[TubeFileEntry])
      : Comp[TubeFileEntry] =
        makeSpecifier.flatMap { specifier =>
          if specifier.tube == tube.name then
            emitDef(value)
          else
            encodeImportSpecifier(specifier).flatMap(emitRef)
        }
        

      override def emitFunction(func: ArFunc, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(func, func.importSpecifier)(emitFunctionDef(id), emitFunctionRef(id))

      private def emitFunctionDef(id: BigInt)(func: ArFunc): Comp[TubeFileEntry] =
        for
          sig <- func.signature
          encSig <- emitFunctionSignature(sig)

          importSpec <- func.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          impl <- ZIO.foreach(func.implementation) { impl =>
            impl.flatMap {
              case context.implementations.FunctionImplementation.Expr(e) =>
                val paramVars = SignatureParameter.getParameterVariables(
                  context.DefaultExprContext.ParameterOwner.Func(func),
                  sig.parameters
                )
                for
                  block <- emitFunctionBody(e, paramVars)
                yield FunctionImplementation.VmIr(block)

              case context.implementations.FunctionImplementation.Extern(externMap) =>
                ZIO.fromEither(
                  externMap.externs.dict.get(platformId)
                    .toRight(new TubeFormatException("Missing extern implementation for platform " + platformId))
                )
                  .map(FunctionImplementation.Extern.apply)
            }
          }

        yield TubeFileEntry.FunctionDefinition(
          FunctionDefinition(
            functionId = id,
            `import` = importSpec,
            signature = encSig,
            implementation = impl,
          )
        )

      private def emitFunctionRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
        ZIO.succeed(TubeFileEntry.FunctionReference(
          functionId = id,
          `import` = specifier,
        ))

      override def emitRecord(rec: ArRecord, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(rec, rec.importSpecifier)(emitRecordDef(id), emitRecordRef(id))

      private def emitRecordDef(id: BigInt)(rec: ArRecord): Comp[TubeFileEntry] =
        for
          sig <- rec.signature
          sig <- emitFunctionSignature(sig)

          importSpec <- rec.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          fields <- rec.fields
          fields <- ZIO.foreach(fields) { field =>
            for
              t <- emitType(field.fieldType)
              recordId <- getRecordId(field.owningRecord)
            yield RecordFieldDefinition(
              name = encodeIdentifier(field.name),
              fieldType = t,
            )
          }

        yield TubeFileEntry.RecordDefinition(
          RecordDefinition(
            recordId = id,
            `import` = importSpec,
            signature = sig,
            fields = fields,
          )
        )

      private def emitRecordRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
        ZIO.succeed(TubeFileEntry.RecordReference(
          recordId = id,
          `import` = specifier,
        ))

      override def emitRecordFieldInfo(field: RecordField, id: BigInt): Comp[TubeFileEntry] =
        for
          recordId <- getRecordId(field.owningRecord)
        yield TubeFileEntry.RecordFieldReference(
          recordFieldId = id,
          recordId = id,
          name = encodeIdentifier(field.name),
        )

      private def emitFunctionSignature(sig: FunctionSignature): Comp[dev.argon.vm.FunctionSignature] =
        for
          params <- ZIO.foreach(sig.parameters.filterNot(_.isErased))(emitSignatureParam)
          returnType <- emitType(sig.returnType)
          ensuresClauses <- ZIO.foreach(sig.ensuresClauses)(emitType)
        yield dev.argon.vm.FunctionSignature(
          parameters = params,
          returnType = returnType,
        )

      private def emitSignatureParam(param: SignatureParameter): Comp[dev.argon.vm.SignatureParameter] =
        for
          t <- emitType(param.paramType)
        yield dev.argon.vm.SignatureParameter(
          name = param.name.map(encodeIdentifier),
          paramType = t,
        )

      private def emitType(t: ArExpr): Comp[VmType] =
        ArgonEvaluator(context).normalizeToValue(t, context.Config.evaluatorFuel).flatMap {
          case ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin)) =>
            ZIO.succeed(
              builtin match {
                case NullaryBuiltin.BoolType => VmType.Builtin(BuiltinType.Bool(), Seq())
                case NullaryBuiltin.IntType => VmType.Builtin(BuiltinType.Int(), Seq())
                case NullaryBuiltin.StringType => VmType.Builtin(BuiltinType.String(), Seq())
                case NullaryBuiltin.NeverType => VmType.Builtin(BuiltinType.Never(), Seq())
              }
            )

          case ArExpr.Tuple(items) =>
            ZIO.foreach(items)(emitType)
              .map(VmType.Tuple.apply)

          case ArExpr.TypeN(_) =>
            ZIO.succeed(VmType.TypeInfo())

          case t =>
            ZIO.logError("Unimplemented type expression: " + t.getClass).as(???)
        }

      private def emitFunctionBody(e: ArExpr, parameters: Seq[context.DefaultExprContext.Var]): Comp[FunctionBody] =
        for
          knownVars <- TMap.make[context.DefaultExprContext.Var, RegisterId](parameters.zipWithIndex.map { (param, index) => param -> RegisterId(index) }*).commit
          variables <- TRef.make(Seq.empty[VariableDefinition]).commit
          instructions <- TRef.make(Seq.empty[Instruction]).commit

          emitter = ExprEmitter(
            varOffset = parameters.size,
            knownVars = knownVars,
            variables = variables,
            instructions = instructions,
          )

          _ <- emitter.exprReturn(e)
          res <- emitter.toFunctionBody
        yield res

      private sealed trait ExprOutput derives CanEqual {
        type ResultType
      }
      private object ExprOutput {
        sealed trait KnownLocation extends ExprOutput {
          type ResultType = Unit
        }

        final case class Register(reg: RegisterId) extends KnownLocation
        case object Discard extends KnownLocation
        case object Return extends KnownLocation

        case object AnyRegister extends ExprOutput {
          type ResultType = RegisterId
        }
      }

      private final class ExprEmitter(
        varOffset: Int,
        knownVars: TMap[context.DefaultExprContext.Var, RegisterId],
        variables: TRef[Seq[VariableDefinition]],
        instructions: TRef[Seq[Instruction]],
      ) {
        private def nestedScope: Comp[ExprEmitter] =
          for
            knownVars <- knownVars.toMap.flatMap(kv => TMap.make(kv.toSeq*)).commit
            instructions <- TRef.make(Seq.empty[Instruction]).commit
          yield ExprEmitter(
            varOffset = varOffset,
            knownVars = knownVars,
            variables = variables,
            instructions = instructions,
          )

        private def nestedBlock[A](f: ExprEmitter => Comp[Unit]): Comp[Block] =
          nestedScope.tap(f).flatMap(_.toBlock)

        private def declareVar(v: context.DefaultExprContext.LocalVar): Comp[RegisterId] =
          for
            t <- emitType(v.varType)
            id <- addVar(t).tap { id => knownVars.put(v, id) }.commit
          yield id

        private def addVar(t: VmType): USTM[RegisterId] =
          for
            size <- variables.get.map(_.size)
            id = size + varOffset
            _ <- variables.update(_ :+ VariableDefinition(t))
          yield RegisterId(id)


        private def getVar(v: context.DefaultExprContext.Var): Comp[RegisterId] =
          for
            indexOpt <- knownVars.get(v).commit
            r <- ZIO.succeed(indexOpt.get)
          yield r
          
        def toBlock: Comp[Block] =
          for
            insns <- instructions.get.commit
          yield Block(
            instructions = insns,
          )
          
        def toFunctionBody: Comp[FunctionBody] =
          for
            vars <- variables.get.commit
            block <- toBlock
          yield FunctionBody(
            variables = vars,
            block = block,
          )

        private def emit(insn: Instruction): Comp[Unit] =
          instructions.update(_ :+ insn).commit
        

        private def knownLocation(e: ArExpr, output: ExprOutput)(f: ExprOutput.KnownLocation => Comp[Unit]): Comp[output.ResultType] =
          (output : ExprOutput & output.type) match {
            case o: (ExprOutput.KnownLocation & output.type) => f(o) : Comp[o.ResultType]
            case o: (ExprOutput.AnyRegister.type & output.type) =>
              for
                t <- getExprType(e)
                vmType <- emitType(t)
                r <- addVar(vmType).commit
                _ <- f(ExprOutput.Register(r))
              yield r : o.ResultType
          }

        private def existingRegister(output: ExprOutput)(res: => Comp[RegisterId]): Comp[output.ResultType] =
          (output : ExprOutput & output.type) match {
            case o: (ExprOutput.Register & output.type) =>
              for
                r <- res
                _ <- emit(Instruction.Move(o.reg, r))
              yield () : o.ResultType

            case o: (ExprOutput.Discard.type & output.type) =>
              res.unit : Comp[o.ResultType]

            case o: (ExprOutput.Return.type & output.type) =>
              for
                r <- res
                _ <- emit(Instruction.Return(r))
              yield () : o.ResultType

            case o: (ExprOutput.AnyRegister.type & output.type) =>
              res : Comp[o.ResultType]
          }

        private def intoRegister(e: ArExpr, output: ExprOutput)(f: RegisterId => Comp[Unit]): Comp[output.ResultType] =
          (output : ExprOutput & output.type) match {
            case o: (ExprOutput.Register & output.type) =>
              f(o.reg) : Comp[o.ResultType]

            case _ =>
              getExprType(e)
                .flatMap(emitType)
                .flatMap(t => addVar(t).commit)
                .flatMap { r =>
                  existingRegister(output)(f(r).as(r))
                }
          }

        private def functionResult(e: ArExpr, output: ExprOutput)(f: FunctionResult => Comp[Unit]): Comp[output.ResultType] =
          knownLocation(e, output) { location =>
            val functionResult = location match {
              case ExprOutput.Register(reg) => FunctionResult.Register(reg)
              case ExprOutput.Discard => FunctionResult.Discard()
              case ExprOutput.Return => FunctionResult.ReturnValue()
            }

            f(functionResult)
          }


        private def expr(e: ArExpr, output: ExprOutput): Comp[output.ResultType] =
          e match {
            case ArExpr.BoolLiteral(b) =>
              intoRegister(e, output) { r =>
                emit(Instruction.ConstBool(r, b))
              }

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Binary(builtin, a, b)) =>
              intoRegister(e, output) { r =>
                for
                  ar <- expr(a, ExprOutput.AnyRegister)
                  br <- expr(b, ExprOutput.AnyRegister)
                  op = builtin match {
                    case BinaryBuiltin.IntAdd => BuiltinBinaryOp.IntAdd
                    case BinaryBuiltin.IntSub => BuiltinBinaryOp.IntSub
                    case BinaryBuiltin.IntMul => BuiltinBinaryOp.IntMul
                    case BinaryBuiltin.IntBitAnd => BuiltinBinaryOp.IntBitAnd
                    case BinaryBuiltin.IntBitOr => BuiltinBinaryOp.IntBitOr
                    case BinaryBuiltin.IntBitXOr => BuiltinBinaryOp.IntBitXor
                    case BinaryBuiltin.IntBitShiftLeft => BuiltinBinaryOp.IntBitShiftLeft
                    case BinaryBuiltin.IntBitShiftRight => BuiltinBinaryOp.IntBitShiftRight
                    case BinaryBuiltin.IntEQ => BuiltinBinaryOp.IntEq
                    case BinaryBuiltin.IntNE => BuiltinBinaryOp.IntNe
                    case BinaryBuiltin.IntLT => BuiltinBinaryOp.IntLt
                    case BinaryBuiltin.IntLE => BuiltinBinaryOp.IntLe
                    case BinaryBuiltin.IntGT => BuiltinBinaryOp.IntGt
                    case BinaryBuiltin.IntGE => BuiltinBinaryOp.IntGe
                    case BinaryBuiltin.StringConcat => BuiltinBinaryOp.StringConcat
                    case BinaryBuiltin.StringEQ => BuiltinBinaryOp.StringEq
                    case BinaryBuiltin.StringNE => BuiltinBinaryOp.StringNe
                    case _ =>
                      println("Unimplemented binary builtin: " + builtin)
                      ???
                  }
                  _ <- emit(Instruction.BuiltinBinary(r, op, ar, br))
                yield ()
              }

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Unary(builtin, a)) =>
              intoRegister(e, output) { r =>
                for
                  ar <- expr(a, ExprOutput.AnyRegister)
                  op = builtin match {
                    case UnaryBuiltin.IntNegate => BuiltinUnaryOp.IntNegate
                    case UnaryBuiltin.IntBitNot => BuiltinUnaryOp.IntBitNot
                  }
                  _ <- emit(Instruction.BuiltinUnary(r, op, ar))
                yield ()
              }

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin)) =>
              intoRegister(e, output) { r =>
                def typeLiteral(t: VmType): Comp[Unit] =
                  emit(Instruction.LoadTypeInfo(r, t))

                def builtinType(t: BuiltinType): Comp[Unit] =
                  typeLiteral(VmType.Builtin(t, Seq()))

                builtin match {
                  case NullaryBuiltin.IntType => builtinType(BuiltinType.Int())
                  case NullaryBuiltin.BoolType => builtinType(BuiltinType.Bool())
                  case NullaryBuiltin.StringType => builtinType(BuiltinType.String())
                  case NullaryBuiltin.NeverType => builtinType(BuiltinType.Never())
                }
              }

            case ArExpr.FunctionCall(f, args) =>
              functionResult(e, output) { funcResult =>
                for
                  id <- getFunctionId(f)
                  sig <- f.signature
                  argRegs <- ZIO.foreach(
                    sig.parameters
                      .iterator
                      .zip(args)
                      .filter { (param, _) => !param.isErased }
                      .map { (_, arg) => arg }
                      .toSeq
                  ) { arg =>
                    expr(arg, ExprOutput.AnyRegister)
                  }

                  _ <- emit(Instruction.FunctionCall(funcResult, id, argRegs))
                yield ()
              }

            case ifElse: ArExpr.IfElse =>
              knownLocation(e, output) { output =>
                for
                  cond <- expr(ifElse.condition, ExprOutput.AnyRegister)
                  whenTrue <- nestedBlock { _.expr(ifElse.trueBody, output) }
                  whenFalse <- nestedBlock { _.expr(ifElse.falseBody, output) }
                  _ <- emit(Instruction.IfElse(cond, whenTrue, whenFalse))
                yield ()
              }

            case ArExpr.Sequence(stmts, result) =>
              ZIO.foreach(stmts) { stmt => expr(stmt, ExprOutput.Discard) } *>
                expr(result, output)
                
            case ArExpr.StringLiteral(s) =>
              intoRegister(e, output) { r =>
                emit(Instruction.ConstString(r, s))
              }

            case ArExpr.Variable(v) =>
              existingRegister(output) {
                getVar(v)
              }

            case _ =>
              ZIO.logError("Unimplemented block expression: " + e.getClass).as(???)
          }

        private def boolType = ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(NullaryBuiltin.BoolType))
        private def intType = ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(NullaryBuiltin.IntType))
        private def stringType = ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(NullaryBuiltin.StringType))

        private def getExprType(e: ArExpr): Comp[ArExpr] =
          e match {
            case ArExpr.BoolLiteral(_) =>
              ZIO.succeed(boolType)

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Binary(builtin, _, _)) =>
              ZIO.succeed(builtin match {
                case BinaryBuiltin.IntAdd | BinaryBuiltin.IntSub | BinaryBuiltin.IntMul |
                  BinaryBuiltin.IntBitAnd | BinaryBuiltin.IntBitOr | BinaryBuiltin.IntBitXOr |
                  BinaryBuiltin.IntBitShiftLeft | BinaryBuiltin.IntBitShiftRight =>
                    intType

                case BinaryBuiltin.IntEQ | BinaryBuiltin.IntNE |
                  BinaryBuiltin.IntLT | BinaryBuiltin.IntLE |
                  BinaryBuiltin.IntGT | BinaryBuiltin.IntGE |
                  BinaryBuiltin.StringEQ | BinaryBuiltin.StringNE =>
                    boolType

                case BinaryBuiltin.StringConcat => stringType
                case _ =>
                  println("Unimplemented getExprType binary builtin: " + builtin)
                  ???
              })

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Unary(builtin, _)) =>
              ZIO.succeed(builtin match {
                case UnaryBuiltin.IntNegate | UnaryBuiltin.IntBitNot => intType
              })

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin)) =>
              ZIO.succeed(builtin match {
                case NullaryBuiltin.BoolType | NullaryBuiltin.IntType |
                  NullaryBuiltin.StringType | NullaryBuiltin.NeverType =>
                    ArExpr.TypeN(ArExpr.IntLiteral(1))
              })

            case ArExpr.FunctionCall(f, args) =>
              for
                sig <- f.signature
              yield sig.returnTypeForArgs(
                context.DefaultExprContext.ParameterOwner.Func(f),
                args
              )

            case ArExpr.Variable(v) =>
              ZIO.succeed(v.varType)

            case ArExpr.StringLiteral(_) =>
              ZIO.succeed(stringType)

            case _ =>
              ZIO.logError("Unimplemented getExprType expression: " + e.getClass).as(???)
          }

        def exprReturn(e: ArExpr): Comp[Unit] =
          expr(e, ExprOutput.Return)

      }
    }
}


