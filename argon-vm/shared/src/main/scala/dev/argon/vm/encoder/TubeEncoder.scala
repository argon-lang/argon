package dev.argon.vm.encoder

import dev.argon.compiler as c
import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.encoder.TubeEncoderBase
import dev.argon.vm.*
import zio.*
import zio.stream.*
import zio.stm.{TMap, TRef, TSet, USTM, ZSTM}
import dev.argon.ast
import dev.argon.compiler.{ArgonEvaluator, HasContext, SignatureEraser, UsingContext}
import dev.argon.expr.{BinaryBuiltin, CaptureScanner, FreeVariableScanner, NullaryBuiltin, UnaryBuiltin, ValueUtil}
import sourcecode.Text.generate


private[vm] class TubeEncoder(platformId: String) extends TubeEncoderBase[TubeFileEntry] {
  override def createEmitter(state: EncodeState): state.Emitter =
    new state.Emitter {
      import state.*
      import context.DefaultExprContext.{Expr as ArExpr, Pattern as ArPattern}
      val ctx: context.type = context
  
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
        yield ImportSpecifier.Global(
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

          case c.ModuleExportC.Enum(e) =>
            getEnumId(e).unit
            
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
          encSig <- emitFunctionSignature(context.DefaultExprContext.ParameterOwner.Func(func), sig)

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
                  block <- emitFunctionBody(e, encSig, importSpec)
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
            signature = encSig.sig,
            implementation = impl,
          )
        )

      private def emitSyntheticFunctionDef(sig: FunctionSignatureWithMapping, id: BigInt, importSpec: ImportSpecifier, body: ArExpr): Comp[TubeFileEntry] =
        for
          block <- emitFunctionBody(body, sig, importSpec)
        yield TubeFileEntry.FunctionDefinition(
          FunctionDefinition(
            functionId = id,
            `import` = importSpec,
            signature = sig.sig,
            implementation = Some(FunctionImplementation.VmIr(block)),
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
          sig <- emitFunctionSignature(context.DefaultExprContext.ParameterOwner.Rec(rec), sig)

          importSpec <- rec.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          typeEmitter = TypeEmitter.fromIndexMap(sig.typeParamMapping)

          fields <- rec.fields
          fields <- ZIO.foreach(fields)(emitRecordField(typeEmitter))

        yield TubeFileEntry.RecordDefinition(
          RecordDefinition(
            recordId = id,
            `import` = importSpec,
            signature = sig.sig,
            fields = fields,
          )
        )

      private def emitRecordRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
        ZIO.succeed(TubeFileEntry.RecordReference(
          recordId = id,
          `import` = specifier,
        ))

      private def emitRecordField(typeEmitter: TypeEmitter)(field: RecordField): Comp[RecordFieldDefinition] =
        for
          t <- typeEmitter.typeExpr(field.fieldType)
        yield RecordFieldDefinition(
          name = encodeIdentifier(field.name),
          fieldType = t,
          mutable = field.isMutable,
        )


      override def emitRecordFieldInfo(field: RecordField, id: BigInt): Comp[TubeFileEntry] =
        field.owningRecord match {
          case record: ArRecord =>
            for
              recordId <- getRecordId(record)
            yield TubeFileEntry.RecordFieldReference(
              recordFieldId = id,
              recordId = recordId,
              name = encodeIdentifier(field.name),
            )

          case variant: EnumVariant =>
            for
              variantId <- getEnumVariantId(variant)
            yield TubeFileEntry.EnumVariantRecordFieldReference(
              recordFieldId = id,
              variantId = variantId,
              name = encodeIdentifier(field.name),
            )
        }

      override def emitEnum(e: ArEnum, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(e, e.importSpecifier)(emitEnumDef(id), emitEnumRef(id))

      private def emitEnumDef(id: BigInt)(e: ArEnum): Comp[TubeFileEntry] =
        for
          sig <- e.signature
          sig <- emitFunctionSignature(context.DefaultExprContext.ParameterOwner.Enum(e), sig)

          importSpec <- e.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          typeEmitter = TypeEmitter.fromIndexMap(sig.typeParamMapping)

          variants <- e.variants
          variants <- ZIO.foreach(variants) { variant =>
            for
              sig <- variant.signature
              sig <- emitFunctionSignature(context.DefaultExprContext.ParameterOwner.EnumVariant(variant), sig)
              fields <- variant.fields
              fields <- ZIO.foreach(fields)(emitRecordField(typeEmitter))
            yield EnumVariantDefinition(
              name = encodeIdentifier(variant.name),
              signature = sig.sig,
              fields = fields,
            )
          }

        yield TubeFileEntry.EnumDefinition(
          EnumDefinition(
            enumId = id,
            `import` = importSpec,
            signature = sig.sig,
            variants = variants,
          )
        )

      private def emitEnumRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
        ZIO.succeed(TubeFileEntry.EnumReference(
          enumId = id,
          `import` = specifier,
        ))

      override def emitEnumVariantInfo(v: EnumVariant, id: BigInt): Comp[TubeFileEntry] =
        for
          enumId <- getEnumId(v.owningEnum)
        yield TubeFileEntry.EnumVariantReference(
          variantId = id,
          enumId = enumId,
          name = encodeIdentifier(v.name),
        )
        

      private final case class FunctionSignatureWithMapping(
        sig: dev.argon.vm.FunctionSignature,
        argConsumers: Seq[ArgConsumer],
        typeParamMapping: Map[context.DefaultExprContext.Var, Int],
        paramVarMapping: Map[context.DefaultExprContext.Var, Int],
      )


      private final class FunctionSignatureBuilder private(
        typeParams: TRef[Seq[dev.argon.vm.SignatureTypeParameter]],
        params: TRef[Seq[dev.argon.vm.SignatureParameter]],

        argConsumers: TRef[Seq[ArgConsumer]],

        typeParamMapping: TMap[context.DefaultExprContext.Var, Int],
        paramVarMapping: TMap[context.DefaultExprContext.Var, Int],
      ) {



        private def typeEmitter: UIO[TypeEmitter] =
          (
            for
              tpm <- typeParamMapping.toMap
            yield TypeEmitter.fromIndexMap(tpm)
          ).commit

        def addParameter(v: context.DefaultExprContext.Var): Comp[Unit] =
          if v.isErased then
            argConsumers.update(_ :+ ArgConsumer.Erased).commit
          else if isTypeType(v.varType) then
            val tp = dev.argon.vm.SignatureTypeParameter(
              name = v.name.map(encodeIdentifier),
            )

            (
              for
                size <- typeParams.modify(tps => (tps.size, tps :+ tp))
                _ <- typeParamMapping.put(v, size)
                _ <- argConsumers.update(_ :+ ArgConsumer.TypeArg)
              yield ()
            ).commit
          else
            def putParam(sigParam: dev.argon.vm.SignatureParameter): USTM[Unit] =
              for
                size <- params.modify(params => (params.size, params :+ sigParam))
                _ <- paramVarMapping.put(v, size)
                _ <- argConsumers.update(_ :+ ArgConsumer.Arg)
              yield ()

            for
              te <- typeEmitter

              t <- te.typeExpr(v.varType)
              sigParam = dev.argon.vm.SignatureParameter(
                name = v.name.map(encodeIdentifier),
                paramType = t,
              )
              _ <- putParam(sigParam).commit

            yield ()
          end if

        def finish(returnType: ArExpr): Comp[FunctionSignatureWithMapping] =
          for
            returnType <- typeEmitter.flatMap(_.typeExpr(returnType))

            typeParams <- typeParams.get.commit
            params <- params.get.commit
            argConsumers <- argConsumers.get.commit
            paramVarMapping <- paramVarMapping.toMap.commit
            typeParamMapping <- typeParamMapping.toMap.commit
          yield FunctionSignatureWithMapping(
            sig = dev.argon.vm.FunctionSignature(
              typeParameters = typeParams,
              parameters = params,
              returnType = returnType,
            ),
            argConsumers = argConsumers,
            typeParamMapping = typeParamMapping,
            paramVarMapping = paramVarMapping,
          )
      }

      private object FunctionSignatureBuilder {
        def make: UIO[FunctionSignatureBuilder] =
          for
            typeParams <- TRef.make(Seq.empty[dev.argon.vm.SignatureTypeParameter]).commit
            params <- TRef.make(Seq.empty[dev.argon.vm.SignatureParameter]).commit

            argConsumers <- TRef.make(Seq.empty[ArgConsumer]).commit

            paramVarMapping <- TMap.empty[context.DefaultExprContext.Var, Int].commit
            typeParamMapping <- TMap.empty[context.DefaultExprContext.Var, Int].commit
          yield FunctionSignatureBuilder(typeParams, params, argConsumers, paramVarMapping, typeParamMapping)
      }

      private enum ArgConsumer derives CanEqual {
        case Erased
        case TypeArg
        case Arg
      }

      private def emitFunctionSignature(paramOwner: context.DefaultExprContext.ParameterOwner, sig: FunctionSignature): Comp[FunctionSignatureWithMapping] =
        FunctionSignatureBuilder.make
          .tap { sb =>
            ZIO.foreachDiscard(sig.parameters.view.zipWithIndex) { (param, origIndex) =>
              val paramVar = param.asParameterVar(paramOwner, origIndex)
              sb.addParameter(paramVar)
            }
          }
          .flatMap { sb => sb.finish(sig.returnType) }

      enum VariableRealization {
        case Reg(register: RegisterId)
        case TypeParam(tp: VmType)
      }

      private def emitFunctionBody(e: ArExpr, funcSig: FunctionSignatureWithMapping, funcImport: ImportSpecifier): Comp[FunctionBody] =
        for
          varOffset <- TRef.makeCommit(funcSig.sig.parameters.size)
          knownVars <- TMap.fromIterable[context.DefaultExprContext.Var, VariableRealization](
            funcSig.paramVarMapping.map { (param, index) => param -> VariableRealization.Reg(RegisterId(index)) } ++
              funcSig.typeParamMapping.map { (param, index) => param -> VariableRealization.TypeParam(VmType.TypeParameter(index)) }
          ).commit
          instructions <- TRef.make(Seq.empty[Instruction]).commit
          nextSyntheticIndex <- TRef.makeCommit[BigInt](0)

          capturedVars = CaptureScanner(context.DefaultExprContext)(e)

          emitter = ExprEmitter(
            varOffset = varOffset,
            knownVars = knownVars,
            instructions = instructions,
            varInstructions = instructions,
            importSpecifier = funcImport,
            nextSyntheticIndex = nextSyntheticIndex,
            capturedVars = capturedVars,
          )

          _ <- emitter.exprReturn(e)
          res <- emitter.toFunctionBody
        yield res
      end emitFunctionBody


      private def isTypeType(t: ArExpr): Boolean =
        ValueUtil.isTypeType(context.DefaultExprContext)(t)

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

      private trait TypeEmitterBase {

        protected def fallbackTypeExpr(t: ArExpr): Comp[VmType]
        protected def getParameterAsType(v: context.DefaultExprContext.Var): Comp[Option[VmType]]

        def typeExpr(t: ArExpr): Comp[VmType] =
          ArgonEvaluator(context).normalizeToValue(t, context.Config.evaluatorFuel).flatMap {
            case ArExpr.Boxed(_) =>
              ZIO.succeed(VmType.Boxed())

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin)) =>
              ZIO.succeed(
                builtin match {
                  case NullaryBuiltin.BoolType => VmType.Builtin(BuiltinType.Bool(), Seq())
                  case NullaryBuiltin.IntType => VmType.Builtin(BuiltinType.Int(), Seq())
                  case NullaryBuiltin.StringType => VmType.Builtin(BuiltinType.String(), Seq())
                  case NullaryBuiltin.NeverType => VmType.Builtin(BuiltinType.Never(), Seq())
                }
              )

            case ArExpr.FunctionType(a, r) if a.isErased =>
              for
                r <- typeExpr(r)
              yield VmType.FunctionErased(r)

            case ArExpr.FunctionType(a, r) =>
              for
                a <- typeExpr(a.varType)
                r <- typeExpr(r)
              yield VmType.Function(a, r)

            case ArExpr.RecordType(rec, args) =>
              for
                id <- getRecordId(rec)
                args <- ZIO.foreach(args)(typeExpr)
              yield VmType.Record(id, args)

            case ArExpr.EnumType(rec, args) =>
              for
                id <- getEnumId(rec)
                args <- ZIO.foreach(args)(typeExpr)
              yield VmType.Enum(id, args)
              
            case ArExpr.Tuple(items) =>
              ZIO.foreach(items)(typeExpr)
                .map(VmType.Tuple.apply)

            case ArExpr.TypeN(_) =>
              ZIO.succeed(VmType.TypeInfo())

            case ArExpr.Variable(v) if v.isErased =>
              ZIO.succeed(VmType.Erased())

            case t @ ArExpr.Variable(v) =>
              getParameterAsType(v).flatMap {
                case Some(tp) => ZIO.succeed(tp)
                case None => fallbackTypeExpr(t)
              }

            case t =>
              fallbackTypeExpr(t)
          }

      }

      private final class TypeEmitter(
        typeParams: Map[context.DefaultExprContext.Var, VmType],
      ) extends TypeEmitterBase {
        override protected def fallbackTypeExpr(t: ArExpr): Comp[VmType] =
          ZIO.logError("Unimplemented type expression: " + t.productPrefix).as(???)

        override protected def getParameterAsType(v: state.context.DefaultExprContext.Var): Comp[Option[VmType]] =
          ZIO.succeed(typeParams.get(v))
      }

      private object TypeEmitter {
        def fromIndexMap(typeParams: Map[context.DefaultExprContext.Var, Int]): TypeEmitter =
          TypeEmitter(typeParams.view.mapValues(i => VmType.TypeParameter(i)).toMap)
      }

      private final class ExprEmitter(
        varOffset: TRef[Int],
        knownVars: TMap[context.DefaultExprContext.Var, VariableRealization],
        instructions: TRef[Seq[Instruction]],
        varInstructions: TRef[Seq[Instruction]],
        importSpecifier: ImportSpecifier,
        nextSyntheticIndex: TRef[BigInt],
        capturedVars: Set[context.DefaultExprContext.Var],
      ) extends TypeEmitterBase {
        private def nestedScope: Comp[ExprEmitter] =
          for
            currentVarOffset <- varOffset.get.commit
            varOffset2 <- TRef.makeCommit(currentVarOffset)
            knownVars <- knownVars.toMap.flatMap(kv => TMap.make(kv.toSeq*)).commit
            instructions <- TRef.makeCommit(Seq.empty[Instruction])
          yield ExprEmitter(
            varOffset = varOffset2,
            knownVars = knownVars,
            instructions = instructions,
            varInstructions = instructions,
            importSpecifier = importSpecifier,
            nextSyntheticIndex = nextSyntheticIndex,
            capturedVars = capturedVars,
          )

        private def nestedBlock[A](f: ExprEmitter => Comp[A]): Comp[(Block, A)] =
          for
            scope <- nestedScope
            a <- f(scope)
            block <- scope.toBlock
          yield (block, a)

        private def nestedBlockNoScope[A](f: ExprEmitter => Comp[A]): Comp[(Block, A)] =
          for
            instructions <- TRef.makeCommit(Seq.empty[Instruction])
            scope = ExprEmitter(
              varOffset = varOffset,
              knownVars = knownVars,
              instructions = instructions,
              varInstructions = this.varInstructions,
              importSpecifier = importSpecifier,
              nextSyntheticIndex = nextSyntheticIndex,
              capturedVars = capturedVars,
            )
            a <- f(scope)
            block <- scope.toBlock
          yield (block, a)

        private def declareVar(v: context.DefaultExprContext.LocalVar): Comp[RegisterId] =
          for
            t <- typeExpr(v.varType)
            id <- addVar(t).tap { id => knownVars.put(v, VariableRealization.Reg(id)) }.commit
          yield id

        private def addVar(t: VmType, captured: Option[VariableCaptureMode] = None): USTM[RegisterId] =
          for
            id <- varOffset.getAndUpdate(_ + 1)
            _ <- varInstructions.update(_ :+ Instruction.DeclareVariable(t, captured = captured))
          yield RegisterId(id)
          
        def toBlock: Comp[Block] =
          for
            insns <- instructions.get.commit
          yield Block(
            instructions = insns,
          )
          
        def toFunctionBody: Comp[FunctionBody] =
          for
            block <- toBlock
          yield FunctionBody(
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
                vmType <- typeExpr(t)
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
                .flatMap(typeExpr)
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

        private def unitResult(e: ArExpr, output: ExprOutput)(f: => Comp[Unit]): Comp[output.ResultType] =
          (output : ExprOutput & output.type) match {
            case o: (ExprOutput.Discard.type & output.type) =>
              f : Comp[o.ResultType]

            case _ =>
              intoRegister(e, output) { r =>
                f *> emit(Instruction.Tuple(r, Seq()))
              }
          }

        private def expr(e: ArExpr, output: ExprOutput): Comp[output.ResultType] =
          e match {
            case ArExpr.BindVariable(v, _) if v.isErased =>
              unitResult(e, output)(ZIO.unit)
            
            case ArExpr.BindVariable(v, value) =>
              unitResult(e, output)(
                for
                  r <- declareVar(v)
                  _ <- expr(value, ExprOutput.Register(r))
                yield ()
              )

            case ArExpr.BoolLiteral(b) =>
              intoRegister(e, output) { r =>
                emit(Instruction.ConstBool(r, b))
              }

            case ArExpr.Box(t, value) =>
              intoRegister(e, output) { r =>
                for
                  a <- expr(value, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.Box(r, a))
                yield ()
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
                    case BinaryBuiltin.BoolEQ => BuiltinBinaryOp.BoolEq
                    case BinaryBuiltin.BoolNE => BuiltinBinaryOp.BoolNe
                    case _ =>
                      println("Unimplemented binary builtin: " + builtin)
                      ???
                  }
                  _ <- emit(Instruction.BuiltinBinary(op, r, ar, br))
                yield ()
              }

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Unary(builtin, a)) =>
              intoRegister(e, output) { r =>
                for
                  ar <- expr(a, ExprOutput.AnyRegister)
                  op = builtin match {
                    case UnaryBuiltin.IntNegate => BuiltinUnaryOp.IntNegate
                    case UnaryBuiltin.IntBitNot => BuiltinUnaryOp.IntBitNot
                    case UnaryBuiltin.BoolNot => BuiltinUnaryOp.BoolNot
                  }
                  _ <- emit(Instruction.BuiltinUnary(op, r, ar))
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

            case ArExpr.EnumVariantLiteral(enumType, v, args, fields) =>
              intoRegister(e, output) { r =>
                for
                  enumType <- typeExpr(enumType)
                  
                  id <- getEnumVariantId(v)
                  
                  sig <- v.signature
                  funcArgs <- emitArguments(context.DefaultExprContext.ParameterOwner.EnumVariant(v), sig, args)
                  
                  fieldRegs <- ZIO.foreach(fields) { field =>
                    for
                      fieldReg <- expr(field.value, ExprOutput.AnyRegister)
                    yield field.field -> fieldReg
                  }
                  fieldRegsMap = fieldRegs.toMap
                  fieldDefs <- v.fields
                  fieldRegsOrdered <- ZIO.foreach(fieldDefs) { fieldDef =>
                    for
                      fieldId <- getRecordFieldId(fieldDef)
                    yield RecordFieldLiteral(fieldId, fieldRegsMap(fieldDef))
                  }
                  _ <- emit(Instruction.EnumVariantLiteral(
                    r,
                    enumType,
                    id,
                    funcArgs.typeArguments,
                    funcArgs.arguments,
                    fieldRegsOrdered,
                  ))
                yield ()
              }

            case ArExpr.Finally(action, ensuring) =>
              knownLocation(e, output) { output =>
                for
                  (action, res) <- nestedBlock(_.expr(action, output))
                  (ensuring, _) <- nestedBlock(_.expr(ensuring, ExprOutput.Discard))
                  _ <- emit(Instruction.Finally(action, ensuring))
                yield res
              }

            case ArExpr.FunctionCall(f, args) =>
              functionResult(e, output) { funcResult =>
                for
                  id <- getFunctionId(f)
                  sig <- f.signature
                  funcArgs <- emitArguments(context.DefaultExprContext.ParameterOwner.Func(f), sig, args)
                  _ <- emit(Instruction.FunctionCall(id, funcResult, funcArgs.typeArguments, funcArgs.arguments))
                yield ()
              }

            case ArExpr.FunctionObjectCall(f, a) =>
              functionResult(e, output) { funcResult =>
                getExprType(f).flatMap {
                  case ArExpr.FunctionType(v, r) if v.isErased =>
                    for
                      func <- expr(f, ExprOutput.AnyRegister)
                      _ <- emit(Instruction.FunctionObjectErasedCall(funcResult, func))
                    yield ()

                  case ArExpr.FunctionType(v, r) if isTypeType(v.varType) =>
                    for
                      func <- expr(f, ExprOutput.AnyRegister)
                      a <- typeExpr(a)
                      _ <- emit(Instruction.FunctionObjectTypeCall(funcResult, func, a))
                    yield ()

                  case ArExpr.FunctionType(v, r) =>
                    for
                      func <- expr(f, ExprOutput.AnyRegister)
                      a <- expr(a, ExprOutput.AnyRegister)
                      _ <- emit(Instruction.FunctionObjectCall(funcResult, func, a))
                    yield ()

                  case _ =>
                    ZIO.die(RuntimeException("Invalid type for lambda expression"))
                }
              }

            case ifElse: ArExpr.IfElse =>
              knownLocation(e, output) { output =>
                for
                  cond <- expr(ifElse.condition, ExprOutput.AnyRegister)
                  (whenTrue, _) <- nestedBlock { _.expr(ifElse.trueBody, output) }
                  (whenFalse, _) <- nestedBlock { _.expr(ifElse.falseBody, output) }
                  _ <- emit(Instruction.IfElse(cond, whenTrue, whenFalse))
                yield ()
              }

            case ArExpr.IntLiteral(i) =>
              intoRegister(e, output) { r =>
                emit(Instruction.ConstInt(r, i))
              }

            case ArExpr.Is(value, pattern) =>
              intoRegister(e, output) { r =>                  
                expr(value, ExprOutput.AnyRegister).flatMap { valueReg =>
                  emitPattern(r, valueReg, pattern)
                }
              }
              
            case ArExpr.Lambda(param, returnType, body) =>
              import context.DefaultExprContext.Var

              def captureVariables(vars: Seq[Var]): Comp[(Seq[VmType], Seq[Capture], FunctionSignatureWithMapping)] =
                FunctionSignatureBuilder.make
                  .tap { sb =>
                    ZIO.foreachDiscard(vars)(sb.addParameter) *>
                      sb.addParameter(param)
                  }
                  .flatMap { sb =>
                    for
                      sig <- sb.finish(returnType)
                      typeArgs <- Ref.make(Seq.empty[VmType])
                      args <- Ref.make(Seq.empty[Capture])

                      // Not including param here because itis not a capture
                      _ <- ZIO.foreachDiscard(sig.argConsumers.zip(vars)) {
                        case (ArgConsumer.Erased, _) => ZIO.unit
                        case (ArgConsumer.TypeArg, v) =>
                          knownVars.get(v).commit.flatMap {
                            case Some(VariableRealization.TypeParam(tp)) =>
                              typeArgs.update(_ :+ tp)

                            case _ =>
                              ZIO.fail(TubeFormatException("Capture type mismatch"))
                          }

                        case (ArgConsumer.Arg, v) =>
                          knownVars.get(v).commit.flatMap {
                            case Some(VariableRealization.Reg(r)) =>
                              val capture =
                                if v.isMutable then
                                  Capture.Mutable(r)
                                else
                                  Capture.Value(r)

                              args.update(_ :+ capture)

                            case _ =>
                              ZIO.fail(TubeFormatException("Capture type mismatch"))
                          }

                      }

                      typeArgs <- typeArgs.get
                      args <- args.get
                    yield (typeArgs, args, sig)
                  }

              intoRegister(e, output) { r =>
                for
                  synIndex <- nextSyntheticIndex.getAndUpdate(_ + 1).commit
                  synId <- newSyntheticFunctionId
                  freeVars = FreeVariableScanner(context.DefaultExprContext)(e).toSeq
                  (typeArgs, args, sig) <- captureVariables(freeVars)
                  synImportSpec = ImportSpecifier.SyntheticNested(importSpecifier, synIndex)

                  _ <- state.emitEntryBuilder(emitSyntheticFunctionDef(sig, synId, synImportSpec, body))
                  _ <-
                    if param.isErased then
                      emit(Instruction.PartiallyAppliedFunctionErased(synId, r, typeArgs, args))
                    else if isTypeType(param.varType) then
                      emit(Instruction.PartiallyAppliedTypeFunction(synId, r, typeArgs, args))
                    else
                      emit(Instruction.PartiallyAppliedFunction(synId, r, typeArgs, args))


                yield ()
              }

            case e @ ArExpr.RecordType(_, _) =>
              intoRegister(e, output) { r =>
                typeExpr(e).flatMap { t =>
                  emit(Instruction.LoadTypeInfo(r, t))
                }
              }

            case e @ ArExpr.EnumType(_, _) =>
              intoRegister(e, output) { r =>
                typeExpr(e).flatMap { t =>
                  emit(Instruction.LoadTypeInfo(r, t))
                }
              }

            case ArExpr.RecordFieldLoad(recordType, field, recordValue) =>
              intoRegister(e, output) { r =>
                for
                  fieldId <- getRecordFieldId(field)
                  recordValue <- expr(recordValue, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.RecordFieldLoad(fieldId, r, recordValue))
                yield ()
              }

            case ArExpr.RecordFieldStore(recordType, field, recordValue, fieldValue) =>
              unitResult(e, output)(
                for
                  fieldId <- getRecordFieldId(field)
                  recordValue <- expr(recordValue, ExprOutput.AnyRegister)
                  fieldValue <- expr(fieldValue, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.RecordFieldStore(fieldId, recordValue, fieldValue))
                yield ()
              )

            case ArExpr.RecordLiteral(recordType, fields) =>
              intoRegister(e, output) { r =>
                for
                  recType <- typeExpr(recordType)
                  fieldRegs <- ZIO.foreach(fields) { field =>
                    for
                      fieldReg <- expr(field.value, ExprOutput.AnyRegister)
                    yield field.field -> fieldReg
                  }
                  fieldRegsMap = fieldRegs.toMap
                  fieldDefs <- recordType.record.fields
                  fieldRegsOrdered <- ZIO.foreach(fieldDefs) { fieldDef =>
                    for
                      fieldId <- getRecordFieldId(fieldDef)
                    yield RecordFieldLiteral(fieldId, fieldRegsMap(fieldDef))
                  }
                  _ <- emit(Instruction.RecordLiteral(r, recType, fieldRegsOrdered))
                yield ()
              }

            case ArExpr.Sequence(stmts, result) =>
              ZIO.foreachDiscard(stmts)(stmt => expr(stmt, ExprOutput.Discard)) *>
                expr(result, output)
                
            case ArExpr.StringLiteral(s) =>
              intoRegister(e, output) { r =>
                emit(Instruction.ConstString(r, s))
              }

            case ArExpr.Tuple(items) =>
              intoRegister(e, output) { r =>
                for
                  itemRegs <- ZIO.foreach(items) { item => expr(item, ExprOutput.AnyRegister) }
                  _ <- emit(Instruction.Tuple(r, itemRegs))
                yield ()
              }

            case ArExpr.TupleElement(index, tuple) =>
              intoRegister(e, output) { r =>
                for
                  tupleReg <- expr(tuple, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.TupleElement(index, r, tupleReg))
                yield ()
              }

            case ArExpr.Unbox(t, value) =>
              intoRegister(e, output) { r =>
                for
                  t <- typeExpr(t)
                  a <- expr(value, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.Unbox(r, t, a))
                yield ()
              }
              

            case ArExpr.Variable(v) =>
              knownVars.get(v).commit.flatMap {
                case Some(VariableRealization.Reg(id)) =>
                  existingRegister(output)(ZIO.succeed(id))

                case Some(VariableRealization.TypeParam(tp)) =>
                  intoRegister(e, output) { r =>
                    emit(Instruction.LoadTypeInfo(r, tp))
                  }

                case None =>
                  ZIO.fail(TubeFormatException("Could not get index for variable: " + v))
              }

            case ArExpr.VariableStore(v, value) =>
              unitResult(e, output)(
                knownVars.get(v).commit.flatMap {
                  case Some(VariableRealization.Reg(r)) =>
                    expr(value, ExprOutput.Register(r))

                  case Some(VariableRealization.TypeParam(_)) =>
                    ZIO.fail(TubeFormatException("Cannot assign to type parameter"))

                  case None =>
                    ZIO.fail(TubeFormatException("Could not get index for variable"))

                }
              )


            case _ =>
              ZIO.logError("Unimplemented block expression: " + e.getClass).as(???)
          }
        
        private def emitPattern(r: RegisterId, valueReg: RegisterId, pattern: ArPattern): Comp[Unit] =
          pattern match {
            case ArPattern.Discard(_) =>
              emit(Instruction.ConstBool(r, true))

            case ArPattern.Tuple(elements) =>              
              emitTuplePattern(r, valueReg, elements, 0)
              
            case ArPattern.Binding(v, pattern) =>
              for
                vr <- declareVar(v)
                _ <- emit(Instruction.Move(vr, valueReg))
                _ <- emitPattern(r, valueReg, pattern)
              yield ()
              
            case ArPattern.EnumVariant(enumType, variant, args, fields) =>
              for
                argPatterns <- ZIO.foreach(args) { arg =>
                  for
                    vt <- typeExpr(exprType.getPatternType(arg))
                    argReg <- addVar(vt).commit
                  yield (argReg, arg)
                }
                
                fieldPatterns <- ZIO.foreach(fields) { field =>
                  for
                    vt <- typeExpr(exprType.getPatternType(field.pattern))
                    fieldReg <- addVar(vt).commit
                    fieldId <- getRecordFieldId(field.field)
                  yield (fieldReg, fieldId, field.pattern)
                }
                
                et <- typeExpr(enumType)
                variantId <- getEnumVariantId(variant)
                
                _ <- emit(Instruction.IsEnumVariant(
                  r,
                  et,
                  variantId,
                  valueReg,
                  argPatterns.map(_._1),
                  fieldPatterns.map { (fieldReg, fieldId, _) => FieldExtractor(fieldReg, fieldId) },
                ))
                (block, _) <- nestedBlockNoScope { blockEmitter =>
                  blockEmitter.processPatterns(
                    r,
                    argPatterns ++ fieldPatterns.map { (fieldReg, _, fieldPattern) => (fieldReg, fieldPattern) },
                  )
                }
                _ <- emit(Instruction.IfElse(r, block, Block(Seq())))
              yield ()

            case ArPattern.String(s) =>
              for
                sr <- expr(ArExpr.StringLiteral(s), ExprOutput.AnyRegister)
                _ <- emit(Instruction.BuiltinBinary(BuiltinBinaryOp.StringEq, r, valueReg, sr))
              yield ()

            case ArPattern.Int(i) =>
              for
                sr <- expr(ArExpr.IntLiteral(i), ExprOutput.AnyRegister)
                _ <- emit(Instruction.BuiltinBinary(BuiltinBinaryOp.IntEq, r, valueReg, sr))
              yield ()

            case ArPattern.Bool(b) =>
              for
                sr <- expr(ArExpr.BoolLiteral(b), ExprOutput.AnyRegister)
                _ <- emit(Instruction.BuiltinBinary(BuiltinBinaryOp.BoolEq, r, valueReg, sr))
              yield ()
          }
          
        private def emitTuplePattern(r: RegisterId, valueReg: RegisterId, elements: Seq[ArPattern], index: BigInt): Comp[Unit] =
          elements match {
            case h +: t =>
              for
                et <- typeExpr(exprType.getPatternType(h))
                elementReg <- addVar(et).commit
                _ <- emit(Instruction.TupleElement(index, elementReg, valueReg))
                _ <- emitPattern(r, elementReg, h)
                _ <- (
                  for
                    (block, _) <- nestedBlockNoScope { blockEmitter =>
                      blockEmitter.emitTuplePattern(r, valueReg, t, index + 1)
                    }
                    _ <- emit(Instruction.IfElse(r, block, Block(Seq())))
                  yield ()
                  ).whenDiscard(t.nonEmpty)
              yield ()

            case _ => emit(Instruction.ConstBool(r, true))
          }
        
        private def processPatterns(r: RegisterId, patterns: Seq[(RegisterId, ArPattern)]): Comp[Unit] =
          patterns match {
            case (valueReg, pattern) +: t =>
              for
                (block, _) <- nestedBlockNoScope { blockEmitter =>
                  blockEmitter.emitPattern(r, valueReg, pattern) *>
                    blockEmitter.processPatterns(r, t)      
                }
                _ <- emit(Instruction.IfElse(r, block, Block(Seq())))
              yield ()

            case _ =>
              ZIO.unit
          }

        override protected def fallbackTypeExpr(t: ArExpr): Comp[VmType] =
          expr(t, ExprOutput.AnyRegister).map { r =>
            VmType.OfTypeInfo(r)
          }

        override protected def getParameterAsType(v: state.context.DefaultExprContext.Var): Comp[Option[VmType]] =
          knownVars.get(v).commit.map(_.flatMap {
            case VariableRealization.Reg(_) => None
            case VariableRealization.TypeParam(tp) => Some(tp)
          })

        private final case class FunctionArguments(
          typeArguments: Seq[VmType],
          arguments: Seq[RegisterId],
        )

        private def emitArguments(owner: context.DefaultExprContext.ParameterOwner, sig: FunctionSignature, args: Seq[ArExpr]): Comp[FunctionArguments] =
          for
            typeArgs <- TRef.make(Seq.empty[VmType]).commit
            argRegs <- TRef.make(Seq.empty[RegisterId]).commit

            sigWithMapping <- emitFunctionSignature(owner, sig)

            _ <- ZIO.foreachDiscard(sigWithMapping.argConsumers.view.zip(args)) { (consumer, arg) =>
              consumer match {
                case ArgConsumer.Erased => ZIO.unit
                case ArgConsumer.TypeArg =>
                  typeExpr(arg).flatMap { typeArg =>
                    typeArgs.update(_ :+ typeArg).commit
                  }

                case ArgConsumer.Arg =>
                  expr(arg, ExprOutput.AnyRegister).flatMap { r =>
                    argRegs.update(_ :+ r).commit
                  }
              }
            }

            typeArgs <- typeArgs.get.commit
            argRegs <- argRegs.get.commit
          yield FunctionArguments(
            typeArgs,
            argRegs,
          )


        private lazy val exprType = new c.ExprType {
          override val context: ctx.type = ctx
          override val exprContext: context.DefaultExprContext.type = context.DefaultExprContext
          override val sigContext: context.DefaultSignatureContext.type = context.DefaultSignatureContext
          override protected def getHoleType(hole: Nothing): ArExpr = hole
        }


        private def getExprType(e: ArExpr): Comp[ArExpr] =
          exprType.getExprType(e)

        def exprReturn(e: ArExpr): Comp[Unit] =
          expr(e, ExprOutput.Return)

      }
    }
}


