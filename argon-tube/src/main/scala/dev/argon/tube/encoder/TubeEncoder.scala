package dev.argon.tube.encoder

import dev.argon.compiler as c
import dev.argon.tube.*
import zio.*
import zio.stream.*
import zio.stm.{TMap, ZSTM}
import dev.argon.ast
import dev.argon.compiler.{HasContext, SignatureEraser, UsingContext}
import esexpr.Dictionary


private[tube] object TubeEncoder extends TubeEncoderBase[TubeFileEntry] {
  override def createEmitter(state: EncodeState): state.Emitter =
    new state.Emitter {
      import state.*
      import context.DefaultExprContext.{Expr as ArExpr, Pattern as ArPattern}
  
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
            platforms = tube.metadata._1,
            platformMetadata = Dictionary(tube.metadata._2),
            referencedTubes = orderedTubes.map(encodeTubeName),
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
        
      private def encodeEffectInfo(effects: context.DefaultExprContext.EffectInfo): EffectInfo =
        effects match {
          case context.DefaultExprContext.EffectInfo.Pure => EffectInfo.Pure()
          case context.DefaultExprContext.EffectInfo.Effectful => EffectInfo.Effectful()
        }


      private def emitModules(orderedModules: Seq[ArModule]): Comp[Seq[Module]] =
        ZIO.foreach(orderedModules.zipWithIndex) {
          case (module, index) =>
            for
              modExports <- module.allExports(Set.empty)
              groups <- ZIO.foreach(modExports.toSeq) { (name, items) =>
                for
                  exps <- ZIO.foreach(items)(emitModuleExport)
                yield ExportGroup(
                  name = name.map(encodeIdentifier),
                  exports = exps,
                )
              }
            yield Module(
              path = encodeModulePath(module.path),
              groups = groups,
            )
        }

      private def emitModuleExport(exp: ModuleExport): Comp[dev.argon.tube.ModuleExport] =
        exp match {
          case c.ModuleExportC.Function(f) =>
            for
              id <- getFunctionId(f)
              sig <- f.signature
              sig <- SignatureEraser(context).eraseSignature(sig)
              sig <- encodeErasedSignature(sig)
            yield dev.argon.tube.ModuleExport.Function(id, sig)

          case c.ModuleExportC.Record(r) =>
            for
              id <- getRecordId(r)
              sig <- r.signature
              sig <- SignatureEraser(context).eraseSignature(sig)
              sig <- encodeErasedSignature(sig)
            yield dev.argon.tube.ModuleExport.Record(id, sig)

          case c.ModuleExportC.Enum(e) =>
            for
              id <- getEnumId(e)
              sig <- e.signature
              sig <- SignatureEraser(context).eraseSignature(sig)
              sig <- encodeErasedSignature(sig)
            yield dev.argon.tube.ModuleExport.Enum(id, sig)
            

          case c.ModuleExportC.Exported(exp) =>
            for
              inner <- emitModuleExport(exp)
            yield dev.argon.tube.ModuleExport.Exported(inner)
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
          sig <- emitFunctionSignature(sig)

          importSpec <- func.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          impl <- ZIO.foreach(func.implementation) { impl =>
            impl.flatMap {
              case context.implementations.FunctionImplementation.Expr(e) =>
                for
                  e <- emitExpr(e)
                yield FunctionImplementation.Expr(e)
              case context.implementations.FunctionImplementation.Extern(name) =>
                ZIO.succeed(FunctionImplementation.Extern(name))
            }
          }

        yield TubeFileEntry.FunctionDefinition(
          FunctionDefinition(
            functionId = id,
            `import` = importSpec,
            `inline` = func.isInline,
            erased = func.isErased,
            witness = func.isWitness,
            effects = encodeEffectInfo(func.effects),
            signature = sig,
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
          fields <- ZIO.foreach(fields)(emitRecordFieldDef)

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
        field.owningRecord match {
          case r: ArRecord =>
            for
              recordId <- getRecordId(r)
            yield TubeFileEntry.RecordFieldReference(
              recordFieldId = id,
              recordId = id,
              name = encodeIdentifier(field.name),
            )

          case v: EnumVariant =>
            for
              variantId <- getEnumVariantId(v)
            yield TubeFileEntry.EnumVariantRecordFieldReference(
              recordFieldId = id,
              variantId = variantId,
              name = encodeIdentifier(field.name),
            )
        }

      private def emitRecordFieldDef(field: RecordField): Comp[RecordFieldDefinition] =
        for
          t <- emitExpr(field.fieldType)
        yield RecordFieldDefinition(
          name = encodeIdentifier(field.name),
          fieldType = t,
          mutable = field.isMutable,
        )


      override def emitEnum(e: ArEnum, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(e, e.importSpecifier)(emitEnumDef(id), emitEnumRef(id))

      private def emitEnumDef(id: BigInt)(e: ArEnum): Comp[TubeFileEntry] =
        for
          sig <- e.signature
          sig <- emitFunctionSignature(sig)

          importSpec <- e.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          variants <- e.variants
          variants <- ZIO.foreach(variants) { variant =>
            for
              sig <- variant.signature
              sig <- emitFunctionSignature(sig)

              fields <- variant.fields
              fields <- ZIO.foreach(fields)(emitRecordFieldDef)
            yield EnumVariantDefinition(
              name = encodeIdentifier(variant.name),
              signature = sig,
              fields = fields,
            )
          }

        yield TubeFileEntry.EnumDefinition(
          EnumDefinition(
            enumId = id,
            `import` = importSpec,
            signature = sig,
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

      private def emitFunctionSignature(sig: FunctionSignature): Comp[dev.argon.tube.FunctionSignature] =
        for
          params <- ZIO.foreach(sig.parameters)(emitSignatureParam)
          returnType <- emitExpr(sig.returnType)
          ensuresClauses <- ZIO.foreach(sig.ensuresClauses)(emitExpr)
        yield dev.argon.tube.FunctionSignature(
          parameters = params,
          returnType = returnType,
          ensuresClauses = ensuresClauses,
        )

      private def emitSignatureParam(param: SignatureParameter): Comp[dev.argon.tube.SignatureParameter] =
        for
          bindings <- ZIO.foreach(param.bindings) { binding =>
            for
              t <- emitExpr(binding.paramType)
            yield ParameterBinding(
              name = binding.name.map(encodeIdentifier),
              paramType = t,
            )
          }
          t <- emitExpr(param.paramType)
        yield dev.argon.tube.SignatureParameter(
          listType = param.listType match {
            case dev.argon.ast.FunctionParameterListType.NormalList => FunctionParameterListType.NormalList
            case dev.argon.ast.FunctionParameterListType.InferrableList => FunctionParameterListType.InferrableList
            case dev.argon.ast.FunctionParameterListType.QuoteList => FunctionParameterListType.QuoteList
            case dev.argon.ast.FunctionParameterListType.RequiresList => FunctionParameterListType.RequiresList
          },
          erased = param.isErased,
          bindings = bindings,
          name = param.name.map(encodeIdentifier),
          paramType = t,
        )

      private def emitExpr(e: ArExpr): Comp[Expr] =
        for
          knownVars <- Ref.make(Map.empty[context.DefaultExprContext.LocalVar, Int])
          res <- ExprEmitter(
            knownVars = knownVars,
          ).expr(e)
        yield res

      private final class ExprEmitter(
        knownVars: Ref[Map[context.DefaultExprContext.LocalVar, Int]]
      ) {

        private def declareVar(v: context.DefaultExprContext.LocalVar): Comp[LocalVar] =
          for
            id <- knownVars.modify(kv => kv.get(v) match {
              case Some(id) => (id, kv)
              case None =>
                val id = kv.size
                (id, kv + (v -> id))
            })
            t <- expr(v.varType)
          yield LocalVar(
            id = id,
            varType = t,
            name = v.name.map(encodeIdentifier),
            mutable = v.isMutable,
            erased = v.isErased,
            witness = v.isWitness,
          )

        private def getVar(v: context.DefaultExprContext.Var): Comp[Var] =
          v match {
            case v: context.DefaultExprContext.LocalVar =>
              for
                kv <- knownVars.get
                index <- ZIO.succeed(kv(v))
              yield Var.LocalVar(index)

            case v: context.DefaultExprContext.ParameterVar =>
              for
                owner <- v.owner match {
                  case context.DefaultExprContext.ParameterOwner.Func(f) =>
                    for
                      id <- getFunctionId(f)
                    yield ParameterOwner.Func(id)

                  case context.DefaultExprContext.ParameterOwner.Rec(r) =>
                    for
                      id <- getRecordId(r)
                    yield ParameterOwner.Rec(id)

                  case context.DefaultExprContext.ParameterOwner.Enum(e) =>
                    for
                      id <- getEnumId(e)
                    yield ParameterOwner.Enum(id)

                  case context.DefaultExprContext.ParameterOwner.EnumVariant(v) =>
                    for
                      id <- getEnumVariantId(v)
                    yield ParameterOwner.EnumVariant(id)
                }

                varType <- expr(v.varType)

              yield Var.ParameterVar(
                owner = owner,
                parameterIndex = v.parameterIndex,
                name = v.name.map(encodeIdentifier),
                varType = varType,
                erased = v.isErased,
                witness = v.isWitness,
              )
          }
          
        
        def expr(e: ArExpr): Comp[Expr] =
          e match {
            case ArExpr.Error() => ZIO.succeed(Expr.Error())
            case ArExpr.ErasedValue() => ZIO.succeed(Expr.ErasedValue())
            case ArExpr.And(a, b) =>
              for
                a <- expr(a)
                b <- expr(b)
              yield Expr.And(a, b)

            case ArExpr.AnyType() => ZIO.succeed(Expr.AnyType())
            case ArExpr.Hole(hole) => hole
            case ArExpr.BindVariable(v, value) =>
              for
                value <- expr(value)
                v <- declareVar(v)
              yield Expr.BindVariable(v, value)

            case ArExpr.BoolLiteral(b) => ZIO.succeed(Expr.BoolLiteral(b))

            case ArExpr.Box(t, value) =>
              for
                t <- expr(t)
                value <- expr(value)
              yield Expr.Box(t, value)

            case ArExpr.Boxed(t) =>
              for
                t <- expr(t)
              yield Expr.Boxed(t)
            
            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin)) =>
              val builtin2 = builtin match {
                case dev.argon.expr.NullaryBuiltin.IntType => NullaryBuiltin.IntType
                case dev.argon.expr.NullaryBuiltin.BoolType => NullaryBuiltin.BoolType
                case dev.argon.expr.NullaryBuiltin.StringType => NullaryBuiltin.StringType
                case dev.argon.expr.NullaryBuiltin.NeverType => NullaryBuiltin.NeverType
              }

              ZIO.succeed(Expr.NullaryBuiltin(builtin2))

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Unary(builtin, a)) =>
              val builtin2 = builtin match {
                case dev.argon.expr.UnaryBuiltin.IntNegate => UnaryBuiltin.IntNegate
                case dev.argon.expr.UnaryBuiltin.IntBitNot => UnaryBuiltin.IntBitNot
                case dev.argon.expr.UnaryBuiltin.BoolNot => UnaryBuiltin.BoolNot
              }

              for
                a <- expr(a)
              yield Expr.UnaryBuiltin(builtin2, a)

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Binary(builtin, a, b)) =>
              val builtin2 = builtin match {
                case dev.argon.expr.BinaryBuiltin.ConjunctionType => BinaryBuiltin.ConjunctionType
                case dev.argon.expr.BinaryBuiltin.DisjunctionType => BinaryBuiltin.DisjunctionType
                case dev.argon.expr.BinaryBuiltin.IntAdd => BinaryBuiltin.IntAdd
                case dev.argon.expr.BinaryBuiltin.IntSub => BinaryBuiltin.IntSub
                case dev.argon.expr.BinaryBuiltin.IntMul => BinaryBuiltin.IntMul
                case dev.argon.expr.BinaryBuiltin.IntBitAnd => BinaryBuiltin.IntBitAnd
                case dev.argon.expr.BinaryBuiltin.IntBitOr => BinaryBuiltin.IntBitOr
                case dev.argon.expr.BinaryBuiltin.IntBitXOr => BinaryBuiltin.IntBitXor
                case dev.argon.expr.BinaryBuiltin.IntBitShiftLeft => BinaryBuiltin.IntBitShiftLeft
                case dev.argon.expr.BinaryBuiltin.IntBitShiftRight => BinaryBuiltin.IntBitShiftRight
                case dev.argon.expr.BinaryBuiltin.IntEQ => BinaryBuiltin.IntEq
                case dev.argon.expr.BinaryBuiltin.IntNE => BinaryBuiltin.IntNe
                case dev.argon.expr.BinaryBuiltin.IntLT => BinaryBuiltin.IntLt
                case dev.argon.expr.BinaryBuiltin.IntLE => BinaryBuiltin.IntLe
                case dev.argon.expr.BinaryBuiltin.IntGT => BinaryBuiltin.IntGt
                case dev.argon.expr.BinaryBuiltin.IntGE => BinaryBuiltin.IntGe
                case dev.argon.expr.BinaryBuiltin.StringConcat => BinaryBuiltin.StringConcat
                case dev.argon.expr.BinaryBuiltin.StringEQ => BinaryBuiltin.StringEq
                case dev.argon.expr.BinaryBuiltin.StringNE => BinaryBuiltin.StringNe
                case dev.argon.expr.BinaryBuiltin.BoolEQ => BinaryBuiltin.BoolEq
                case dev.argon.expr.BinaryBuiltin.BoolNE => BinaryBuiltin.BoolNe
              }

              for
                a <- expr(a)
                b <- expr(b)
              yield Expr.BinaryBuiltin(builtin2, a, b)

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.EqualTo(t, a, b)) =>
              for
                t <- expr(t)
                a <- expr(a)
                b <- expr(b)
              yield Expr.BuiltinEqualTo(t, a, b)

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.EqualToRefl(t, a)) =>
              for
                t <- expr(t)
                a <- expr(a)
              yield Expr.BuiltinEqualToRefl(t, a)

            case et: ArExpr.EnumType =>
              encodeEnumType(et).map(Expr.EnumType.apply)
              
            case ArExpr.EnumVariantLiteral(enumType, v, args, fields) =>
              for
                enumType <- encodeEnumType(enumType)
                variantId <- getEnumVariantId(v)
                args <- ZIO.foreach(args)(expr)
                fields <- ZIO.foreach(fields)(encodeRecordFieldLiteral)
              yield Expr.EnumVariantLiteral(enumType, variantId, args, fields)
              
            case ArExpr.Finally(action, ensuring) =>
              for
                action <- expr(action)
                ensuring <- expr(ensuring)
              yield Expr.Finally(action, ensuring)
              
            case ArExpr.FunctionCall(f, args) =>
              for
                functionId <- getFunctionId(f)
                args <- ZIO.foreach(args)(expr)
              yield Expr.FunctionCall(functionId, args)

            case ArExpr.FunctionObjectCall(f, a) =>
              for
                f <- expr(f)
                a <- expr(a)
              yield Expr.FunctionObjectCall(f, a)

            case ArExpr.FunctionType(a, r) =>
              for
                a <- declareVar(a)
                r <- expr(r)
              yield Expr.FunctionType(a, r)

            case ifElse: ArExpr.IfElse =>
              for
                condition <- expr(ifElse.condition)


                whenTrueWitness <- ZIO.foreach(ifElse.whenTrueWitness)(declareVar)
                trueBody <- expr(ifElse.trueBody)

                whenFalseWitness <- ZIO.foreach(ifElse.whenFalseWitness)(declareVar)
                falseBody <- expr(ifElse.falseBody)

              yield Expr.IfElse(
                condition = condition,
                trueBody = trueBody,
                falseBody = falseBody,
                whenTrueWitness = whenTrueWitness,
                whenFalseWitness = whenFalseWitness,
              )

            case ArExpr.IntLiteral(i) =>
              ZIO.succeed(Expr.IntLiteral(i))

            case ArExpr.Is(value, p) =>
              for
                value <- expr(value)
                p <- pattern(p)
              yield Expr.Is(value, p)

            case lambda: ArExpr.Lambda =>
              for
                v <- declareVar(lambda.v)
                returnType <- expr(lambda.returnType)
                body <- expr(lambda.body)

              yield Expr.Lambda(
                v = v,
                returnType = returnType,
                body = body,
              )

            case ArExpr.Match(value, cases) =>
              for
                value <- expr(value)
                cases <- ZIO.foreach(cases) { matchCase =>
                  for
                    p <- pattern(matchCase.pattern)
                    body <- expr(matchCase.body)
                  yield MatchCase(p, body)
                }
              yield Expr.Match(value, cases)

            case ArExpr.Or(a, b) =>
              for
                a <- expr(a)
                b <- expr(b)
              yield Expr.Or(a, b)

            case rt: ArExpr.RecordType =>
              encodeRecordType(rt).map(Expr.RecordType.apply)

            case ArExpr.RecordLiteral(rt, fields) =>
              for
                rt <- encodeRecordType(rt)
                fields <- ZIO.foreach(fields)(encodeRecordFieldLiteral)
              yield Expr.RecordLiteral(rt, fields)

            case ArExpr.RecordFieldLoad(rt, field, recordValue) =>
              for
                rt <- encodeRecordType(rt)
                field <- getRecordFieldId(field)
                recordValue <- expr(recordValue)
              yield Expr.RecordFieldLoad(rt, field, recordValue)

            case ArExpr.RecordFieldStore(rt, field, recordValue, fieldValue) =>
              for
                rt <- encodeRecordType(rt)
                field <- getRecordFieldId(field)
                recordValue <- expr(recordValue)
                fieldValue <- expr(fieldValue)
              yield Expr.RecordFieldStore(rt, field, recordValue, fieldValue)

            case ArExpr.Sequence(stmts, result) =>
              for
                head <- expr(stmts.headOption.getOrElse(result))
                tail <- ZIO.foreach(stmts.drop(1) ++ (if stmts.isEmpty then Seq() else Seq(result)))(expr)
              yield Expr.Sequence(head, tail)

            case ArExpr.StringLiteral(s) =>
              ZIO.succeed(Expr.StringLiteral(s))

            case ArExpr.Tuple(items) =>
              for
                items <- ZIO.foreach(items)(expr)
              yield Expr.Tuple(items)

            case ArExpr.TupleElement(index, tuple) =>
              for
                tuple <- expr(tuple)
              yield Expr.TupleElement(index, tuple)

            case ArExpr.TypeN(n) =>
              for
                n <- expr(n)
              yield Expr.TypeN(n)

            case ArExpr.TypeBigN(n) =>
              ZIO.succeed(Expr.TypeBigN(n))
              
            case ArExpr.Unbox(t, value) =>
              for
                t <- expr(t)
                value <- expr(value)
              yield Expr.Unbox(t, value)

            case ArExpr.Variable(v) =>
              for
                v <- getVar(v)
              yield Expr.Variable(v)

            case ArExpr.VariableStore(v, value) =>
              for
                v <- getVar(v)
                value <- expr(value)
              yield Expr.VariableStore(v, value)
          }

        def pattern(p: ArPattern): Comp[Pattern] =
          p match {
            case ArPattern.Discard(t) => 
              for
                t <- expr(t)
              yield Pattern.Discard(t)
            case ArPattern.Tuple(elements) =>
              for
                elements <- ZIO.foreach(elements)(pattern)
              yield Pattern.Tuple(elements)

            case ArPattern.Binding(v, p) =>
              for
                v <- declareVar(v)
                p <- pattern(p)
              yield Pattern.Binding(v, p)

            case ArPattern.EnumVariant(enumType, variant, args, fields) =>
              for
                enumType <- encodeEnumType(enumType)
                variant <- getEnumVariantId(variant)
                args <- ZIO.foreach(args)(pattern)
                fields <- ZIO.foreach(fields)(encodeRecordFieldPattern)
              yield Pattern.EnumVariant(enumType, variant, args, fields)

            case ArPattern.String(s) =>
              ZIO.succeed(Pattern.String(s))

            case ArPattern.Int(i) =>
              ZIO.succeed(Pattern.Int(i))

            case ArPattern.Bool(b) =>
              ZIO.succeed(Pattern.Bool(b))
          }

        private def encodeRecordFieldPattern(field: context.DefaultExprContext.RecordFieldPattern): Comp[RecordFieldPattern] =
          for
            fieldId <- getRecordFieldId(field.field)
            pattern <- pattern(field.pattern)
          yield RecordFieldPattern(
            fieldId = fieldId,
            pattern = pattern,
          )

        private def encodeRecordType(rt: ArExpr.RecordType): Comp[RecordType] =
          for
            recordId <- getRecordId(rt.record)
            args <- ZIO.foreach(rt.args)(expr)
          yield RecordType(recordId, args)

        private def encodeRecordFieldLiteral(field: context.DefaultExprContext.RecordFieldLiteral): Comp[RecordFieldLiteral] =
          for
            fieldId <- getRecordFieldId(field.field)
            value <- expr(field.value)
          yield RecordFieldLiteral(
            fieldId = fieldId,
            value = value,
          )

        private def encodeEnumType(e: ArExpr.EnumType): Comp[EnumType] =
          for
            enumId <- getEnumId(e.e)
            args <- ZIO.foreach(e.args)(expr)
          yield EnumType(enumId, args)

      }
    }
}


