package dev.argon.vm.encoder

import dev.argon.{ast, compiler, compiler as c, expr as e}
import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.encoder.TubeEncoderBase
import dev.argon.vm.*
import zio.*
import zio.stm.{TMap, TRef, USTM}
import dev.argon.compiler.{ArgonEvaluator, HasContext, MethodOwner, SignatureEraser, VTableBuilder}
import dev.argon.expr.{BinaryBuiltin, CaptureScanner, ErasureMode, FreeVariableScanner, NullaryBuiltin, UnaryBuiltin, ValueUtil}
import dev.argon.util.UniqueIdentifier


private[vm] class TubeEncoder(platformId: String) extends TubeEncoderBase[TubeFileEntry] {
  override def createEmitter(state: EncodeState): state.Emitter =
    new state.Emitter {
      import state.*
      import context.DefaultExprContext.{Expr as ArExpr, Pattern as ArPattern, ExpressionOwner, Var}
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
        specifier match {
          case specifier: c.ImportSpecifier.Global =>
            for
              moduleId <- getModuleId(c.ModuleName(specifier.tube, specifier.module))
              sig <- encodeErasedSignature(specifier.signature)
            yield ImportSpecifier.Global(
              moduleId = moduleId,
              name = encodeIdentifier(specifier.name),
              sig = sig
            )

          case specifier: c.ImportSpecifier.Local =>
            for
              parent <- encodeImportSpecifier(specifier.parent)
              id <- getLocalImportId(specifier)
            yield ImportSpecifier.Local(parent, id)
        }

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
        ZIO.foreach(orderedModules) { module =>
            for
              modExports <- module.allExports(Set.empty)
              _ <- ZIO.foreachDiscard(modExports.values) { items =>
                ZIO.foreachDiscard(items)(emitModuleExport)
              }
            yield Module(
              path = encodeModulePath(module.path),
            )
        }

      private def emitModuleExport(exp: ModuleExport): Comp[Unit] =
        exp match {
          case c.ModuleExportC.Function(f) =>
            getFunctionId(f).unit.whenDiscard(f.erasureMode != ErasureMode.Erased)

          case c.ModuleExportC.Record(r) =>
            getRecordId(r).unit

          case c.ModuleExportC.Enum(e) =>
            getEnumId(e).unit

          case c.ModuleExportC.Trait(t) =>
            getTraitId(t).unit

          case c.ModuleExportC.Instance(i) =>
            getInstanceId(i).unit
            
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
          def getTubeFromSpecifier(specifier: c.ImportSpecifier): c.TubeName =
            specifier match {
              case c.ImportSpecifier.Global(name, _, _, _) => name
              case c.ImportSpecifier.Local(parent, _) => getTubeFromSpecifier(parent)
            }


          if getTubeFromSpecifier(specifier) == tube.name then
            emitDef(value)
          else
            encodeImportSpecifier(specifier).flatMap(emitRef)
        }
        

      override def emitFunction(func: ArFunc, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(func, func.importSpecifier)(emitFunctionDef(id), emitFunctionRef(id))

      private def emitFunctionDef(id: BigInt)(func: ArFunc): Comp[TubeFileEntry] =
        for
          sig <- func.signature
          encSig <- emitFunctionSignature(context.DefaultExprContext.ExpressionOwner.Func(func), sig)

          importSpec <- func.importSpecifier

          impl <- ZIO.foreach(func.implementation) { impl =>
            impl.flatMap {
              case context.implementations.FunctionImplementation.Expr(e) =>
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

          importSpec <- encodeImportSpecifier(importSpec)

        yield TubeFileEntry.FunctionDefinition(
          FunctionDefinition(
            functionId = id,
            `import` = importSpec,
            signature = encSig.sig,
            implementation = impl,
          )
        )

      private def emitSyntheticFunctionDef(sig: FunctionSignatureWithMapping, id: BigInt, importSpec: c.ImportSpecifier, body: ArExpr): Comp[TubeFileEntry] =
        for
          block <- emitFunctionBody(body, sig, importSpec)
          importSpec <- encodeImportSpecifier(importSpec)
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
          sig <- emitFunctionSignature(context.DefaultExprContext.ExpressionOwner.Rec(rec), sig)

          importSpec <- rec.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          fields <- rec.fields
          fields <- ZIO.foreach(fields)(emitRecordField(sig.typeEmitter))

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

      private def emitRecordField(typeEmitter: TokenEmitter)(field: RecordField): Comp[RecordFieldDefinition] =
        for
          t <- typeEmitter.tokenExpr(field.fieldType)
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
          sig <- emitFunctionSignature(context.DefaultExprContext.ExpressionOwner.Enum(e), sig)

          importSpec <- e.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          variants <- e.variants
          variants <- ZIO.foreach(variants) { variant =>
            for
              sig <- variant.signature
              sig <- emitFunctionSignature(context.DefaultExprContext.ExpressionOwner.EnumVariant(variant), sig)
              fields <- variant.fields
              fields <- ZIO.foreach(fields)(emitRecordField(sig.typeEmitter))
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

      override def emitTrait(t: ArTrait, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(t, t.importSpecifier)(emitTraitDef(id), emitTraitRef(id))

      private def emitTraitDef(id: BigInt)(t: ArTrait): Comp[TubeFileEntry] =
        for
          sig <- t.signature
          sig <- emitFunctionSignature(ExpressionOwner.Trait(t), sig)

          importSpec <- t.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          methods <- t.methods
          methodDefs <- ZIO.foreach(methods)(emitMethodDef)

          vtableBuilder = VTableBuilder(context)
          vtable <- vtableBuilder.buildVTable(MethodOwner.ByTrait(t))
          vtable <- encodeVTable(vtableBuilder)(sig, methods)(vtable)

        yield TubeFileEntry.TraitDefinition(
          TraitDefinition(
            traitId = id,
            `import` = importSpec,
            signature = sig.sig,
            vtable = vtable,
            methods = methodDefs,
          )
        )

      private def emitTraitRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
        ZIO.succeed(TubeFileEntry.TraitReference(
          traitId = id,
          `import` = specifier,
        ))


      override def emitMethod(m: ArMethod, id: BigInt): Comp[TubeFileEntry] =
        m.owner match {
          case MethodOwner.ByTrait(t) =>
            for
              traitId <- getTraitId(t)
              sig <- m.signature
              sig <- SignatureEraser(context).eraseSignature(sig)
              sig <- encodeErasedSignature(sig)
            yield TubeFileEntry.TraitMethodReference(
              methodId = id,
              traitId = traitId,
              name = encodeIdentifier(m.name),
              signature = sig,
            )

          case MethodOwner.ByInstance(_) =>
            ???
        }

      private def emitMethodDef(m: ArMethod): Comp[MethodDefinition] =
        for
          sig <- m.signature
          encSig <- emitMethodSignature(m)
          erasedSig <- SignatureEraser(context).eraseSignature(sig)
          erasedSig <- encodeErasedSignature(erasedSig)

          impl <- ZIO.foreach(m.implementation) { impl =>
            impl.flatMap {
              case context.implementations.MethodImplementation.Abstract =>
                ZIO.none

              case context.implementations.MethodImplementation.Expr(e) =>
                for
                  parentImportSpec <- m.owner match {
                    case MethodOwner.ByTrait(t) => t.importSpecifier
                    case MethodOwner.ByInstance(i) => i.importSpecifier
                  }

                  importSpec = c.ImportSpecifier.Local(parentImportSpec, m.id)

                  block <- emitFunctionBody(e, encSig, importSpec)
                yield Some(FunctionImplementation.VmIr(block))

              case context.implementations.MethodImplementation.Extern(externMap) =>
                ZIO.fromEither(
                    externMap.externs.dict.get(platformId)
                      .toRight(new TubeFormatException("Missing extern implementation for platform " + platformId))
                  )
                  .map(expr => Some(FunctionImplementation.Extern(expr)))
            }
          }

        yield MethodDefinition(
          name = encodeIdentifier(m.name),
          erasedSignature = erasedSig,
          signature = encSig.sig,
          implementation = impl.flatten,
        )

      private def encodeVTable(
        vtableBuilder: VTableBuilder & HasContext[context.type]
      )(
        ownerSig: FunctionSignatureWithMapping,
        methods: Seq[ArMethod],
      )(
        vtable: vtableBuilder.VTable
      ): Comp[Vtable] = {
        def encodeTarget(target: vtableBuilder.VTableTarget): Comp[VtableTarget] =
          target match {
            case vtableBuilder.VTableTarget.Abstract => ZIO.succeed(VtableTarget.Abstract())
            case vtableBuilder.VTableTarget.Ambiguous(_) => ZIO.succeed(VtableTarget.Ambiguous())
            case vtableBuilder.VTableTarget.Implementation(method) =>
              val methodIndex = methods.indexOf(method)
              if methodIndex >= 0 then
                ZIO.succeed(VtableTarget.Implementation(methodIndex))
              else
                ZIO.succeed(???)
          }

        for
          entries <- ZIO.foreach(vtable.entries.toSeq) { (slot, slotValue) =>
            for
              slotMethodId <- getMethodId(slot.method)
              slotInstanceType <- ownerSig.typeEmitter.tokenExpr(slotValue.slotInstanceType)
              target <- encodeTarget(slotValue.target)
            yield VtableEntry(slotMethodId, slotInstanceType, target)
          }
        yield Vtable(entries)
      }
      end encodeVTable

      override def emitInstance(i: ArInstance, id: BigInt): Comp[TubeFileEntry] =
        importOrDefine(i, i.importSpecifier)(emitInstanceDef(id), emitInstanceRef(id))

      private def emitInstanceDef(id: BigInt)(i: ArInstance): Comp[TubeFileEntry] =
        for
          sig <- i.signature
          sig <- emitFunctionSignature(ExpressionOwner.Instance(i), sig)

          importSpec <- i.importSpecifier
          importSpec <- encodeImportSpecifier(importSpec)

          methods <- i.methods
          methodDefs <- ZIO.foreach(methods)(emitMethodDef)

          vtableBuilder = VTableBuilder(context)
          vtable <- vtableBuilder.buildVTable(MethodOwner.ByInstance(i))
          vtable <- encodeVTable(vtableBuilder)(sig, methods)(vtable)

        yield TubeFileEntry.InstanceDefinition(
          InstanceDefinition(
            instanceId = id,
            `import` = importSpec,
            signature = sig.sig,
            vtable = vtable,
            methods = methodDefs,
          )
        )

      private def emitInstanceRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
        ZIO.succeed(TubeFileEntry.InstanceReference(
          instanceId = id,
          `import` = specifier,
        ))

      private final case class FunctionSignatureWithMapping(
        sig: dev.argon.vm.FunctionSignature,
        argConsumers: Seq[ArgConsumer],
        typeEmitter: TokenEmitter,
        knownVars: Map[context.DefaultExprContext.Var, VariableRealization],
        asInstanceTokenParams: Map[context.DefaultExprContext.Var, Token],
      )


      private final class FunctionSignatureBuilder private(
        instanceTypeParams: Map[context.DefaultExprContext.Var, Token],

        tokenParams: TRef[Seq[dev.argon.vm.SignatureTokenParameter]],
        params: TRef[Seq[dev.argon.vm.SignatureParameter]],

        argConsumers: TRef[Seq[ArgConsumer]],

        instanceParam: TRef[Option[context.DefaultExprContext.Var]],
        typeParamMapping: TMap[context.DefaultExprContext.Var, Int],
        paramVarMapping: TMap[context.DefaultExprContext.Var, Int],
      ) {

        private def typeEmitter: UIO[TokenEmitter] =
          (
            for
              tpm <- typeParamMapping.toMap
            yield TokenEmitter(instanceTypeParams ++ tpm.view.mapValues(i => Token.TokenParameter(i)).toMap)
          ).commit
          
        private def knownVars: UIO[Map[context.DefaultExprContext.Var, VariableRealization]] =
          (
            for
              inst <- instanceParam.get
              tpm <- typeParamMapping.toMap
              pvm <- paramVarMapping.toMap
            yield {
              val regOffset = if inst.isDefined then 1 else 0
              
              instanceTypeParams.view.mapValues(VariableRealization.Tok.apply).toMap ++
                inst.toList.map { inst => inst -> VariableRealization.Reg(RegisterId(0)) }.toMap ++
                pvm.map { (param, index) => param -> VariableRealization.Reg(RegisterId(regOffset + index)) } ++
                tpm.map { (param, index) => param -> VariableRealization.Tok(Token.TokenParameter(index)) }
            }
          ).commit

        private def asInstanceTokenParams: UIO[Map[context.DefaultExprContext.Var, Token]] =
          (
            for
              tpm <- typeParamMapping.toMap
            yield tpm.view.mapValues(i => Token.InstanceTokenParameter(i)).toMap
            ).commit

        def addInstanceParameter(v: context.DefaultExprContext.InstanceParameterVar): Comp[Unit] =
          instanceParam.set(Some(v)).commit
          
        
        def addParameter(v: context.DefaultExprContext.Var): Comp[Unit] =
          v.erasureMode match {
            case ErasureMode.Erased =>
              argConsumers.update(_ :+ ArgConsumer.Erased).commit

            case ErasureMode.Token =>
                for
                  te <- typeEmitter
                  tokenKind <- te.tokenExpr(v.varType)
                  tp = dev.argon.vm.SignatureTokenParameter(
                    name = v.name.map(encodeIdentifier),
                    kind = tokenKind,
                  )

                  _ <- (for
                    size <- tokenParams.modify(tps => (tps.size, tps :+ tp))
                    _ <- typeParamMapping.put(v, size)
                    _ <- argConsumers.update(_ :+ ArgConsumer.Token)
                  yield ()).commit
                yield ()

            case ErasureMode.Concrete =>
              def putParam(sigParam: dev.argon.vm.SignatureParameter): USTM[Unit] =
                for
                  size <- params.modify(params => (params.size, params :+ sigParam))
                  _ <- paramVarMapping.put(v, size)
                  _ <- argConsumers.update(_ :+ ArgConsumer.Arg)
                yield ()

              for
                te <- typeEmitter

                t <- te.tokenExpr(v.varType)
                sigParam = dev.argon.vm.SignatureParameter(
                  name = v.name.map(encodeIdentifier),
                  paramType = t,
                )
                _ <- putParam(sigParam).commit

              yield ()
          }

        def finish(returnType: ArExpr): Comp[FunctionSignatureWithMapping] =
          for
            returnType <- typeEmitter.flatMap(_.tokenExpr(returnType))

            tokenParams <- tokenParams.get.commit
            params <- params.get.commit
            argConsumers <- argConsumers.get.commit
            te <- typeEmitter
            kv <- knownVars
            itp <- asInstanceTokenParams
          yield FunctionSignatureWithMapping(
            sig = dev.argon.vm.FunctionSignature(
              tokenParameters = tokenParams,
              parameters = params,
              returnType = returnType,
            ),
            argConsumers = argConsumers,
            typeEmitter = te,
            knownVars = kv,
            asInstanceTokenParams = itp
          )
      }

      private object FunctionSignatureBuilder {
        def make(instanceTokenParams: Map[context.DefaultExprContext.Var, Token]): UIO[FunctionSignatureBuilder] =
          for
            typeParams <- TRef.make(Seq.empty[dev.argon.vm.SignatureTokenParameter]).commit
            params <- TRef.make(Seq.empty[dev.argon.vm.SignatureParameter]).commit

            argConsumers <- TRef.make(Seq.empty[ArgConsumer]).commit

            instParam <- TRef.make(Option.empty[context.DefaultExprContext.Var]).commit
            paramVarMapping <- TMap.empty[context.DefaultExprContext.Var, Int].commit
            typeParamMapping <- TMap.empty[context.DefaultExprContext.Var, Int].commit
          yield FunctionSignatureBuilder(instanceTokenParams, typeParams, params, argConsumers, instParam, paramVarMapping, typeParamMapping)
      }

      private enum ArgConsumer derives CanEqual {
        case Erased
        case Token
        case Arg
      }

      private def emitFunctionSignature(paramOwner: ExpressionOwner, sig: FunctionSignature): Comp[FunctionSignatureWithMapping] =
        FunctionSignatureBuilder.make(Map())
          .tap(emitFunctionSignatureParameters(paramOwner, sig))
          .flatMap { sb => sb.finish(sig.returnType) }

      private def methodOwnerInstanceTokenParams(methodOwner: MethodOwner[context.type]): Comp[Map[context.DefaultExprContext.Var, Token]] =
        (methodOwner match {
          case MethodOwner.ByTrait(t) =>
            t.signature.flatMap(emitFunctionSignature(ExpressionOwner.Trait(t), _))

          case MethodOwner.ByInstance(i) =>
            i.signature.flatMap(emitFunctionSignature(ExpressionOwner.Instance(i), _))
        }).map(_.asInstanceTokenParams)
      
      private def emitMethodSignature(method: ArMethod): Comp[FunctionSignatureWithMapping] =
        for
          itp <- methodOwnerInstanceTokenParams(method.owner)
          sb <- FunctionSignatureBuilder.make(itp)
          
          paramOwner = ExpressionOwner.Method(method)

          instanceType <- method.instanceType
          instanceParam <- method.instanceParam
          instanceVar = instanceParam.asInstanceVar(paramOwner, instanceType)
          _ <- sb.addInstanceParameter(instanceVar)

          sig <- method.signature
          _ <- emitFunctionSignatureParameters(paramOwner, sig)(sb)

          res <- sb.finish(sig.returnType)
        yield res

      private def emitFunctionSignatureParameters(paramOwner: ExpressionOwner, sig: FunctionSignature)(sb: FunctionSignatureBuilder): Comp[Unit] =
        ZIO.foreachDiscard(sig.parameters.view.zipWithIndex) { (param, origIndex) =>
          val paramVar = param.asParameterVar(paramOwner, origIndex)
          sb.addParameter(paramVar)
        }


      enum VariableRealization {
        case Reg(register: RegisterId)
        case Tok(token: Token)
      }

      private def emitFunctionBody(e: ArExpr, funcSig: FunctionSignatureWithMapping, funcImport: c.ImportSpecifier): Comp[FunctionBody] =
        for
          varOffset <- TRef.makeCommit(funcSig.sig.parameters.size)
          knownVars <- TMap.fromIterable[context.DefaultExprContext.Var, VariableRealization](funcSig.knownVars).commit
          instructions <- TRef.make(Seq.empty[Instruction]).commit

          capturedVars = CaptureScanner(context.DefaultExprContext)(e)

          emitter = ExprEmitter(
            varOffset = varOffset,
            knownVars = knownVars,
            instructions = instructions,
            varInstructions = instructions,
            parentImportSpecifier = funcImport,
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

      private trait TokenEmitterBase {
        protected def getParameterAsToken(v: context.DefaultExprContext.Var): Comp[Option[Token]]

        private def fallbackTokenExpr(t: ArExpr): Comp[Token] =
          ZIO.logError("Unimplemented token expression: " + t).as(???)


        def tokenExpr(t: ArExpr): Comp[Token] =
          ArgonEvaluator(context).normalizeToValue(t, context.Config.evaluatorFuel).flatMap {
            case ArExpr.Boxed(_) =>
              ZIO.succeed(Token.Boxed())

            case ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin)) =>
              ZIO.succeed(
                builtin match {
                  case NullaryBuiltin.BoolType => Token.Builtin(BuiltinType.Bool(), Seq())
                  case NullaryBuiltin.IntType => Token.Builtin(BuiltinType.Int(), Seq())
                  case NullaryBuiltin.StringType => Token.Builtin(BuiltinType.String(), Seq())
                  case NullaryBuiltin.NeverType => Token.Builtin(BuiltinType.Never(), Seq())
                }
              )

            case ArExpr.FunctionType(a, r) if a.erasureMode == ErasureMode.Erased =>
              for
                r <- tokenExpr(r)
              yield Token.FunctionErased(r)

            case ArExpr.FunctionType(a, r) =>
              for
                a <- tokenExpr(a.varType)
                r <- tokenExpr(r)
              yield Token.Function(a, r)

            case ArExpr.RecordType(rec, args) =>
              for
                id <- getRecordId(rec)
                args <- ZIO.foreach(args)(tokenExpr)
              yield Token.Record(id, args)

            case ArExpr.EnumType(rec, args) =>
              for
                id <- getEnumId(rec)
                args <- ZIO.foreach(args)(tokenExpr)
              yield Token.Enum(id, args)

            case ArExpr.TraitType(trt, args) =>
              for
                id <- getTraitId(trt)
                args <- ZIO.foreach(args)(tokenExpr)
              yield Token.Trait(id, args)

            case ArExpr.Tuple(items) =>
              ZIO.foreach(items)(tokenExpr)
                .map(Token.Tuple.apply)

            case ArExpr.TypeN(_) =>
              ZIO.succeed(Token.TypeInfo())

            case ArExpr.Variable(v) =>
              v.erasureMode match {
                case ErasureMode.Erased | ErasureMode.Concrete =>
                  fallbackTokenExpr(t)
                  
                case ErasureMode.Token =>
                  getParameterAsToken(v).flatMap {
                    case Some(tp) => ZIO.succeed(tp)
                    case None => fallbackTokenExpr(t)
                  }
                  
              }

            case t =>
              fallbackTokenExpr(t)
          }

      }

      private final class TokenEmitter(
        tokenParams: Map[context.DefaultExprContext.Var, Token],
      ) extends TokenEmitterBase {
        override protected def getParameterAsToken(v: state.context.DefaultExprContext.Var): Comp[Option[Token]] =
          ZIO.succeed(tokenParams.get(v))
      }

      private final class ExprEmitter(
        varOffset: TRef[Int],
        knownVars: TMap[context.DefaultExprContext.Var, VariableRealization],
        instructions: TRef[Seq[Instruction]],
        varInstructions: TRef[Seq[Instruction]],
        parentImportSpecifier: c.ImportSpecifier,
        capturedVars: Set[context.DefaultExprContext.Var],
      ) extends TokenEmitterBase {
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
            parentImportSpecifier = parentImportSpecifier,
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
              parentImportSpecifier = parentImportSpecifier,
              capturedVars = capturedVars,
            )
            a <- f(scope)
            block <- scope.toBlock
          yield (block, a)

        private def declareVar(v: context.DefaultExprContext.LocalVar): Comp[RegisterId] =
          for
            t <- tokenExpr(v.varType)
            id <- addVar(t).tap { id => knownVars.put(v, VariableRealization.Reg(id)) }.commit
          yield id

        private def addVar(t: Token, captured: Option[VariableCaptureMode] = None): USTM[RegisterId] =
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
                vmType <- tokenExpr(t)
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
                .flatMap(tokenExpr)
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

            case ArExpr.And(a, b) =>
              intoRegister(e, output) { r =>
                for
                  _ <- expr(a, ExprOutput.Register(r))
                  (block, _) <- nestedBlockNoScope { emitter =>
                    emitter.expr(b, ExprOutput.Register(r))
                  }
                  _ <- emit(Instruction.IfElse(r, block, Block(Seq())))
                yield ()
              }
              
            case ArExpr.BindVariable(v, value) =>
              v.erasureMode match {
                case ErasureMode.Erased =>
                  unitResult(e, output)(ZIO.unit)

                case ErasureMode.Concrete =>
                  unitResult(e, output)(
                    for
                      r <- declareVar(v)
                      _ <- expr(value, ExprOutput.Register(r))
                    yield ()
                  )
              }

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
                def tokenLiteral(t: Token): Comp[Unit] =
                  emit(Instruction.LoadToken(r, t))

                def builtinToken(t: BuiltinType): Comp[Unit] =
                  tokenLiteral(Token.Builtin(t, Seq()))

                builtin match {
                  case NullaryBuiltin.IntType => builtinToken(BuiltinType.Int())
                  case NullaryBuiltin.BoolType => builtinToken(BuiltinType.Bool())
                  case NullaryBuiltin.StringType => builtinToken(BuiltinType.String())
                  case NullaryBuiltin.NeverType => builtinToken(BuiltinType.Never())
                }
              }

            case ArExpr.EnumVariantLiteral(enumType, v, args, fields) =>
              intoRegister(e, output) { r =>
                for
                  enumType <- tokenExpr(enumType)
                  
                  id <- getEnumVariantId(v)
                  
                  sig <- v.signature
                  funcArgs <- emitArguments(ExpressionOwner.EnumVariant(v), sig, args)
                  
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
                    funcArgs.tokenArguments,
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
                  funcArgs <- emitArguments(ExpressionOwner.Func(f), sig, args)
                  _ <- emit(Instruction.FunctionCall(id, funcResult, funcArgs.tokenArguments, funcArgs.arguments))
                yield ()
              }

            case ArExpr.FunctionObjectCall(f, a) =>
              functionResult(e, output) { funcResult =>
                getExprType(f).flatMap {
                  case ArExpr.FunctionType(v, r) =>
                    v.erasureMode match {
                      case ErasureMode.Erased =>
                        for
                          func <- expr(f, ExprOutput.AnyRegister)
                          _ <- emit(Instruction.FunctionObjectErasedCall(funcResult, func))
                        yield ()

                      case ErasureMode.Token =>
                        for
                          func <- expr(f, ExprOutput.AnyRegister)
                          a <- tokenExpr(a)
                          _ <- emit(Instruction.FunctionObjectTokenCall(funcResult, func, a))
                        yield ()

                      case ErasureMode.Concrete =>
                        for
                          func <- expr(f, ExprOutput.AnyRegister)
                          a <- expr(a, ExprOutput.AnyRegister)
                          _ <- emit(Instruction.FunctionObjectCall(funcResult, func, a))
                        yield ()
                    }

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

            case ArExpr.InstanceMethodCall(m, instanceType, obj, args) =>
              functionResult(e, output) { funcResult =>
                for
                  id <- getMethodId(m)
                  instanceType <- tokenExpr(instanceType)
                  instanceObj <- expr(obj, ExprOutput.AnyRegister)
                  funcArgs <- emitMethodArguments(m, args)
                  _ <- emit(Instruction.InstanceMethodCall(funcResult, id, instanceType, instanceObj, funcArgs.tokenArguments, funcArgs.arguments))
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
              intoRegister(e, output) { r =>
                for
                  funcId <- UniqueIdentifier.make
                  synImportSpec: c.ImportSpecifier.Local = c.ImportSpecifier.Local(parentImportSpecifier, funcId)
                  synId <- newSyntheticFunctionId
                  freeVars = FreeVariableScanner(context.DefaultExprContext)(e)
                  (tokenArgs, args, sig) <- captureVariables(freeVars)(_.addParameter(param), _.finish(returnType))

                  _ <- state.emitEntryBuilder(emitSyntheticFunctionDef(sig, synId, synImportSpec, body))
                  _ <- param.erasureMode match {
                    case ErasureMode.Erased =>
                      emit(Instruction.PartiallyAppliedFunctionErased(synId, r, tokenArgs, args))

                    case ErasureMode.Token =>
                      emit(Instruction.PartiallyAppliedTokenFunction(synId, r, tokenArgs, args))

                    case ErasureMode.Concrete =>
                      emit(Instruction.PartiallyAppliedFunction(synId, r, tokenArgs, args))
                  }


                yield ()
              }

            case ArExpr.Match(value, cases) =>
              knownLocation(e, output) { output =>
                for
                  valueReg <- expr(value, ExprOutput.AnyRegister)
                  condReg <- addVar(Token.Builtin(BuiltinType.Bool(), Seq())).commit
                  _ <- emitCases(condReg, valueReg, output, cases)
                yield ()
              }

            case ArExpr.NewInstance(i, args) =>
              intoRegister(e, output) { r =>
                for
                  id <- getInstanceId(i)
                  sig <- i.signature
                  funcArgs <- emitArguments(ExpressionOwner.Instance(i), sig, args)
                  _ <- emit(Instruction.NewInstance(id, r, funcArgs.tokenArguments, funcArgs.arguments))
                yield ()
              }

            case ArExpr.Or(a, b) =>
              intoRegister(e, output) { r =>
                for
                  _ <- expr(a, ExprOutput.Register(r))
                  (block, _) <- nestedBlockNoScope { emitter =>
                    emitter.expr(b, ExprOutput.Register(r))
                  }
                  _ <- emit(Instruction.IfElse(r, Block(Seq()), block))
                yield ()
              }

            case e @ (ArExpr.RecordType(_, _) | ArExpr.EnumType(_, _)) =>
              intoRegister(e, output) { r =>
                tokenExpr(e).flatMap { t =>
                  emit(Instruction.LoadToken(r, t))
                }
              }

            case ArExpr.RecordFieldLoad(_, field, recordValue) =>
              intoRegister(e, output) { r =>
                for
                  fieldId <- getRecordFieldId(field)
                  recordValue <- expr(recordValue, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.RecordFieldLoad(fieldId, r, recordValue))
                yield ()
              }

            case ArExpr.RecordFieldStore(_, field, recordValue, fieldValue) =>
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
                  recType <- tokenExpr(recordType)
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
                  t <- tokenExpr(t)
                  a <- expr(value, ExprOutput.AnyRegister)
                  _ <- emit(Instruction.Unbox(r, t, a))
                yield ()
              }
              

            case ArExpr.Variable(v) =>
              knownVars.get(v).commit.flatMap {                
                case Some(VariableRealization.Reg(id)) =>
                  existingRegister(output)(ZIO.succeed(id))

                case Some(VariableRealization.Tok(tk)) =>
                  intoRegister(e, output) { r =>
                    emit(Instruction.LoadToken(r, tk))
                  }

                case None =>
                  knownVars.toMap.commit.flatMap { kv => zio.Console.printLineError("Known variables: " + kv).orDie } *>
                  ZIO.fail(TubeFormatException("Could not get index for variable: " + v))
              }

            case ArExpr.VariableStore(v, value) =>
              unitResult(e, output)(
                knownVars.get(v).commit.flatMap {
                  case Some(VariableRealization.Reg(r)) =>
                    expr(value, ExprOutput.Register(r))

                  case Some(VariableRealization.Tok(_)) =>
                    ZIO.fail(TubeFormatException("Cannot assign to token parameter"))

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
                    vt <- tokenExpr(exprType.getPatternType(arg))
                    argReg <- addVar(vt).commit
                  yield (argReg, arg)
                }
                
                fieldPatterns <- ZIO.foreach(fields) { field =>
                  for
                    vt <- tokenExpr(exprType.getPatternType(field.pattern))
                    fieldReg <- addVar(vt).commit
                    fieldId <- getRecordFieldId(field.field)
                  yield (fieldReg, fieldId, field.pattern)
                }
                
                et <- tokenExpr(enumType)
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
                et <- tokenExpr(exprType.getPatternType(h))
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

        private def emitCases(condReg: RegisterId, valueReg: RegisterId, output: ExprOutput.KnownLocation, cases: Seq[context.DefaultExprContext.MatchCase]): Comp[Unit] =
          cases match {
            case h +: t =>
              for
                _ <- emitPattern(condReg, valueReg, h.pattern)
                (caseBlock, _) <- nestedBlock { emitter => emitter.expr(h.body, output) }
                (elseBlock, _) <- nestedBlock { emitter =>
                  emitter.emitCases(condReg, valueReg, output, t)
                }

                _ <- emit(Instruction.IfElse(condReg, caseBlock, elseBlock))
              yield ()

            case _ =>
              emit(Instruction.Unreachable())
          }

        private def captureVariables(
          vars: Seq[Var],
        )(
          buildRest: FunctionSignatureBuilder => Comp[Unit],
          finish: FunctionSignatureBuilder => Comp[FunctionSignatureWithMapping],
        ): Comp[(Seq[Token], Seq[Capture], FunctionSignatureWithMapping)] =
          FunctionSignatureBuilder.make(Map())
            .tap { sb =>
              ZIO.foreachDiscard(vars)(sb.addParameter) *>
                buildRest(sb)
            }
            .flatMap { sb =>
              for
                sig <- finish(sb)
                tokenArgs <- Ref.make(Seq.empty[Token])
                args <- Ref.make(Seq.empty[Capture])

                // This will drop any consumers after the args
                _ <- ZIO.foreachDiscard(sig.argConsumers.zip(vars)) {
                  case (ArgConsumer.Erased, _) => ZIO.unit
                  case (ArgConsumer.Token, v) =>
                    knownVars.get(v).commit.flatMap {
                      case Some(VariableRealization.Tok(tk)) =>
                        tokenArgs.update(_ :+ tk)

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

                tokenArgs <- tokenArgs.get
                args <- args.get
              yield (tokenArgs, args, sig)
            }

        override protected def getParameterAsToken(v: state.context.DefaultExprContext.Var): Comp[Option[Token]] =
          knownVars.get(v).commit.map(_.flatMap {
            case VariableRealization.Reg(_) => None
            case VariableRealization.Tok(tk) => Some(tk)
          })

        private final case class FunctionArguments(
          tokenArguments: Seq[Token],
          arguments: Seq[RegisterId],
        )

        private def emitArguments(owner: ExpressionOwner, sig: FunctionSignature, args: Seq[ArExpr]): Comp[FunctionArguments] =
          emitFunctionSignature(owner, sig).flatMap { sigWithMapping =>
            emitArgumentsCommon(sigWithMapping, args)
          }
        
        private def emitMethodArguments(method: ArMethod, args: Seq[ArExpr]): Comp[FunctionArguments] =
          for
            sigWithMapping <- emitMethodSignature(method)
            funcArgs <- emitArgumentsCommon(sigWithMapping, args)
          yield funcArgs

        private def emitArgumentsCommon(sig: FunctionSignatureWithMapping, args: Seq[ArExpr]): Comp[FunctionArguments] =
          for
            tokenArgs <- TRef.make(Seq.empty[Token]).commit
            argRegs <- TRef.make(Seq.empty[RegisterId]).commit

            _ <- ZIO.foreachDiscard(sig.argConsumers.view.zip(args)) { (consumer, arg) =>
              consumer match {
                case ArgConsumer.Erased => ZIO.unit
                case ArgConsumer.Token =>
                  tokenExpr(arg).flatMap { typeArg =>
                    tokenArgs.update(_ :+ typeArg).commit
                  }

                case ArgConsumer.Arg =>
                  expr(arg, ExprOutput.AnyRegister).flatMap { r =>
                    argRegs.update(_ :+ r).commit
                  }
              }
            }

            tokenArgs <- tokenArgs.get.commit
            argRegs <- argRegs.get.commit
          yield FunctionArguments(
            tokenArgs,
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


