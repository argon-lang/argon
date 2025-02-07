package dev.argon.tube.encoder

import dev.argon.compiler as c

import dev.argon.tube.*

import zio.*
import zio.stream.*
import zio.stm.{ZSTM, TMap}
import dev.argon.ast
import dev.argon.compiler.SignatureEraser
import dev.argon.compiler.HasContext


private[tube] sealed abstract class TubeEncoderImpl extends c.UsingContext {
  import context.DefaultExprContext.Expr as ArExpr

  val tube: ArTube
  
  protected def emitEntryBuilder(entry: Comp[TubeFileEntry]): UIO[Unit]
  protected def assignTubeId(tubeName: c.TubeName, id: Int): UIO[Unit]
  protected def assignModuleId(moduleName: c.ModuleName, id: Int): UIO[Unit]

  protected def getTubeId(tubeName: c.TubeName): UIO[BigInt]
  protected def getModuleId(moduleName: c.ModuleName): UIO[BigInt]
  protected def getFunctionId(func: ArFunc): UIO[BigInt]
  protected def getRecordId(rec: ArRecord): UIO[BigInt]
  protected def getRecordFieldId(rec: RecordField): UIO[BigInt]
  
  def emitTube: Comp[Unit] =
    val orderedTubes = tube.referencedTubes.toSeq
    val orderedModules = tube.modules.values.toSeq

    val header =
      TubeHeader(
        formatName = "Ar18",
        formatVersionMajor = 0,
        formatVersionMinor = 0,
      )

    def emitMetadata: Comp[TubeMetadata] =
      for
        modules <- emitModules(orderedModules)
      yield TubeMetadata(
        name = encodeTubeName(tube.name),
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

      case c.ModuleExportC.Exported(exp) =>
        for
          inner <- emitModuleExport(exp)
        yield dev.argon.tube.ModuleExport.Exported(inner)
    }

  protected def emitModuleReference(moduleName: c.ModuleName, id: BigInt): Comp[TubeFileEntry] =
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
    

  protected def emitFunction(func: ArFunc, id: BigInt): Comp[TubeFileEntry] =
    importOrDefine(func, func.importSpecifier)(emitFunctionDef(id), emitFunctionRef(id))

  private def emitFunctionDef(id: BigInt)(func: ArFunc): Comp[TubeFileEntry] =
    for
      sig <- func.signature
      sig <- emitFunctionSignature(sig)

      importSpec <- func.importSpecifier
      importSpec <- encodeImportSpecifier(importSpec)

      impl <- ZIO.foreach(func.implementation) { impl =>
        impl.flatMap {
          case context.Implementations.FunctionImplementation.Expr(e) =>
            for
              e <- emitExpr(e)
            yield FunctionImplementation.Expr(e)
          case context.Implementations.FunctionImplementation.Extern(name) =>
            ZIO.succeed(FunctionImplementation.Extern(name))
        }
      }

    yield TubeFileEntry.FunctionDefinition(
      FunctionDefinition(
        functionId = id,
        `import` = importSpec,
        `inline` = func.isInline,
        erased = func.isErased,
        signature = sig,
        implementation = impl,
      )
    )

  private def emitFunctionRef(id: BigInt)(specifier: ImportSpecifier): Comp[TubeFileEntry] =
    ZIO.succeed(TubeFileEntry.FunctionReference(
      functionId = id,
      `import` = specifier,
    ))

  protected def emitRecord(rec: ArRecord, id: BigInt): Comp[TubeFileEntry] =
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
          t <- emitExpr(field.fieldType)
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

  protected def emitRecordFieldInfo(field: RecordField, id: BigInt): Comp[TubeFileEntry] =
    for
      recordId <- getRecordId(field.owningRecord)
    yield TubeFileEntry.RecordFieldReference(
      recordFieldId = id,
      recordId = id,
      name = encodeIdentifier(field.name),
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

  private final case class EmitExprState(
    knownVars: Map[context.DefaultExprContext.LocalVar, Int],
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

    private def nestedScope: Comp[ExprEmitter] =
      for
        knownVars <- knownVars.get.flatMap(Ref.make)
      yield ExprEmitter(
        knownVars = knownVars,
      )

    private def declareVar(v: context.DefaultExprContext.LocalVar): Comp[LocalVar] =
      for
        _ <- knownVars.update(kv => if kv.contains(v) then kv else kv + (v -> kv.size))
        t <- expr(v.varType)
      yield LocalVar(
        varType = t,
        name = v.name.map(encodeIdentifier),
        mutable = v.isMutable,
        erased = v.isErased,
        proof = v.isProof,
      )

    private def getVar(v: context.DefaultExprContext.Var): Comp[Var] =
      v match {
        case v: context.DefaultExprContext.LocalVar =>
          for
            kv <- knownVars.get
            index <- ZIO.succeed(kv.get(v).get)
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
            }

            varType <- expr(v.varType)

          yield Var.ParameterVar(
            owner = owner,
            parameterIndex = v.parameterIndex,
            name = v.name.map(encodeIdentifier),
            varType = varType,
            erased = v.isErased,
            proof = v.isProof,
          )
      }
      
    
    def expr(e: ArExpr): Comp[Expr] =
      e match {
        case ArExpr.Error() => ZIO.succeed(Expr.Error())
        case ArExpr.ErasedValue() => ZIO.succeed(Expr.ErasedValue())
        case ArExpr.Hole(hole) => hole
        case ArExpr.BindVariable(v, value) =>
          for
            value <- expr(value)
            v <- declareVar(v)
          yield Expr.BindVariable(v, value)

        case ArExpr.BoolLiteral(b) => ZIO.succeed(Expr.BoolLiteral(b))

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
            a <- expr(a)
            r <- expr(r)
          yield Expr.FunctionObjectCall(a, r)

        case ifElse: ArExpr.IfElse =>
          for
            condition <- expr(ifElse.condition)

            trueScope <- nestedScope
            whenTrueWitness <- ZIO.foreach(ifElse.whenTrueWitness)(trueScope.declareVar)
            trueBody <- trueScope.expr(ifElse.trueBody)

            falseScope <- nestedScope
            whenFalseWitness <- ZIO.foreach(ifElse.whenFalseWitness)(falseScope.declareVar)
            falseBody <- falseScope.expr(ifElse.falseBody)

          yield Expr.IfElse(
            condition = condition,
            trueBody = trueBody,
            falseBody = falseBody,
            whenTrueWitness = whenTrueWitness,
            whenFalseWitness = whenFalseWitness,
          )

        case ArExpr.IntLiteral(i) =>
          ZIO.succeed(Expr.IntLiteral(i))

        case lambda: ArExpr.Lambda =>
          for
            lambdaScope <- nestedScope
            v <- lambdaScope.declareVar(lambda.v)
            returnType <- lambdaScope.expr(lambda.returnType)
            body <- lambdaScope.expr(lambda.body)

          yield Expr.Lambda(
            v = v,
            returnType = returnType,
            body = body,
          )

        case rt: ArExpr.RecordType =>
          encodeRecordType(rt).map(Expr.RecordType.apply)

        case ArExpr.RecordLiteral(rt, fields) =>
          for
            rt <- encodeRecordType(rt)
            fields <- ZIO.foreach(fields) { field =>
              for
                fieldId <- getRecordFieldId(field.field)
                value <- expr(field.value)
              yield RecordFieldLiteral(
                fieldId = fieldId,
                value = value,
              )
            }
          yield Expr.RecordLiteral(rt, fields)

        case ArExpr.RecordFieldLoad(rt, field, recordValue) =>
          for
            rt <- encodeRecordType(rt)
            field <- getRecordFieldId(field)
            value <- expr(recordValue)
          yield Expr.RecordFieldLoad(rt, field, value)

        case ArExpr.Sequence(stmts, result) =>
          for
            scope <- nestedScope
            head <- scope.expr(stmts.headOption.getOrElse(result))
            tail <- ZIO.foreach(stmts.drop(1) :+ result)(scope.expr)
          yield Expr.Sequence(head, tail)

        case ArExpr.StringLiteral(s) =>
          ZIO.succeed(Expr.StringLiteral(s))

        case ArExpr.StoreVariable(v, value) =>
          for
            v <- getVar(v)
            value <- expr(value)
          yield Expr.StoreVariable(v, value)

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

        case ArExpr.Variable(v) =>
          for
            v <- getVar(v)
          yield Expr.Variable(v)
      }

    private def encodeRecordType(rt: ArExpr.RecordType): Comp[RecordType] =
      for
        recordId <- getRecordId(rt.record)
        args <- ZIO.foreach(rt.args)(expr)
      yield RecordType(recordId, args)
  }
}

private[tube] object TubeEncoderImpl {
  def encode(context: c.Context)(tube: c.ArTubeC & c.HasContext[context.type]): ZStream[context.Env, context.Error, TubeFileEntry] =
    val ctx: context.type = context
    val tube2: tube.type = tube

    class IdManager[A](mapping: TMap[A, Int])(using CanEqual[A, A]) {
      def assignId(value: A, id: Int): UIO[Unit] =
        mapping.put(value, id).commit
      
      def getIdWith(value: A)(emitEntryBuilder: context.Comp[TubeFileEntry] => UIO[Unit])(emit: (A, BigInt) => context.Comp[TubeFileEntry]): UIO[BigInt] =
        mapping.get(value).flatMap {
          case Some(id) => ZSTM.succeed(ZIO.succeed(id : BigInt))
          case none =>
            for
              id <- mapping.size
              _ <- mapping.put(value, id)
            yield emitEntryBuilder(emit(value, id)).as(id : BigInt)

        }.commit.flatten

      def getIdOrFail(value: A): UIO[BigInt] =
        mapping.get(value).map { id => id.get : BigInt }.commit

    }

    def createIdManager[A](using CanEqual[A, A]): UIO[IdManager[A]] =
      for
        mapping <- TMap.empty[A, Int].commit
      yield IdManager(mapping)


    ZStream.unwrap(
      for
        entryQueue <- Queue.unbounded[Option[TubeFileEntry]]()
        entryBuilders <- Ref.make(Seq.empty[context.Comp[TubeFileEntry]])

        tubeIds <- createIdManager[c.TubeName]
        moduleIds <- createIdManager[c.ModuleName]
        functionIds <- createIdManager[c.ArFuncC & c.HasContext[context.type]]
        recordIds <- createIdManager[c.ArRecordC & c.HasContext[context.type]]
        recordFieldIds <- createIdManager[c.RecordFieldC & c.HasContext[context.type]]

        encoder = new TubeEncoderImpl {

          override val context: ctx.type = ctx
          override val tube: ArTube = tube2

          override protected def emitEntryBuilder(entry: context.Comp[TubeFileEntry]): UIO[Unit] =
            entryBuilders.update(_ :+ entry)

          override protected def assignTubeId(tubeName: c.TubeName, id: Int): UIO[Unit] =
            tubeIds.assignId(tubeName, id)

          override protected def assignModuleId(moduleName: c.ModuleName, id: Int): UIO[Unit] =
            moduleIds.assignId(moduleName, id)

          override protected def getTubeId(tubeName: c.TubeName): UIO[BigInt] =
            tubeIds.getIdOrFail(tubeName)

          override protected def getModuleId(moduleName: c.ModuleName): UIO[BigInt] =
            moduleIds.getIdWith(moduleName)(emitEntryBuilder)(emitModuleReference)

          override protected def getFunctionId(func: ArFunc): UIO[BigInt] =
            functionIds.getIdWith(func)(emitEntryBuilder)(emitFunction)

          override protected def getRecordId(rec: ArRecord): UIO[BigInt] =
            recordIds.getIdWith(rec)(emitEntryBuilder)(emitRecord)

          override protected def getRecordFieldId(field: RecordField): UIO[BigInt] =
            recordFieldIds.getIdWith(field)(emitEntryBuilder)(emitRecordFieldInfo)
        }

        encodeJob <- (
          encoder.emitTube *>
          entryBuilders.getAndSet(Seq()).flatMap { builders =>
            if builders.isEmpty then
              ZIO.succeed(false)
            else
              ZIO.foreachDiscard(builders) { builder =>
                builder.flatMap { entry =>
                  entryQueue.offer(Some(entry)).unit
                }
              }.as(true)
          }.repeatWhile(identity)
        )
          .ensuring { entryQueue.offer(None) }
          .fork
      yield ZStream.fromQueue(entryQueue).collectWhileSome ++ ZStream.fromZIO(encodeJob.join).drain
    )
  end encode
}

