package dev.argon.tube.loader

import dev.argon.compiler as c
import dev.argon.tube as t
import dev.argon.ast
import dev.argon.tube.{Identifier, ImportSpecifier, Pattern}
import cats.data.NonEmptySeq
import zio.*
import dev.argon.compiler.{HasContext, UsingContext}
import dev.argon.util.UniqueIdentifier
import dev.argon.ast.IdentifierExpr

private[loader] trait LoaderUtils extends UsingContext {

  protected def elementLoader: ElementLoader & HasContext[context.type]
  
  protected def decodeTubeName(name: t.TubeName): c.TubeName =
    c.TubeName(NonEmptySeq(name.head, name.tail))

  protected def decodeModulePath(path: t.ModulePath): c.ModulePath =
    c.ModulePath(path.path)

  protected def decodeIdentifier(id: Identifier): ast.IdentifierExpr =
    id match {
      case Identifier.Named(s) => ast.IdentifierExpr.Named(s)
      case Identifier.BinOp(op) =>
        val op2 = op match {
          case t.BinaryOperator.Plus => ast.BinaryOperator.Plus
          case t.BinaryOperator.Minus => ast.BinaryOperator.Minus
          case t.BinaryOperator.Mul => ast.BinaryOperator.Mul
          case t.BinaryOperator.Div => ast.BinaryOperator.Div
          case t.BinaryOperator.Equal => ast.BinaryOperator.Equal
          case t.BinaryOperator.NotEqual => ast.BinaryOperator.NotEqual
          case t.BinaryOperator.LessThan => ast.BinaryOperator.LessThan
          case t.BinaryOperator.LessThanEq => ast.BinaryOperator.LessThanEq
          case t.BinaryOperator.GreaterThan => ast.BinaryOperator.GreaterThan
          case t.BinaryOperator.GreaterThanEq => ast.BinaryOperator.GreaterThanEq
          case t.BinaryOperator.BitOr => ast.BinaryOperator.BitOr
          case t.BinaryOperator.BitXor => ast.BinaryOperator.BitXOr
          case t.BinaryOperator.BitAnd => ast.BinaryOperator.BitAnd
          case t.BinaryOperator.ShiftLeft => ast.BinaryOperator.ShiftLeft
          case t.BinaryOperator.ShiftRight => ast.BinaryOperator.ShiftRight
          case t.BinaryOperator.Concat => ast.BinaryOperator.Concat
        }
        ast.IdentifierExpr.Op(op2)

      case Identifier.UnOp(op) =>
        val op2 = op match {
          case t.UnaryOperator.Plus => ast.UnaryOperator.Plus
          case t.UnaryOperator.Minus => ast.UnaryOperator.Minus
          case t.UnaryOperator.BitNot => ast.UnaryOperator.BitNot
          case t.UnaryOperator.LogicalNot => ast.UnaryOperator.LogicalNot
        }
        ast.IdentifierExpr.Op(op2)

      case Identifier.Extension(inner) =>
        ast.IdentifierExpr.Extension(decodeIdentifier(inner))

      case Identifier.Inverse(inner) =>
        ast.IdentifierExpr.Inverse(decodeIdentifier(inner))

      case Identifier.Update(inner) =>
        ast.IdentifierExpr.Update(decodeIdentifier(inner))
    }

  protected def decodeImportSpecifier(specifier: t.ImportSpecifier): Comp[c.ImportSpecifier] =
    specifier match {
      case specifier: ImportSpecifier.Global => 
        for
          module <- elementLoader.getModule(specifier.moduleId)
          sig <- decodeErasedSignature(specifier.sig)
        yield c.ImportSpecifier.Global(
          tube = module.tubeName,
          module = module.path,
          name = decodeIdentifier(specifier.name),
          signature = sig
        )

      case specifier: ImportSpecifier.Local =>
        for
          parent <- decodeImportSpecifier(specifier.parent)
          id <- elementLoader.getLocalImportId(specifier)
        yield c.ImportSpecifier.Local(parent, id)
    }

  def decodeErasedSignature(sig: t.ErasedSignature): Comp[c.ErasedSignature] =
    for
      params <- ZIO.foreach(sig.params)(decodeErasedSignatureType)
      result <- decodeErasedSignatureType(sig.result)
    yield c.ErasedSignature(params, result)

  private def decodeErasedSignatureType(ty: t.ErasedSignatureType): Comp[c.ErasedSignatureType] =
    ty match {
      case t.ErasedSignatureType.Builtin(builtin, args) =>
        val builtin2 = builtin match {
          case t.BuiltinType.Int() => dev.argon.expr.NullaryBuiltin.IntType
          case t.BuiltinType.Bool() => dev.argon.expr.NullaryBuiltin.BoolType
          case t.BuiltinType.String() => dev.argon.expr.NullaryBuiltin.StringType
          case t.BuiltinType.Never() => dev.argon.expr.NullaryBuiltin.NeverType
          case t.BuiltinType.Conjunction() => dev.argon.expr.BinaryBuiltin.ConjunctionType
          case t.BuiltinType.Disjunction() => dev.argon.expr.BinaryBuiltin.DisjunctionType
        }

        for
          args <- ZIO.foreach(args)(decodeErasedSignatureType)
        yield c.ErasedSignatureType.Builtin(builtin2, args)

      case t.ErasedSignatureType.Function(input, output) =>
        for
          input <- decodeErasedSignatureType(input)
          output <- decodeErasedSignatureType(output)
        yield c.ErasedSignatureType.Function(input, output)

      case t.ErasedSignatureType.Record(recordImport, args) =>
        for
          recordImport <- decodeImportSpecifier(recordImport)
          args <- ZIO.foreach(args)(decodeErasedSignatureType)
        yield c.ErasedSignatureType.Record(recordImport, args)

      case t.ErasedSignatureType.Tuple(elements) =>
        for
          elements <- ZIO.foreach(elements)(decodeErasedSignatureType)
        yield c.ErasedSignatureType.Tuple(elements)

      case t.ErasedSignatureType.Erased() =>
        ZIO.succeed(c.ErasedSignatureType.Erased)
    }


  protected def decodeFunctionSignature(sig: dev.argon.tube.FunctionSignature): Comp[FunctionSignature] =
    for
      params <- ZIO.foreach(sig.parameters)(decodeSignatureParam)
      returnType <- decodeExpr(sig.returnType)
      ensuresClauses <- ZIO.foreach(sig.ensuresClauses)(decodeExpr)
    yield FunctionSignature(
      parameters = params,
      returnType = returnType,
      ensuresClauses = ensuresClauses,
    )

  private def decodeSignatureParam(param: dev.argon.tube.SignatureParameter): Comp[SignatureParameter] =
    for
      bindings <- ZIO.foreach(param.bindings) { binding =>
        for
          paramType <- decodeExpr(binding.paramType)
        yield context.DefaultSignatureContext.ParameterBinding(
          name = binding.name.map(decodeIdentifier),
          paramType = paramType,
        )
      }
      paramType <- decodeExpr(param.paramType)
    yield SignatureParameter(
      listType = param.listType match {
        case t.FunctionParameterListType.NormalList => dev.argon.ast.FunctionParameterListType.NormalList
        case t.FunctionParameterListType.InferrableList => dev.argon.ast.FunctionParameterListType.InferrableList
        case t.FunctionParameterListType.QuoteList => dev.argon.ast.FunctionParameterListType.QuoteList
        case t.FunctionParameterListType.RequiresList => dev.argon.ast.FunctionParameterListType.RequiresList
      },
      isErased = param.erased,
      bindings = bindings,
      name = param.name.map(decodeIdentifier),
      paramType = paramType,
    )


  protected def decodeExpr(e: t.Expr): Comp[context.DefaultExprContext.Expr] =
    for
      knownVars <- Ref.make(Map.empty[BigInt, context.DefaultExprContext.LocalVar])
      res <- ExprDecoder(
        knownVars = knownVars,
      ).expr(e).onError { err =>
        ZIO.succeed(println(e))
      }
    yield res

  def decodeExpressionOwner(owner: t.ExpressionOwner): Comp[context.DefaultExprContext.ExpressionOwner] =
    owner match {
      case t.ExpressionOwner.Func(id) =>
        elementLoader.getFunction(id).map(context.DefaultExprContext.ExpressionOwner.Func.apply)
      case t.ExpressionOwner.Rec(id) =>
        elementLoader.getRecord(id).map(context.DefaultExprContext.ExpressionOwner.Rec.apply)
      case t.ExpressionOwner.Enum(id) =>
        elementLoader.getEnum(id).map(context.DefaultExprContext.ExpressionOwner.Enum.apply)
      case t.ExpressionOwner.Trait(id) =>
        elementLoader.getTrait(id).map(context.DefaultExprContext.ExpressionOwner.Trait.apply)
      case t.ExpressionOwner.Instance(id) =>
        elementLoader.getInstance(id).map(context.DefaultExprContext.ExpressionOwner.Instance.apply)
      case t.ExpressionOwner.EnumVariant(id) =>
        elementLoader.getEnumVariant(id).map(context.DefaultExprContext.ExpressionOwner.EnumVariant.apply)
      case t.ExpressionOwner.Method(id) =>
        elementLoader.getMethod(id).map(context.DefaultExprContext.ExpressionOwner.Method.apply)
    }

  private final class ExprDecoder(
    knownVars: Ref[Map[BigInt, context.DefaultExprContext.LocalVar]]
  ) {
    import dev.argon.tube.Expr
    import context.DefaultExprContext.{Expr as ArExpr, Pattern as ArPattern}

    private def declareVar(v: t.LocalVar): Comp[context.DefaultExprContext.LocalVar] =
      for
        id <- UniqueIdentifier.make
        varType <- expr(v.varType)
        localVar = context.DefaultExprContext.LocalVar(
          id = id,
          varType = varType,
          name = v.name.map(decodeIdentifier),
          isMutable = v.mutable,
          isErased = v.erased,
          isWitness = v.witness,
        )
        _ <- knownVars.update(kv => kv + (v.id -> localVar))
      yield localVar

    private def getVar(v: t.Var): Comp[context.DefaultExprContext.Var] =
      v match {
        case t.Var.LocalVar(index) =>
          for
            kv <- knownVars.get
            localVar <- ZIO.succeed(kv(index.toInt))
          yield localVar

        case instanceVar: t.Var.InstanceParameterVar =>
          for
            contextOwner <- decodeExpressionOwner(instanceVar.owner)
            paramType <- decodeExpr(instanceVar.varType)
          yield context.DefaultExprContext.InstanceParameterVar(
            owner = contextOwner,
            name = instanceVar.name.map(decodeIdentifier),
            varType = paramType,
          )

        case paramVar: t.Var.ParameterVar =>
          for
            contextOwner <- decodeExpressionOwner(paramVar.owner)
            paramType <- decodeExpr(paramVar.varType)
          yield context.DefaultExprContext.ParameterVar(
            owner = contextOwner,
            parameterIndex = paramVar.parameterIndex.toInt,
            name = paramVar.name.map(decodeIdentifier),
            varType = paramType,
            isErased = paramVar.erased,
            isWitness = paramVar.witness,
          )
      }

    def expr(e: Expr): Comp[ArExpr] =
      e match {
        case Expr.Error() => ZIO.succeed(ArExpr.Error())
        case Expr.ErasedValue() => ZIO.succeed(ArExpr.ErasedValue())

        case Expr.And(a, b) =>
          for
            a <- expr(a)
            b <- expr(b)
          yield ArExpr.And(a, b)

        case Expr.AnyType() => ZIO.succeed(ArExpr.AnyType())
        case Expr.BindVariable(v, value) =>
          for
            valueExpr <- expr(value)
            localVar <- declareVar(v)
          yield ArExpr.BindVariable(localVar, valueExpr)

        case Expr.BoolLiteral(b) => ZIO.succeed(ArExpr.BoolLiteral(b))

        case Expr.Box(t, value) =>
          for
            t <- expr(t)
            value <- expr(value)
          yield ArExpr.Box(t, value)

        case Expr.Boxed(t) =>
          for
            t <- expr(t)
          yield ArExpr.Boxed(t)

        case et: Expr.EnumType =>
          decodeEnumType(et.enumType)

        case Expr.EnumVariantLiteral(enumType, variantId, args, fields) =>
          for
            enumType <- decodeEnumType(enumType)
            v <- elementLoader.getEnumVariant(variantId)
            args <- ZIO.foreach(args)(expr)
            fields <- ZIO.foreach(fields)(decodeRecordLiteralField)
          yield ArExpr.EnumVariantLiteral(enumType, v, args, fields)
            
            

        case Expr.NullaryBuiltin(builtin) =>
          val builtin2 = builtin match {
            case t.NullaryBuiltin.IntType => dev.argon.expr.NullaryBuiltin.IntType
            case t.NullaryBuiltin.BoolType => dev.argon.expr.NullaryBuiltin.BoolType
            case t.NullaryBuiltin.StringType => dev.argon.expr.NullaryBuiltin.StringType
            case t.NullaryBuiltin.NeverType => dev.argon.expr.NullaryBuiltin.NeverType
          }
          ZIO.succeed(ArExpr.Builtin(context.DefaultExprContext.Builtin.Nullary(builtin2)))

        case Expr.UnaryBuiltin(builtin, a) =>
          val builtin2 = builtin match {
            case t.UnaryBuiltin.IntNegate => dev.argon.expr.UnaryBuiltin.IntNegate
            case t.UnaryBuiltin.IntBitNot => dev.argon.expr.UnaryBuiltin.IntBitNot
            case t.UnaryBuiltin.BoolNot => dev.argon.expr.UnaryBuiltin.BoolNot
          }
          for
            arg <- expr(a)
          yield ArExpr.Builtin(context.DefaultExprContext.Builtin.Unary(builtin2, arg))

        case Expr.BinaryBuiltin(builtin, a, b) =>
          val builtin2 = builtin match {
            case t.BinaryBuiltin.ConjunctionType => dev.argon.expr.BinaryBuiltin.ConjunctionType
            case t.BinaryBuiltin.DisjunctionType => dev.argon.expr.BinaryBuiltin.DisjunctionType
            case t.BinaryBuiltin.IntAdd => dev.argon.expr.BinaryBuiltin.IntAdd
            case t.BinaryBuiltin.IntSub => dev.argon.expr.BinaryBuiltin.IntSub
            case t.BinaryBuiltin.IntMul => dev.argon.expr.BinaryBuiltin.IntMul
            case t.BinaryBuiltin.IntBitAnd => dev.argon.expr.BinaryBuiltin.IntBitAnd
            case t.BinaryBuiltin.IntBitOr => dev.argon.expr.BinaryBuiltin.IntBitOr
            case t.BinaryBuiltin.IntBitXor => dev.argon.expr.BinaryBuiltin.IntBitXOr
            case t.BinaryBuiltin.IntBitShiftLeft => dev.argon.expr.BinaryBuiltin.IntBitShiftLeft
            case t.BinaryBuiltin.IntBitShiftRight => dev.argon.expr.BinaryBuiltin.IntBitShiftRight
            case t.BinaryBuiltin.IntEq => dev.argon.expr.BinaryBuiltin.IntEQ
            case t.BinaryBuiltin.IntNe => dev.argon.expr.BinaryBuiltin.IntNE
            case t.BinaryBuiltin.IntLt => dev.argon.expr.BinaryBuiltin.IntLT
            case t.BinaryBuiltin.IntLe => dev.argon.expr.BinaryBuiltin.IntLE
            case t.BinaryBuiltin.IntGt => dev.argon.expr.BinaryBuiltin.IntGT
            case t.BinaryBuiltin.IntGe => dev.argon.expr.BinaryBuiltin.IntGE
            case t.BinaryBuiltin.StringConcat => dev.argon.expr.BinaryBuiltin.StringConcat
            case t.BinaryBuiltin.StringEq => dev.argon.expr.BinaryBuiltin.StringEQ
            case t.BinaryBuiltin.StringNe => dev.argon.expr.BinaryBuiltin.StringNE
            case t.BinaryBuiltin.BoolEq => dev.argon.expr.BinaryBuiltin.BoolEQ
            case t.BinaryBuiltin.BoolNe => dev.argon.expr.BinaryBuiltin.BoolNE
          }
          for
            argA <- expr(a)
            argB <- expr(b)
          yield ArExpr.Builtin(context.DefaultExprContext.Builtin.Binary(builtin2, argA, argB))

        case Expr.BuiltinEqualTo(t, a, b) =>
          for
            tExpr <- expr(t)
            aExpr <- expr(a)
            bExpr <- expr(b)
          yield ArExpr.Builtin(context.DefaultExprContext.Builtin.EqualTo(tExpr, aExpr, bExpr))

        case Expr.BuiltinEqualToRefl(t, a) =>
          for
            tExpr <- expr(t)
            aExpr <- expr(a)
          yield ArExpr.Builtin(context.DefaultExprContext.Builtin.EqualToRefl(tExpr, aExpr))

        case Expr.Finally(action, ensuring) =>
          for
            action <- expr(action)
            ensuring <- expr(ensuring)
          yield ArExpr.Finally(action, ensuring)
          
        case Expr.FunctionCall(f, args) =>
          for
            function <- elementLoader.getFunction(f)
            argsExpr <- ZIO.foreach(args)(expr)
          yield ArExpr.FunctionCall(function, argsExpr)

        case Expr.FunctionObjectCall(f, a) =>
          for
            funcExpr <- expr(f)
            argExpr <- expr(a)
          yield ArExpr.FunctionObjectCall(funcExpr, argExpr)

        case Expr.FunctionType(a, r) =>
          for
            argExpr <- declareVar(a)
            returnExpr <- expr(r)
          yield ArExpr.FunctionType(argExpr, returnExpr)

        case Expr.IntLiteral(i) => ZIO.succeed(ArExpr.IntLiteral(i))

        case Expr.StringLiteral(s) => ZIO.succeed(ArExpr.StringLiteral(s))

        case Expr.IfElse(condition, trueBody, falseBody, whenTrueWitness, whenFalseWitness) =>
          for
            conditionExpr <- expr(condition)

            trueWitnessVars <- ZIO.foreach(whenTrueWitness)(declareVar)
            trueBodyExpr <- expr(trueBody)

            falseWitnessVars <- ZIO.foreach(whenFalseWitness)(declareVar)
            falseBodyExpr <- expr(falseBody)
          yield ArExpr.IfElse(
            condition = conditionExpr,
            trueBody = trueBodyExpr,
            falseBody = falseBodyExpr,
            whenTrueWitness = trueWitnessVars,
            whenFalseWitness = falseWitnessVars,
          )

        case Expr.InstanceMethodCall(methodId, instanceType, instance, args) =>
          for
            method <- elementLoader.getMethod(methodId)
            instanceType <- decodeInstanceMethodType(instanceType)
            instance <- expr(instance)
            argsExpr <- ZIO.foreach(args)(expr)
          yield ArExpr.InstanceMethodCall(method, instanceType, instance, argsExpr)

        case Expr.InstanceSingletonType(ist) =>
          decodeInstanceSingletonType(ist)

        case Expr.Is(value, p) =>
          for
            value <- expr(value)
            p <- pattern(p)
          yield ArExpr.Is(value, p)

        case Expr.Lambda(v, returnType, body) =>
          for
            localVar <- declareVar(v)
            returnTypeExpr <- expr(returnType)
            bodyExpr <- expr(body)
          yield ArExpr.Lambda(
            v = localVar,
            returnType = returnTypeExpr,
            body = bodyExpr,
          )

        case Expr.Match(value, cases) =>
          for
            value <- expr(value)
            cases <- ZIO.foreach(cases) { matchCase =>
              for
                p <- pattern(matchCase.pattern)
                body <- expr(matchCase.body)
              yield context.DefaultExprContext.MatchCase(p, body)
            }
          yield ArExpr.Match(value, cases)

        case Expr.NewInstance(i, args) =>
          for
            inst <- elementLoader.getInstance(i)
            argsExpr <- ZIO.foreach(args)(expr)
          yield ArExpr.NewInstance(inst, argsExpr)

        case Expr.Or(a, b) =>
          for
            a <- expr(a)
            b <- expr(b)
          yield ArExpr.Or(a, b)

        case Expr.RecordType(recordType) =>
          decodeRecordType(recordType)

        case Expr.RecordFieldLoad(rt, fieldId, recordValue) =>
          for
            rt <- decodeRecordType(rt)
            field <- elementLoader.getRecordField(fieldId)
            value <- expr(recordValue)
          yield ArExpr.RecordFieldLoad(rt, field, value)

        case Expr.RecordFieldStore(rt, fieldId, recordValue, fieldValue) =>
          for
            rt <- decodeRecordType(rt)
            field <- elementLoader.getRecordField(fieldId)
            recordValue <- expr(recordValue)
            fieldValue <- expr(fieldValue)
          yield ArExpr.RecordFieldStore(rt, field, recordValue, fieldValue)

        case Expr.RecordLiteral(recordType, fields) =>
          for
            decodedRecordType <- decodeRecordType(recordType)
            decodedFields <- ZIO.foreach(fields)(decodeRecordLiteralField)
          yield ArExpr.RecordLiteral(decodedRecordType, decodedFields)

        case Expr.Sequence(head, tail) =>
          for
            headExpr <- expr(head)
            tailExpr <- ZIO.foreach(tail)(expr)
          yield ArExpr.Sequence(
            stmts = (if tailExpr.isEmpty then Seq() else Seq(headExpr)) ++ tailExpr.dropRight(1),
            result = tailExpr.lastOption.getOrElse(headExpr),
          )

        case Expr.TraitType(traitType) =>
          decodeTraitType(traitType)

        case Expr.Tuple(items) =>
          for
            decodedItems <- ZIO.foreach(items)(expr)
          yield ArExpr.Tuple(decodedItems)

        case Expr.TupleElement(index, tuple) =>
          for
            tupleExpr <- expr(tuple)
          yield ArExpr.TupleElement(index = index.toInt, tuple = tupleExpr)

        case Expr.TypeN(n) =>
          for
            decodedN <- expr(n)
          yield ArExpr.TypeN(decodedN)

        case Expr.TypeBigN(n) =>
          ZIO.succeed(ArExpr.TypeBigN(n))

        case Expr.Unbox(t, value) =>
          for
            t <- expr(t)
            value <- expr(value)
          yield ArExpr.Unbox(t, value)

        case Expr.Variable(variable) =>
          for
            decodedVar <- getVar(variable)
          yield ArExpr.Variable(decodedVar)

        case Expr.VariableStore(variable, value) =>
          for
            decodedVar <- getVar(variable)
            decodedValue <- expr(value)
          yield ArExpr.VariableStore(decodedVar, decodedValue)

      }

    private def pattern(p: t.Pattern): Comp[ArPattern] =
      p match {
        case Pattern.Discard(t) =>
          for
            t <- expr(t)
          yield ArPattern.Discard(t)
        case Pattern.Tuple(items) =>
          for
            items <- ZIO.foreach(items)(pattern)
          yield ArPattern.Tuple(items)

        case Pattern.Binding(v, p) =>
          for
            v <- declareVar(v)
            p <- pattern(p)
          yield ArPattern.Binding(v, p)

        case Pattern.EnumVariant(enumType, variantId, args, fields) =>
          for
            enumType <- decodeEnumType(enumType)
            v <- elementLoader.getEnumVariant(variantId)
            args <- ZIO.foreach(args)(pattern)
            fields <- ZIO.foreach(fields)(decodeRecordFieldPattern)
          yield ArPattern.EnumVariant(enumType, v, args, fields)

        case Pattern.String(s) =>
          ZIO.succeed(ArPattern.String(s))

        case Pattern.Int(i) =>
          ZIO.succeed(ArPattern.Int(i))

        case Pattern.Bool(b) =>
          ZIO.succeed(ArPattern.Bool(b))
      }

    private def decodeRecordType(recordType: t.RecordType): Comp[ArExpr.RecordType] =
      for
        record <- elementLoader.getRecord(recordType.id)
        decodedArgs <- ZIO.foreach(recordType.args)(expr)
      yield ArExpr.RecordType(
        record = record,
        args = decodedArgs,
      )

    private def decodeEnumType(enumType: t.EnumType): Comp[ArExpr.EnumType] =
      for
        e <- elementLoader.getEnum(enumType.id)
        args <- ZIO.foreach(enumType.args)(expr)
      yield ArExpr.EnumType(e, args)

    private def decodeTraitType(traitType: t.TraitType): Comp[ArExpr.TraitType] =
      for
        trt <- elementLoader.getTrait(traitType.id)
        decodedArgs <- ZIO.foreach(traitType.args)(expr)
      yield ArExpr.TraitType(
        t = trt,
        args = decodedArgs,
      )

    private def decodeInstanceSingletonType(instanceType: t.InstanceSingletonType): Comp[ArExpr.InstanceSingletonType] =
      for
        inst <- elementLoader.getInstance(instanceType.id)
        decodedArgs <- ZIO.foreach(instanceType.args)(expr)
      yield ArExpr.InstanceSingletonType(
        i = inst,
        args = decodedArgs,
      )


    private def decodeInstanceMethodType(instanceType: t.MethodInstanceType): Comp[context.DefaultExprContext.MethodInstanceType] =
      instanceType match {
        case t.MethodInstanceType.TraitType(tt) => decodeTraitType(tt)
        case t.MethodInstanceType.InstanceSingletonType(ist) => decodeInstanceSingletonType(ist)
      }

    private def decodeRecordLiteralField(fieldLit: t.RecordFieldLiteral): Comp[context.DefaultExprContext.RecordFieldLiteral] =
      for
        field <- elementLoader.getRecordField(fieldLit.fieldId)
        fieldValue <- expr(fieldLit.value)
      yield context.DefaultExprContext.RecordFieldLiteral(field, fieldValue)

    private def decodeRecordFieldPattern(fieldPattern: t.RecordFieldPattern): Comp[context.DefaultExprContext.RecordFieldPattern] =
      for
        field <- elementLoader.getRecordField(fieldPattern.fieldId)
        fieldPattern <- pattern(fieldPattern.pattern)
      yield context.DefaultExprContext.RecordFieldPattern(field, fieldPattern)
  }

    
}
