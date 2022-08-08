package dev.argon.plugins.js.emit

import dev.argon.util.*
import dev.argon.plugins.js.*
import dev.argon.compiler.*
import dev.argon.compiler.definitions.{FunctionImplementationC, HasDeclaration, MethodImplementationC}
import dev.argon.compiler.module.{ModuleName, ModulePath}
import dev.argon.compiler.signature.*
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.parser.IdentifierExpr
import zio.*
import zio.stm.*

private[emit] trait ExprEmitter extends EmitModuleCommon {

  import JSExpr.{*, given}

  val nextLocalId: TRef[Int]
  val newLocalNames: TSet[String]
  val localNameMap: TMap[UniqueIdentifier, String]
  val instanceNameMap: TMap[UniqueIdentifier, String]
  val parameterNameMap: TMap[(UniqueIdentifier, Int), String]

  import context.ExprContext.{
    WrapExpr,
    LocalVariable,
    ParameterVariable,
    ParameterVariableOwner,
    InstanceVariable,
    MemberVariable,
  }

  private def newLocalVar: USTM[String] =
    for
      num <- nextLocalId.getAndUpdate(_ + 1)
      name = s"arlocal$num"
      _ <- newLocalNames.put(name)
    yield name

  private def getOrInsertVariable[K](map: TMap[K, String])(key: K): UIO[String] =
    map.get(key).flatMap {
      case Some(name) => ZSTM.succeed(name)
      case None =>
        for
          name <- newLocalVar
          _ <- map.put(key, name)
        yield name
    }.commit


  private def getVariableName(variable: LocalVariable | InstanceVariable | ParameterVariable): UIO[String] =
    variable match
      case variable: context.ExprContext.LocalVariable =>
        getOrInsertVariable(localNameMap)(variable.id)

      case variable: context.ExprContext.InstanceVariable =>
        getOrInsertVariable(instanceNameMap)(variable.method.id)

      case variable: context.ExprContext.ParameterVariable =>
        getOrInsertVariable(parameterNameMap)((variable.owner.id, variable.parameterIndex))
    end match

  private def getSigParamVarNames(owner: context.ExprContext.ParameterVariableOwner)(sig: Signature[WrapExpr, ?]): UIO[Seq[String]] =
    def impl(prev: Seq[String])(sig: Signature[WrapExpr, ?]): UIO[Seq[String]] =
      sig match {
        case Signature.Parameter(_, paramErased, paramName, paramType, next) =>
          getVariableName(context.ExprContext.ParameterVariable(owner, prev.size, paramType, paramErased, paramName)).flatMap { newName =>
            impl(prev :+ newName)(next)
          }

        case Signature.Result(_) => ZIO.succeed(prev)
      }
    impl(Seq.empty)(sig)
  end getSigParamVarNames

  import context.ExprContext.{ArExpr, ExprConstructor, WrapExpr}

  def classExport(arClass: ArClass with HasDeclaration[true]): Comp[estree.ExportNamedDeclaration] =
    for
      sig <- arClass.signature
      erasedSig <- SignatureEraser(context).erasedNoResult(sig)
      exportName = getOverloadExportName(arClass.owner.ownedName, erasedSig)

      sigParamNames <- getSigParamVarNames(arClass)(sig)

      // Constructors

      // Methods
      methods <- arClass.methods
      methodStmts <- createMethods("methods", methods)

      // Static Methods
      staticMethods <- arClass.staticMethods
      staticMethodStmts <- createMethods("staticMethods", staticMethods)

      // Fields
      fields <- arClass.fields
      fieldStmts: Seq[estree.Statement] = fields.map { field =>
        val (Some(fieldName)) = field.name
        id("fields").index(literal(getEscapedName(fieldName))) ::= id("Symbol").call()
      }

      // Prototype
      baseClassExpr <- ZIO.foreach(sig.unsubstitutedResult._2) { baseClass =>
        emitStandaloneScope { emitState =>
          emitExpr(emitState)(baseClass)
        }
          .map(iefe)
      }
      vtableDiff <- vtableBuilder.diffFromClass(arClass)
      prototypeSetup <- emitVTable(vtableDiff, arClass)


    yield `export` const exportName := (
      id(runtimeImportName).prop("createClass").call(
        arrow(sigParamNames*) ==> Seq(

          const("constructors") := id("Object").prop("create").call(literal(null)),

          const("methods") := id("Object").prop("create").call(literal(null)),
          block(methodStmts*),

          const("staticMethods") := id("Object").prop("create").call(literal(null)),
          block(staticMethodStmts*),

          const("fields") := id("Object").prop("create").call(literal(null)),
          block(fieldStmts*),


          const("prototype") := id("Object").prop("create").call(
            baseClassExpr.getOrElse(literal(null))
          ),
          block(prototypeSetup *),

          `return`(obj(
            id("prototype") := id("prototype"),
            id("constructors") := id("constructors"),
            id("methods") := id("methods"),
            id("staticMethods") := id("staticMethods"),
            id("fields") := id("fields"),
          ))
        )
      )
    )


  def traitExport(arTrait: ArTrait with HasDeclaration[true]): Comp[estree.ExportNamedDeclaration] =
    for
      sig <- arTrait.signature
      erasedSig <- SignatureEraser(context).erasedNoResult(sig)
      exportName = getOverloadExportName(arTrait.owner.ownedName, erasedSig)

      sigParamNames <- getSigParamVarNames(arTrait)(sig)

      // Methods
      methods <- arTrait.methods
      methodStmts <- createMethods("methods", methods)

      // Static Methods
      staticMethods <- arTrait.staticMethods
      staticMethodStmts <- createMethods("staticMethods", staticMethods)


    yield `export` const exportName := (
      id(runtimeImportName).prop("createTrait").call(
        arrow(sigParamNames*) ==> Seq(
          const("methods") := id("Object").prop("create").call(literal(null)),
          block(methodStmts*),

          const("staticMethods") := id("Object").prop("create").call(literal(null)),
          block(staticMethodStmts*),

          `return`(obj(
            id("symbol") := id("Symbol").call(),
            id("methods") := id("methods"),
            id("staticMethods") := id("staticMethods"),
          ))
        )
      )
      )

  def functionExport(func: ArFunc with HasDeclaration[true]): Comp[estree.ExportNamedDeclaration] =
    for
      sig <- func.signature
      erasedSig <- SignatureEraser(context).erasedWithResult(sig)
      funcName = getOverloadExportName(func.owner.ownedName, erasedSig)

      impl <- func.implementation
      decl <- impl match {
        case impl: FunctionImplementationC.ExpressionBody =>
          for
            sig <- func.signature
            params <- createSigParams(func, 0, sig)
            body <- emitTopScope(emitWrapExprAsStmt(_)(impl.body))
          yield estree.FunctionDeclaration(
            id = Nullable(id(funcName)),
            params = params,
            body = block(body*),
            generator = false,
            async = true,
            expression = false,
            directive = None,
          )

        case impl: FunctionImplementationC.External =>
          val funcDecl = adapter.extractExternFunctionImplementation(impl.impl)
          ZIO.succeed(estree.FunctionDeclaration(
            id = Nullable(id(funcName)),
            params = funcDecl.params,
            body = funcDecl.body,
            generator = funcDecl.generator,
            async = funcDecl.async,
            expression = funcDecl.expression,
            directive = funcDecl.directive,
          ))
      }
    yield `export`(decl)


  private def createMethods(methodsVarName: String, methods: Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[true]]]): Comp[Seq[estree.Statement]] =
    ZIO.foreach(
      for
        (methodName, methodsOfName) <- methods.toSeq
        method <- methodsOfName
      yield (methodName, method)
    ) { case (methodName, method) =>
      for
        methodExpr <- createMethod(method)
        methodSig <- method.signatureUnsubstituted
        methodSigErased <- SignatureEraser(context).erasedWithResult(methodSig)

      yield id(methodsVarName).prop(getOverloadExportName(methodName, methodSigErased)) ::= obj(
        id("symbol") := id("Symbol").call(),
        id("implementation") := methodExpr,
      )
    }

  private def createMethod(method: ArMethod with HasDeclaration[true]): Comp[estree.Expression] =
    method.implementation.flatMap {
      case _: MethodImplementationC.Abstract =>
        ZIO.succeed(literal(null))
      case impl: MethodImplementationC.ExpressionBody =>
        for
          sig <- method.signatureUnsubstituted
          params <- createSigParams(method, 0, sig)
          body <- emitTopScope(emitWrapExprAsStmt(_)(impl.body))
        yield estree.FunctionExpression(
          id = Nullable(null),
          params = params,
          body = block(body*),
          generator = false,
          async = true,
        )
      case impl: MethodImplementationC.External =>
        val funcDecl = adapter.extractExternMethodImplementation(impl.impl)
        ZIO.succeed(estree.FunctionExpression(
          id = funcDecl.id,
          params = funcDecl.params,
          body = funcDecl.body,
          generator = funcDecl.generator,
          async = funcDecl.async,
        ))
    }

  private def createSigParams(owner: ParameterVariableOwner, index: Int, sig: Signature[WrapExpr, WrapExpr]): Comp[Seq[estree.Pattern]] =
    sig match {
      case Signature.Parameter(_, true, _, _, next) => createSigParams(owner, index + 1, next)
      case Signature.Parameter(_, false, paramName, paramType, next) =>
        val paramVar = ParameterVariable(owner, index, paramType, false, paramName)
        for
          varName <- getVariableName(paramVar)
          tail <- createSigParams(owner, index + 1, next)
        yield id(varName) +: tail

      case Signature.Result(_) => ZIO.succeed(Seq())
    }


  private def iefe(stmts: estree.Statement*): estree.Expression =
    (arrow() ==> stmts).call()

  final case class EmitState
  (
    tailPosition: Boolean,
    discardValue: Boolean,
    scopeVars: TRef[Seq[estree.VariableDeclaration]],
  ) {
    def subExpr: EmitState =
      copy(tailPosition = false, discardValue = false)
  }

  private def emitScope(createScope: TRef[Seq[estree.VariableDeclaration]] => EmitState)(f: EmitState => Comp[Seq[estree.Statement]]): Comp[Seq[estree.Statement]] =
    for
      scopeVars <- TRef.make[Seq[estree.VariableDeclaration]](Seq.empty).commit
      result <- f(createScope(scopeVars))
      varStmts <- scopeVars.get.commit
    yield varStmts ++ result

  private def emitTopScope(f: EmitState => Comp[Seq[estree.Statement]]): Comp[Seq[estree.Statement]] =
    emitScope(scopeVars => EmitState(tailPosition = true, discardValue = false, scopeVars = scopeVars))(f)

  private def emitStandaloneScope(f: EmitState => Comp[estree.Expression]): Comp[Seq[estree.Statement]] =
    emitScope(scopeVars => EmitState(tailPosition = false, discardValue = false, scopeVars = scopeVars)) { emitState =>
      f(emitState).map { expr => Seq(exprStmt(expr)) }
    }

  private def emitNestedScope(emitState: EmitState)(f: EmitState => Comp[Seq[estree.Statement]]): Comp[Seq[estree.Statement]] =
    emitScope(scopeVars => emitState.copy(scopeVars = scopeVars))(f)

  private def emitWrapExprAsStmt(emitState: EmitState)(expr: WrapExpr): Comp[Seq[estree.Statement]] =
    expr match
      case WrapExpr.OfExpr(expr) => emitExprAsStmt(emitState)(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitExprAsStmt(emitState: EmitState)(expr: ArExpr[ExprConstructor]): Comp[Seq[estree.Statement]] =
    expr.constructor match
      case ctor: (expr.constructor.type & ExprConstructor.BindVariable) =>
        val value: WrapExpr = expr.getArgs(ctor)
        for
          valueExpr <- emitWrapExpr(emitState.subExpr)(value)
          varName <- getVariableName(ctor.variable)
        yield Seq(estree.VariableDeclaration(
          kind = if ctor.variable.isMutable then "let" else "const",
          declarations = Seq(estree.VariableDeclarator(
            id = id(varName),
            init = Nullable(valueExpr),
          )),
        )) ++ (if emitState.discardValue then Seq() else Seq(`return`(array())))

      case ctor: (expr.constructor.type & ExprConstructor.Sequence.type) =>
        def emitSequence(emitState: EmitState, acc: Seq[estree.Statement], items: List[WrapExpr]): Comp[Seq[estree.Statement]] =
          items match {
            case head :: tail =>
              emitWrapExprAsStmt(emitState.copy(
                tailPosition = emitState.tailPosition && tail.isEmpty,
                discardValue = emitState.discardValue || tail.nonEmpty
              ))(head).flatMap { stmts =>
                emitSequence(emitState, acc ++ stmts, tail)
              }

            case Nil =>
              ZIO.succeed(acc)
          }

        emitNestedScope(emitState) { emitState =>
          emitSequence(emitState, Seq.empty, expr.getArgs(ctor).toList)
        }

      case _ if emitState.discardValue =>
        for
          jsExpr <- emitExpr(emitState)(expr)
        yield Seq(exprStmt(jsExpr))

      case _ =>
        for
          jsExpr <- emitExpr(emitState)(expr)
        yield Seq(`return`(jsExpr))
    end match

  private def emitWrapExpr(emitState: EmitState)(expr: WrapExpr): Comp[estree.Expression] =
    expr match
      case WrapExpr.OfExpr(expr) => emitExpr(emitState)(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitExpr(emitState: EmitState)(expr: ArExpr[ExprConstructor]): Comp[estree.Expression] =
    if emitState.tailPosition then
      expr.constructor match
        case ctor: (expr.constructor.type & ExprConstructor.FunctionCall) =>
          emitFunctionCall(emitState, expr, ctor).map { expr =>
            id(runtimeImportName).prop("trampoline").prop("delay").call(arrow.async() ==> expr)
          }

        case ctor: (expr.constructor.type & ExprConstructor.MethodCall) =>
          emitMethodCall(emitState, expr, ctor).map { expr =>
            id(runtimeImportName).prop("trampoline").prop("delay").call(arrow.async() ==> expr)
          }

        case _ =>
          emitExpr(emitState.subExpr)(expr).map { expr =>
            id(runtimeImportName).prop("trampoline").prop("result").call(expr)
          }

      end match
    else
      expr.constructor match
        case ctor: (expr.constructor.type & ExprConstructor.BindVariable) =>
          val value: WrapExpr = expr.getArgs(ctor)
          for
            valueExpr <- emitWrapExpr(emitState.subExpr)(value)
            varName <- getVariableName(ctor.variable)
            decl = estree.VariableDeclaration(
              kind = "let",
              declarations = Seq(estree.VariableDeclarator(
                id = id(varName),
                init = Nullable(null),
              )),
            )
            _ <- emitState.scopeVars.update(_ :+ decl).commit
            expr = estree.AssignmentExpression(
              operator = "=",
              left = estree.Identifier(name = varName),
              right = valueExpr,
            )
          yield
            if emitState.discardValue then
              expr
            else
              iefe(
                exprStmt(expr),
                `return`(array()),
              )

        case ctor: (expr.constructor.type & ExprConstructor.FunctionCall) =>
          emitFunctionCall(emitState, expr, ctor).map { expr =>
            id(runtimeImportName).prop("trampoline").prop("resolve").call(expr).await.prop("value")
          }

        case ctor: (expr.constructor.type & ExprConstructor.FunctionObjectCall.type) =>
          val (func, arg) = expr.getArgs(ctor)
          for
            funcExpr <- emitWrapExpr(emitState.subExpr)(func)
            argExpr <- emitWrapExpr(emitState.subExpr)(arg)
          yield funcExpr.call(argExpr)

        case ctor: (expr.constructor.type & ExprConstructor.MethodCall) =>
          emitMethodCall(emitState, expr, ctor).map { expr =>
            id(runtimeImportName).prop("trampoline").prop("resolve").call(expr).await.prop("value")
          }

        case ctor: (expr.constructor.type & ExprConstructor.LoadConstantBool) =>
          for
            _ <- ensureRawImportName(ModuleName(TubeName(NonEmptyList("Argon", "Core")), ModulePath(Seq("Bool"))), "createBool")
          yield id("createBool").call(literal(ctor.b))

        case ctor: (expr.constructor.type & ExprConstructor.LoadConstantInt) =>
          for
            _ <- ensureRawImportName(ModuleName(TubeName(NonEmptyList("Argon", "Core")), ModulePath(Seq("Int"))), "createInt")
          yield id("createInt").call(literal(ctor.i))

        case ctor: (expr.constructor.type & ExprConstructor.LoadConstantString) =>
          for
            _ <- ensureRawImportName(ModuleName(TubeName(NonEmptyList("Argon", "Core")), ModulePath(Seq("String"))), "createString")
          yield id("createString").call(literal(ctor.s))

        case ctor: (expr.constructor.type & ExprConstructor.LoadTuple.type) =>
          for
            argExprs <- ZIO.foreach(expr.getArgs(ctor))(emitWrapExpr(emitState.subExpr))
          yield array(argExprs*)

        case ctor: (expr.constructor.type & ExprConstructor.LoadTupleElement) =>
          for
            tupleExpr <- emitWrapExpr(emitState.subExpr)(expr.getArgs(ctor))
          yield tupleExpr.index(literal(ctor.index.toDouble))

        case ctor: (expr.constructor.type & ExprConstructor.LoadVariable) =>
          ctor.variable match {
            case variable: LocalVariable => getVariableName(variable).map(id)
            case variable: ParameterVariable => getVariableName(variable).map(id)
            case variable: InstanceVariable => getVariableName(variable).map(id)
            case variable: MemberVariable => ???
          }


        case ctor: (expr.constructor.type & ExprConstructor.ClassType) =>
          for
            sig <- ctor.arClass.signature
            erasedSig <- SignatureEraser(context).erasedNoResult(sig)
            classExpr <- emitOwnedByModule(ctor.arClass.owner, erasedSig)
            args <- emitArgExprs(emitState, expr.getArgs(ctor), sig, Seq())
          yield classExpr.call(args*)

        case ctor: (expr.constructor.type & ExprConstructor.TraitType) =>
          for
            sig <- ctor.arTrait.signature
            erasedSig <- SignatureEraser(context).erasedNoResult(sig)
            traitExpr <- emitOwnedByModule(ctor.arTrait.owner, erasedSig)
            args <- emitArgExprs(emitState, expr.getArgs(ctor), sig, Seq())
          yield traitExpr.call(args*)

        case _ =>
          ZIO.logError(s"Unimplemented expression: $expr") *> ZIO.succeed(???)
      end match

  private def emitFunctionCall(emitState: EmitState, expr: ArExpr[ExprConstructor], ctor: expr.constructor.type & ExprConstructor.FunctionCall): Comp[estree.Expression] =
    for
      sig <- ctor.function.signature
      erasedSig <- SignatureEraser(context).erasedWithResult(sig)
      owner = ctor.function.owner
      specifier = ImportSpecifier(owner.module.tube.tubeName, owner.module.moduleName.path, owner.ownedName, erasedSig)
      importName <- getImportName(specifier)

      args <- emitArgExprs(emitState, expr.getArgs(ctor), sig, Seq())
    yield id(importName).call(args*)

  private def emitMethodCall(emitState: EmitState, expr: ArExpr[ExprConstructor], ctor: expr.constructor.type & ExprConstructor.MethodCall): Comp[estree.Expression] =
    for
      sig <- ctor.method.signatureUnsubstituted

      (instanceExpr, methodOwnerType, argExprs) = expr.getArgs(ctor)

      instance <- emitWrapExpr(emitState.subExpr)(instanceExpr)
      methodValue <- emitMethodValue(emitState, ctor.method, methodOwnerType)
      args <- emitArgExprs(emitState, argExprs, sig, Seq())
    yield instance.index(methodValue.prop("symbol")).call(args *)

  private def emitArgExprs(emitState: EmitState, args: Seq[WrapExpr], sig: Signature[WrapExpr, ?], prev: Seq[estree.Expression]): Comp[Seq[estree.Expression]] =
    (args, sig) match {
      case (_ +: restArgs, Signature.Parameter(_, true, _, _, nextSig)) => emitArgExprs(emitState, restArgs, nextSig, prev)
      case (arg +: restArgs, Signature.Parameter(_, false, _, _, nextSig)) =>
        emitWrapExpr(emitState.subExpr)(arg).flatMap { argExpr =>
          emitArgExprs(emitState, restArgs, nextSig, prev :+ argExpr)
        }

      case _ => ZIO.succeed(prev)
    }

  private def emitMethodValue(emitState: EmitState, method: ArMethod, methodOwnerType: ExprConstructor.MethodCallOwnerType): Comp[estree.Expression] =
    for
      ownerType <- methodOwnerType match {
        case ExprConstructor.MethodCallOwnerType.OwnedByClass(classType) =>
          emitExpr(emitState.subExpr)(classType)

        case ExprConstructor.MethodCallOwnerType.OwnedByTrait(traitType) =>
          emitExpr(emitState.subExpr)(traitType)
      }

      methodPropName =
        method.owner match
          case _: OwnedByClass[?] | _: OwnedByTrait[?] => "methods"
          case _: OwnedByClassStatic[?] | _: OwnedByTraitStatic[?] => "staticMethods"
        end match

      sig <- method.signatureUnsubstituted
      erasedSig <- SignatureEraser(context).erasedWithResult(sig)
      methodKeyName = getOverloadExportName(
        method.owner match {
          case owner: OwnedByClass[?] => owner.ownedName
          case owner: OwnedByClassStatic[?] => owner.ownedName
          case owner: OwnedByTrait[?] => owner.ownedName
          case owner: OwnedByTraitStatic[?] => owner.ownedName
        },
        erasedSig
      )

    yield ownerType.prop(methodPropName).prop(methodKeyName)

  private def emitOwnedByModule(ownership: OwnedByModule, sig: ErasedSignature): Comp[estree.Expression] =
    getImportName(ImportSpecifier(ownership.module.tube.tubeName, ownership.module.moduleName.path, ownership.ownedName, sig))
      .map(id)

  private def emitVTable(vtable: vtableBuilder.VTable, ownerClass: ArClass): Comp[Seq[estree.Statement]] =
    ZIO.foreach(vtable.methodMap.toSeq) {
      case (_, vtableBuilder.VTableEntry(name, signature, slotInstanceType, entrySource, vtableBuilder.VTableEntryMethod(implMethod))) =>
        for
          slotSigErased <- SignatureEraser(context).erasedWithResult(signature)
          implSig <- implMethod.signatureUnsubstituted
          implSigErased <- SignatureEraser(context).erasedWithResult(implSig)

          slotMethodsExpr <-
            slotInstanceType match {
              case ExprConstructor.MethodCallOwnerType.OwnedByClass(classType) if classType.constructor.arClass == ownerClass =>
                ZIO.succeed(id("methods"))

              case ExprConstructor.MethodCallOwnerType.OwnedByClass(classType) =>
                emitStandaloneScope { emitState =>
                  for
                    slotInstanceExpr <- emitExpr(emitState)(classType)
                  yield slotInstanceExpr.prop("methods")
                }.map(iefe)

              case ExprConstructor.MethodCallOwnerType.OwnedByTrait(traitType) =>
                emitStandaloneScope { emitState =>
                  for
                    slotInstanceExpr <- emitExpr(emitState)(traitType)
                  yield slotInstanceExpr.prop("methods")
                }.map(iefe)
            }

          slotSymbol = slotMethodsExpr.prop(getOverloadExportName(name, slotSigErased)).prop("symbol")
          implMethod = id("methods").prop(getOverloadExportName(name, implSigErased)).prop("implementation")
        yield Seq(exprStmt(id("prototype").index(slotSymbol) ::= implMethod))

      case _ => ZIO.succeed(Seq())
    }.map { _.flatten }


}


