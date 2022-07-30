package dev.argon.plugins.js.emit

import dev.argon.util.*
import dev.argon.plugins.js.*
import dev.argon.compiler.*
import dev.argon.compiler.definitions.{HasDeclaration, MethodImplementationC}
import dev.argon.compiler.signature.*
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
    ParameterVariable,
    ParameterVariableOwner,
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


  private def getVariableName(variable: context.ExprContext.LocalVariable | context.ExprContext.InstanceVariable | context.ExprContext.ParameterVariable): UIO[String] =
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
        case Signature.Parameter(_, paramErased, paramType, next) =>
          getVariableName(context.ExprContext.ParameterVariable(owner, prev.size, paramType, paramErased)).flatMap { newName =>
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


    yield `export` const exportName := (
      id(runtimeImportName).prop("createClass").call(
        arrow(sigParamNames*) ==> (
          const("methods") := id("Object").prop("create").call(literal(null)),
          block(methodStmts*),

          const("staticMethods") := id("Object").prop("create").call(literal(null)),
          block(staticMethodStmts*),

          const("fields") := id("Object").prop("create").call(literal(null)),
          block(fieldStmts*),

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
/*
  private def classPrototype(arClass: ArClass): Comp[Seq[estree.Statement]] =
    for
      vtable <- vtableBuilder.diffFromClass(arClass)
      methods <- arClass.methods


      vtableExprs <- ZIO.foreach(vtable.methodMap.toSeq) { case (slot, entry) =>
        entry.impl match
          case vtableBuilder.VTableEntryMethod(method) if methods.values.iterator.flatten.contains(method) =>
            for
            yield Seq(
              estree.VariableDeclaration(
                kind = "const",
                declarations = Seq(estree.VariableDeclarator(
                  id = estree.Identifier(slotVarName),
                  init = Nullable(estree.CallExpression(
                    callee = estree.Identifier(name = "Symbol"),
                    arguments = Seq()
                  )),
                )),
              ),
              estree.ExpressionStatement(
                expression = estree.AssignmentExpression(
                  operator = "=",
                  left = estree.MemberExpression(
                    `object` = estree.Identifier(name = "proto"),
                    property = estree.Identifier(name = slotVarName),
                    computed = true,
                    optional = false,
                  ),
                  right = estree.Identifier
                )
              )
            )


          case vtableBuilder.VTableEntryAmbiguous(_) => ZIO.succeed(Seq())
          case vtableBuilder.VTableEntryAbstract => ZIO.succeed(Seq())
      }

      sig <- arClass.signature
      baseClassExpr <- ZIO.foreach(sig.unsubstitutedResult._2)(emitWrapExpr)

    yield Seq(
      estree.VariableDeclaration(
        kind = "const",
        declarations = Seq(estree.VariableDeclarator(
          id = estree.Identifier("proto"),
          init = Nullable(estree.CallExpression(
              callee = estree.MemberExpression(
                `object` = estree.Identifier(name = "Object"),
                property = estree.Identifier(name = "create")
              ),
              arguments = Seq(baseClassExpr.getOrElse { estree.Literal(value = Nullable(null)) }),
          )),
        )),
      ),

      vtableExprs.flatten*,
    )*/


  def traitExport(arTrait: ArTrait): Comp[estree.ExportNamedDeclaration] = ???

  def functionExport(func: ArFunc): Comp[estree.ExportNamedDeclaration] = ???


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

      yield id(methodsVarName).index(literal(getOverloadExportName(methodName, methodSigErased))) ::= obj(
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
          body <- emitWrapExprAsStmt(impl.body)
        yield estree.FunctionExpression(
          id = Nullable(null),
          params = params,
          body = block(body*),
          generator = false,
          async = false,
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
      case Signature.Parameter(_, true, _, next) => createSigParams(owner, index + 1, next)
      case Signature.Parameter(_, false, paramType, next) =>
        val paramVar = ParameterVariable(owner, index, paramType, false)
        for
          varName <- getVariableName(paramVar)
          tail <- createSigParams(owner, index + 1, next)
        yield id(varName) +: tail

      case Signature.Result(_) => ZIO.succeed(Seq())
    }

  private def emitWrapExprAsStmt(expr: WrapExpr): Comp[Seq[estree.Statement]] =
    expr match
      case WrapExpr.OfExpr(expr) => emitExprAsStmt(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitExprAsStmt(expr: ArExpr[ExprConstructor]): Comp[Seq[estree.Statement]] =
    expr.constructor match
      case _ =>
        for
          jsExpr <- emitExpr(expr)
        yield Seq(estree.ExpressionStatement(
          expression = jsExpr
        ))
    end match

  private def emitWrapExpr(expr: WrapExpr): Comp[estree.Expression] =
    expr match
      case WrapExpr.OfExpr(expr) => emitExpr(expr)
      case WrapExpr.OfHole(hole) => hole
    end match

  private def emitExpr(expr: ArExpr[ExprConstructor]): Comp[estree.Expression] =
    expr.constructor match
      case ctor: (expr.constructor.type & ExprConstructor.BindVariable) =>
        val value: WrapExpr = expr.getArgs(ctor)
        for
          valueExpr <- emitWrapExpr(value)
          varName <- getVariableName(ctor.variable)
        yield estree.AssignmentExpression(
          operator = "=",
          left = estree.Identifier(name = varName),
          right = valueExpr,
        )

      case ctor: (expr.constructor.type & ExprConstructor.FunctionCall) =>
        ???

      case ctor: (expr.constructor.type & ExprConstructor.LoadConstantBool) =>
        ZIO.succeed(estree.Literal(
          value = Nullable(ctor.b),
        ))

      case ctor: (expr.constructor.type & ExprConstructor.LoadConstantInt) =>
        ZIO.succeed(estree.Literal(
          value = Nullable(ctor.i),
          bigint = Some(ctor.i.toString)
        ))

      case ctor: (expr.constructor.type & ExprConstructor.LoadConstantString) =>
        ZIO.succeed(estree.Literal(
          value = Nullable(ctor.s),
        ))

      case _ => ???
    end match

}


