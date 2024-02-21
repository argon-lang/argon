package dev.argon.plugins.lua

import dev.argon.compiler.definitions.{FunctionImplementationC, HasImplementation}
import dev.argon.compiler.module.ModuleElementC
import dev.argon.compiler.signature.{ErasedSignature, ErasedSignatureNoResult, ErasedSignatureType, ErasedSignatureWithResult, ImportSpecifier, Signature, SignatureEraser}
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.{Context, UsingContext}
import dev.argon.parser.IdentifierExpr
import dev.argon.plugin.{PluginAdapter, PluginContext}
import dev.argon.plugins.lua.ExprEmitter.OutputMode
import zio.*
import zio.stm.*

import scala.reflect.TypeTest

trait ModuleEmitter[R <: LuaEnv, E >: LuaError] extends ModuleEmitterBase[R, E] {
  import context.ExprContext.*

  def emitModule: ZIO[R, E, AST.Exp] =
    for
      elements <- currentModule.allExports(Set.empty)
      eraser = SignatureEraser(context)

      classes <- emitElements(elements.collect { case ModuleElementC.ClassElement(cls) => cls })(cls =>
        for
          sig <- cls.signature
          erasedSig <- eraser.erasedNoResult(sig)
        yield (cls.owner.ownedName, erasedSig)
      )(emitClass)
      traits <- emitElements(elements.collect { case ModuleElementC.TraitElement(trt) => trt })(trt =>
        for
          sig <- trt.signature
          erasedSig <- eraser.erasedNoResult(sig)
        yield (trt.owner.ownedName, erasedSig)
      )(emitTrait)
      functions <- emitElements(elements.collect { case ModuleElementC.FunctionElement(f) => f })(f =>
        for
          sig <- f.signature
          erasedSig <- eraser.erasedWithResult(sig)
        yield (f.owner.ownedName, erasedSig)
      )(emitFunction)

    yield AST.TableConstructor(Seq(
      AST.Field.NamedFixed(
        "classes",
        classes,
      ),
      AST.Field.NamedFixed(
        "traits",
        traits,
      ),
      AST.Field.NamedFixed(
        "functions",
        functions,
      ),
    ))






  private def emitElements[A](elements: Seq[A])(getInfo: A => ZIO[R, E, (Option[IdentifierExpr], ErasedSignature)])(f: A => ZIO[R, E, AST.Exp]): ZIO[R, E, AST.Exp] =
    for
      infoElements <- ZIO.foreach(elements) { elem => getInfo(elem).map { _ -> elem } }
      groupedElements = infoElements.groupBy { case ((name, _), _) => name }.toSeq
      fields <- ZIO.foreach(groupedElements) { (name, group) =>
        for
          groupFields <- ZIO.foreach(group) { case ((_, sig), element) =>
            for
              exp <- f(element)
            yield AST.Field.NamedWithExp(
              getErasedSigKeyExprMemo(sig),
              exp,
            )
          }
        yield AST.Field.NamedWithExp(
          getIdentifierKeyExprMemo(name),
          AST.TableConstructor(groupFields)
        )
      }
    yield AST.TableConstructor(fields)

  private trait ExprEmitterCommon extends ExprEmitter[R, E] {
    override val context: ModuleEmitter.this.context.type = ModuleEmitter.this.context
    override val plugin: ModuleEmitter.this.plugin.type = ModuleEmitter.this.plugin
    override val pluginAdapter: PluginAdapter[R, E, context.plugin.type, plugin.type] = ModuleEmitter.this.pluginAdapter
    override val currentTube: ArTube & HasImplementation[true] = ModuleEmitter.this.currentTube
    override val currentModule: ArModule & HasImplementation[true] = ModuleEmitter.this.currentModule
  }

  private def emitClass(cls: ArClass & HasImplementation[true]): ZIO[R, E, AST.Exp] =
    for
      sig <- cls.signature
      (paramMapping, paramNames) = getNonErasedParams(cls, sig)

      nli <- TRef.make(0).commit
      vm <- TMap.empty[LocalVariable, String].commit
      emitter = new ExprEmitterCommon {
        override val nextLocalIndex: TRef[Int] = nli
        override val varMapping: TMap[LocalVariable, String] = vm

        override val instanceVarMapping: Map[context.ExprContext.InstanceVariable, AST.Var] = Map.empty
        override val parameterVarMapping: Map[context.ExprContext.ParameterVariable, AST.Var] = paramMapping
        override val memberVarMapping: Map[context.ExprContext.MemberVariable, AST.Var] = Map.empty
      }
      
      methods <- emitMethods(cls.methods, emitter)
      staticMethods <- emitMethods(cls.staticMethods, emitter)
      constructors <- emitConstructors(cls.constructors, emitter)
      
      fields <- cls.fields.flatMap { fields =>
        AST.TableConstructor(
          fields.map { field =>
            AST.Field.NamedWithExp(getIdentifierKeyExprMemo(field), AST.TableConstructor(Seq.empty))
          }
        )
      }

      
      
      metatable <-
        for
          sig <- cls.signature
          baseClass <- sig.unsubstitutedResult.baseClass
          baseClassBlock <- emitter.emit(OutputMode.ReturnValue)(baseClass)
          vtable <- cls.vtableDiff
        yield AST.FunctionDefinitionExp(Seq(), hasRest = false, AST.Block(Seq(
          AST.Return(Seq(AST.TableConstructor(Seq(
            AST.Field.NamedFixed(
              "__index",
              AST.SimpleFunctionCall(
                AST.NameExp("setmetatable"),
                Seq(
                  AST.TableConstructor(Seq(
                    
                  )),
                  AST.MemberAccessIndex(
                    AST.SimpleFunctionCall(
                      AST.ParenExp(AST.FunctionDefinitionExp(Seq.empty, hasRest = false, baseClassBlock)),
                      Seq(),
                    ),
                    "metatable"
                  )
                ),
              ),
            ),
          )))),
        )))

    yield AST.SimpleFunctionCall(
      AST.MemberAccessName(AST.NameExp("ArgonRuntime"), "define_type"),
      Seq(

        AST.FunctionDefinitionExp(paramNames, false, AST.Block(Seq(

        )))
      )
    )


  private def emitTrait(trt: ArTrait & HasImplementation[true]): ZIO[R, E, AST.Exp] = ???
  private def emitFunction(f: ArFunc & HasImplementation[true]): ZIO[R, E, AST.Exp] =
    for
      sig <- f.signature
      (paramMapping, paramNames) = getNonErasedParams(f, sig)

      nli <- TRef.make(0).commit
      vm <- TMap.empty[LocalVariable, String].commit

      impl <- f.implementation

      body <- impl match {
        case impl: FunctionImplementationC.ExpressionBody =>
          (new ExprEmitterCommon {
            override val nextLocalIndex: TRef[Int] = nli
            override val varMapping: TMap[LocalVariable, String] = vm

            override val instanceVarMapping: Map[context.ExprContext.InstanceVariable, AST.Var] = Map.empty
            override val parameterVarMapping: Map[context.ExprContext.ParameterVariable, AST.Var] = paramMapping
            override val memberVarMapping: Map[context.ExprContext.MemberVariable, AST.Var] = Map.empty
          }).emit(OutputMode.ReturnValue)(impl.body)

        case impl: FunctionImplementationC.External =>
          val extern = pluginAdapter.getExternFunctionImplementation(impl.impl)
          ZIO.succeed(AST.Block(Seq(
            AST.Return(Seq(
              AST.MemberAccessIndex(
                AST.SimpleFunctionCall(
                  AST.NameExp("require"),
                  Seq(AST.StringLiteral(extern.importPath))
                ),
                AST.StringLiteral(extern.memberName)
              ),
            )),
          )))
      }


    yield AST.FunctionDefinitionExp(paramNames, hasRest = false, body)

  private def emitMethods(methodMap: Comp[Map[Option[IdentifierExpr]], Seq[ArMethod & HasImplementation[IsImplementation]]], emitter: ExprEmitter[R, E]): Comp[AST.Exp] =
    for
      methods <- methodMap
      methodsExps <- ZIO.foreach(methods.toSeq) { methodGroup =>

      }
    yield ???
    
  private def emitConstructors(constructorsComp: Comp[Seq[ClassConstructor & HasImplementation[IsImplementation]]]): Comp[AST.Exp] = ???
  

  private def getNonErasedParams(owner: ParameterVariableOwner, sig: Signature[WrapExpr, ?]): (Map[ParameterVariable, AST.Var], Seq[String]) =
    def impl(sig: Signature[WrapExpr, ?], index: Int, accMap: Map[ParameterVariable, AST.Var], accSeq: Seq[String]): (Map[ParameterVariable, AST.Var], Seq[String]) =
      sig match
        case Signature.Parameter(_, true, _, _, next) =>
          impl(next, index + 1, accMap, accSeq)

        case Signature.Parameter(_, false, name, paramType, next) =>
          val paramVarName = s"param${accSeq.size}"

          impl(
            next,
            index + 1,
            accMap + (ParameterVariable(owner, index, paramType, false, name) -> AST.NameExp(paramVarName)),
            accSeq :+ paramVarName
          )

        case Signature.Result(_) => (accMap, accSeq)
      end match

    impl(sig, 0, Map.empty, Seq.empty)
  end getNonErasedParams


}


