package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.module.{ModuleElementC, ModulePath}
import dev.argon.compiler.signature.*
import dev.argon.expr.*
import dev.argon.parser
import dev.argon.parser.{FunctionParameterListType, IdentifierExpr}
import dev.argon.util.UniqueIdentifier
import dev.argon.util.{*, given}
import dev.argon.prover.Proof
import zio.*
import zio.stream.*

trait ExprUtilImplicitResolver
  extends ExprUtilWithHolesBase
    with ExprUtilSubstitution
    with ExprUtilHoleResolver
    with ExprUtilImports
{
  import exprContext.{
    ArExpr,
    ExprConstructor,
    WrapExpr,
    ClassResult,
    TraitResult,
    FunctionResult,
    Variable,
    ParameterVariable,
    LocalVariable,
    InstanceVariable,
  }

  val fuel: Int = 10


  final lazy val implicitResolver: ImplicitResolver[context.Env, context.Error] { val exprContext: ExprUtilImplicitResolver.this.exprContext.type } =
    new ImplicitResolver[context.Env, context.Error] {
      override val exprContext: ExprUtilImplicitResolver.this.exprContext.type = ExprUtilImplicitResolver.this.exprContext

      override def createHole: Comp[UniqueIdentifier] = UniqueIdentifier.make

      private def getResult[Res]
      (prologContext: IRPrologContext)
      (sigHandler: SignatureHandler[Res])
      (owner: exprContext.ParameterVariableOwner, index: Int, sig: Signature[WrapExpr, Res], args: Seq[ExprProverSyntax.Expr])
      : Comp[Res] =
        (sig, args) match {
          case (Signature.Parameter(_, isErased, paramName, paramType, nextSig), arg +: tailArgs) =>
            val variable = ParameterVariable(owner, index, paramType, isErased, paramName)

            prologContext.exprToWrapExpr(arg).flatMap { arg =>
              val nextSigSubst = substituteSignature(variable)(arg)(sigHandler)(nextSig)
              getResult(prologContext)(sigHandler)(owner, index, nextSigSubst, tailArgs)
            }

          case (Signature.Result(res), Seq()) => ZIO.succeed(res)
          case _ => ???
        }

      private def solveAll[E, T, U]
      (prologContext: IRPrologContext)
      (model: prologContext.Model)
      (values: Seq[(T, T)])
      (f: (T, T, prologContext.Model) => ZStream[context.Env, E, prologContext.ProofResult.Yes])
      : ZStream[context.Env, E, (Seq[Proof[TCAtomicProof]], prologContext.Model)] =
        values match {
          case (headA, headB) +: tail =>
            for
              headRes <- f(headA, headB, model)
              (tailResProofs, tailResModel) <- solveAll(prologContext)(headRes.model)(tail)(f)
            yield (headRes.proof +: tailResProofs, tailResModel)

          case _ => ZStream.succeed((Seq.empty, model))
        }

      override protected def isSubClass
      (
        prologContext: IRPrologContext,
        classA: ArClass,
        aArgs: Seq[ExprProverSyntax.Expr],
        classB: ArClass,
        bArgs: Seq[ExprProverSyntax.Expr],
        model: prologContext.Model,
        solveState: prologContext.SolveState,
      )
      : ZStream[context.Env, Either[context.Error, prologContext.ProofResult.No], prologContext.ProofResult.Yes] =
        import ExprProverSyntax.*
        if classA.id == classB.id then
          solveAll(prologContext)(model)(aArgs.zip(bArgs)) { (aArg, bArg, model) =>
            prologContext.solve(PredicateFunction(ExprConstructor.EqualTo, Seq(prologContext.wrapExprToExpr(anyType), aArg, bArg)), model, solveState)
          }
            .map { case (_, model) =>
              val proofExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple))
              val proof = Proof.Atomic(TCAtomicProof.ExprProof(proofExpr))
              prologContext.ProofResult.Yes(proof, model)
            }
        else
          ZStream.unwrap(
            classA.signature
              .map(convertSig(classSigHandler))
              .flatMap { sig =>
                getResult(prologContext)(classSigHandler)(classA, 0, sig, aArgs)
              }
              .flatMap { classRes => classRes.baseClass }
              .mapError(Left.apply)
              .map {
                case Some(baseClass) =>
                  val baseClassArgs = baseClass.args.map(prologContext.wrapExprToExpr)
                  isSubClass(prologContext, baseClass.constructor.arClass, baseClassArgs, classB, bArgs, model, solveState)

                case None =>
                  val notProofExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple))
                  val notProof = Proof.Atomic(TCAtomicProof.ExprProof(notProofExpr))
                  val result = prologContext.ProofResult.No(notProof, model)
                  ZStream.fail(Right(result))
              }
          )
      end isSubClass

      override protected def isSubTrait
      (
        prologContext: IRPrologContext,
        traitA: ArTrait,
        aArgs: Seq[ExprProverSyntax.Expr],
        traitB: ArTrait,
        bArgs: Seq[ExprProverSyntax.Expr],
        model: prologContext.Model,
        solveState: prologContext.SolveState,
      )
      : ZStream[context.Env, Either[context.Error, prologContext.ProofResult.No], prologContext.ProofResult.Yes] =
        import ExprProverSyntax.*
        if traitA.id == traitB.id then
          solveAll(prologContext)(model)(aArgs.zip(bArgs)) { (aArg, bArg, model) =>
            prologContext.solve(PredicateFunction(ExprConstructor.EqualTo, Seq(prologContext.wrapExprToExpr(anyType), aArg, bArg)), model, solveState)
          }
            .map { case (_, model) =>
              val proofExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple))
              val proof = Proof.Atomic(TCAtomicProof.ExprProof(proofExpr))
              prologContext.ProofResult.Yes(proof, model)
            }
        else
          ZStream.unwrap(
            traitA.signature
              .map(convertSig(traitSigHandler))
              .flatMap { sig =>
                getResult(prologContext)(traitSigHandler)(traitA, 0, sig, aArgs)
              }
              .flatMap { traitRes => traitRes.baseTraits }
              .mapError(Left.apply)
              .map { baseTraits =>
                ZStream.fromIterable(baseTraits)
                  .flatMap { baseTrait =>
                    val baseTraitArgs = baseTrait.args.map(prologContext.wrapExprToExpr)
                    isSubTrait(prologContext, baseTrait.constructor.arTrait, baseTraitArgs, traitB, bArgs, model, solveState)
                  }
              }
          )
      end isSubTrait

      protected override def classImplementsTrait
      (
        prologContext: IRPrologContext,
        classA: ArClass,
        aArgs: Seq[ExprProverSyntax.Expr],
        traitB: ArTrait,
        bArgs: Seq[ExprProverSyntax.Expr],
        model: prologContext.Model,
        solveState: prologContext.SolveState,
      )
      : ZStream[context.Env, Either[context.Error, prologContext.ProofResult.No], prologContext.ProofResult.Yes] =
        ZStream.unwrap(
          classA.signature
            .map(convertSig(classSigHandler))
            .flatMap { sig =>
              getResult(prologContext)(classSigHandler)(classA, 0, sig, aArgs)
            }
            .mapError(Left.apply)
            .map { classResult =>
              val baseTraitResults =
                ZStream.fromIterableZIO(classResult.baseTraits.mapError(Left.apply))
                  .flatMap { baseTrait =>
                    val baseTraitArgs = baseTrait.args.map(prologContext.wrapExprToExpr)
                    isSubTrait(prologContext, baseTrait.constructor.arTrait, baseTraitArgs, traitB, bArgs, model, solveState)
                  }

              val baseClassResult =
                ZStream.fromIterableZIO(classResult.baseClass.mapBoth(Left.apply, _.toList))
                  .flatMap { baseClass =>
                    val baseClassArgs = baseClass.args.map(prologContext.wrapExprToExpr)
                    classImplementsTrait(prologContext, baseClass.constructor.arClass, baseClassArgs, traitB, bArgs, model, solveState)
                  }

              baseTraitResults ++ baseClassResult
            }
        )


      override protected def typeOfClass(classObj: ArClass, args: Seq[WrapExpr]): Comp[WrapExpr] =
        for
          sig <- classObj.signature
          sigConv = convertSig(classSigHandler)(sig)
          ClassResult(classT, _, _) <- substituedSigResult(classObj)(classSigHandler)(sigConv, args.toList)
        yield classT

      override protected def typeOfTrait(traitObj: ArTrait, args: Seq[WrapExpr]): Comp[WrapExpr] = ???

      protected override def traitRelations(arTrait: ArTrait): Comp[Seq[ExprRelation]] =
        arTrait.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def classRelations(arClass: ArClass): Comp[Seq[ExprRelation]] =
        arClass.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def functionRelations(function: ArFunc): Comp[Seq[ExprRelation]] =
        function.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def methodRelations(method: ArMethod): Comp[Seq[ExprRelation]] =
        method.signatureUnsubstituted.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def classConstructorRelations(classCtor: ClassConstructor): Comp[Seq[ExprRelation]] =
        classCtor.signatureUnsubstituted.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }


      override protected def substituteVariables(vars: Map[Variable, WrapExpr])(expr: WrapExpr): WrapExpr =
        substituteWrapExprMany(vars)(expr)


      override protected def natLessThanFunction: Comp[ArFunc] =
        loadKnownExport[ModuleElementC.FunctionElement[context.type, ?]](ImportSpecifier(
          argonCoreTubeName,
          ModulePath(Seq("Nat")),
          Some(IdentifierExpr.OperatorIdentifier(parser.BinaryOperator.LessThan)),
          ErasedSignatureWithResult(
            Seq(
              ErasedSignatureType.Class(natSpecifier, Seq()),
              ErasedSignatureType.Class(natSpecifier, Seq()),
            ),
            ErasedSignatureType.Class(boolSpecifier, Seq())
          ),
        )).map {
          _.func
        }

      override protected def boolType: Comp[WrapExpr] =
        ExprUtilImplicitResolver.this.boolType

      protected override def invalidExpr: Comp[Nothing] = ???

      protected override def invalidPredicateExpr: Comp[Nothing] = ???

      protected override lazy val evaluator
      : Evaluator[context.Env, context.Error] {val exprContext: ExprUtilImplicitResolver.this.exprContext.type} =
        new ArgonEvaluator[context.Env, context.Error] {
          override val context: ExprUtilImplicitResolver.this.context.type = ExprUtilImplicitResolver.this.context
          override val exprContext: ExprUtilImplicitResolver.this.exprContext.type = ExprUtilImplicitResolver.this.exprContext
        }
    }

  def tryResolveImplicit(env: Env, t: WrapExpr): Comp[Option[(Env, WrapExpr)]] =
    def buildImplicits(value: ImplicitValue)(newVariable: Comp[UniqueIdentifier]): Comp[implicitResolver.Assertion] =
      value match {
        case ImplicitValue.OfVariable(variable) =>
          val loadVar = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(variable), EmptyTuple))
          val assertion = implicitResolver.Assertion(loadVar, variable.varType)
          ZIO.succeed(assertion)

        case ImplicitValue.OfFunction(function) =>
          def buildCall(sig: Signature[WrapExpr, FunctionResult], args: Seq[WrapExpr]): Comp[implicitResolver.Assertion] =
            sig match
              case Signature.Parameter(FunctionParameterListType.InferrableList | FunctionParameterListType.InferrableList2, isErased, paramName, paramType, next) =>
                val variable = ParameterVariable(function, args.size, paramType, isErased, paramName)
                for
                  hole <- newVariable
                  holeExpr = WrapExpr.OfHole(hole)
                  nextSubst = substituteSignature(variable)(holeExpr)(functionSigHandler)(next)

                  assertion <- buildCall(nextSubst, args :+ holeExpr)
                yield assertion

              case Signature.Parameter(FunctionParameterListType.RequiresList, isErased, paramName, paramType, next) =>
                for
                  varId <- newVariable
                  local = LocalVariable(varId, paramType, paramName, isMutable = false, isErased = isErased)
                  loadLocal = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(local), EmptyTuple))
                  implicitResolver.Assertion(witness, assertionType) <- buildCall(next, args :+ loadLocal)
                yield implicitResolver.Assertion(
                  witness = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadLambda(local), witness)),
                  assertionType = WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionType, (paramType, assertionType)))
                )

              case Signature.Parameter(FunctionParameterListType.NormalList, _, _, paramType, next) => ???

              case Signature.Result(resultType) =>
                ZIO.succeed(implicitResolver.Assertion(
                  witness = WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionCall(function), args)),
                  assertionType = resultType.returnType,
                ))
            end match

          function.signature.flatMap { sig =>
            val sig2 = convertSig(functionSigHandler)(sig)
            buildCall(sig2, Seq())
          }

      }

    for
      givenAssertions <- env.implicitSource.givenAssertions
      assertions = givenAssertions.map(buildImplicits)
      resolved <- implicitResolver.tryResolve(t, env.model, assertions, env.knownVarValues, fuel)
      res <- ZIO.foreach(resolved) {
        case implicitResolver.ResolvedImplicit(proof, model) =>
          def extractProof(proof: Proof[WrapExpr]): Comp[WrapExpr] =
            proof match {
              case Proof.Atomic(expr) => ZIO.succeed(expr)

              case Proof.ModusPonens(implication, premise) =>
                for
                  implication <- extractProof(implication)
                  premise <- extractProof(premise)
                yield WrapExpr.OfExpr(ArExpr(ExprConstructor.FunctionObjectCall, (implication, premise)))

              case _ =>
                // TODO: Implement actual types for these proofs
                ZIO.succeed(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
            }

          for
            proof <- extractProof(proof)
          yield (env.copy(model = model), proof)
      }
    yield res
  end tryResolveImplicit

  def resolveImplicit(env: Env, t: WrapExpr, location: SourceLocation): Comp[(Env, WrapExpr)] =
    tryResolveImplicit(env, t).flatMap {
      case Some(res) => ZIO.succeed(res)
      case None => ZIO.fail(DiagnosticError.ImplicitNotFound(DiagnosticSource.Location(location)))
    }

}
