package dev.argon.compiler.expr

import dev.argon.compiler.module.{ModuleElementC, ModulePath}
import dev.argon.compiler.signature.*
import dev.argon.expr.*
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import dev.argon.util.UniqueIdentifier
import dev.argon.util.{*, given}

import zio.*
import zio.stream.*

trait ExprUtilImplicitResolver
  extends ExprUtilWithHolesBase
    with ExprUtilSubstitution
    with ExprUtilHoleResolver
    with ExprUtilImports
{
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  val fuel: Int = 6


  final lazy val implicitResolver: ImplicitResolver[context.Env, context.Error] { val exprContext: ExprUtilImplicitResolver.this.exprContext.type } =
    new ImplicitResolver[context.Env, context.Error] {
      override val exprContext: ExprUtilImplicitResolver.this.exprContext.type = ExprUtilImplicitResolver.this.exprContext

      import exprContext.{WrapExpr, ArExpr, ExprConstructor, Variable, LocalVariable, ParameterVariable, InstanceVariable}

      override def createHole: Comp[UniqueIdentifier] = UniqueIdentifier.make

      private def getResult[Res](sigHandler: SignatureHandler[Res])
                                (owner: exprContext.ParameterVariableOwner, index: Int, sig: Signature[WrapExpr, Res], args: Seq[WrapExpr])
      : Comp[Res] =
        (sig, args) match {
          case (Signature.Parameter(_, isErased, paramName, paramType, nextSig), arg +: tailArgs) =>
            val variable = ParameterVariable(owner, index, paramType, isErased, paramName)
            val nextSigSubst = substituteSignature(variable)(arg)(sigHandler)(nextSig)
            getResult(sigHandler)(owner, index, nextSigSubst, tailArgs)

          case (Signature.Result(res), Seq()) => ZIO.succeed(res)
          case _ => ???
        }

      protected override def isSubClass
      (classA: ArClass, aArgs: Seq[WrapExpr], classB: ArClass, bArgs: Seq[WrapExpr], fuel: Int)
      : Comp[SubClassResult] =
        if classA.id == classB.id then
          ZIO.forall(aArgs.zip(bArgs)) { case (aArg, bArg) =>
            tryResolve(WrapExpr.OfExpr(ArExpr(ExprConstructor.EqualTo, (aArg, bArg))), Map.empty, fuel).map {
              _.isDefined
            }
          }
            .map {
              case true =>
                SubClassResult.SubClassProof(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
              case false => SubClassResult.Unknown
            }
        else
          classA.signature
            .map(convertSig(classSigHandler))
            .flatMap { sig =>
              getResult(classSigHandler)(classA, 0, sig, aArgs)
            }
            .flatMap {
              case (_, Some(baseClass), _) =>
                isSubClass(baseClass.constructor.arClass, baseClass.args, classB, bArgs, fuel)
              case (_, None, _) => ZIO.succeed(SubClassResult.NotSubClassProof(WrapExpr.OfExpr(ArExpr(
                ExprConstructor.AssumeErasedValue,
                EmptyTuple,
              ))))
            }

      private def getSubClassResultSink[E]: Sink[E, SubClassResult, SubClassResult, Option[SubClassResult]] =
        ZSink.fold(Option.empty[SubClassResult]) {
          case Some(SubClassResult.SubClassProof(_)) => false
          case _ => true
        } {
          case (state, SubClassResult.NotSubClassProof(_)) =>
            state

          case (_, result) => Some(result)
        }

      private def getOrNotSubClass(result: Option[SubClassResult]): SubClassResult =
        result match {
          case Some(result) => result
          case None =>
            SubClassResult.NotSubClassProof(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
        }

      protected override def isSubTrait
      (traitA: ArTrait, aArgs: Seq[WrapExpr], traitB: ArTrait, bArgs: Seq[WrapExpr], fuel: Int)
      : Comp[SubClassResult] =
        if traitA.id == traitB.id then
          ZIO.forall(aArgs.zip(bArgs)) { case (aArg, bArg) =>
            tryResolve(WrapExpr.OfExpr(ArExpr(ExprConstructor.EqualTo, (aArg, bArg))), Map.empty, fuel).map {
              _.isDefined
            }
          }
            .map {
              case true =>
                SubClassResult.SubClassProof(WrapExpr.OfExpr(ArExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)))
              case false => SubClassResult.Unknown
            }
        else
          traitA.signature
            .map(convertSig(traitSigHandler))
            .flatMap { sig =>
              getResult(traitSigHandler)(traitA, 0, sig, aArgs)
            }
            .flatMap { case (_, baseTraits) =>
              ZStream.fromIterable(baseTraits)
                .mapZIO { baseTrait =>
                  isSubTrait(baseTrait.constructor.arTrait, baseTrait.args, traitB, bArgs, fuel)
                }
                .run(getSubClassResultSink)
                .map(getOrNotSubClass)
            }

      protected override def classImplementsTrait
      (classA: ArClass, aArgs: Seq[WrapExpr], traitB: ArTrait, bArgs: Seq[WrapExpr], fuel: Int)
      : Comp[SubClassResult] =
        classA.signature
          .map(convertSig(classSigHandler))
          .flatMap { sig =>
            getResult(classSigHandler)(classA, 0, sig, aArgs)
          }
          .flatMap { case (_, baseClass, baseTraits) =>
            val baseTraitResults =
              ZStream.fromIterable(baseTraits)
                .mapZIO { baseTrait =>
                  isSubTrait(baseTrait.constructor.arTrait, baseTrait.args, traitB, bArgs, fuel)
                }

            val baseClassResult =
              ZStream.fromIterable(baseClass.toList)
                .mapZIO { baseClass =>
                  classImplementsTrait(baseClass.constructor.arClass, baseClass.args, traitB, bArgs, fuel)
                }

            (baseTraitResults ++ baseClassResult)
              .run(getSubClassResultSink)
              .map(getOrNotSubClass)
          }


      override protected def typeOfClass(classObj: ArClass, args: Seq[WrapExpr]): Comp[WrapExpr] =
        for
          sig <- classObj.signature
          sigConv = convertSig(classSigHandler)(sig)
          (classT, _, _) <- substituedSigResult(classObj)(classSigHandler)(sigConv, args.toList)
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
        new Evaluator[context.Env, context.Error] {
          override val exprContext: ExprUtilImplicitResolver.this.exprContext.type = ExprUtilImplicitResolver.this.exprContext

          override def getFunctionBody(function: ArFunc, args: Seq[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] =
            ZIO.none

          override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int)
          : Comp[Option[WrapExpr]] =
            ZIO.none

        }
    }
}
