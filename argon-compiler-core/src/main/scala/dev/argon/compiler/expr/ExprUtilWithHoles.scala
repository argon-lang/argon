package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.{ArClassC, ArTraitC}
import dev.argon.compiler.module.*
import dev.argon.compiler.signature.*
import dev.argon.expr.*
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*

trait ExprUtilWithHoles extends ExprUtil {
  override val exprContext: ArgonExprContext {
    val context: ExprUtilWithHoles.this.context.type
    type THole = UniqueIdentifier
  }
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}


  def resolveHoles(env: Env, expr: WrapExpr): Comp[(context.ExprContext.WrapExpr, Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- ArgonExprContext.convertWrapExpr(context)(exprContext, context.ExprContext)(resolveHole(envRef))(expr)
      env <- envRef.get
    yield (convExpr, env)

  private def resolveHole(envRef: Ref.Synchronized[Env])(hole: UniqueIdentifier): Comp[context.ExprContext.WrapExpr] =
    ???

  def resolveHolesClassType(env: Env, expr: ArExpr[ExprConstructor.ClassType])
  : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType], Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- ArgonExprContext.convertClassType(context)(exprContext, context.ExprContext)(resolveHole(envRef))(expr)
      env <- envRef.get
    yield (convExpr, env)

  def resolveHolesTraitType(env: Env, expr: ArExpr[ExprConstructor.TraitType])
  : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType], Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- ArgonExprContext.convertTraitType(context)(exprContext, context.ExprContext)(resolveHole(envRef))(expr)
      env <- envRef.get
    yield (convExpr, env)


  type ClassResultContext = (context.ExprContext.WrapExpr, Option[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType]], Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]])
  type TraitResultContext = (context.ExprContext.WrapExpr, Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]])


  trait SignatureHandlerPlus[Res1, Res2] extends SignatureHandler[Res2] {
    def convertResult(res: Res1): Res2
    def resolveResultHoles(env: Env, res: Res2): Comp[(Res1, Env)]
  }


  override val functionSigHandler: SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr] =
    new SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr] with FunctionSigHandlerBase:
      override def convertResult(res: context.ExprContext.WrapExpr): WrapExpr =
        ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(res)

      override def resolveResultHoles(env: Env, res: WrapExpr): Comp[(context.ExprContext.WrapExpr, Env)] =
        resolveHoles(env, res)
    end new


  override val classSigHandler: SignatureHandlerPlus[ClassResultContext, ClassResultConv] =
    new SignatureHandlerPlus[ClassResultContext, ClassResultConv] with ClassSigHandlerBase:
      override def convertResult(res: ClassResultContext): ClassResultConv =
        val (classType, baseClass, baseTraits) = res

        val classType2 =
          ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(classType)
        val baseClass2 =
          baseClass.map(ArgonExprContext.convertClassType[Id](context)(context.ExprContext, exprContext)(identity))
        val baseTraits2 =
          baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

        (classType2, baseClass2, baseTraits2)
      end convertResult

      override def resolveResultHoles(env: Env, res: ClassResultConv): Comp[(ClassResultContext, Env)] =
        val (classType, baseClass, baseTraits) = res
        resolveHoles(env, classType).flatMap { case (classType2, env) =>
          ZIO.foreach(baseClass)(resolveHolesClassType(env, _))
            .map {
              case Some((baseClass2, env)) => (Some(baseClass2), env)
              case None => (None, env)
            }
            .flatMap { case (baseClass2, env) =>
              for {
                envState <- Ref.make(env)
                baseTraits2 <-
                  ZIO.foreach(baseTraits) { baseTrait =>
                    for {
                      env <- envState.get
                      baseTrait2Res <- resolveHolesTraitType(env, baseTrait)
                      (baseTrait2, env) = baseTrait2Res
                      _ <- envState.set(env)
                    } yield baseTrait2
                  }
                env <- envState.get
              } yield ((classType2, baseClass2, baseTraits2), env)

            }
        }
      end resolveResultHoles
    end new


  override val traitSigHandler: SignatureHandlerPlus[TraitResultContext, TraitResultConv] =
    new SignatureHandlerPlus[TraitResultContext, TraitResultConv] with TraitSigHandlerBase:
      override def convertResult(res: TraitResultContext): TraitResultConv =
        val (traitType, baseTraits) = res

        val traitType2 =
          ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(traitType)
        val baseTraits2 =
          baseTraits.map(ArgonExprContext.convertTraitType[Id](context)(context.ExprContext, exprContext)(identity))

        (traitType2, baseTraits2)
      end convertResult

      override def resolveResultHoles(env: Env, res: TraitResultConv): Comp[(TraitResultContext, Env)] =
        val (classType, baseTraits) = res
        resolveHoles(env, classType).flatMap { case (classType2, env) =>
          for {
            envState <- Ref.make(env)
            baseTraits2 <-
              ZIO.foreach(baseTraits) { baseTrait =>
                for {
                  env <- envState.get
                  baseTrait2Res <- resolveHolesTraitType(env, baseTrait)
                  (baseTrait2, env) = baseTrait2Res
                  _ <- envState.set(env)
                } yield baseTrait2
              }
            env <- envState.get
          } yield ((classType2, baseTraits2), env)
        }
      end resolveResultHoles
    end new



  final def convertSig[Res1, Res2](sigHandler: SignatureHandlerPlus[Res1, Res2])
                                  (sig: Signature[context.ExprContext.WrapExpr, Res1])
  : Signature[WrapExpr, Res2] =
    sig match {
      case Signature.Parameter(paramListType, isErased, paramName, paramType, next) =>
        Signature.Parameter(
          paramListType,
          isErased,
          paramName,
          ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprContext)(identity)(paramType),
          convertSig(sigHandler)(next),
        )

      case Signature.Result(res) =>
        Signature.Result(sigHandler.convertResult(res))
    }



  final lazy val implicitResolver: ImplicitResolver[context.Env, context.Error] { val exprContext: ExprUtilWithHoles.this.exprContext.type } =
    new ImplicitResolver[context.Env, context.Error] {
      override val exprContext: ExprUtilWithHoles.this.exprContext.type = ExprUtilWithHoles.this.exprContext

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

      protected override def traitRelations(arTrait: ArTrait): Comp[Seq[ExprRelation]] =
        arTrait.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def classRelations(arClass: ArClass): Comp[Seq[ExprRelation]] =
        arClass.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def functionRelations(function: ArFunc): Comp[Seq[ExprRelation]] =
        function.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def methodRelations(method: ArMethod): Comp[Seq[ExprRelation]] =
        method.signatureUnsubstituted.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

      protected override def classConstructorRelations(classCtor: ClassConstructor): Comp[Seq[ExprRelation]] =
        classCtor.signature.map { sig => Seq.fill(sig.parameterCount)(ExprRelation.SyntacticEquality) }

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
        )).map { _.func }

      override protected def boolType: Comp[WrapExpr] =
        ExprUtilWithHoles.this.boolType

      protected override def invalidExpr: Comp[Nothing] = ???
      protected override def invalidPredicateExpr: Comp[Nothing] = ???

      protected override lazy val evaluator
      : Evaluator[context.Env, context.Error] { val exprContext: ExprUtilWithHoles.this.exprContext.type } =
        new Evaluator[context.Env, context.Error] {
          override val exprContext: ExprUtilWithHoles.this.exprContext.type = ExprUtilWithHoles.this.exprContext

          override def getFunctionBody(function: ArFunc, args: Seq[WrapExpr], fuel: Int): Comp[Option[WrapExpr]] =
            ZIO.none

          override def getMethodBody(method: ArMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int)
          : Comp[Option[WrapExpr]] =
            ZIO.none

        }
    }


  val fuel: Int = 6

  // Ensures that a <: b
  def checkSubType(env: Env, a: WrapExpr, b: WrapExpr, location: SourceLocation): Comp[Env] =
    ZIO.logTrace(s"checkSubType a=$a, b=$b") *>
    isSubType(env, a, b).flatMap {
      case Some(env) => ZIO.succeed(env)
      case None => ZIO.fail(DiagnosticError.TypeError(DiagnosticSource.Location(location)))
    }

  def isSubType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] =
    if isSubTypeQuick(a, b) then
      ZIO.succeed(Some(env))
    else
      val prop = WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (a, b)))
      implicitResolver.tryResolve(prop, env.model, fuel).map(_.map {
        case implicitResolver.ResolvedImplicit(_, model) =>
          env.copy(model = model)
      })
    end if


  // Fast paths are needed to avoid circular loading.
  // Type checker needs to load (<): (Nat, Nat) -> Bool which requires type checking.
  // This implements enough of the type checker to load this function.
  private def isSubTypeQuick(a: WrapExpr, b: WrapExpr): Boolean =
    a.equals(b) || ((a, b) match {
      case (WrapExpr.OfExpr(a), WrapExpr.OfExpr(b)) =>
        (a.constructor, b.constructor) match {
          case (ExprConstructor.TypeN, ExprConstructor.AnyType) => true
          case (ExprConstructor.TypeN, ExprConstructor.OmegaTypeN(_)) => true
          case (ExprConstructor.OmegaTypeN(n), ExprConstructor.OmegaTypeN(m)) => n <= m
          case _ => false
        }

      case _ => false
    })

  def isSameType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] =
    isSubType(env, a, b).flatMap {
      case Some(env) => isSubType(env, b, a)
      case None => ZIO.none
    }
}
