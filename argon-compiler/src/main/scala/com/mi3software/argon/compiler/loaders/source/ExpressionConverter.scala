package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.types._
import com.mi3software.argon.parser
import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation, WithSource}
import com.mi3software.argon.util.AnyExtensions._

import scala.collection.immutable.Set
import scalaz._
import Scalaz._
import PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.HoleTypeHole
import com.mi3software.argon.parser.UnitLiteral

import Function.const

sealed trait ExpressionConverter[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem[context.type]
  val scopeContext: ScopeContext[context.type] { val typeSystem: ExpressionConverter.this.typeSystem.type }
  val signatureContext: SignatureContext[context.type] { val typeSystem: ExpressionConverter.this.typeSystem.type }

  import ExpressionConverter.{ HoleType, TypeCheck => TypeCheckA, TypeConstraint }
  import typeSystem.{ context => _, _ }
  import scopeContext.{ context => _, _ }
  import signatureContext.Signature



  type Env = ExpressionConverter.Env[context.type, Scope]
  private type TypeCheckT[TCType, TComp[_]] = TypeCheckA[context.type, TCType, TComp]
  private type TypeCheck[TComp[_]] = TypeCheckT[typeSystem.TType, TComp]

  final case class ArgumentInfo[TComp[_]](argFactory: ExprFactory[TComp], location: SourceLocation)

  trait ExprFactory[TComp[_]] {
    def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr]
    def memberAccessExpr(memberName: MemberName, location: SourceLocation): ExprFactory[TComp] = ???
    def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] = ???
  }

  def convertExpr[TComp[_] : TypeCheck](env: Env)(expr: WithSource[parser.Expr]): ExprFactory[TComp] =
    expr.value match {
      case parser.AsExpr(value, valueTypeExpr) =>
        compFactory(
          for {
            expectedType <- evaluateTypeExprAST(env)(valueTypeExpr)
            result <- convertExpr(env)(value).forExpectedType(expectedType)
          } yield factoryForExpr(env)(expr.location)(result)
        )

      case parser.BoolValueExpr(b) =>
        compFactory(
          for {
            boolType <- resolveBoolClass(env)(expr.location)
          } yield factoryForExpr(env)(expr.location)(LoadConstantBool(b, boolType))
        )

      case parser.DotExpr(obj, member) =>
        convertExpr(env)(obj).memberAccessExpr(MemberName.Normal(member), expr.location)

      case parser.FunctionCallExpr(func, arg) =>
        convertExpr[TComp](env)(func).forArguments(ArgumentInfo[TComp](convertExpr(env)(arg), arg.location))

      case parser.IdentifierExpr(name) =>
        createLookupFactory(env)(LookupDescription.Identifier(name))(expr.location)(env.scope.findIdentifier(name, env.fileSpec, expr.location))

      case parser.IfExpr(cond, ifBody) =>
        createIfExpr(env)(expr.location)(cond, ifBody, WithSource(Vector.empty, SourceLocation(expr.location.end, expr.location.end)))

      case parser.IfElseExpr(cond, ifBody, elseBody) =>
        createIfExpr(env)(expr.location)(cond, ifBody, elseBody)

      case parser.IntValueExpr(sign, base, digits) =>
        val value = sign * digits.foldRight(0 : BigInt) { (digit, acc) => acc * base + digit }

        compFactory(
          for {
            intType <- resolveModuleClass(env)(expr.location)(ModuleDescriptor("Argon.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("Int"))
          } yield factoryForExpr(env)(expr.location)(LoadConstantInt(value, intType))
        )

      case parser.StringValueExpr(str) =>
        compFactory(
          for {
            stringType <- resolveModuleClass(env)(expr.location)(ModuleDescriptor("Argon.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("String"))
          } yield factoryForExpr(env)(expr.location)(LoadConstantString(str, stringType))
        )

      case _ => ???
    }

  def createIfExpr[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(cond: WithSource[parser.Expr], ifBody: WithSource[Vector[WithSource[parser.Stmt]]], elseBody: WithSource[Vector[WithSource[parser.Stmt]]]) =
    new ExprFactory[TComp] {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
        for {
          boolType <- resolveBoolClass(env)(location)
          condTC <- convertExpr(env)(cond).forExpectedType(boolType)
          ifBodyTC <- convertStmts(env)(ifBody).forExpectedType(expectedType)
          elseBodyTC <- convertStmts(env)(elseBody).forExpectedType(expectedType)
        } yield IfElse(condTC, ifBodyTC, elseBodyTC)
    }

  def convertStmts[TComp[_] : TypeCheck](env: Env)(stmts: WithSource[Vector[WithSource[parser.Stmt]]]): ExprFactory[TComp] =
    stmts.value match {
      case Vector() => loadUnitLiteral(env)(stmts.location)
      case Vector(stmt) => convertStmt(env)(stmt)
      case head +: tail =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
            for {
              unitType <- resolveUnitType[TComp](env)(head.location)
              first <- convertStmt(env)(head).forExpectedType(unitType)

              secondStartPos = tail.headOption.map { _.location.start }.getOrElse(stmts.location.end)
              second <- convertStmts(env)(WithSource(tail, SourceLocation(secondStartPos, stmts.location.end))).forExpectedType(expectedType)
            } yield Sequence(first, second)
        }
    }

  def convertStmt[TComp[_] : TypeCheck](env: Env)(stmt: WithSource[parser.Stmt]): ExprFactory[TComp] =
    stmt.value match {
      case expr: parser.Expr =>
        convertExpr(env)(WithSource(expr, stmt.location))

      case _ => ???
    }

  def resolveModuleClassFactory[TComp[_] : TypeCheck]
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  (args: Vector[ArgumentInfo[TComp]])
  : ExprFactory[TComp] = compFactory(
    for {
      arClass <- Compilation[TComp].requireSome(
        ModuleLookup.lookupValue(context)(env.referencedModules)(moduleDesc)(namespacePath, name)(ModuleLookup.lookupGlobalClass)
      )(CompilationError.NamespaceElementNotFound(moduleDesc, namespacePath, name, CompilationMessageSource.SourceFile(env.fileSpec, location)))
      classSig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClass.signature)

      classFactory = signatureFactory[TComp, ArClass.ResultInfo](env)(
        convertSignature(classSig)
      ) { (args, classResult) =>
        for {
          argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
        } yield ClassType(AbsRef[context.type, ReferencePayloadSpecifier, ArClass](arClass), argsAsTypes, classResult.baseTypes)
      }

    } yield args.foldLeft(classFactory) { (factory, arg) => factory.forArguments(arg) }
  )

  def resolveModuleClass[TComp[_] : TypeCheck]
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  : TComp[TType] =
    evaluateTypeExprFactory(env)(location)(resolveModuleClassFactory(env)(location)(moduleDesc)(namespacePath, name)(Vector.empty))

  def resolveBoolClass[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor("Argon.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("Bool"))

  def resolveUnitType[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor("Argon.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("Unit"))

  def loadUnitLiteral[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): ExprFactory[TComp] =
    compFactory(
      for {
        unitType <- resolveUnitType(env)(location)
      } yield factoryForExpr(env)(location)(LoadUnit(unitType))
    )

  def factoryForExpr[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr): ExprFactory[TComp] =
    new ExprFactory[TComp] {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
        convertExprType(env)(location)(expr)(expectedType)
    }

  def createLookupFactory[TComp[_] : TypeCheck](env: Env)(description: LookupDescription)(location: SourceLocation)(lookupResult: LookupResult): ExprFactory[TComp] =
    lookupResult match {
      case LookupResult.ScopeResult(scope) =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
            Compilation[TComp].forErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def memberAccessExpr(memberName: MemberName, location: SourceLocation): ExprFactory[TComp] = {
            val newResult = memberName match {
              case MemberName.Normal(name) => scope.findIdentifier(name, env.fileSpec, location)
              case _ => LookupResult.Failed
            }

            createLookupFactory(env)(LookupDescription.Member(description, memberName))(location)(newResult)
          }

          override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
            compFactory(
              Compilation[TComp].forErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))
            )
        }

      case LookupResult.ValuesResult(OverloadResult.List(Vector(result), _)) =>
        result match {
          case VariableScopeValue(variable) =>
            factoryForExpr(env)(location)(LoadVariable(variable))

          case FunctionScopeValue(func) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(func.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(convSig) { (args, result) => FunctionCall(func, args, result.returnType).upcast[ArExpr].point[TComp] }
            )

          case TraitScopeValue(arTrait) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arTrait.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
                } yield TraitType(arTrait, argsAsTypes, result.baseTypes)
              }
            )

          case ClassScopeValue(arClass) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClass.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
                } yield ClassType(arClass, argsAsTypes, result.baseTypes)
              }
            )

          case DataConstructorScopeValue(ctor) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(ctor.value.signature)
                convSig = convertSignature(sig)
              } yield signatureFactory(env)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(location)(_))
                } yield DataConstructorCall(
                  DataConstructorType(
                    ctor,
                    argsAsTypes,
                    result.instanceType
                  ),
                  args
                )
              }
            )
        }

      case LookupResult.ValuesResult(_) => ???

      case LookupResult.Failed =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
            Compilation[TComp].forErrors(CompilationError.LookupFailedError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

          override def memberAccessExpr(memberName: MemberName, location: SourceLocation): ExprFactory[TComp] = this
          override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] = this
        }
    }

  def compFactory[TComp[_] : TypeCheck](compFac: TComp[ExprFactory[TComp]]): ExprFactory[TComp] =
    new ExprFactory[TComp] {
      override def forExpectedType(expectedType: typeSystem.TType): TComp[ArExpr] =
        compFac.flatMap { _.forExpectedType(expectedType) }

      override def memberAccessExpr(memberName: MemberName, location: SourceLocation): ExprFactory[TComp] =
        compFactory(
          compFac.map { _.memberAccessExpr(memberName, location) }
        )

      override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] =
        compFactory(
          compFac.map { _.forArguments(argInfo) }
        )
    }

  def signatureFactory[TComp[_] : TypeCheck, TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (env: Env)
  (signature: Signature[TResult])
  (f: (Vector[ArExpr], TResult[context.type, typeSystem.type]) => TComp[ArExpr])
  : ExprFactory[TComp] = {

    final class SigFactory(env: Env)(signature: Signature[TResult])(prevArgs: Vector[ArExpr]) extends ExprFactory[TComp] {
      override def forExpectedType(expectedType: TType): TComp[ArExpr] =
        signature.visit(
          sigParams => ???,
          sigResult => f(prevArgs, sigResult.result)
        )

      override def forArguments(argInfo: ArgumentInfo[TComp]): ExprFactory[TComp] = ???
    }

    new SigFactory(env)(signature)(Vector.empty)
  }

  def convertSignature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (sig: context.signatureContext.Signature[TResult])
  : Signature[TResult] =
    sig.convertTypeSystem(signatureContext)(ArTypeSystemConverter(context)(typeSystem))

  def convertExprType[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr)(t: typeSystem.TType): TComp[ArExpr] =
    typeSystem.isSubType[TComp](t, expr.exprType).flatMap {
      case Some(info) => implicitly[TypeCheck[TComp]].recordConstraint(info).map(const(expr))
      case None => Compilation[TComp].forErrors(CompilationError.CouldNotConvertType(context)(typeSystem)(t, expr.exprType)(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }

  def evaluateTypeExprFactory[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(factory: ExprFactory[TComp]): TComp[TType] =
    inferExprType(factory).flatMap { expr =>
      evaluateTypeExpr(env)(location)(expr)
    }

  def evaluateTypeExpr[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr): TComp[TType] =
    expr match {
      case t: SimpleType => typeSystem.fromSimpleType(t).point[TComp]
      case _ => Compilation[TComp].forErrors(CompilationError.ExpressionNotTypeError(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }

  def evaluateTypeExprAST[TComp[_] : TypeCheck](env: Env)(expr: WithSource[parser.Expr]): TComp[TType] =
    evaluateTypeExprFactory(env)(expr.location)(convertExpr(env)(expr))


  private def inferExprType[TComp[_] : TypeCheck](factory: ExprFactory[TComp]): TComp[ArExpr] =
    for {
      hole <- implicitly[TypeCheck[TComp]].createHole
      expr <- factory.forExpectedType(hole)
      _ <- implicitly[TypeCheck[TComp]].resolveType(hole)
    } yield expr

  def inferTypeFromConstraints[TComp[_]]
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[typeSystem.TTypeWrapper[A]]
  })
  (constraints: Set[TypeConstraint[holeTS.TType]]): TComp[TType] = ???

}

object ExpressionConverter {

  final case class Env[TContext <: Context with Singleton, TScope]
  (
    descriptor: Descriptor,
    fileSpec: FileSpec,
    referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]],
    scope: TScope,
  )

  trait EnvCreator[TContext <: Context with Singleton] {
    def apply(context: TContext)(descriptor: Descriptor): Env[context.type, context.scopeContext.Scope]

    val fileSpec: FileSpec
  }

  private def createConverter(context: Context)(ts: TypeSystem[context.type]): ExpressionConverter[context.type] {
    val typeSystem: ts.type
  } = {
    val ctx: context.type = context

    new ExpressionConverter[ctx.type] {
      override val context: ctx.type = ctx
      override val typeSystem: ts.type = ts

      override val scopeContext: ScopeContext[context.type] { val typeSystem: ts.type } =
        new ScopeContext[context.type] {
          override val context: ctx.type = ctx
          override val typeSystem: ts.type = ts
        }

      override val signatureContext: SignatureContext[context.type] { val typeSystem: ts.type } =
        new SignatureContext[context.type] {
          override val context: ctx.type = ctx
          override val typeSystem: ts.type = ts
        }
    }
  }

  def convertExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.ArExpr] = {
    val ts = holeTypeSystem(context)
    val converter = createConverter(context)(ts)

    val tcInstance = typeCheckHoleTypeInstance[TComp](context)(ts)

    val tsConverter = holeTypeConverter(context)(context.typeSystem)(ts)

    val env2 = Env(
      descriptor = env.descriptor,
      fileSpec = env.fileSpec,
      referencedModules = env.referencedModules,
      scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
    )

    val tcExpr = converter
      .convertExpr[HoleTypeCheckComp[TComp, ts.TType, ?]](env2)(expr)(tcInstance)
      .forExpectedType(TypeSystem.convertTypeSystem(context)(context.typeSystem)(ts)(tsConverter)(expectedType))

    fillHoles[TComp](context)(converter)(tcExpr)
  }




  def convertTypeExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.TType] = {
    val ts = holeTypeSystem(context)
    val converter = createConverter(context)(ts)

    val tcInstance = typeCheckHoleTypeInstance[TComp](context)(ts)

    val tsConverter = holeTypeConverter(context)(context.typeSystem)(ts)

    val env2 = Env(
      descriptor = env.descriptor,
      fileSpec = env.fileSpec,
      referencedModules = env.referencedModules,
      scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
    )

    val tcExpr = converter.evaluateTypeExprAST[HoleTypeCheckComp[TComp, ts.TType, ?]](env2)(expr)(tcInstance)

    fillHolesType[TComp](context)(converter)(tcExpr)
  }


  def resolveUnitType[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (location: SourceLocation): TComp[context.typeSystem.TType] = {
    val ts = holeTypeSystem(context)
    val converter = createConverter(context)(ts)

    val tcInstance = typeCheckHoleTypeInstance[TComp](context)(ts)

    val tsConverter = holeTypeConverter(context)(context.typeSystem)(ts)

    val env2 = Env(
      descriptor = env.descriptor,
      fileSpec = env.fileSpec,
      referencedModules = env.referencedModules,
      scope = env.scope.convertScopeContext(converter.scopeContext)(tsConverter),
    )

    val tcExpr = converter.resolveUnitType[HoleTypeCheckComp[TComp, ts.TType, ?]](env2)(location)(tcInstance)

    fillHolesType[TComp](context)(converter)(tcExpr)
  }


  private def fillHoles[TComp[_] : Compilation]
  (context: Context)
  (exprConv: ExpressionConverter[context.type])
  (expr: HoleTypeCheckComp[TComp, exprConv.typeSystem.TType, exprConv.typeSystem.ArExpr])
  : TComp[context.typeSystem.ArExpr] = ???


  private def fillHolesType[TComp[_] : Compilation]
  (context: Context)
  (exprConv: ExpressionConverter[context.type])
  (expr: HoleTypeCheckComp[TComp, exprConv.typeSystem.TType, exprConv.typeSystem.TType])
  : TComp[context.typeSystem.TType] = ???

  sealed trait HoleType[+T]
  private final case class HoleTypeType[+T](t: T) extends HoleType[T]
  private final case class HoleTypeHole[+T](id: Int) extends HoleType[T]

  private def holeTypeSystem
  (context: Context)
  : TypeSystem[context.type] {
    type TTypeWrapper[+A] = HoleType[A]
  } = {
    val ctx: context.type = context

    final class HoleTypeSystem extends TypeSystem[context.type] {
      override val context: ctx.type = ctx
      override type TTypeWrapper[+A] = HoleType[A]

      override def wrapType[A](a: A): HoleType[A] =
        HoleTypeType(a)

      override def mapTypeWrapper[A, B](t: HoleType[A])(f: A => B): HoleType[B] =
        t match {
          case HoleTypeType(a) => HoleTypeType(f(a))
          case HoleTypeHole(id) => HoleTypeHole(id)
        }

      override def traverseTypeWrapper[A, B, F[_] : Applicative](t: HoleType[A])(f: A => F[B]): F[HoleType[B]] =
        t match {
          case HoleTypeType(a) => f(a).map(HoleTypeType.apply)
          case HoleTypeHole(id) => (HoleTypeHole(id) : TTypeWrapper[B]).point[F]
        }

      override def wrapExprType(expr: WrapExpr): TType = ???


      override def isSubTypeWrapper[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[SubTypeInfo[TType]]] =
        (a, b) match {
          case (HoleTypeType(aInner), HoleTypeType(bInner)) => isSimpleSubType(aInner, bInner)
          case (_, _) => Option.empty[SubTypeInfo[TType]].point[TComp]
        }

      override def universeOfExpr(expr: WrapExpr): Universe = ???

      override def universeOfType(t: TType): TypeUniverse = ???
    }

    new HoleTypeSystem
  }

  private def holeTypeConverter
  (context: Context)
  (innerTS: TypeSystem[context.type])
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  })
  : TypeSystemConverter[context.type, innerTS.type, holeTS.type, Id] =
    new TypeSystemConverter[context.type, innerTS.type, holeTS.type, Id] {
      override def convertType[A](ts1: innerTS.type)(ts2: holeTS.type)(fromSimpleType: ts2.SimpleType => A)(t: ts1.TTypeWrapper[A]): HoleType[innerTS.TTypeWrapper[A]] =
        HoleTypeType(t)
    }


  sealed trait HoleConstraint[TType]
  private final case class HoleResolved[TType](t: TType) extends HoleConstraint[TType]
  private final case class HoleBounds[TType](bounds: Set[TypeConstraint[TType]]) extends HoleConstraint[TType]

  private sealed trait TypeConstraint[TType]
  private final case class SuperTypeConstraint[TType](superType: TType) extends TypeConstraint[TType]
  private final case class SubTypeConstraint[TType](subType: TType) extends TypeConstraint[TType]

  private trait TypeCheck[TContext <: Context with Singleton, TType, TComp[_]] extends Compilation[TComp] {
    def fromContextComp[A](context: TContext)(comp: context.Comp[A]): TComp[A]
    def createHole: TComp[TType]
    def recordConstraint(info: SubTypeInfo[TType]): TComp[Unit]
    def resolveType(t: TType): TComp[TType]
  }

  final case class TypeCheckState[TType]
  (
    nextHoleId: Int,
    constraints: Map[Int, HoleConstraint[TType]],
  )

  type HoleTypeCheckComp[TComp[_], TType, A] = StateT[TComp, TypeCheckState[TType], A]

  private def typeCheckHoleTypeInstance[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (ts: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[A]
  })
  : TypeCheck[context.type, ts.TType, HoleTypeCheckComp[TComp, ts.TType, ?]] =
    new TypeCheck[context.type, ts.TType, HoleTypeCheckComp[TComp, ts.TType, ?]] {

      override def fromContextComp[A](context2: context.type)(comp: context.Comp[A]): HoleTypeCheckComp[TComp, ts.TType, A] =
        StateT.liftM[TComp, TypeCheckState[ts.TType], A](comp)

      override def createHole: HoleTypeCheckComp[TComp, ts.TType, ts.TType] =
        for {
          state <- StateT.get[TComp, TypeCheckState[ts.TType]]
          _ <- StateT.put[TComp, TypeCheckState[ts.TType]](state.copy(nextHoleId = state.nextHoleId + 1))
        } yield HoleTypeHole(state.nextHoleId)

      private def addConstraint(id: Int, constraint: TypeConstraint[ts.TType]): HoleTypeCheckComp[TComp, ts.TType, Unit] =
        StateT.modify[TComp, TypeCheckState[ts.TType]] { state =>
          state.constraints.getOrElse(id, HoleBounds(Set.empty[TypeConstraint[ts.TType]])) match {
            case HoleResolved(_) => state
            case HoleBounds(bounds) =>
              state.copy(constraints = state.constraints.updated(id, HoleBounds(bounds + constraint)))
          }
        }

      override def recordConstraint(info: SubTypeInfo[ts.TType]): HoleTypeCheckComp[TComp, ts.TType, Unit] =
        (info.subType, info.superType) match {
          case (a @ HoleTypeHole(idA), b @ HoleTypeHole(idB)) =>
            addConstraint(idA, SuperTypeConstraint(b)).flatMap { _ =>
              addConstraint(idB, SubTypeConstraint(a))
            }

          case (HoleTypeHole(idA), b) =>
            addConstraint(idA, SuperTypeConstraint(b))

          case (a, HoleTypeHole(idB)) =>
            addConstraint(idB, SubTypeConstraint(a))

          case (HoleTypeType(_), HoleTypeType(_)) =>
            info.args.traverse_(recordConstraint(_))(this)
        }

      override def resolveType(t: ts.TType): HoleTypeCheckComp[TComp, ts.TType, ts.TType] = ???

      override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): HoleTypeCheckComp[TComp, ts.TType, A] =
        StateT.liftM(Compilation[TComp].diagnostic(value, messages))

      override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): HoleTypeCheckComp[TComp, ts.TType, A] =
        StateT.liftM(Compilation[TComp].forErrors[A](errors, messages))


      override def bind[A, B](fa: HoleTypeCheckComp[TComp, ts.TType, A])(f: A => HoleTypeCheckComp[TComp, ts.TType, B]): HoleTypeCheckComp[TComp, ts.TType, B] =
        fa.flatMap(f)

      override def point[A](a: => A): HoleTypeCheckComp[TComp, ts.TType, A] =
        StateT.liftM(a.point[TComp])
    }

}
