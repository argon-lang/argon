package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.types.{ExpandTypeSystemConverter, TypeSystem, TypeSystemConverter}
import com.mi3software.argon.parser
import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation, WithSource}
import com.mi3software.argon.util.AnyExtensions._

import scala.collection.immutable.Set
import scalaz._
import Scalaz._
import PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler.loaders.source.ExpressionConverter.HoleTypeHole

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
            expectedType <- evaluateTypeExprFactory(env)(convertExpr(env)(valueTypeExpr))
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
            intType <- resolveModuleClass(env)(expr.location)(ModuleDescriptor("Ar.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("Int"))(Vector.empty)
          } yield factoryForExpr(env)(expr.location)(LoadConstantInt(value, intType))
        )

      case parser.StringValueExpr(str) =>
        compFactory(
          for {
            stringType <- resolveModuleClass(env)(expr.location)(ModuleDescriptor("Ar.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("String"))(Vector.empty)
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
      case Vector() => factoryForExpr(env)(stmts.location)(CreateTuple(Vector.empty))
      case Vector(stmt) => convertStmt(env)(stmt)
      case head +: tail =>
        new ExprFactory[TComp] {
          override def forExpectedType(expectedType: typeSystem.TType): TComp[typeSystem.ArExpr] =
            for {
              first <- inferExprType(new TypeInferencer {
                override def inferType[TComp2[_]]
                (inferTS: TypeSystem[context.type])
                (converter: TypeSystemConverter[context.type, typeSystem.type, inferTS.type])
                (expectedType: inferTS.TType)
                (implicit compEv: TypeCheckT[inferTS.TType, TComp2])
                : TComp2[inferTS.ArExpr] = {
                  val otherConv = new ExpressionConverter[context.type] {
                    override val context: ExpressionConverter.this.context.type = ExpressionConverter.this.context
                    override val typeSystem: inferTS.type = inferTS

                    override val scopeContext: ScopeContext[context.type] { val typeSystem: inferTS.type } =
                      new ScopeContext[context.type] {
                        override val context: ExpressionConverter.this.context.type = ExpressionConverter.this.context
                        override val typeSystem: inferTS.type = inferTS
                      }

                    override val signatureContext: SignatureContext[context.type] { val typeSystem: inferTS.type } =
                      new SignatureContext[context.type] {
                        override val context: ExpressionConverter.this.context.type = ExpressionConverter.this.context
                        override val typeSystem: inferTS.type = inferTS
                      }
                  }

                  val env2 = ExpressionConverter.Env(
                    descriptor = env.descriptor,
                    fileSpec = env.fileSpec,
                    referencedModules = env.referencedModules,
                    scope = env.scope.convertScopeContext(otherConv.scopeContext)(TypeSystem.convertTypeSystem(context)(typeSystem)(inferTS)(converter)(_)),
                  )

                  otherConv.convertStmt(env2)(head).forExpectedType(expectedType)
                }
              })

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

  def resolveModuleClass[TComp[_] : TypeCheck]
  (env: Env)
  (location: SourceLocation)
  (moduleDesc: ModuleDescriptor)
  (namespacePath: NamespacePath, name: GlobalName)
  (args: Vector[ArgumentInfo[TComp]])
  : TComp[TType] = for {
    arClass <- Compilation[TComp].requireSome(
      ModuleLookup.lookupValue(context)(env.referencedModules)(moduleDesc)(namespacePath, name)(ModuleLookup.lookupGlobalClass)
    )(CompilationError.ModuleLookupFailedError(moduleDesc, namespacePath, name, CompilationMessageSource.SourceFile(env.fileSpec, location)))
    classSig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClass.signature)

    classFactory = signatureFactory[TComp, ArClass.ResultInfo](env)(
      classSig.convertTypeSystem(signatureContext)(fromArType(_))
    ) { (args, classResult) =>
      for {
        argsAsTypes <- args.traverse(evaluateTypeExpr(env)(_))
      } yield LoadTypeValue(fromSimpleType(ClassType(AbsRef[context.type, ReferencePayloadSpecifier, ArClass](arClass), argsAsTypes, classResult.baseTypes)))
    }

    argsFactory = args.foldLeft(classFactory) { (factory, arg) => factory.forArguments(arg) }
    result <- evaluateTypeExprFactory(env)(argsFactory)
  } yield result

  def resolveBoolClass[TComp[_] : TypeCheck](env: Env)(location: SourceLocation): TComp[TType] =
    resolveModuleClass(env)(location)(ModuleDescriptor("Ar.Core"))(NamespacePath(Vector("Ar")), GlobalName.Normal("Bool"))(Vector.empty)

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
                convSig = sig.convertTypeSystem(signatureContext)(fromArType(_))
              } yield signatureFactory(env)(convSig) { (args, result) => FunctionCall(func, args, result.returnType).upcast[ArExpr].point[TComp] }
            )

          case TraitScopeValue(arTrait) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arTrait.value.signature)
                convSig = sig.convertTypeSystem(signatureContext)(fromArType(_))
              } yield signatureFactory(env)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(_))
                } yield LoadTypeValue(fromSimpleType(TraitType(arTrait, argsAsTypes, result.baseTypes)))
              }
            )

          case ClassScopeValue(arClass) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(arClass.value.signature)
                convSig = sig.convertTypeSystem(signatureContext)(fromArType(_))
              } yield signatureFactory(env)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(_))
                } yield LoadTypeValue(fromSimpleType(ClassType(arClass, argsAsTypes, result.baseTypes)))
              }
            )

          case DataConstructorScopeValue(ctor) =>
            compFactory(
              for {
                sig <- implicitly[TypeCheck[TComp]].fromContextComp(context)(ctor.value.signature)
                convSig = sig.convertTypeSystem(signatureContext)(fromArType(_))
              } yield signatureFactory(env)(convSig) { (args, result) =>
                for {
                  argsAsTypes <- args.traverse(evaluateTypeExpr(env)(_))
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

  def convertExprType[TComp[_] : TypeCheck](env: Env)(location: SourceLocation)(expr: ArExpr)(t: typeSystem.TType): TComp[ArExpr] =
    typeSystem.isSubType[TComp](t, expr.exprType).flatMap {
      case true => implicitly[TypeCheck[TComp]].recordSubTypeConstraint(TypeConstraint(t, expr.exprType)).map(const(expr))
      case false => Compilation[TComp].forErrors(CompilationError.CouldNotConvertType(context)(typeSystem)(t, expr.exprType)(CompilationMessageSource.SourceFile(env.fileSpec, location)))
    }

  def evaluateTypeExprFactory[TComp[_] : TypeCheck](env: Env)(exprFactory: ExprFactory[TComp]): TComp[TType] = ???
  def evaluateTypeExpr[TComp[_] : TypeCheck](env: Env)(expr: ArExpr): TComp[TType] = ???


  private trait TypeInferencer {
    def inferType[TComp[_]]
    (inferTS: TypeSystem[context.type])
    (converter: TypeSystemConverter[context.type, typeSystem.type, inferTS.type])
    (expectedType: inferTS.TType)
    (implicit compEv: TypeCheckT[inferTS.TType, TComp])
    : TComp[inferTS.ArExpr]
  }

  private def inferExprType[TComp[_] : TypeCheck](inferencer: TypeInferencer): TComp[ArExpr] = {
    val inferTS = ExpressionConverter.holeTypeSystem(context)(typeSystem)
    val converter = ExpressionConverter.holeTypeConverter(context)(typeSystem)(inferTS)
    val typeCheckInstance = ExpressionConverter.typeCheckHoleTypeInstance[TComp](context)(typeSystem)(inferTS)

    inferencer.inferType(inferTS)(converter)(HoleTypeHole())(typeCheckInstance)
      .run
      .flatMap { case (constraints, expr) =>
        for {
          fill <- inferTypeFromConstraints[TComp](inferTS)(constraints)
          convExpr = TypeSystem.convertExprTypeSystem(context)(inferTS)(typeSystem)(fillTypeHole(inferTS)(fill))(expr)
          _ <- constraints.toVector.traverse_ { constr =>
            implicitly[TypeCheck[TComp]].recordSubTypeConstraint(
              constr.map(TypeSystem.convertTypeSystem(context)(inferTS)(typeSystem)(fillTypeHole(inferTS)(fill))(_))
            )
          }
        } yield convExpr
      }
  }

  def inferTypeFromConstraints[TComp[_]]
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[typeSystem.TTypeWrapper[A]]
  })
  (constraints: Set[TypeConstraint[holeTS.TType]]): TComp[TType] = ???

  def fillTypeHole
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[typeSystem.TTypeWrapper[A]]
  })
  (fill: TType)
  : TypeSystemConverter[context.type, holeTS.type, typeSystem.type] =
    new TypeSystemConverter[context.type, holeTS.type, typeSystem.type] {
      override def convertType(ts1: holeTS.type)(ts2: typeSystem.type)(t: HoleType[typeSystem.TTypeWrapper[ts2.SimpleType]]): ts2.TTypeWrapper[ts2.SimpleType] =
        t match {
          case ExpressionConverter.HoleTypeType(inner) => inner
          case ExpressionConverter.HoleTypeHole() => fill
        }
    }

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

  def convertExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.ArExpr] = {
    val ctx: context.type = context

    val converter = new ExpressionConverter[ctx.type] {
      override val context: ctx.type = ctx
      override val typeSystem: context.typeSystem.type = context.typeSystem
      override val scopeContext: context.scopeContext.type = context.scopeContext
      override val signatureContext: context.signatureContext.type = context.signatureContext
    }

    converter
      .convertExpr[TComp](env)(expr)(typeCheckArTypeInstance[TComp](context))
      .forExpectedType(expectedType)
  }



  def convertTypeExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.type, context.scopeContext.Scope])
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.TType] = ???


  sealed trait HoleType[T]
  private final case class HoleTypeType[T](t: T) extends HoleType[T]
  private final case class HoleTypeHole[T]() extends HoleType[T]

  private def holeTypeSystem
  (context: Context)
  (innerTS: TypeSystem[context.type])
  : TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  } = {
    val ctx: context.type = context

    final class HoleTypeSystem extends TypeSystem[context.type] {
      override val context: ctx.type = ctx
      override type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]

      val expandConverter: TypeSystemConverter[context.type, innerTS.type, this.type] =
        ExpandTypeSystemConverter[context.type, HoleType](innerTS)(this)(new ExpandTypeSystemConverter.Expander[HoleType] {
          override def apply[A](a: A): HoleType[A] = HoleTypeType(a)
        })

      override def fromArType(arType: context.typeSystem.TType): TType =
        TypeSystem.convertTypeSystem(context)(innerTS)(this)(expandConverter)(innerTS.fromArType(arType))

      override def wrapType[A](a: A): HoleType[innerTS.TTypeWrapper[A]] =
        HoleTypeType(innerTS.wrapType(a))

      override def mapTypeWrapper[A, B](t: HoleType[innerTS.TTypeWrapper[A]])(f: A => B): HoleType[innerTS.TTypeWrapper[B]] =
        t match {
          case HoleTypeType(inner) => HoleTypeType(innerTS.mapTypeWrapper(inner)(f))
          case HoleTypeHole() => HoleTypeHole()
        }

      override def isSubTypeWrapper[TComp[_] : Compilation, T](f: (T, T) => TComp[Boolean])(a: HoleType[innerTS.TTypeWrapper[T]], b: HoleType[innerTS.TTypeWrapper[T]]): TComp[Boolean] = ???
    }

    new HoleTypeSystem
  }

  private def holeTypeConverter
  (context: Context)
  (innerTS: TypeSystem[context.type])
  (holeTS: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  })
  : TypeSystemConverter[context.type, innerTS.type, holeTS.type] =
    new TypeSystemConverter[context.type, innerTS.type, holeTS.type] {
      override def convertType(ts1: innerTS.type)(ts2: holeTS.type)(t: ts1.TTypeWrapper[ts2.SimpleType]): HoleType[innerTS.TTypeWrapper[ts2.SimpleType]] =
        HoleTypeType(t)
    }




  final case class TypeConstraint[TType](superType: TType, subType: TType) {
    def map[B](f: TType => B): TypeConstraint[B] = TypeConstraint(f(superType), f(subType))
  }

  private trait TypeCheck[TContext <: Context with Singleton, TType, TComp[_]] extends Compilation[TComp] {
    def fromContextComp[A](context: TContext)(comp: context.Comp[A]): TComp[A]
    def recordSubTypeConstraint(constraint: TypeConstraint[TType]): TComp[Unit]
  }

  private def typeCheckArTypeInstance[TComp[+_] : Compilation](context: ContextComp[TComp]): TypeCheck[context.type, context.typeSystem.TType, TComp] =
    new TypeCheck[context.type, context.typeSystem.TType, TComp] {

      override def fromContextComp[A](context2: context.type)(comp: context.Comp[A]): TComp[A] =
        comp

      override def recordSubTypeConstraint(constraint: TypeConstraint[context.typeSystem.TType]): TComp[Unit] = ().pure[TComp]

      override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): TComp[A] =
        implicitly[Compilation[TComp]].diagnostic(value, messages)

      override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): TComp[A] =
        implicitly[Compilation[TComp]].forErrors(errors, messages)

      override def bind[A, B](fa: TComp[A])(f: A => TComp[B]): TComp[B] =
        implicitly[Compilation[TComp]].bind(fa)(f)

      override def point[A](a: => A): TComp[A] =
        implicitly[Compilation[TComp]].point(a)
    }



  private def typeCheckHoleTypeInstance[TComp[_]]
  (context: Context)
  (innerTS: TypeSystem[context.type])
  (ts: TypeSystem[context.type] {
    type TTypeWrapper[A] = HoleType[innerTS.TTypeWrapper[A]]
  })
  (implicit innerTypeCheck: TypeCheck[context.type, innerTS.TType, TComp])
  : TypeCheck[context.type, ts.TType, WriterT[TComp, Set[TypeConstraint[ts.TType]], ?]] =
    new TypeCheck[context.type, ts.TType, WriterT[TComp, Set[TypeConstraint[ts.TType]], ?]] {

      override def fromContextComp[A](context2: context.type)(comp: context.Comp[A]): WriterT[TComp, Set[TypeConstraint[ts.TType]], A] =
        WriterT(
          innerTypeCheck.fromContextComp(context)(comp)
            .map { a => (Set.empty[TypeConstraint[ts.TType]], a) }
        )

      override def recordSubTypeConstraint(constraint: TypeConstraint[ts.TType]): WriterT[TComp, Set[TypeConstraint[ts.TType]], Unit] =
        WriterT((Set(constraint), ()).point[TComp])

      override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): WriterT[TComp, Set[TypeConstraint[ts.TType]], A] =
        WriterT(Compilation[TComp].diagnostic((Set.empty[TypeConstraint[ts.TType]], value), messages))

      override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): WriterT[TComp, Set[TypeConstraint[ts.TType]], A] =
        WriterT(Compilation[TComp].forErrors[(Set[TypeConstraint[ts.TType]], A)](errors, messages))

      override def bind[A, B](fa: WriterT[TComp, Set[TypeConstraint[ts.TType]], A])(f: A => WriterT[TComp, Set[TypeConstraint[ts.TType]], B]): WriterT[TComp, Set[TypeConstraint[ts.TType]], B] =
        fa.flatMap(f)

      override def point[A](a: => A): WriterT[TComp, Set[TypeConstraint[ts.TType]], A] =
        WriterT(Compilation[TComp].point((Set.empty[TypeConstraint[ts.TType]], a)))
    }

}
