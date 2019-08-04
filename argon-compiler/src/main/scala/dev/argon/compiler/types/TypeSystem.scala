package dev.argon.compiler.types

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.AnyExtensions._
import cats._
import cats.implicits._
import cats.data._
import Compilation.Operators._
import dev.argon.compiler.types.TypeSystem.PrimitiveOperation

import scala.collection.immutable.Vector

trait TypeSystem[TContext <: Context with Singleton] {

  val context: TContext

  type TTypeWrapper[+_]

  type WrapExpr = TTypeWrapper[ArExpr]
  type TType = WrapExpr
  
  type WrapRef[T[_ <: Context with Singleton, _[_, _]]] = AbsRef[context.type, T]
  type TSubTypeInfo = SubTypeInfo[TType]


  final def fromSimpleType(simpleType: ArExpr): TType = wrapType(simpleType)

  def wrapType[A](a: A): TTypeWrapper[A]
  def unwrapType[A](t: TTypeWrapper[A]): Option[A]
  def mapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => B): TTypeWrapper[B]
  def traverseTypeWrapper[A, B, F[_] : Applicative](t: TTypeWrapper[A])(f: A => F[B]): F[TTypeWrapper[B]]

  def wrapExprType(expr: WrapExpr): TType

  def isSubTypeWrapper[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[SubTypeInfo[TType]]]

  def universeOfExpr(expr: WrapExpr): Universe

  final def largestUniverse[U <: Universe](default: U)(universes: Vector[U]): U =
    universes.foldLeft(default)(Universe.union)

  final def largestUniverse1[U <: Universe](universes: NonEmptyList[U]): U =
    universes.reduceLeft[U](Universe.union)

  final def isSubType[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[TSubTypeInfo]] =
    isSubTypeWrapper(a, b)


  sealed trait Variable {
    val descriptor: VariableLikeDescriptor
    val name: VariableName
    val mutability: Mutability
    val varType: TType
  }

  final case class LocalVariable(descriptor: VariableDescriptor, name: VariableName, mutability: Mutability, varType: TType) extends Variable
  final case class ParameterVariable(descriptor: ParameterDescriptor, name: VariableName, mutability: Mutability, varType: TType) extends Variable
  final case class FieldVariable(descriptor: FieldDescriptor, ownerClass: AbsRef[context.type, ArClass], name: VariableName.Normal, mutability: Mutability, varType: TType) extends Variable

  final case class ParameterElement(paramVar: ParameterVariable, name: VariableName, elemType: TType, index: Int)
  final case class Parameter(paramVar: ParameterVariable, elements: Vector[ParameterElement]) {
    def paramType: TType = paramVar.varType
  }


  sealed trait ArExpr {
    val exprType: TType
    val universe: Universe
  }

  final case class ClassConstructorCall(classType: ClassType, classCtor: AbsRef[context.type, ClassConstructor], args: Vector[ArExpr]) extends ArExpr {
    override val exprType: TType = fromSimpleType(classType)
    override val universe: Universe = ValueUniverse
  }
  final case class DataConstructorCall(dataCtorInstanceType: DataConstructorType, args: Vector[ArExpr]) extends ArExpr {
    override val exprType: TType = fromSimpleType(dataCtorInstanceType)
    override val universe: Universe = ValueUniverse
  }
  final case class FunctionCall(function: AbsRef[context.type, ArFunc], args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: Universe = universeOfExpr(returnType).prevUnsafe
  }
  final case class FunctionObjectCall(function: ArExpr, arg: ArExpr, returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: Universe = universeOfExpr(returnType).prevUnsafe
  }
  final case class IfElse(condition: ArExpr, ifBody: ArExpr, elseBody: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(UnionType(ifBody.exprType, elseBody.exprType))
    override lazy val universe: Universe = Universe.union(ifBody.universe, elseBody.universe)
  }
  final case class LetBinding(variable: LocalVariable, value: ArExpr, next: ArExpr) extends ArExpr {
    override lazy val exprType: TType = next.exprType
    override lazy val universe: Universe = next.universe
  }
  final case class LoadConstantBool(value: Boolean, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadConstantInt(value: BigInt, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadConstantString(value: String, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadLambda(argVariable: LocalVariable, body: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(FunctionType(argVariable.varType, body.exprType))
    override lazy val universe: Universe = Universe.union(
      universeOfExpr(argVariable.varType).prevUnsafe,
      universeOfExpr(body.exprType).prevUnsafe
    )
  }
  final case class TupleElement(value: WrapExpr) {
    lazy val elementTypeElement: TupleElement = TupleElement(wrapExprType(value))
  }
  final case class LoadTuple(values: NonEmptyList[TupleElement]) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(LoadTuple(values.map { _.elementTypeElement }))
    override lazy val universe: Universe = largestUniverse1(values.map { elem => universeOfExpr(elem.value) })
  }
  final case class LoadTupleElement(tupleValue: ArExpr, elemType: TType, index: Int) extends ArExpr {
    override val exprType: TType = elemType
    override val universe: Universe = universeOfExpr(elemType).prevUnsafe
  }
  final case class LoadUnit(exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadVariable(variable: Variable) extends ArExpr {
    override lazy val exprType: TType = variable.varType
    override lazy val universe: Universe = universeOfExpr(variable.varType).prevUnsafe
  }
  final case class MethodCall(method: AbsRef[context.type, ArMethod], instance: ArExpr, args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: Universe = universeOfExpr(returnType).prevUnsafe
  }
  final case class PrimitiveOp(operation: PrimitiveOperation, left: ArExpr, right: ArExpr, exprType: TType) extends ArExpr {
    override val universe: Universe = universeOfExpr(exprType).prevUnsafe
  }
  final case class Sequence(first: ArExpr, second: ArExpr) extends ArExpr {
    override lazy val exprType: TType = second.exprType
    override lazy val universe: Universe = second.universe
  }
  final case class StoreVariable(variable: Variable, value: ArExpr, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }

  sealed trait TypeIsTypeOfTypeExpr extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(TypeOfType(fromSimpleType(this), TypeUniverse(universe)))
  }

  sealed trait TypeWithMethods extends TypeIsTypeOfTypeExpr

  sealed trait TypeArgument
  object TypeArgument {
    final case class Expr(expr: WrapExpr) extends TypeArgument
    case object Wildcard extends TypeArgument
  }

  final case class TypeOfType(inner: TType, universe: TypeUniverse) extends TypeIsTypeOfTypeExpr
  final case class TypeN(universe: TypeUniverse, subtypeConstraint: Option[TType], supertypeConstraint: Option[TType]) extends TypeIsTypeOfTypeExpr

  final case class TraitType(arTrait: WrapRef[ArTrait], args: Vector[TypeArgument], baseTypes: BaseTypeInfoTrait) extends TypeWithMethods {
    override val universe: TypeUniverse = TypeUniverse(ValueUniverse)
  }
  final case class ClassType(arClass: WrapRef[ArClass], args: Vector[TypeArgument], baseTypes: BaseTypeInfoClass) extends TypeWithMethods {
    override val universe: TypeUniverse = TypeUniverse(ValueUniverse)
  }
  final case class DataConstructorType(ctor: WrapRef[DataConstructor], args: Vector[TypeArgument], instanceType: TraitType) extends TypeWithMethods {
    override val universe: TypeUniverse = TypeUniverse(ValueUniverse)
  }

  final case class FunctionType(argumentType: TType, resultType: TType) extends TypeIsTypeOfTypeExpr {
    override lazy val universe: Universe = Universe.union(
      universeOfExpr(argumentType),
      universeOfExpr(resultType)
    )
  }
  final case class UnionType(first: TType, second: TType) extends TypeIsTypeOfTypeExpr {
    override lazy val universe: Universe = Universe.union(
      universeOfExpr(first),
      universeOfExpr(second)
    )
  }
  final case class IntersectionType(first: TType, second: TType) extends TypeIsTypeOfTypeExpr {
    override lazy val universe: Universe = Universe.union(
      universeOfExpr(first),
      universeOfExpr(second)
    )
  }


  final case class BaseTypeInfoTrait(baseTraits: Vector[TraitType])
  final case class BaseTypeInfoClass(baseClass: Option[ClassType], baseTraits: Vector[TraitType])


  sealed trait ClassConstructorStatement
  final case class ClassConstructorStatementExpr(expr: ArExpr) extends ClassConstructorStatement
  final case class InitializeFieldStatement(field: FieldVariable, value: ArExpr) extends ClassConstructorStatement

  final case class ClassConstructorBody
  (
    initStatements: Vector[ClassConstructorStatement],
    baseConstructorCall: Option[ClassConstructorCall],
    endExpr: ArExpr,
  )





  protected final def isSimpleSubType[F[_] : Compilation](a: ArExpr, b: ArExpr): F[Option[TSubTypeInfo]] = {

    val notSubType = Option.empty[TSubTypeInfo].pure[F]

    def compareTypeArg(a: TypeArgument, b: TypeArgument): F[Option[Vector[TSubTypeInfo]]] = {
      def fromIsSame(b: Boolean): F[Option[Vector[TSubTypeInfo]]] =
        if(b)
          Some(Vector.empty).upcast[Option[Vector[TSubTypeInfo]]].pure[F]
        else
          Option.empty[Vector[TSubTypeInfo]].pure[F]

      (a, b) match {
        case (TypeArgument.Wildcard, _) => fromIsSame(true)
        case (_, TypeArgument.Wildcard) => fromIsSame(false)

        case (TypeArgument.Expr(LoadConstantString(a, _)), TypeArgument.Expr(LoadConstantString(b, _))) =>
          fromIsSame(a === b)

        case (TypeArgument.Expr(LoadConstantInt(a, _)), TypeArgument.Expr(LoadConstantInt(b, _))) =>
          fromIsSame(a === b)

        case (TypeArgument.Expr(LoadConstantBool(a, _)), TypeArgument.Expr(LoadConstantBool(b, _))) =>
          fromIsSame(a === b)

        case (TypeArgument.Expr(LoadUnit(_)), TypeArgument.Expr(LoadUnit(_))) => fromIsSame(true)

        case (TypeArgument.Expr(a), TypeArgument.Expr(b)) =>
          (
            for {
              proof1 <- OptionT(isSubType(a, b))
              proof2 <- OptionT(isSubType(b, a))
            } yield Vector(SubTypeInfo(a, b, Vector(proof1, proof2)))
          ).value
      }
    }

    def compareArguments(aType: TType, bType: TType)(a: Vector[TypeArgument])(b: Vector[TypeArgument]): F[Option[TSubTypeInfo]] =
      if(a.size === b.size)
        a.zip(b)
          .traverse { case (aArg, bArg) => OptionT(compareTypeArg(aArg, bArg)) }
          .map { args => SubTypeInfo(aType, bType, args.flatten) }
          .value
      else
        notSubType

    def isSubTrait(a: TraitType)(b: TraitType): F[Option[TSubTypeInfo]] =
      if(a.arTrait.value.descriptor === b.arTrait.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        b.baseTypes.baseTraits.collectFirstSomeM(isSubTrait(a))

    def isSubClass(a: ClassType)(b: ClassType): F[Option[TSubTypeInfo]] =
      if(a.arClass.value.descriptor === b.arClass.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        b.baseTypes.baseClass.collectFirstSomeM(isSubClass(a))

    def classImplementsTrait(a: TraitType)(b: ClassType): F[Option[TSubTypeInfo]] =
      Vector(
        () => b.baseTypes.baseTraits.collectFirstSomeM(isSubTrait(a)),
        () => b.baseTypes.baseClass.collectFirstSomeM(classImplementsTrait(a)),
      ).collectFirstSomeM(_())

    def isSameDataCtor(a: DataConstructorType)(b: DataConstructorType): F[Option[TSubTypeInfo]] =
      if(a.ctor.value.descriptor === b.ctor.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        Option.empty[TSubTypeInfo].pure[F]

    Vector(
      () => a match {
        case a: UnionType =>
          isSubType[F](a.first, fromSimpleType(b)).flatMap {
            case Some(left) =>
              isSubType[F](a.second, fromSimpleType(b)).map { _.map { right =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(left, right))
              } }

            case None => notSubType
          }
        case _ => notSubType
      },
      () => b match {
        case b: IntersectionType =>
          isSubType[F](fromSimpleType(a), b.first).flatMap {
            case Some(left) =>
              isSubType[F](fromSimpleType(a), b.second).map { _.map { right =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(left, right))
              } }

            case None => notSubType
          }

        case _ => notSubType
      },
      () => b match {
        case b: UnionType =>
          Vector(
            () => isSubType[F](fromSimpleType(a), b.first),
            () => isSubType[F](fromSimpleType(a), b.second),
          )
            .collectFirstSomeM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }

        case _ => notSubType
      },
      () => a match {
        case a: IntersectionType =>
          Vector(
            () => isSubType[F](a.first, fromSimpleType(b)),
            () => isSubType[F](a.second, fromSimpleType(b)),
          )
            .collectFirstSomeM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }
        case _ => notSubType
      },
      () => a match {
        case LoadTuple(NonEmptyList(TupleElement(a), Nil)) =>
          isSubType(a, fromSimpleType(b))
        case _ => notSubType
      },
      () => b match {
        case LoadTuple(NonEmptyList(TupleElement(b), Nil)) =>
          isSubType(fromSimpleType(a), b)
        case _ => notSubType
      },
      () => (a, b) match {

        case (aTrait: TraitType, bTrait: TraitType) => isSubTrait(aTrait)(bTrait)
        case (aClass: ClassType, bClass: ClassType) => isSubClass(aClass)(bClass)
        case (aTrait: TraitType, bClass: ClassType) => classImplementsTrait(aTrait)(bClass)
        case (_: ClassType, _: TraitType) => notSubType

        case (aDataCtor: DataConstructorType, bDataCtor: DataConstructorType) =>
          isSameDataCtor(aDataCtor)(bDataCtor)

        case (aTrait: TraitType, bDataCtor: DataConstructorType) =>
          isSubTrait(aTrait)(bDataCtor.instanceType)


        case (aTuple: LoadTuple, bTuple: LoadTuple) =>
          if(aTuple.values.size =!= bTuple.values.size)
            notSubType
          else
            aTuple.values.toList.toVector.zip(bTuple.values.toList.toVector)
              .traverse { case (aElem, bElem) =>
                OptionT(isSubType(aElem.value, bElem.value))
              }
              .map(SubTypeInfo(fromSimpleType(a), fromSimpleType(b), _))
              .value

        case (FunctionType(argA, resA), FunctionType(argB, resB)) =>
          (
            for {
              argCheck <- OptionT(isSubType(argB, argA))
              resCheck <- OptionT(isSubType(resA, resB))
            } yield SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(argCheck, resCheck))
          ).value

        case (TypeOfType(innerA, _), TypeOfType(innerB, _)) =>
          (
            for {
              c1 <- OptionT(isSubType(innerA, innerB))
              c2 <- OptionT(isSubType(innerB, innerA))
            } yield SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(c1, c2))
          ).value

        case (TypeN(uA, subtypeConstraint, supertypeConstraint), TypeOfType(innerB, uB)) if uA.toBigInt >= uB.toBigInt =>
          (
            subtypeConstraint.map { sub => OptionT(isSubType(innerB, sub)) }.toList ++
              supertypeConstraint.map { sup => OptionT(isSubType(sup, innerB)) }.toList
          ).toVector.sequence.value
            .map { _.map {
              SubTypeInfo(fromSimpleType(a), fromSimpleType(b), _)
            } }

        case (TypeN(uA, _, _), b @ LoadTuple(_)) if uA.toBigInt >= b.universe.toBigInt =>
          SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector.empty).pure[Option].pure[F]


        case (TypeN(uA, subA, supA), TypeN(uB, subB, supB)) if uA.toBigInt >= uB.toBigInt =>
          (
            ((subA, subB) match {
              case (Some(subA), Some(subB)) => Vector(OptionT(isSubType(subB, subA)))
              case _ => Vector()
            }) ++
              ((supA, supB) match {
                case (Some(supA), Some(supB)) => Vector(OptionT(isSubType(supA, supB)))
                case _ => Vector()
              })
          ).sequence.value
            .map { _.map {
              SubTypeInfo(fromSimpleType(a), fromSimpleType(b), _)
            } }

        case (LoadVariable(varA), LoadVariable(varB)) if varA.descriptor === varB.descriptor =>
          SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector.empty).pure[Option].pure[F]

        case (_, _) => notSubType
      },
      () => unwrapType(b.exprType) match {
        case Some(TypeN(_, Some(subtypeConstraint), _)) =>
          isSubType(fromSimpleType(a), subtypeConstraint)

        case _ => notSubType
      },
      () => unwrapType(a.exprType) match {
        case Some(TypeN(_, _, Some(supertypeConstraint))) =>
          isSubType(supertypeConstraint, fromSimpleType(b))

        case _ => notSubType
      },
    ).collectFirstSomeM(_())
  }

}

object TypeSystem {

  sealed trait PrimitiveOperation
  object PrimitiveOperation {
    case object AddInt extends PrimitiveOperation
    case object SubInt extends PrimitiveOperation
    case object MulInt extends PrimitiveOperation
    case object IntEqual extends PrimitiveOperation
  }

  def convertTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (t1: ts.TType)
  : F[otherTS.TType] =
    ts.traverseTypeWrapper(t1)(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
      .flatMap(converter.convertType(ts)(otherTS)(identity)(_))

  def convertTypeArg[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (t1: ts.TypeArgument)
  : F[otherTS.TypeArgument] =
    t1 match {
      case ts.TypeArgument.Expr(expr) => convertTypeSystem(context)(ts)(otherTS)(converter)(expr).map(otherTS.TypeArgument.Expr)
      case ts.TypeArgument.Wildcard => otherTS.TypeArgument.Wildcard.upcast[otherTS.TypeArgument].pure[F]
    }

  def convertTraitType[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (traitType: ts.TraitType)
  : F[otherTS.TraitType] = for {
    newArgs <- traitType.args.traverse(convertTypeArg(context)(ts)(otherTS)(converter)(_))
    newBaseTraits <- traitType.baseTypes.baseTraits.traverse(convertTraitType(context)(ts)(otherTS)(converter)(_))
  } yield otherTS.TraitType(
    traitType.arTrait,
    newArgs,
    otherTS.BaseTypeInfoTrait(newBaseTraits)
  )

  def convertClassType[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (classType: ts.ClassType)
  : F[otherTS.ClassType] = for {
    newArgs <- classType.args.traverse(convertTypeArg(context)(ts)(otherTS)(converter)(_))
    newBaseClass <- classType.baseTypes.baseClass.traverse(convertClassType(context)(ts)(otherTS)(converter)(_))
    newBaseTraits <- classType.baseTypes.baseTraits.traverse(convertTraitType(context)(ts)(otherTS)(converter)(_))
  } yield otherTS.ClassType(
    classType.arClass,
    newArgs,
    otherTS.BaseTypeInfoClass(
      newBaseClass,
      newBaseTraits
    )
  )


  def convertDataConstructorType[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (dataCtorType: ts.DataConstructorType)
  : F[otherTS.DataConstructorType] = for {
    newArgs <- dataCtorType.args.traverse(convertTypeArg(context)(ts)(otherTS)(converter)(_))
    newInstanceType <- convertTraitType(context)(ts)(otherTS)(converter)(dataCtorType.instanceType)
  } yield otherTS.DataConstructorType(
    dataCtorType.ctor,
    newArgs,
    newInstanceType
  )

  final def convertLocalVariableTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (v: ts.LocalVariable)
  : F[otherTS.LocalVariable] =
    for {
      newType <- TypeSystem.convertTypeSystem(context)(ts)(otherTS)(converter)(v.varType)
    } yield otherTS.LocalVariable(v.descriptor, v.name, v.mutability, newType)

  final def convertParamVariableTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (v: ts.ParameterVariable)
  : F[otherTS.ParameterVariable] =
    for {
      newType <- TypeSystem.convertTypeSystem(context)(ts)(otherTS)(converter)(v.varType)
    } yield otherTS.ParameterVariable(v.descriptor, v.name, v.mutability, newType)

  final def convertVariableTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (v: ts.Variable)
  : F[otherTS.Variable] =
    v match {
      case v @ ts.LocalVariable(_, _, _, _) =>
        convertLocalVariableTypeSystem(context)(ts)(otherTS)(converter)(v).map(identity)

      case v @ ts.ParameterVariable(_, _, _, _) =>
        convertParamVariableTypeSystem(context)(ts)(otherTS)(converter)(v).map(identity)

      case ts.FieldVariable(descriptor, ownerClass, name, mutability, varType) =>
        for {
          newType <- TypeSystem.convertTypeSystem(context)(ts)(otherTS)(converter)(varType)
        } yield otherTS.FieldVariable(descriptor, ownerClass, name, mutability, newType)

    }

  final def convertParameterElementTypeSystem[F[_]: Monad, Desc <: VariableLikeDescriptor]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (p: ts.ParameterElement)
  : F[otherTS.ParameterElement] =
    for {
      newParamVar <- convertParamVariableTypeSystem(context)(ts)(otherTS)(converter)(p.paramVar)
      newElemType <- convertTypeSystem(context)(ts)(otherTS)(converter)(p.elemType)
    } yield otherTS.ParameterElement(newParamVar, p.name, newElemType, p.index)

  final def convertParameterTypeSystem[F[_]: Monad, Desc <: VariableLikeDescriptor]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (p: ts.Parameter)
  : F[otherTS.Parameter] =
    for {
      newParamVar <- convertParamVariableTypeSystem(context)(ts)(otherTS)(converter)(p.paramVar)
      newElems <- p.elements.traverse(convertParameterElementTypeSystem(context)(ts)(otherTS)(converter)(_))
    } yield otherTS.Parameter(newParamVar, newElems)

  def convertExprTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (expr: ts.ArExpr)
  : F[otherTS.ArExpr] = expr match {


    case ts.ClassConstructorCall(classType, classCtor, args) =>
      for {
        newClassType <- convertClassType(context)(ts)(otherTS)(converter)(classType)
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
      } yield otherTS.ClassConstructorCall(newClassType, classCtor, newArgs)

    case ts.DataConstructorCall(dataCtor, args) =>
      for {
        newType <- convertDataConstructorType(context)(ts)(otherTS)(converter)(dataCtor)
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
      } yield otherTS.DataConstructorCall(newType, newArgs)

    case ts.FunctionCall(function, args, returnType) =>
      for {
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
        newReturnType <- convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      } yield otherTS.FunctionCall(function, newArgs, newReturnType)

    case ts.FunctionObjectCall(function, args, returnType) =>
      for {
        newFunction <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(function)
        newArgs <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(args)
        newReturnType <- convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      } yield otherTS.FunctionObjectCall(newFunction, newArgs, newReturnType)

    case ts.IfElse(condition, ifBody, elseBody) =>
      for {
        newCondition <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(condition)
        newIfBody <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(ifBody)
        newElseBody <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(elseBody)
      } yield otherTS.IfElse(newCondition, newIfBody, newElseBody)

    case ts.LetBinding(variable, value, next) =>
      for {
        newVar <- convertLocalVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
        newValue <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(value)
        newNext <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(next)
      } yield otherTS.LetBinding(newVar, newValue, newNext)

    case ts.LoadConstantBool(value, exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadConstantBool(value, newType)

    case ts.LoadConstantInt(value, exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadConstantInt(value, newType)

    case ts.LoadConstantString(value, exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadConstantString(value, newType)

    case ts.LoadLambda(variable, body) =>
      for {
        newVar <- convertLocalVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
        newBody <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(body)
      } yield otherTS.LoadLambda(newVar, newBody)

    case expr: ts.LoadTuple =>
      expr.values
        .traverse { case ts.TupleElement(elementType) =>
          convertTypeSystem(context)(ts)(otherTS)(converter)(elementType).map(otherTS.TupleElement(_))
        }
        .map(otherTS.LoadTuple(_))

    case ts.LoadTupleElement(tupleValue, elemType, index) =>
      for {
        newValue <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(tupleValue)
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(elemType)
      } yield otherTS.LoadTupleElement(newValue, newType, index)

    case ts.LoadUnit(exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadUnit(newType)

    case ts.LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
      } yield otherTS.LoadVariable(newVar)

    case ts.MethodCall(method, instance, args, returnType) =>
      for {
        newInstance <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(instance)
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
        newReturnType <- convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      } yield otherTS.MethodCall(method, newInstance, newArgs, newReturnType)

    case ts.PrimitiveOp(operation, left, right, exprType) =>
      for {
        newLeft <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(left)
        newRight <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(right)
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.PrimitiveOp(operation, newLeft, newRight, newType)

    case ts.Sequence(first, second) =>
      for {
        newFirst <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(first)
        newSecond <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(second)
      } yield otherTS.Sequence(newFirst, newSecond)

    case ts.StoreVariable(variable, value, unitType) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
        newValue <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(value)
        newUnitType <- convertTypeSystem(context)(ts)(otherTS)(converter)(unitType)
      } yield otherTS.StoreVariable(newVar, newValue, newUnitType)

    case t1: ts.TraitType => convertTraitType(context)(ts)(otherTS)(converter)(t1).map(identity)
    case t1: ts.ClassType => convertClassType(context)(ts)(otherTS)(converter)(t1).map(identity)
    case t1: ts.DataConstructorType => convertDataConstructorType(context)(ts)(otherTS)(converter)(t1).map(identity)

    case ts.TypeOfType(inner, universe) =>
      for {
        newInner <- convertTypeSystem(context)(ts)(otherTS)(converter)(inner)
      } yield (otherTS.TypeOfType(newInner, universe) : otherTS.ArExpr)

    case ts.TypeN(universe, subtypeConstraint, supertypeConstraint) =>
      for {
        newSub <- subtypeConstraint.traverse(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
        newSup <- supertypeConstraint.traverse(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
      } yield (otherTS.TypeN(universe, newSub, newSup) : otherTS.ArExpr)

    case ts.LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
      } yield otherTS.LoadVariable(newVar)

    case ts.FunctionType(argumentType, resultType) =>
      for {
        newArgType <- convertTypeSystem(context)(ts)(otherTS)(converter)(argumentType)
        newResultType <- convertTypeSystem(context)(ts)(otherTS)(converter)(resultType)
      } yield otherTS.FunctionType(newArgType, newResultType)

    case ts.UnionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(context)(ts)(otherTS)(converter)(first)
        newSecond <- convertTypeSystem(context)(ts)(otherTS)(converter)(second)
      } yield otherTS.UnionType(newFirst, newSecond)

    case ts.IntersectionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(context)(ts)(otherTS)(converter)(first)
        newSecond <- convertTypeSystem(context)(ts)(otherTS)(converter)(second)
      } yield otherTS.IntersectionType(newFirst, newSecond)
  }



}
