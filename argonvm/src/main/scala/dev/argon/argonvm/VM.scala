package dev.argon.argonvm

import dev.argon.compiler.module.ModulePath
import dev.argon.compiler.signature.{ErasedSignature, ImportSpecifier}
import dev.argon.compiler.tube.TubeName
import dev.argon.expr.ArgonBuiltin
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{NList, UniqueIdentifier}
import zio.ZIO

trait VM[R, E] {
  type TClass
  type TTrait
  type TMethod
  type TFunction
  
  type TField
  
  enum ParameterOwner {
    case ClassOwner(cls: TClass)
    case TraitOwner(trt: TTrait)
    case MethodOwner(m: TMethod)
    case FunctionOwner(f: TFunction)
  }
  
  def getClassDef(id: TClass): ZIO[R, E, ClassDefinition]
  def getTraitDef(id: TTrait): ZIO[R, E, TraitDefinition]
  def getMethodSlot(id: TMethod): ZIO[R, E, MethodSlot]
  def getMethodImpl(id: TMethod): ZIO[R, E, MethodImplementation]
  def getFunctionDef(id: TFunction): ZIO[R, E, FunctionDefinition]
  def getFieldDef(id: TField): ZIO[R, E, FieldDefinition]

  def getClassImport(id: TClass): ZIO[R, E, ImportSpecifier]
  def getTraitImport(id: TTrait): ZIO[R, E, ImportSpecifier]
  def getMethodInfo(id: TMethod): ZIO[R, E, (MethodOwner, Option[IdentifierExpr], MethodType, ErasedSignature)]
  def getFunctionImport(id: TFunction): ZIO[R, E, ImportSpecifier]
  
  def getClassSignature(cls: TClass): ZIO[R, E, SignatureNoResult]
  def getTraitSignature(trt: TTrait): ZIO[R, E, SignatureNoResult]
  def getMethodSignature(m: TMethod): ZIO[R, E, SignatureWithResult]
  def getFieldSignature(f: TFunction): ZIO[R, E, SignatureWithResult]
  
  
  enum MethodOwner {
    case ClassOwner(cls: TClass)
    case TraitOwner(trt: TTrait)
  }
  
  enum MethodType {
    case Instance, Static
  }
  
  final case class SignatureNoResult(args: Seq[TypeLiteral])
  final case class SignatureWithResult(args: Seq[TypeLiteral], result: TypeLiteral)

  sealed trait TypeLiteral
  object TypeLiteral {
    case object Erased extends TypeLiteral

    final case class ClassType(id: TClass, args: Seq[VMType]) extends TypeLiteral
    final case class TraitType(id: TTrait, args: Seq[VMType]) extends TypeLiteral
    final case class Tuple(elements: Seq[VMType]) extends TypeLiteral
    final case class Function(arg: VMType, res: VMType) extends TypeLiteral
    final case class Builtin[N <: Int](builtin: ArgonBuiltin.Type[N], args: NList[N, VMType]) extends TypeLiteral


    final case class Parameter(ownerId: ParameterOwner, index: Int) extends TypeLiteral
    final case class TupleElement(t: VMType, index: Int) extends TypeLiteral
  }

  // Disjunctive normal form
  final case class VMType(t: Seq[Seq[TypeLiteral]])

  final class ClassDefinition(
    val id: TClass,
    val isAbstract: Boolean,
    val parameters: Seq[VMType],
    val baseClass: Option[TypeLiteral.ClassType],
    val baseTraits: Seq[TypeLiteral.TraitType],
    val methodSlots: Seq[MethodSlot],
    val methodImplementations: Seq[MethodImplementation],
    val fields: Seq[FieldDefinition],
    val vtableDiff: Map[TMethod, TMethod],
  )
  
  final class FieldDefinition(
    val name: IdentifierExpr,
    val fieldType: TypeLiteral,
  )

  final class TraitDefinition(
    val id: TTrait,
    val parameters: Seq[VMType],
    val baseTraits: Seq[TypeLiteral.TraitType],
    val methodSlots: Seq[MethodSlot],
  )

  final class MethodSlot(
    val id: TMethod,
    val parameters: Seq[VMType],
    val returnType: VMType,
  )

  final class MethodImplementation(
    val primarySlotId: TMethod,
    val overrideSlots: Seq[TMethod],
    val instanceType: VMType,
    val parameters: Seq[VMType],
    val returnType: VMType,

    // Registers:
    // 0...N - owner args
    // 0 - instance
    // 1...N - arguments
    val body: CodeBlock,
  )

  final class FunctionDefinition(
    val id: TFunction,
    val parameters: Seq[VMType],
    val returnType: VMType,

    // Registers:
    // 0...N - arguments
    val body: CodeBlock,
  )

  final class CodeBlock(
    val registers: Seq[VMType],
    val instructions: Instruction,
  )

  enum Instruction {
    case Move(to: BigInt, from: BigInt)
    case Return(r: BigInt)

    case LoadConstantInt(r: BigInt, value: BigInt)
    case LoadConstantString(r: BigInt, value: String)
    case LoadConstantBool(r: BigInt, value: Boolean)
    
    case LoadTuple(r: BigInt, values: Seq[BigInt])
    case GetTuple(r: BigInt, t: BigInt)
    
    case StoreField(o: BigInt, field: TField, value: BigInt)
    case LoadField(r: BigInt, o: BigInt, field: TField)
    case NewObject(r: BigInt, cls: TClass, classArgs: Seq[TypeLiteral])
    
    
    case Builtin0(r: BigInt, builtin: ArgonBuiltin.SimpleValue)
    case Builtin1(r: BigInt, builtin: ArgonBuiltin.SimpleValue, a: BigInt)
    case Builtin2(r: BigInt, builtin: ArgonBuiltin.SimpleValue, a: BigInt, b: BigInt)

    case Call(r: BigInt, invocation: Invocation)
    case ReturnCall(invocation: Invocation)
    
    case ClassType(r: BigInt, id: TClass, args: Seq[BigInt])
    case TraitType(r: BigInt, id: TTrait, args: Seq[BigInt])
    case FunctionType(r: BigInt, arg: BigInt, result: BigInt)
  }

  enum Invocation {
    case Function(f: TFunction, args: Seq[BigInt])
    case InstanceMethod(m: TMethod, instance: BigInt, args: Seq[BigInt])
    case StaticMethod(m: TMethod, t: BigInt, args: Seq[BigInt])
    case FunctionObject(f: BigInt, arg: BigInt)
  }

  trait Tube {
    val name: TubeName

    def modules: Set[ModulePath]
    def getModule(path: ModulePath): ZIO[R, E, Module]
  }

  final class Module(
    val classes: Set[TClass],
    val traits: Set[TTrait],
    val functions: Set[TFunction],
  )
}
