package dev.argon.compiler

import cats.data.NonEmptySeq
import dev.argon.ast.IdentifierExpr
import dev.argon.expr.{ErasureMode, ExprContext, ExprContextWithHoles}
import dev.argon.util.{Fuel, UniqueIdentifier}
import zio.*

import scala.reflect.TypeTest
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import scala.compiletime.{asMatchable, deferred}

trait Context extends ScopeContext {
  type Env <: Context.Env0
  type Error >: Context.Error0 <: Matchable
  given environmentTag: EnvironmentTag[Env] = deferred
  given errorTypeTest: TypeTest[Any, Error] = deferred

  type Comp[+A] = ZIO[Env, Error, A]

  trait ArgonExprContext extends ExprContext {
    override type Function = ArFuncC & HasContext[Context.this.type]
    override type Record = ArRecordC & HasContext[Context.this.type]
    override type RecordField = RecordFieldC & HasContext[Context.this.type]
    override type Enum = ArEnumC & HasContext[Context.this.type]
    override type EnumVariant = EnumVariantC & HasContext[Context.this.type]
    override type Trait = ArTraitC & HasContext[Context.this.type]
    override type Method = ArMethodC & HasContext[Context.this.type]
    override type Instance = ArInstanceC & HasContext[Context.this.type]

    override def getRecordFieldName(f: RecordFieldC & HasContext[Context.this.type]): IdentifierExpr = f.name
  }

  object DefaultExprContext extends ArgonExprContext {
    override type Hole = Nothing
  }

  object TRExprContext extends ArgonExprContext with ExprContextWithHoles

  sealed abstract class ArgonSignatureContextBase extends SignatureContext {
    override val exprContext: ArgonExprContext
    import exprContext.Expr
    def exprFromDefault(expr: DefaultExprContext.Expr): Expr

    def instanceVarFromDefault(p: DefaultSignatureContext.InstanceParameter): InstanceParameter =
      InstanceParameter(
        name = p.name,
      )

    def parameterFromDefault(p: DefaultSignatureContext.SignatureParameter): SignatureParameter =
      SignatureParameter(
        listType = p.listType,
        erasureMode = p.erasureMode,
        bindings = p.bindings.map { binding => ParameterBinding(
          name = binding.name,
          paramType = exprFromDefault(binding.paramType),
        ) },
        name = p.name,
        paramType = exprFromDefault(p.paramType),
      )

    def signatureFromDefault(sig: DefaultSignatureContext.FunctionSignature): FunctionSignature =
      FunctionSignature(
        parameters = sig.parameters.map(parameterFromDefault),
        returnType = exprFromDefault(sig.returnType),
        ensuresClauses = sig.ensuresClauses.map(exprFromDefault),
      )

    def recordFieldSig(r: Expr.RecordType, field: RecordFieldC & HasContext[Context.this.type]): Comp[FunctionSignature] =
      for
        recordSig <- r.record.signature

        fieldType = signatureFromDefault(recordSig).substituteWithinExprForArgs(
          exprContext.ExpressionOwner.Rec(r.record),
          r.args,
          exprFromDefault(field.fieldType),
        )

      yield FunctionSignature(
        parameters = Seq(),
        returnType = fieldType,
        ensuresClauses = Seq(),
      )

    def recordFieldSig(v: exprContext.EnumVariant, args: Seq[Expr], field: RecordFieldC & HasContext[Context.this.type]): Comp[FunctionSignature] =
      for
        recordSig <- v.signature

        fieldType = signatureFromDefault(recordSig).substituteWithinExprForArgs(
          exprContext.ExpressionOwner.EnumVariant(v),
          args,
          exprFromDefault(field.fieldType),
        )

      yield FunctionSignature(
        parameters = Seq(),
        returnType = fieldType,
        ensuresClauses = Seq(),
      )

    def recordFieldUpdateSig(r: Expr.RecordType, field: RecordFieldC & HasContext[Context.this.type]): Comp[FunctionSignature] =
      for
        recordSig <- r.record.signature

        fieldType = signatureFromDefault(recordSig).substituteWithinExprForArgs(
          exprContext.ExpressionOwner.Rec(r.record),
          r.args,
          exprFromDefault(field.fieldType),
        )

      yield FunctionSignature(
        parameters = Seq(
          SignatureParameter(
            listType = dev.argon.ast.FunctionParameterListType.NormalList,
            erasureMode = field.erasureMode,
            bindings = Seq(),
            name = None,
            paramType = fieldType,
          )
        ),
        returnType = Expr.Tuple(Seq()),
        ensuresClauses = Seq(),
      )
      
    def instanceMethodSig(method: exprContext.Method, instanceType: exprContext.MethodInstanceType): Comp[FunctionSignature] = {
      instanceType match {
        case instanceType: Expr.TraitType =>
          for
            methodSig <- method.signature
            sig = signatureFromDefault(methodSig)
            ownerSig <- instanceType.t.signature
            params = signatureFromDefault(ownerSig).parameters
            args = instanceType.args
            owner = exprContext.ExpressionOwner.Trait(instanceType.t)

            withInstanceTypeParams = params.zip(args).zipWithIndex.foldLeft(sig) {
              case (sig, ((param, arg), i)) =>
                sig.substituteVar(param.asParameterVar(owner, i), arg)
            }
            
          yield withInstanceTypeParams 
          
        case instanceType: Expr.InstanceSingletonType => ???
      }
    }
  }


  object DefaultSignatureContext extends ArgonSignatureContextBase {
    override val exprContext: DefaultExprContext.type = DefaultExprContext

    override def exprFromDefault(expr: DefaultExprContext.Expr): exprContext.Expr =
      expr
  }

  object TRSignatureContext extends ArgonSignatureContextBase {
    override val exprContext: TRExprContext.type = TRExprContext

    override def exprFromDefault(expr: DefaultExprContext.Expr): exprContext.Expr =
      DefaultToTRShifter[Context.this.type](Context.this).shiftExpr(expr)
  }
  
  object Config {
    val evaluatorFuel: Fuel = Fuel(10)
    val prologFuel: Fuel = Fuel(10)
    val smtFuel: Fuel = Fuel(5)
  }

  trait Implementations extends Context.ImplementationExterns {
    enum FunctionImplementation {
      case Expr(e: DefaultExprContext.Expr)
      case Extern(e: ExternFunction)
    }

    enum MethodImplementation {
      case Abstract
      case Expr(e: DefaultExprContext.Expr)
      case Extern(e: ExternMethod)
    }
  }

  val implementations: Implementations
}

object Context {
  type Env0 = ErrorLog
  type Error0 = Nothing

  trait ImplementationExterns {
    type TubeMetadata
    type ExternFunction
    type ExternMethod
  }

  abstract class Of[R <: Env0, E >: Error0 <: Matchable](using EnvironmentTag[R], TypeTest[Any, E]) extends Context {
    override type Env = R
    override type Error = E
  }
}

type HasContext[Ctx <: Context] = {val context: Ctx}

trait UsingContext {
  val context: Context

  export context.Comp
  export context.DefaultSignatureContext.{
    FunctionSignature,
    SignatureParameter,
  }

  type ArTube = ArTubeC & HasContext[context.type]
  type ArModule = ArModuleC & HasContext[context.type]

  type ModuleExport = ModuleExportC[context.type]

  type ArFunc = ArFuncC & HasContext[context.type]
  type ArRecord = ArRecordC & HasContext[context.type]
  type ArEnum = ArEnumC & HasContext[context.type]
  type ArTrait = ArTraitC & HasContext[context.type]
  type ArInstance = ArInstanceC & HasContext[context.type]

  type RecordField = RecordFieldC & HasContext[context.type]
  type EnumVariant = EnumVariantC & HasContext[context.type]
  type ArMethod = ArMethodC & HasContext[context.type]
}


final case class TubeName(parts: NonEmptySeq[String]) derives CanEqual {
  def encode: String = parts.map(part =>
    part
      .replace("%", "%25").nn
      .replace("/", "%2F").nn
      .replace("\\", "%5C").nn
      .replace(".", "%2E").nn
  ).toSeq.mkString(".")
}

object TubeName {
  def apply(head: String, tail: String*): TubeName =
    TubeName(NonEmptySeq(head, tail))

  def decode(s: String): Option[TubeName] =
    if s.isEmpty then
      None
    else
      NonEmptySeq.fromSeq(
        s.split("\\.").nn
          .iterator
          .map(part => URLDecoder.decode(part, StandardCharsets.UTF_8).nn)
          .toSeq
      )
        .filter(_.forall(_.nonEmpty))
        .map(TubeName.apply)
}

final case class ModulePath(parts: Seq[String]) derives CanEqual {
  def encode: String = parts.map(part =>
    part
      .replace("%", "%25").nn
      .replace("/", "%2F").nn
      .replace("\\", "%5C").nn
  ).mkString("/")
}

final case class ModuleName(tubeName: TubeName, path: ModulePath) derives CanEqual {
  def encode(tube: TubeName): String =
    if path.parts.isEmpty then tube.encode
    else tube.encode + "/" + path.encode
}

trait TubeImporter extends UsingContext {
  def getTube(name: TubeName): Comp[ArTube]
}

sealed trait DeclarationBase {
  self: UsingContext =>

  val id: UniqueIdentifier
  def importSpecifier: Comp[ImportSpecifier]

  def signature: Comp[FunctionSignature]
}

abstract class ArTubeC extends UsingContext {
  def name: TubeName

  def modules: Map[ModulePath, ArModule]

  def metadata: context.implementations.TubeMetadata

  def referencedTubes: Set[TubeName]
}

abstract class ArModuleC extends UsingContext {
  def tubeName: TubeName
  def path: ModulePath

  def allExports(reexportingModules: Set[ModuleName]): Comp[Map[IdentifierExpr, Seq[ModuleExport]]]
  def getExports(reexportingModules: Set[ModuleName])(id: IdentifierExpr): Comp[Option[Seq[ModuleExport]]]
}

enum ModuleExportC[Ctx <: Context] {
  case Function(f: ArFuncC & HasContext[Ctx])
  case Record(r: ArRecordC & HasContext[Ctx])
  case Enum(e: ArEnumC & HasContext[Ctx])
  case Trait(t: ArTraitC & HasContext[Ctx])
  case Instance(i: ArInstanceC & HasContext[Ctx])
  case Exported(exp: ModuleExportC[Ctx])
}

abstract class ArFuncC extends UsingContext with DeclarationBase derives CanEqual {
  def isInline: Boolean
  def erasureMode: ErasureMode.Declared
  def isWitness: Boolean
  def effects: context.DefaultExprContext.EffectInfo


  def implementation: Option[Comp[context.implementations.FunctionImplementation]]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: ArFuncC => id == other.id
      case _ => false
    }
}

abstract class ArRecordC extends UsingContext with DeclarationBase derives CanEqual {
  def fields: Comp[Seq[RecordField]]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: ArRecordC => id == other.id
      case _ => false
    }
}

abstract class RecordFieldC extends UsingContext derives CanEqual {
  import context.DefaultExprContext.Expr

  def owningRecord: ArRecord | EnumVariant

  val id: UniqueIdentifier
  val isMutable: Boolean
  def erasureMode: ErasureMode.DeclaredNonToken = ErasureMode.Concrete
  val name: IdentifierExpr
  val fieldType: Expr

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: RecordFieldC => id == other.id
      case _ => false
    }
}

abstract class ArEnumC extends UsingContext with DeclarationBase derives CanEqual {
  def variants: Comp[Seq[EnumVariant]]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: ArEnumC => id == other.id
      case _ => false
    }
}

abstract class EnumVariantC extends UsingContext derives CanEqual {
  def owningEnum: ArEnum

  val id: UniqueIdentifier
  val name: IdentifierExpr
  def signature: Comp[FunctionSignature]
  def fields: Comp[Seq[RecordField]]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: EnumVariantC => id == other.id
      case _ => false
    }
}

abstract class ArTraitC extends UsingContext with DeclarationBase derives CanEqual {
  def methods: Comp[Seq[ArMethod]]

  override def hashCode(): Int = id.hashCode()

  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: ArTraitC => id == other.id
      case _ => false
    }
}

abstract class ArMethodC extends UsingContext derives CanEqual {
  val id: UniqueIdentifier
  val name: IdentifierExpr
  val owner: MethodOwner[context.type]

  def isInline: Boolean
  def erasureMode: ErasureMode.DeclaredNonToken
  def isWitness: Boolean
  def effects: context.DefaultExprContext.EffectInfo

  def slot: MethodSlot

  def instanceParam: Comp[context.DefaultSignatureContext.InstanceParameter]
  def signature: Comp[FunctionSignature]

  def implementation: Option[Comp[context.implementations.MethodImplementation]]
  
  def instanceType: Comp[context.DefaultExprContext.Expr] =
    owner match {
      case MethodOwner.ByTrait(t) =>
        for
          traitOwner = context.DefaultExprContext.ExpressionOwner.Trait(t)
          traitSig <- t.signature
          args = traitSig
            .parameters
            .zipWithIndex
            .map { (param, i) => context.DefaultExprContext.Expr.Variable(param.asParameterVar(traitOwner, i)) }
        yield context.DefaultExprContext.Expr.TraitType(t, args)

      case MethodOwner.ByInstance(i) =>
        for
          instSig <- i.signature
        yield instSig.returnType
    }
  
  
  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: ArMethodC => id == other.id
      case _ => false
    }
}

enum MethodSlot derives CanEqual {
  case Abstract
  case AbstractOverride
  case Virtual
  case Override
  case Final
  case FinalOverride
}

enum MethodOwner[Ctx <: Context] derives CanEqual {
  case ByTrait(t: ArTraitC & HasContext[Ctx])
  case ByInstance(i: ArInstanceC & HasContext[Ctx])
}


abstract class ArInstanceC extends UsingContext with DeclarationBase derives CanEqual {
  def methods: Comp[Seq[ArMethod]]

  override def hashCode(): Int = id.hashCode()

  override def equals(obj: Any): Boolean =
    obj.asMatchable match {
      case other: ArInstanceC => id == other.id
      case _ => false
    }
}


