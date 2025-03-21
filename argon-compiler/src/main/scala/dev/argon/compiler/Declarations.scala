package dev.argon.compiler

import cats.data.NonEmptySeq
import dev.argon.ast.IdentifierExpr
import dev.argon.expr.{ExprContext, ExprContextWithHoles, Substitution}
import dev.argon.util.{FilePosition, Fuel, Location, UniqueIdentifier}
import zio.*

import scala.reflect.TypeTest
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import scala.compiletime.deferred

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
    
    override def getRecordFieldName(f: RecordFieldC & HasContext[Context.this.type]): IdentifierExpr = f.name
  }

  object DefaultExprContext extends ArgonExprContext {
    override type Hole = Nothing
  }

  object TRExprContext extends ArgonExprContext with ExprContextWithHoles

  sealed abstract class ArgonSignatureContextBase extends SignatureContext {
    override val exprContext: ArgonExprContext
    def exprFromDefault(expr: DefaultExprContext.Expr): exprContext.Expr

    def parameterFromDefault(p: DefaultSignatureContext.SignatureParameter): SignatureParameter =
      SignatureParameter(
        listType = p.listType,
        isErased = p.isErased,
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

    def recordFieldSig(r: exprContext.Expr.RecordType, field: RecordFieldC & HasContext[Context.this.type]): Comp[FunctionSignature] =
      for
        recordSig <- r.record.signature

        fieldType = signatureFromDefault(recordSig).substituteWithinExprForArgs(
          exprContext.ParameterOwner.Rec(r.record),
          r.args,
          exprFromDefault(field.fieldType),
        )

      yield FunctionSignature(
        parameters = Seq(),
        returnType = fieldType,
        ensuresClauses = Seq(),
      )

    def recordFieldUpdateSig(r: exprContext.Expr.RecordType, field: RecordFieldC & HasContext[Context.this.type]): Comp[FunctionSignature] =
      for
        recordSig <- r.record.signature

        fieldType = signatureFromDefault(recordSig).substituteWithinExprForArgs(
          exprContext.ParameterOwner.Rec(r.record),
          r.args,
          exprFromDefault(field.fieldType),
        )

      yield FunctionSignature(
        parameters = Seq(
          SignatureParameter(
            listType = dev.argon.ast.FunctionParameterListType.NormalList,
            isErased = field.isErased,
            bindings = Seq(),
            name = None,
            paramType = fieldType,
          )
        ),
        returnType = exprContext.Expr.Tuple(Seq()),
        ensuresClauses = Seq(),
      )
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
  }

  val implementations: Implementations
}

object Context {
  type Env0 = ErrorLog
  type Error0 = Nothing

  trait ImplementationExterns {
    type TubeMetadata
    type ExternFunction
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

  type RecordField = RecordFieldC & HasContext[context.type]
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
    if s.isEmpty() then
      None
    else
      for
        parts <- NonEmptySeq.fromSeq(
          s.split("\\.").nn
            .iterator
            .map(part => URLDecoder.decode(part, StandardCharsets.UTF_8).nn)
            .toSeq
        )
      yield TubeName(parts)
      
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

  def importSpecifier: Comp[ImportSpecifier]
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

  def allExports(reexportingModules: Set[ModuleName]): Comp[Map[Option[IdentifierExpr], Seq[ModuleExport]]]
  def getExports(reexportingModules: Set[ModuleName])(id: Option[IdentifierExpr]): Comp[Option[Seq[ModuleExport]]]
}

enum ModuleExportC[Ctx <: Context] {
  case Function(f: ArFuncC & HasContext[Ctx])
  case Record(r: ArRecordC & HasContext[Ctx])
  case Exported(exp: ModuleExportC[Ctx])
}

abstract class ArFuncC extends UsingContext with DeclarationBase derives CanEqual {
  val id: UniqueIdentifier

  def isInline: Boolean
  def isErased: Boolean
  def isProof: Boolean
  def effects: context.DefaultExprContext.EffectInfo

  def importSpecifier: Comp[ImportSpecifier]

  def signature: Comp[FunctionSignature]

  def implementation: Option[Comp[context.implementations.FunctionImplementation]]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArFuncC => id == other.id
      case _ => false
    }
}

abstract class ArRecordC extends UsingContext with DeclarationBase derives CanEqual {
  val id: UniqueIdentifier

  def importSpecifier: Comp[ImportSpecifier]

  def signature: Comp[FunctionSignature]

  def fields: Comp[Seq[RecordField]]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArRecordC => id == other.id
      case _ => false
    }
}

abstract class RecordFieldC extends UsingContext derives CanEqual {
  import context.DefaultExprContext.Expr

  def owningRecord: ArRecord

  val id: UniqueIdentifier
  val isMutable: Boolean
  def isErased: Boolean = false
  val name: IdentifierExpr
  val fieldType: Expr

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: RecordFieldC => id == other.id
      case _ => false
    }
}

