package dev.argon.compiler

import cats.data.NonEmptySeq
import dev.argon.ast.IdentifierExpr
import dev.argon.expr.ExprContext
import dev.argon.util.{Fuel, UniqueIdentifier}
import zio.ZIO

trait Context extends ScopeContext {
  type Env0 = ErrorLog
  type Env <: ErrorLog
  type Error0 = Nothing
  type Error

  type Comp[+A] = ZIO[Env, Error, A]

  trait ArgonExprContext extends ExprContext {
    override type Function = ArFuncC & HasContext[Context.this.type]
    override def functionCanEqual: CanEqual[Function, Function] = summon
  }

  object DefaultExprContext extends ArgonExprContext {
    override type Hole = Nothing
    override def holeCanEqual: CanEqual[Hole, Hole] = CanEqual.derived
  }

  object TRExprContext extends ArgonExprContext {
    override type Hole = UniqueIdentifier
    override def holeCanEqual: CanEqual[Hole, Hole] = summon
  }

  object DefaultSignatureContext extends SignatureContext {
    override val exprContext: DefaultExprContext.type = DefaultExprContext
  }

  object TRSignatureContext extends SignatureContext {
    override val exprContext: TRExprContext.type = TRExprContext

    def parameterFromDefault(p: DefaultSignatureContext.SignatureParameter): SignatureParameter =
      SignatureParameter(
        listType = p.listType,
        isErased = p.isErased,
        bindings = p.bindings.map { binding => ParameterBinding(
          name = binding.name,
          paramType = DefaultToTRShifter[Context.this.type](Context.this).shiftExpr(binding.paramType),
        ) },
        name = p.name,
        paramType = DefaultToTRShifter[Context.this.type](Context.this).shiftExpr(p.paramType),
      )

    def signatureFromDefault(sig: DefaultSignatureContext.FunctionSignature): FunctionSignature =
      FunctionSignature(
        parameters = sig.parameters.map(parameterFromDefault),
        returnType = DefaultToTRShifter[Context.this.type](Context.this).shiftExpr(sig.returnType),
        ensuresClauses = sig.ensuresClauses.map(DefaultToTRShifter[Context.this.type](Context.this).shiftExpr),
      )

  }
  
  object Config {
    val evaluatorFuel: Fuel = Fuel(10)
    val prologFuel: Fuel = Fuel(10)
    val smtFuel: Fuel = Fuel(5)
  }

  trait Implementations {
    type ExternFunctionImplementation
    type FunctionReference

    enum FunctionImplementation {
      case Expr(e: DefaultExprContext.Expr)
      case Extern(e: ExternFunctionImplementation)
    }
  }

  val implementations: Implementations


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

abstract class ArTubeC extends UsingContext {
  def name: TubeName

  def modules: Map[ModulePath, ArModule]
}

abstract class ArModuleC extends UsingContext {
  def tubeName: TubeName
  def path: ModulePath

  def allExports(reexportingModules: Set[ModuleName]): Comp[Map[Option[IdentifierExpr], Seq[ModuleExport]]]
  def getExports(reexportingModules: Set[ModuleName])(id: Option[IdentifierExpr]): Comp[Option[Seq[ModuleExport]]]
}

enum ModuleExportC[Ctx <: Context] {
  case Function(f: ArFuncC & HasContext[Ctx])
  case Exported(exp: ModuleExportC[Ctx])
}

abstract class ArFuncC extends UsingContext derives CanEqual {
  val id: UniqueIdentifier

  def isInline: Boolean
  def isErased: Boolean

  def signature: Comp[FunctionSignature]

  def implementation: Option[Comp[context.implementations.FunctionImplementation]]
  def reference: Comp[context.implementations.FunctionReference]

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArFuncC => id == other.id
      case _ => false
    }
}


