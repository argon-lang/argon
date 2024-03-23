package dev.argon.compiler

import dev.argon.util.{*, given}
import dev.argon.ast
import dev.argon.ast.{FunctionParameterListType, IdentifierExpr}
import dev.argon.util.{FilePosition, Location}
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given

trait ScopeContext {
  self: Context =>

  object Scopes {

    import TRExprContext.{Var, LocalVar}

    sealed trait LookupResult
    object LookupResult {
      sealed trait OverloadableOnly extends LookupResult

      final case class NotFound() extends OverloadableOnly
      final case class Variable(v: Var) extends LookupResult
      final case class VariableTupleElement(v: Var, index: Int, t: TRExprContext.Expr) extends LookupResult
      final case class Overloaded(overloads: Seq[Overloadable], next: Comp[LookupResult.OverloadableOnly]) extends OverloadableOnly
    }

    enum Overloadable {
      case Function(f: ArFuncC & HasContext[self.type])

      def signature: Comp[TRSignatureContext.FunctionSignature] =
        this match {
          case Function(f) => f.signature.map(TRSignatureContext.signatureFromDefault)
        }

      def asOwner: TRExprContext.ParameterOwner =
        this match {
          case Function(f) => f
        }
    }

    trait Scope {
      def lookup(id: IdentifierExpr): Comp[LookupResult]
    }

    final class GlobalScopeBuilder private(importer: TubeImporter & HasContext[self.type], imports: Seq[WithSource[ast.ImportStmt]], module: ArModuleC & HasContext[self.type]) {
      def addImport(importSpec: WithSource[ast.ImportStmt]): GlobalScopeBuilder =
        GlobalScopeBuilder(importer, imports :+ importSpec, module)

      def toScope: Comp[Scope] = ZIO.succeed(CurrentModuleScope(GlobalScope(importer, imports, module), module))
    }

    object GlobalScopeBuilder {
      def empty(importer: TubeImporter & HasContext[self.type], module: ArModuleC & HasContext[self.type]): GlobalScopeBuilder =
        GlobalScopeBuilder(importer, Seq.empty, module)
    }

    private def moduleExportToOverloadable(exp: ModuleExportC[self.type]): Overloadable =
      exp match {
        case ModuleExportC.Function(f) => Overloadable.Function(f)
        case ModuleExportC.Exported(exp) => moduleExportToOverloadable(exp)
      }

    final class GlobalScope private[Scopes] (importer: TubeImporter & HasContext[self.type], imports: Seq[WithSource[ast.ImportStmt]], module: ArModuleC & HasContext[self.type]) extends Scope {
      override def lookup(id: IdentifierExpr): Comp[LookupResult.OverloadableOnly] =
        Foldable[Seq].collectFold(imports)(ImportUtil.getModuleExports(self)(importer)(Set.empty)(module.tubeName, module.path)(_))
          .map { importMap =>
            importMap.get(id) match {
              case Some(res) => LookupResult.Overloaded(res.map(moduleExportToOverloadable), ZIO.succeed(LookupResult.NotFound()))
              case None => LookupResult.NotFound()
            }
          }
    }

    final class CurrentModuleScope(parent: GlobalScope, module: ArModuleC & HasContext[self.type]) extends Scope {
      override def lookup(id: IdentifierExpr): Comp[LookupResult] =
        module.getExports(Set.empty)(Some(id)).flatMap {
          case Some(exports) =>
            ZIO.succeed(LookupResult.Overloaded(
              exports.map(moduleExportToOverloadable),
              parent.lookup(id)
            ))


          case None => parent.lookup(id)
        }
    }

    final class ParameterScope(owner: TRExprContext.ParameterOwner, parentScope: Scope, sigParams: Seq[DefaultSignatureContext.SignatureParameter]) extends Scope {

      private val parameters =
        val shifter = DefaultToTRShifter[self.type](self)
        DefaultSignatureContext.SignatureParameter.getParameterVariables(owner, sigParams).map(shifter.shiftVar)
      end parameters


      override def lookup(id: IdentifierExpr): Comp[LookupResult] =
        parameters.find(_.name.contains(id))
          .map { v => ZIO.succeed(LookupResult.Variable(v)) }
          .orElse {
            (
              for
                (param, sigParam) <- parameters.zip(sigParams)
                (binding, i) <- sigParam.bindings.zipWithIndex
              yield (param, binding, i)
            )
              .find { (_, binding, _) => binding.name.contains(id) }
              .map { (param, binding, i) =>
                val shifter = DefaultToTRShifter[self.type](self)
                val bindingType = shifter.shiftExpr(binding.paramType)
                ZIO.succeed(LookupResult.VariableTupleElement(param, i, bindingType))
              }
          }
          .getOrElse {
            parentScope.lookup(id)
          }
    }

    final class LocalScope private(parent: Scope, variables: TMap[IdentifierExpr, LocalVar]) extends Scope {
      def addVariable(v: LocalVar): UIO[Unit] =
        ZIO.foreachDiscard(v.name)(name => variables.put(name, v).commit)

      def addPartialScope(s: PartialScope): UIO[Unit] =
        ZSTM.foreachDiscard(s.variables.toSeq) { (name, v) =>
          variables.put(name, v)
        }.commit

      override def lookup(id: IdentifierExpr): Comp[LookupResult] =
        variables.get(id).commit.flatMap {
          case Some(v) => ZIO.succeed(LookupResult.Variable(v))
          case None => parent.lookup(id)
        }

      def toPartialScope: UIO[PartialScope] =
        for
          vars <- variables.toMap.commit
        yield PartialScope(vars)
    }

    object LocalScope {
      def make(parentScope: Scope): UIO[LocalScope] =
        for
          variables <- TMap.empty[IdentifierExpr, LocalVar].commit
        yield LocalScope(parentScope, variables)
    }

    final class PartialScope private[Scopes](private[Scopes] val variables: Map[IdentifierExpr, LocalVar])
  }
}
