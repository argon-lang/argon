package dev.argon.compiler

import dev.argon.util.*
import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import dev.argon.expr.ErasureMode
import zio.interop.catz.core.given

trait ScopeContext {
  self: Context =>

  object Scopes {

    import TRExprContext.{Var, LocalVar, Expr}

    sealed trait LookupResult
    object LookupResult {
      sealed trait OverloadableOnly extends LookupResult {
        def chain(other: OverloadableOnly): OverloadableOnly
        def chainZIO(other: Comp[OverloadableOnly]): Comp[OverloadableOnly]
      }

      final case class NotFound() extends OverloadableOnly {
        override def chain(other: OverloadableOnly): OverloadableOnly =
          other

        override def chainZIO(other: Comp[OverloadableOnly]): Comp[OverloadableOnly] =
          other
      }
      final case class Variable(v: Var) extends LookupResult
      final case class VariableTupleElement(v: Var, index: Int, t: TRExprContext.Expr) extends LookupResult
      final case class Overloaded(overloads: Seq[Overloadable], next: Comp[LookupResult.OverloadableOnly]) extends OverloadableOnly {
        override def chain(other: OverloadableOnly): OverloadableOnly =
          Overloaded(overloads, next.map(_.chain(other)))

        override def chainZIO(other: Comp[OverloadableOnly]): Comp[OverloadableOnly] =
          ZIO.succeed(Overloaded(overloads, next.flatMap(_.chainZIO(other))))
      }
    }

    enum Overloadable {
      case Function(f: ArFuncC & HasContext[self.type])
      case Record(r: ArRecordC & HasContext[self.type])
      case Enum(e: ArEnumC & HasContext[self.type])
      case Trait(t: ArTraitC & HasContext[self.type])
      case Instance(i: ArInstanceC & HasContext[self.type])
      case ExtensionMethod(f: ArFuncC & HasContext[self.type], obj: TRExprContext.AnnotatedExpr)
      case InstanceMethod(m: ArMethodC & HasContext[self.type], instanceType: TRExprContext.MethodInstanceType, obj: Expr)
      case RecordField(r: Expr.RecordType, field: RecordFieldC & HasContext[self.type], recordValue: Expr)
      case RecordFieldUpdate(r: Expr.RecordType, field: RecordFieldC & HasContext[self.type], recordValue: Expr)
      case EnumVariant(v: EnumVariantC & HasContext[self.type])

      def initialArgs: Seq[TRExprContext.AnnotatedExpr] =
        this match {
          case Function(_) | Record(_) | Enum(_) | Trait(_) | Instance(_) => Seq()
          case ExtensionMethod(f, obj) => Seq(obj)
          case InstanceMethod(_, _, _) => Seq()
          case RecordField(_, _, _) => Seq()
          case RecordFieldUpdate(_, _, _) => Seq()
          case EnumVariant(_) => Seq()
        }

      def signature: Comp[TRSignatureContext.FunctionSignature] =
        this match {
          case Function(f) => f.signature.map(TRSignatureContext.signatureFromDefault)
          case Record(r) => r.signature.map(TRSignatureContext.signatureFromDefault)
          case Enum(r) => r.signature.map(TRSignatureContext.signatureFromDefault)
          case Trait(t) => t.signature.map(TRSignatureContext.signatureFromDefault)
          case Instance(i) => i.signature.map(TRSignatureContext.signatureFromDefault)
          case ExtensionMethod(f, obj) => f.signature.map(TRSignatureContext.signatureFromDefault)
          case InstanceMethod(m, instanceType, _) => TRSignatureContext.instanceMethodSig(m, instanceType)
          case RecordField(r, field, _) => TRSignatureContext.recordFieldSig(r, field)
          case RecordFieldUpdate(r, field, _) => TRSignatureContext.recordFieldUpdateSig(r, field)
          case EnumVariant(v) => v.signature.map(TRSignatureContext.signatureFromDefault)
        }

      def asOwner: Option[TRExprContext.ExpressionOwner] =
        this match {
          case Function(f) => Some(TRExprContext.ExpressionOwner.Func(f))
          case Record(r) => Some(TRExprContext.ExpressionOwner.Rec(r))
          case Enum(e) => Some(TRExprContext.ExpressionOwner.Enum(e))
          case Trait(t) => Some(TRExprContext.ExpressionOwner.Trait(t))
          case Instance(i) => Some(TRExprContext.ExpressionOwner.Instance(i))
          case ExtensionMethod(f, _) => Some(TRExprContext.ExpressionOwner.Func(f))
          case InstanceMethod(m, _, _) => Some(TRExprContext.ExpressionOwner.Method(m))
          case RecordField(_, _, _) => None
          case RecordFieldUpdate(_, field, _) => None
          case EnumVariant(v) => Some(TRExprContext.ExpressionOwner.EnumVariant(v))
        }

      def noArgumentMembers(name: IdentifierExpr): Comp[Seq[Overloadable]] =
        this match {
          case Function(_) | Record(_) | Trait(_) | Instance(_) => ZIO.succeed(Seq())
          case Enum(e) =>
            e.variants.map { variants =>
              variants
                .filter(_.name == name)
                .map(Overloadable.EnumVariant.apply)
            }
              
          case ExtensionMethod(_, _) => ZIO.succeed(Seq())
          case InstanceMethod(_, _, _) => ZIO.succeed(Seq())
          case RecordField(_, _, _) => ZIO.succeed(Seq())
          case RecordFieldUpdate(_, _, _) => ZIO.succeed(Seq())
          case EnumVariant(_) => ZIO.succeed(Seq())
        }
    }

    enum ImplicitValue {
      case OfVar(v: Var)
      case OfFunction(f: ArFuncC & HasContext[self.type])
    }

    trait Scope {
      def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult]
      def givenAssertions: Comp[Seq[ImplicitValue]]
      def knownVarValues: Comp[Map[Var, TRExprContext.Expr]]
    }

    object Empty extends Scope {
      override def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult] =
        ZIO.succeed(LookupResult.NotFound())

      override def givenAssertions: Comp[Seq[ImplicitValue]] = ZIO.succeed(Seq.empty)

      override def knownVarValues: Comp[Map[Var, TRExprContext.Expr]] = ZIO.succeed(Map.empty)
    }

    final class GlobalScopeBuilder private(imports: Seq[WithSource[ast.ImportStmt]], module: ArModuleC & HasContext[self.type])(using TubeImporter & HasContext[self.type]) {
      def addImport(importSpec: WithSource[ast.ImportStmt]): GlobalScopeBuilder =
        GlobalScopeBuilder(imports :+ importSpec, module)

      def toScope: Scope = CurrentModuleScope(GlobalScope(imports, module), module)
    }

    private def toExportedGiven(exp: ModuleExportC[self.type]): Option[ImplicitValue] =
      exp match {
        case ModuleExportC.Binding(_, ModuleExportBindingC.Function(f)) if f.isWitness =>
          Some(ImplicitValue.OfFunction(f))

        case ModuleExportC.Binding(_, ModuleExportBindingC.Function(_)) => None
        case ModuleExportC.Binding(_, ModuleExportBindingC.Record(_)) => None
        case ModuleExportC.Binding(_, ModuleExportBindingC.Enum(_)) => None
        case ModuleExportC.Binding(_, ModuleExportBindingC.Trait(_)) => None
        case ModuleExportC.Binding(_, ModuleExportBindingC.Instance(_)) => None
        case ModuleExportC.Exported(exp) => toExportedGiven(exp)
      }

    object GlobalScopeBuilder {
      def empty(module: ArModuleC & HasContext[self.type])(using TubeImporter & HasContext[self.type]): GlobalScopeBuilder =
        GlobalScopeBuilder(Seq.empty, module)
    }

    private def moduleExportToOverloadable(exp: ModuleExportC[self.type]): Overloadable =
      exp match {
        case ModuleExportC.Binding(_, ModuleExportBindingC.Function(f)) => Overloadable.Function(f)
        case ModuleExportC.Binding(_, ModuleExportBindingC.Record(r)) => Overloadable.Record(r)
        case ModuleExportC.Binding(_, ModuleExportBindingC.Enum(e)) => Overloadable.Enum(e)
        case ModuleExportC.Binding(_, ModuleExportBindingC.Trait(t)) => Overloadable.Trait(t)
        case ModuleExportC.Binding(_, ModuleExportBindingC.Instance(i)) => Overloadable.Instance(i)
        case ModuleExportC.Exported(exp) => moduleExportToOverloadable(exp)
      }

    private def canAccessModuleExport(access: AccessToken & HasContext[self.type])(exp: ModuleExportC[self.type]): Comp[Boolean] =
      exp match {
        case ModuleExportC.Binding(bindingAccess, ModuleExportBindingC.Any(decl)) =>
          access.allows(AccessRequest(
            decl,
            None,
            bindingAccess,
          ))

        case ModuleExportC.Exported(exp) =>
          canAccessModuleExport(access)(exp)
      }

    final class GlobalScope private[Scopes] (imports: Seq[WithSource[ast.ImportStmt]], module: ArModuleC & HasContext[self.type])(using TubeImporter & HasContext[self.type]) extends Scope {
      override def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult.OverloadableOnly] = {
        Foldable[Seq].collectFold(imports)(ImportUtil.getModuleExports(self)(Set.empty)(module.tubeName, module.path)(_))
          .flatMap { importMap =>
            ZIO.foreach(importMap.get(id)) { res =>
                ZIO.filter(res)(canAccessModuleExport(access))
            }
          }
          .map { res =>
            res.filter(_.nonEmpty) match {
              case Some(res) => LookupResult.Overloaded(res.map(moduleExportToOverloadable), ZIO.succeed(LookupResult.NotFound()))
              case None => LookupResult.NotFound()
            }
          }
      }

      override def givenAssertions: Comp[Seq[ImplicitValue]] =
        ZIO.foreach(imports) { imp =>
          ImportUtil.getModuleExports(self)(Set.empty)(module.tubeName, module.path)(imp)
            .map { exports =>
              exports.values.view
                .flatten
                .flatMap(toExportedGiven)
            }
        }.map(_.flatten)

      override def knownVarValues: Comp[Map[Var, TRExprContext.Expr]] = ZIO.succeed(Map.empty)
    }

    final class CurrentModuleScope(parent: GlobalScope, module: ArModuleC & HasContext[self.type]) extends Scope {
      override def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult] =
        module.getExports(Set.empty)(id).flatMap {
          case Some(exports) =>
            ZIO.succeed(LookupResult.Overloaded(
              exports.map(moduleExportToOverloadable),
              parent.lookup(id)(access)
            ))

          case None => parent.lookup(id)(access)
        }

      override def givenAssertions: Comp[Seq[ImplicitValue]] =
        for
          exports <- module.allExports(Set.empty)
          parentAssertions <- parent.givenAssertions
        yield parentAssertions ++
          exports.values.view
            .flatten
            .flatMap(toExportedGiven)
            .toSeq

      override def knownVarValues: Comp[Map[Var, TRExprContext.Expr]] = ZIO.succeed(Map.empty)
    }

    final class InstanceVarScope(parentScope: Scope, instanceVar: TRExprContext.InstanceParameterVar) extends Scope {
      override def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult] =
        if instanceVar.name.contains(id) then
          ZIO.succeed(LookupResult.Variable(instanceVar))
        else
          parentScope.lookup(id)(access)

      override def givenAssertions: Comp[Seq[ImplicitValue]] =
        parentScope.givenAssertions

      override def knownVarValues: Comp[Map[Var, TRExprContext.Expr]] =
        parentScope.knownVarValues
    }

    final class ParameterScope(owner: TRExprContext.ExpressionOwner, parentScope: Scope, sigParams: Seq[DefaultSignatureContext.SignatureParameter]) extends Scope {

      private val parameters: Seq[Var] =
        val shifter = DefaultToTRShifter[self.type](self)
        val owner2 = owner match {
          case TRExprContext.ExpressionOwner.Func(f) => DefaultExprContext.ExpressionOwner.Func(f)
          case TRExprContext.ExpressionOwner.Rec(r) => DefaultExprContext.ExpressionOwner.Rec(r)
          case TRExprContext.ExpressionOwner.Enum(e) => DefaultExprContext.ExpressionOwner.Enum(e)
          case TRExprContext.ExpressionOwner.Trait(t) => DefaultExprContext.ExpressionOwner.Trait(t)
          case TRExprContext.ExpressionOwner.EnumVariant(v) => DefaultExprContext.ExpressionOwner.EnumVariant(v)
          case TRExprContext.ExpressionOwner.Method(m) => DefaultExprContext.ExpressionOwner.Method(m)
          case TRExprContext.ExpressionOwner.Instance(i) => DefaultExprContext.ExpressionOwner.Instance(i)
        }
        DefaultSignatureContext.SignatureParameter.getParameterVariables(owner2, sigParams).map(shifter.shiftVar)
      end parameters


      override def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult] =
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
            parentScope.lookup(id)(access)
          }

      override def givenAssertions: Comp[Seq[ImplicitValue]] =
        for
          parentAssertions <- parentScope.givenAssertions
        yield parentAssertions ++ parameters.view.filter(_.isWitness).map(ImplicitValue.OfVar.apply)

      override def knownVarValues: Comp[Map[Var, TRExprContext.Expr]] =
        parentScope.knownVarValues
    }

    final class LocalScope private(parent: Scope, variables: TMap[IdentifierExpr, Var], variableValues: TMap[Var, TRExprContext.Expr], givenVars: TSet[Var]) extends Scope {
      def addVariable(v: Var, value: Option[TRExprContext.Expr]): UIO[Unit] =
        ZIO.foreachDiscard(v.name)(name => variables.put(name, v).commit) *>
          (
            value match {
              case Some(value) if !v.isMutable && PurityScanner(self)(TRExprContext)(value) =>
                variableValues.put(v, value)

              case _ => STM.unit
            }
          ).commit *>
          givenVars.put(v).commit.whenDiscard(v.erasureMode == ErasureMode.Erased)

      override def givenAssertions: Comp[Seq[ImplicitValue]] =
        for
          parentAssertions <- parent.givenAssertions
          variables <- givenVars.toSet.commit
        yield parentAssertions ++ variables.view.filter(_.isWitness).map(ImplicitValue.OfVar.apply)

      override def knownVarValues: Comp[Map[Var, TRExprContext.Expr]] =
        for
          parentVars <- parent.knownVarValues
          vv <- variableValues.toMap.commit
        yield parentVars ++ vv

      def addPartialScope(s: PartialScope): UIO[Unit] =
        ZSTM.foreachDiscard(s.variables.toSeq) { (name, v) =>
          variables.put(name, v)
        }.commit *>
          ZSTM.foreachDiscard(s.variableValues.toSeq) { (v, value) =>
            variableValues.put(v, value)
          }.commit *>
          ZSTM.foreachDiscard(s.givenVars) { v =>
            givenVars.put(v)
          }.commit
          

      override def lookup(id: IdentifierExpr)(access: AccessToken & HasContext[self.type]): Comp[LookupResult] =
        variables.get(id).commit.flatMap {
          case Some(v) => ZIO.succeed(LookupResult.Variable(v))
          case None => parent.lookup(id)(access)
        }

      def toPartialScope: UIO[PartialScope] =
        for
          vars <- variables.toMap.commit
          variableValues <- variableValues.toMap.commit
          givenVars <- givenVars.toSet.commit
        yield PartialScope(vars, variableValues, givenVars)


    }

    object LocalScope {
      def make(parentScope: Scope): UIO[LocalScope] =
        for
          variables <- TMap.empty[IdentifierExpr, Var].commit
          variableValues <- TMap.empty[Var, TRExprContext.Expr].commit
          givenVars <- TSet.empty[Var].commit
        yield LocalScope(parentScope, variables, variableValues, givenVars)
    }

    final class PartialScope private[Scopes](
      private[Scopes] val variables: Map[IdentifierExpr, Var],
      private[Scopes] val variableValues: Map[Var, TRExprContext.Expr],
      private[Scopes] val givenVars: Set[Var],
    )
  }
}
