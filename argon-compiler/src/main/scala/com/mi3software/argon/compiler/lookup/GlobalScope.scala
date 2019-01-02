package com.mi3software.argon.compiler.lookup

import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation}
import scala.collection.Set

object GlobalScope {

  private sealed trait ResolvedName
  private object NotFound extends ResolvedName
  private object FoundOverloadable extends ResolvedName
  private final case class NestedNamespaces(paths: Set[NamespacePath]) extends ResolvedName

  def createNSScope
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (parentScope: context.scopeContext.NamespacesOnlyScope)
  : context.scopeContext.Scope =
    new context.scopeContext.NamespaceScope(
      (name, fileSpec, sourceLocation) => createLookupResult(context)(imports)(modules)(name, fileSpec, sourceLocation),
      parentScope
    )


  private def createLookupResult
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (
    name: String,
    fileSpec: FileSpec,
    sourceLocation: SourceLocation,
  )
  : context.scopeContext.LookupResult =
    resolveName(context)(imports)(modules)(name)(NotFound) match {
      case NotFound => context.scopeContext.LookupResult.Failed
      case FoundOverloadable => context.scopeContext.LookupResult.ValuesResult(overloadResult(context)(imports)(modules)(name))
      case NestedNamespaces(paths) => context.scopeContext.LookupResult.ScopeResult(
        createNSScope(context)(Vector(paths.toVector))(modules)(context.scopeContext.EmptyScope)
      )
    }

  // First resolve from higher-precedence modules.
  // Then resolve from higher-precedence imports.
  // If an overloadable (non-namespace), then done.
  // Otherwise look for any other namespaces to merge.

  private def resolveName
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (name: String)
  (acc: ResolvedName)
  : ResolvedName = modules match {
    case Vector() => acc
    case head +: tail => resolveNameStage1(context)(imports)(head)(name)(acc) match {
      case NotFound => resolveName(context)(imports)(tail)(name)(NotFound)
      case FoundOverloadable => FoundOverloadable
      case next @ NestedNamespaces(_) => resolveName(context)(imports)(modules)(name)(next)
    }
  }

  private def resolveNameStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (acc: ResolvedName)
  : ResolvedName = imports match {
    case Vector() => acc
    case head +: tail => resolveNameStage2(context)(head)(modules)(name)(acc) match {
      case FoundOverloadable => FoundOverloadable
      case next => resolveNameStage1(context)(tail)(modules)(name)(next)
    }
  }

  private def resolveNameStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (acc: ResolvedName)
  : ResolvedName =
    (
      for {
        importNS <- imports
        module <- modules
      } yield ModuleLookup.lookupNamespaceValue(context)(module.value)(importNS, GlobalName.Normal(name)) {
        case GlobalBinding.NestedNamespace(_, _) => NestedNamespaces(Set(NamespacePath(importNS.ns :+ name)))
        case _ => FoundOverloadable
      }
    ).foldLeft(NotFound : ResolvedName) {
      case (prev, None) => prev
      case (prev, Some(NotFound)) => prev
      case (NotFound | FoundOverloadable, Some(curr)) => curr
      case (prev @ NestedNamespaces(_), Some(FoundOverloadable)) => prev
      case (NestedNamespaces(paths1), Some(NestedNamespaces(paths2))) => NestedNamespaces(paths1 ++ paths2)
    }



  private def overloadResult
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (name: String)
  : context.scopeContext.OverloadResult = modules match {
    case Vector() => context.scopeContext.OverloadResult.End
    case head +: tail => overloadResultStage1(context)(imports)(head)(name)(overloadResult(context)(imports)(tail)(name))
  }

  private def overloadResultStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (nextResult: context.scopeContext.OverloadResult)
  : context.scopeContext.OverloadResult = imports match {
    case Vector() => nextResult
    case head +: tail => overloadResultStage2(context)(head)(modules)(name)(overloadResultStage1(context)(tail)(modules)(name)(nextResult))
  }

  private def overloadResultStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (nextResult: context.scopeContext.OverloadResult)
  : context.scopeContext.OverloadResult = {
    val overloads =
      for {
        importNS <- imports
        module <- modules
        binding <- ModuleLookup.lookupNamespaceValue(context)(module.value)(importNS, GlobalName.Normal(name)) {
          case binding: GlobalBinding.NonNamespace[context.type, module.PayloadSpec] =>
            getScopeValue(context)(binding)
        }.toList.toVector
      } yield binding

    if(overloads.isEmpty)
      nextResult
    else
      context.scopeContext.OverloadResult.List(overloads, nextResult)
  }

  private def getScopeValue[TPayloadSpec[_, _]]
  (context: Context)
  (binding: GlobalBinding.NonNamespace[context.type, TPayloadSpec])
  : context.scopeContext.ScopeValue = binding match {
    case GlobalBinding.GlobalClass(_, _, arClass) => context.scopeContext.ClassScopeValue(AbsRef(arClass))
    case GlobalBinding.GlobalTrait(_, _, arTrait) => context.scopeContext.TraitScopeValue(AbsRef(arTrait))
    case GlobalBinding.GlobalFunction(_, _, func) => context.scopeContext.FunctionScopeValue(AbsRef(func))
    case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) => context.scopeContext.DataConstructorScopeValue(AbsRef(dataCtor))
  }

}
