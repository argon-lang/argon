package com.mi3software.argon.compiler.lookup

import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation}
import scala.collection.Set
import scalaz._
import Scalaz._

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
  : context.Comp[context.scopeContext.Scope] =
    context.scopeContext.NamespaceScope(
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
  : context.Comp[context.scopeContext.LookupResult] =
    context.compCompilationInstance.bind(
      resolveName(context)(imports)(modules)(name)(NotFound)
    ) {
      case NotFound => context.compCompilationInstance.point(context.scopeContext.LookupResult.Failed)

      case FoundOverloadable => context.compCompilationInstance.map(
        overloadResult(context)(imports)(modules)(name)
      )(context.scopeContext.LookupResult.ValuesResult)

      case NestedNamespaces(paths) =>
        context.compCompilationInstance.map(
          createNSScope(context)(Vector(paths.toVector))(modules)(context.scopeContext.EmptyScope)
        )(context.scopeContext.LookupResult.ScopeResult)
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
  : context.Comp[ResolvedName] = modules match {
    case Vector() => context.compCompilationInstance.point(acc)
    case head +: tail => context.compCompilationInstance.bind(
      resolveNameStage1(context)(imports)(head)(name)(acc)
    ) {
      case NotFound => resolveName(context)(imports)(tail)(name)(NotFound)
      case FoundOverloadable => context.compCompilationInstance.point(FoundOverloadable)
      case next @ NestedNamespaces(_) => resolveName(context)(imports)(modules)(name)(next)
    }
  }

  private def resolveNameStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (acc: ResolvedName)
  : context.Comp[ResolvedName] = imports match {
    case Vector() => context.compCompilationInstance.point(acc)
    case head +: tail => context.compCompilationInstance.bind(
      resolveNameStage2(context)(head)(modules)(name)(acc)
    ) {
      case FoundOverloadable => context.compCompilationInstance.point(FoundOverloadable)
      case next => resolveNameStage1(context)(tail)(modules)(name)(next)
    }
  }

  private def resolveNameStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (acc: ResolvedName)
  : context.Comp[ResolvedName] =
    context.compCompilationInstance.map(
      imports.traverseM { importNS =>
        modules.traverse { module =>
          ModuleLookup.lookupNamespaceValue(context)(module.value)(importNS, GlobalName.Normal(name)) {
            case GlobalBinding.NestedNamespace(_, _) => NestedNamespaces(Set(NamespacePath(importNS.ns :+ name)))
            case _ => FoundOverloadable
          }
        }(context.compCompilationInstance)
      }(context.compCompilationInstance, implicitly)
    ) { foundValue =>
      foundValue.foldLeft(NotFound : ResolvedName) {
        case (prev, None) => prev
        case (prev, Some(NotFound)) => prev
        case (NotFound | FoundOverloadable, Some(curr)) => curr
        case (prev @ NestedNamespaces(_), Some(FoundOverloadable)) => prev
        case (NestedNamespaces(paths1), Some(NestedNamespaces(paths2))) => NestedNamespaces(paths1 ++ paths2)
      }
    }





  private def overloadResult
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (name: String)
  : context.Comp[OverloadResult[context.scopeContext.ScopeValue]] = modules match {
    case Vector() => context.compCompilationInstance.point(OverloadResult.End)
    case head +: tail =>
      context.compCompilationInstance.bind(
        overloadResult(context)(imports)(tail)(name)
      )(overloadResultStage1(context)(imports)(head)(name)(_))
  }

  private def overloadResultStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (nextResult: OverloadResult[context.scopeContext.ScopeValue])
  : context.Comp[OverloadResult[context.scopeContext.ScopeValue]] = imports match {
    case Vector() => context.compCompilationInstance.point(nextResult)
    case head +: tail =>
      context.compCompilationInstance.bind(
        overloadResultStage1(context)(tail)(modules)(name)(nextResult)
      )(overloadResultStage2(context)(head)(modules)(name)(_))
  }

  private def overloadResultStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (nextResult: OverloadResult[context.scopeContext.ScopeValue])
  : context.Comp[OverloadResult[context.scopeContext.ScopeValue]] =
    context.compCompilationInstance.map(
      imports.traverseM { importNS =>
        modules.traverseM { module =>
          context.compCompilationInstance.map(
            ModuleLookup.lookupNamespaceValue(context)(module.value)(importNS, GlobalName.Normal(name)) {
              case binding: GlobalBinding.NonNamespace[context.type, module.PayloadSpec] =>
                getScopeValue(context)(binding)
            }) { _.toVector }
        }(context.compCompilationInstance, implicitly)
      }(context.compCompilationInstance, implicitly)
    ) { overloads =>
      if(overloads.isEmpty)
        nextResult
      else
        OverloadResult.List(overloads, nextResult)
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
