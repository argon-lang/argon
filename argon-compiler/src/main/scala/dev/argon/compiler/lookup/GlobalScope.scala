package dev.argon.compiler.lookup

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.util.{FileSpec, NamespacePath, SourceLocation}

import scala.collection.Set
import cats._
import cats.data.NonEmptyVector
import cats.implicits._

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
  : context.Comp[context.scopeContext.LookupResult] = {
    import context._

    resolveName(context)(imports)(modules)(name)(NotFound).flatMap {
      case NotFound =>
        context.scopeContext.LookupResult.Failed.pure[Comp]

      case FoundOverloadable =>
        overloadResult(context)(imports)(modules)(name)
          .map(context.scopeContext.LookupResult.ValuesResult)

      case NestedNamespaces(paths) =>
        createNSScope(context)(Vector(paths.toVector))(modules)(context.scopeContext.EmptyScope)
          .map(context.scopeContext.LookupResult.ScopeResult)
    }
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
  : context.Comp[ResolvedName] = {
    import context._

    modules match {
      case Vector() => acc.pure[Comp]
      case head +: tail =>
        resolveNameStage1(context)(imports)(head)(name)(acc).flatMap {
          case NotFound => resolveName(context)(imports)(tail)(name)(NotFound)
          case FoundOverloadable => FoundOverloadable.pure[Comp]
          case next @ NestedNamespaces(_) => resolveName(context)(imports)(modules)(name)(next)
        }
    }
  }

  private def resolveNameStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (acc: ResolvedName)
  : context.Comp[ResolvedName] = {
    import context._

    imports match {
      case Vector() => acc.pure[Comp]
      case head +: tail =>
        resolveNameStage2(context)(head)(modules)(name)(acc).flatMap {
          case FoundOverloadable => FoundOverloadable.pure[Comp]
          case next => resolveNameStage1(context)(tail)(modules)(name)(next)
        }
    }
  }

  private def resolveNameStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (acc: ResolvedName)
  : context.Comp[ResolvedName] = {
    import context._

    imports
      .flatTraverse { importNS =>
        modules.traverse { module =>
          ModuleLookup.lookupNamespaceValue(context)(module.value)(importNS, GlobalName.Normal(name)) {
            case GlobalBinding.NestedNamespace(_, _) => NestedNamespaces(Set(NamespacePath(importNS.ns :+ name)))
            case _ => FoundOverloadable
          }
        }
      }
      .map { foundValue =>
        foundValue.foldLeft(NotFound : ResolvedName) {
          case (prev, None) => prev
          case (prev, Some(NotFound)) => prev
          case (NotFound | FoundOverloadable, Some(curr)) => curr
          case (prev @ NestedNamespaces(_), Some(FoundOverloadable)) => prev
          case (NestedNamespaces(paths1), Some(NestedNamespaces(paths2))) => NestedNamespaces(paths1 ++ paths2)
        }
      }
  }


  private def overloadResult
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (name: String)
  : context.Comp[OverloadResult[context.scopeContext.ScopeValueOverload]] = {
    import context._
    modules match {
      case Vector() => OverloadResult.End.pure[Comp]
      case head +: tail =>
        overloadResult(context)(imports)(tail)(name)
          .flatMap(overloadResultStage1(context)(imports)(head)(name)(_))
    }
  }

  private def overloadResultStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (nextResult: OverloadResult[context.scopeContext.ScopeValueOverload])
  : context.Comp[OverloadResult[context.scopeContext.ScopeValueOverload]] = imports match {
    case Vector() => context.compCompilationInstance.point(nextResult)
    case head +: tail =>
      context.compCompilationInstance.flatMap(
        overloadResultStage1(context)(tail)(modules)(name)(nextResult)
      )(overloadResultStage2(context)(head)(modules)(name)(_))
  }

  private def overloadResultStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: String)
  (nextResult: OverloadResult[context.scopeContext.ScopeValueOverload])
  : context.Comp[OverloadResult[context.scopeContext.ScopeValueOverload]] = {
    import context._


    imports
      .flatTraverse { importNS =>
        modules.flatTraverse { module =>
          context.compCompilationInstance.map(
            ModuleLookup.lookupNamespaceValue(context)(module.value)(importNS, GlobalName.Normal(name)) {
              case binding: GlobalBinding.NonNamespace[context.type, module.PayloadSpec] =>
                getScopeValue(context)(binding)
            }) { _.toList.toVector }
        }
      }
      .map { overloads =>
        NonEmptyVector.fromVector(overloads) match {
          case Some(overloads) => OverloadResult.List(overloads, nextResult)
          case None => nextResult
        }
      }
  }

  private def getScopeValue[TPayloadSpec[_, _]]
  (context: Context)
  (binding: GlobalBinding.NonNamespace[context.type, TPayloadSpec])
  : context.scopeContext.ScopeValueOverload = binding match {
    case GlobalBinding.GlobalClass(_, _, arClass) => context.scopeContext.ClassScopeValue(AbsRef(arClass))
    case GlobalBinding.GlobalTrait(_, _, arTrait) => context.scopeContext.TraitScopeValue(AbsRef(arTrait))
    case GlobalBinding.GlobalFunction(_, _, func) => context.scopeContext.FunctionScopeValue(AbsRef(func))
    case GlobalBinding.GlobalDataConstructor(_, _, dataCtor) => context.scopeContext.DataConstructorScopeValue(AbsRef(dataCtor))
  }

}
