package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.{FileSpec, NamespacePath, SourceLocation, VectorUnCons}

import scala.collection.Set
import cats.data.NonEmptyVector
import dev.argon.compiler.Comp
import zio.IO
import zio.stream.Stream

object GlobalScope {

  private sealed trait ResolvedName
  private object NotFound extends ResolvedName
  private object FoundOverloadable extends ResolvedName
  private final case class NestedNamespaces(paths: Set[NamespacePath]) extends ResolvedName

  def createNSScope
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  : Comp[context.scopeContext.Scope] =
    context.scopeContext.NamespaceScope(
      (name, fileSpec, sourceLocation) => createLookupResult(context)(imports)(modules)(name, fileSpec, sourceLocation)
    )


  private def createLookupResult
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (
    name: GlobalName.NonEmpty,
    fileSpec: FileSpec,
    sourceLocation: SourceLocation,
  )
  : Comp[context.scopeContext.LookupResult] =
    resolveName(context)(imports)(modules)(name)(NotFound).flatMap {
      case NotFound => IO.succeed(context.scopeContext.LookupResult.Failed)

      case FoundOverloadable =>
        overloadResult(context)(imports)(modules)(name).map {
          case overloads @ OverloadResult.List(_, _) =>
            context.scopeContext.LookupResult.ValuesResult(overloads)

          case OverloadResult.End =>
            context.scopeContext.LookupResult.Failed
        }

      case NestedNamespaces(paths) =>
        createNSScope(context)(Vector(paths.toVector))(modules)
          .map(context.scopeContext.LookupResult.ScopeResult(_))
    }

  // First resolve from higher-precedence modules.
  // Then resolve from higher-precedence imports.
  // If an overloadable (non-namespace), then done.
  // Otherwise look for any other namespaces to merge.

  private def resolveName
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (name: GlobalName.NonEmpty)
  (acc: ResolvedName)
  : Comp[ResolvedName] =
    modules match {
      case VectorUnCons(VectorUnCons.Empty) => IO.succeed(acc)
      case VectorUnCons(VectorUnCons.NonEmpty(head, tail)) =>
        resolveNameStage1(context)(imports)(head)(name)(acc).flatMap {
          case NotFound => resolveName(context)(imports)(tail)(name)(NotFound)
          case FoundOverloadable => IO.succeed(FoundOverloadable)
          case next @ NestedNamespaces(_) => resolveName(context)(imports)(modules)(name)(next)
        }
    }

  private def resolveNameStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: GlobalName.NonEmpty)
  (acc: ResolvedName)
  : Comp[ResolvedName] =
    imports match {
      case VectorUnCons(VectorUnCons.Empty) => IO.succeed(acc)
      case VectorUnCons(VectorUnCons.NonEmpty(head, tail)) =>
        resolveNameStage2(context)(head)(modules)(name)(acc).flatMap {
          case FoundOverloadable => IO.succeed(FoundOverloadable)
          case next => resolveNameStage1(context)(tail)(modules)(name)(next)
        }
    }

  private def resolveNameStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: GlobalName.NonEmpty)
  (acc: ResolvedName)
  : Comp[ResolvedName] =
    Stream.fromIterable(imports).flatMap { importNS =>
      Stream.fromIterable(modules).flatMap { module =>
        module.value.lookupNamespaceValues(importNS, name) {
          case GlobalBinding.NestedNamespace(name2) => IO.some(NestedNamespaces(Set(NamespacePath(importNS.ns :+ name2.name))))
          case _ => IO.some(FoundOverloadable)
        }
      }
    }
      .fold(acc){
        case (prev, NotFound) => prev
        case (NotFound | FoundOverloadable, curr) => curr
        case (prev @ NestedNamespaces(_), FoundOverloadable) => prev
        case (NestedNamespaces(paths1), NestedNamespaces(paths2)) => NestedNamespaces(paths1 ++ paths2)
      }


  private def overloadResult
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[Vector[AbsRef[context.type, ArModule]]])
  (name: GlobalName.NonEmpty)
  : Comp[OverloadResult[context.scopeContext.ScopeValueOverload]] =
    modules match {
      case VectorUnCons(VectorUnCons.Empty) => IO.succeed(OverloadResult.End)
      case VectorUnCons(VectorUnCons.NonEmpty(head, tail)) =>
        overloadResult(context)(imports)(tail)(name)
          .flatMap(overloadResultStage1(context)(imports)(head)(name)(_))
    }

  private def overloadResultStage1
  (context: Context)
  (imports: Vector[Vector[NamespacePath]])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: GlobalName.NonEmpty)
  (nextResult: OverloadResult[context.scopeContext.ScopeValueOverload])
  : Comp[OverloadResult[context.scopeContext.ScopeValueOverload]] =
    imports match {
      case VectorUnCons(VectorUnCons.Empty) => IO.succeed(nextResult)
      case VectorUnCons(VectorUnCons.NonEmpty(head, tail)) =>
        overloadResultStage1(context)(tail)(modules)(name)(nextResult)
          .flatMap(overloadResultStage2(context)(head)(modules)(name)(_))
    }

  private def overloadResultStage2
  (context: Context)
  (imports: Vector[NamespacePath])
  (modules: Vector[AbsRef[context.type, ArModule]])
  (name: GlobalName.NonEmpty)
  (nextResult: OverloadResult[context.scopeContext.ScopeValueOverload])
  : Comp[OverloadResult[context.scopeContext.ScopeValueOverload]] = {
    Stream.fromIterable(imports).flatMap { importNS =>
      Stream.fromIterable(modules).flatMap { module =>
        module.value.lookupNamespaceBindings(importNS, name){ binding =>
          import module.payloadSpecInfo
          getScopeValue(context)(binding).asSome
        }
      }
    }
      .runCollect
      .map { overloads =>
        NonEmptyVector.fromVector(overloads.toVector) match {
          case Some(overloads) => OverloadResult.List(overloads, nextResult)
          case None => nextResult
        }
      }
  }

  private def getScopeValue[TPayloadSpec[_, _]: PayloadSpecInfo]
  (context: Context)
  (binding: GlobalBinding.NonNamespace[context.type, TPayloadSpec])
  : Comp[context.scopeContext.ScopeValueOverload] = binding match {
    case GlobalBinding.GlobalClass(_, _, _, arClassComp) => arClassComp.map { arClass => context.scopeContext.ClassScopeValue(AbsRef(arClass)) }
    case GlobalBinding.GlobalTrait(_, _, _, arTraitComp) => arTraitComp.map { arTrait => context.scopeContext.TraitScopeValue(AbsRef(arTrait)) }
    case GlobalBinding.GlobalFunction(_, _, _, funcComp) => funcComp.map { func => context.scopeContext.FunctionScopeValue(AbsRef(func)) }
    case GlobalBinding.GlobalDataConstructor(_, _, _, dataCtorComp) => dataCtorComp.map { dataCtor => context.scopeContext.DataConstructorScopeValue(AbsRef(dataCtor)) }
  }

}
