package dev.argon.compiler.loaders

import dev.argon.compiler.{Comp, ErrorList, RComp}
import dev.argon.util.NamespacePath
import dev.argon.compiler.core._
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import zio.{IO, ZManaged}
import zio.stream._

object NamespaceBuilder {

  def createNamespace[R, TContext <: Context with Singleton, TPayloadSpec[_, _]](elements: ZStream[R, ErrorList, ModuleElement[TContext, TPayloadSpec]]): RComp[R, Namespace[TContext, TPayloadSpec]] =
    createNamespaceWithPath(NamespacePath.empty, elements)

  def createNamespaceWithPath[R, TContext <: Context with Singleton, TPayloadSpec[_, _]](path: NamespacePath, elements: ZStream[R, ErrorList, ModuleElement[TContext, TPayloadSpec]]): RComp[R, Namespace[TContext, TPayloadSpec]] =
    divideNamespaceElements(elements).use { case (directElements, nestedElemenets) =>

      val nestedElementGroups =
        nestedElemenets
        .groupByKey({ case (name, _, _) => name }) { (nestedNSName, subNSElems) =>
          val nestedElements = subNSElems.map { case (_, nestedNSPath, binding) => ModuleElement(nestedNSPath, binding) }

          ZStream.fromEffect(
            createNamespaceWithPath(NamespacePath(path.ns :+ nestedNSName), nestedElements)
              .map { nestedNS =>
                GlobalBinding.NestedNamespace(GlobalName.Normal(nestedNSName), nestedNS)
              }
          )
        }

      nestedElementGroups.merge(directElements)
        .runCollect
        .map { bindings =>
          Namespace(path, bindings.toVector)
        }

    }

  private def divideNamespaceElements[R, TContext <: Context with Singleton, TPayloadSpec[_, _]]
  (elements: ZStream[R, ErrorList, ModuleElement[TContext, TPayloadSpec]])
  : ZManaged[R, ErrorList, (Stream[ErrorList, GlobalBinding[TContext, TPayloadSpec]], Stream[ErrorList, (String, NamespacePath, GlobalBinding[TContext, TPayloadSpec])])] =
    elements.partitionEither {
      case ModuleElement(NamespacePath(headNS +: tailNS), binding) =>
        IO.succeed(Right((headNS, NamespacePath(tailNS), binding)))

      case ModuleElement(NamespacePath(Vector()), binding) =>
        IO.succeed(Left(binding))
    }

}
