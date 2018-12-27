package com.mi3software.argon.compiler.loaders

import com.mi3software.argon.util.NamespacePath
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.types._

object NamespaceBuilder {

  def createNamespace[TContext <: Context, TPayloadSpec[_, _]](elements: Vector[ModuleElement[TContext, TPayloadSpec]]): Namespace[TContext, TPayloadSpec] =
    createNamespaceWithPath(NamespacePath.empty, elements)

  def createNamespaceWithPath[TContext <: Context, TPayloadSpec[_, _]](path: NamespacePath, elements: Vector[ModuleElement[TContext, TPayloadSpec]]): Namespace[TContext, TPayloadSpec] = {
    val (directElements, nestedElements) = divideNamespaceElements(elements)(Vector.empty, Vector.empty)

    val nestedElementGroups =
      nestedElements
        .groupBy { case (nestedNSName, _, _) => nestedNSName }
        .map { case (nestedNSName, subNSElems) =>
          val nestedElements = subNSElems.map { case (_, nestedNSPath, binding) => ModuleElement(nestedNSPath, binding) }

          val nestedNS = createNamespaceWithPath(NamespacePath(path.ns :+ nestedNSName), nestedElements)
          GlobalBinding.NestedNamespace(GlobalName.Normal(nestedNSName), nestedNS)
        }
        .toVector

    Namespace(path, nestedElementGroups ++ directElements)
  }

  private def divideNamespaceElements[TContext <: Context, TPayloadSpec[_, _]]
  (elements: Vector[ModuleElement[TContext, TPayloadSpec]])
  (accDirect: Vector[GlobalBinding[TContext, TPayloadSpec]], accNested: Vector[(String, NamespacePath, GlobalBinding[TContext, TPayloadSpec])])
  : (Vector[GlobalBinding[TContext, TPayloadSpec]], Vector[(String, NamespacePath, GlobalBinding[TContext, TPayloadSpec])]) =
    elements match {
      case ModuleElement(NamespacePath(headNS +: tailNS), binding) +: tail =>
        divideNamespaceElements(tail)(accDirect, accNested :+ ((headNS, NamespacePath(tailNS), binding)))

      case ModuleElement(NamespacePath(Vector()), binding) +: tail =>
        divideNamespaceElements(tail)(accDirect :+ binding, accNested)

      case Vector() =>
        (accDirect, accNested)
    }

}
