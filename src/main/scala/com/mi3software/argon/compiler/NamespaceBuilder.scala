package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

object NamespaceBuilder {

  def createNamespace[Types <: ScopeTypes](elements: Vector[ModuleElement[ScopeValue[Types]]]): Namespace[ScopeValue[Types]] =
    createNamespaceWithPath(NamespacePath.empty, elements)

  def createNamespaceWithPath[Types <: ScopeTypes](path: NamespacePath, elements: Vector[ModuleElement[ScopeValue[Types]]]): Namespace[ScopeValue[Types]] = {
    val (directElements, nestedElements) = divideNamespaceElements(elements)(Vector.empty, Vector.empty)

    val nestedElementGroups =
      nestedElements
        .groupBy { case (nestedNSName, _, _) => nestedNSName }
        .map { case (nestedNSName, subNSElems) =>
          val nestedElements = subNSElems.map { case (_, nestedNSPath, binding) => ModuleElement(nestedNSPath, binding) }

          val nestedNS = createNamespaceWithPath(NamespacePath(path.ns :+ nestedNSName), nestedElements)
          NamespaceBinding(GlobalName.Normal(nestedNSName), AccessModifier.Public, NamespaceScopeValue(nestedNS))
        }
        .toVector

    Namespace(path, nestedElementGroups ++ directElements)
  }

  private def divideNamespaceElements[TScopeValue]
  (elements: Vector[ModuleElement[TScopeValue]])
  (accDirect: Vector[NamespaceBinding[TScopeValue]], accNested: Vector[(String, NamespacePath, NamespaceBinding[TScopeValue])])
  : (Vector[NamespaceBinding[TScopeValue]], Vector[(String, NamespacePath, NamespaceBinding[TScopeValue])]) =
    elements match {
      case ModuleElement(NamespacePath(headNS +: tailNS), binding) +: tail =>
        divideNamespaceElements(tail)(accDirect, accNested :+ ((headNS, NamespacePath(tailNS), binding)))

      case ModuleElement(NamespacePath(Vector()), binding) +: tail =>
        divideNamespaceElements(tail)(accDirect :+ binding, accNested)

      case Vector() =>
        (accDirect, accNested)
    }

}
