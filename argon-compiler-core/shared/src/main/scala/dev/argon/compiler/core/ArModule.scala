package dev.argon.compiler.core

import PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.{Comp, CompStream}
import dev.argon.util.NamespacePath
import cats.implicits._
import zio.stream.Stream

trait ArModule[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext

  val id: ModuleId
  val namespaces: CompStream[NamespacePath]
  val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]

  def getNamespace(ns: NamespacePath): CompStream[GlobalBinding.NonNamespace[context.type, TPayloadSpec]]

  def bindings: CompStream[GlobalBinding.NonNamespace[context.type, TPayloadSpec]] =
    namespaces
      .flatMap(getNamespace)
      .collect {
        case binding: GlobalBinding.NonNamespace[context.type, TPayloadSpec] => binding
      }

  def lookupNamespaceBindings[T]
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding.NonNamespace[context.type, TPayloadSpec] => Comp[Option[T]])
  : CompStream[T] =
    getNamespace(namespace)
      .filter { binding => binding.name === name }
      .mapM(f)
      .collectSome

  def lookupNamespaceValues[T]
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding[context.type, TPayloadSpec] => Comp[Option[T]])
  : CompStream[T] =
    lookupNamespaceBindings(namespace, name)(f)
      .concat(
        name match {
          case GlobalName.Normal(name) =>
            namespaces
              .map { otherNS => detectChildNamespaceName(namespace.ns, otherNS.ns) }
              .collectSome
              .filter { childNSName => childNSName === name }
              .mapM { childNSName =>
                f(GlobalBinding.NestedNamespace(GlobalName.Normal(childNSName)))
              }
              .collectSome

          case _ => Stream.empty
        }
      )

  private def detectChildNamespaceName(parentNS: Seq[String], childNS: Seq[String]): Option[String] =
    (parentNS, childNS) match {
      case (_, Seq()) => None
      case (Seq(), Seq(name)) => Some(name)
      case (h1 +: t1, h2 +: t2) if h1 === h2 => detectChildNamespaceName(t1, t2)
      case _ => None
    }

}

final case class ModuleElement[TContext <: Context with Singleton, TPayloadSpec[_, _]](namespacePath: NamespacePath, binding: GlobalBinding.NonNamespace[TContext, TPayloadSpec])
