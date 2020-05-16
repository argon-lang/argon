package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.NamespacePath

import scala.collection.immutable.Vector
import cats._
import cats.implicits._
import dev.argon.compiler.{Comp, Compilation}
import zio.interop.catz.core._

object ModuleLookup {

  def lookupValues[T, TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleDescriptor)
  (namespace: NamespacePath, name: GlobalName)
  (f: PartialFunction[GlobalBinding[context.type, TPayloadSpec], T])
  : Comp[Vector[T]] =
    findModule(context)(referencedModules)(moduleDesc).toList.toVector.flatTraverse { module =>
      lookupNamespaceValues(context)(module)(namespace, name)(f)
    }

  def findModule[TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleDescriptor)
  : Option[ArModule[context.type, TPayloadSpec]] =
    referencedModules.find { _.descriptor === moduleDesc }

  def lookupNamespaceValues[T, TPayloadSpec[_, _]]
  (context: Context)
  (module: ArModule[context.type, TPayloadSpec])
  (namespace: NamespacePath, name: GlobalName)
  (f: PartialFunction[GlobalBinding[context.type, TPayloadSpec], T])
  : Comp[Vector[T]] = {
    def impl(namespaceParts: Vector[String])(namespaceValues: Namespace[context.type, TPayloadSpec]): Vector[T] =
      namespaceParts match {
        case head +: tail =>
          namespaceValues.bindings.filter(_.name === GlobalName.Normal(head)).flatMap {
            case GlobalBinding.NestedNamespace(_, nestedNS) => impl(tail)(nestedNS)
            case _ => Vector()
          }

        case Vector() =>
          namespaceValues.bindings
            .filter { _.name === name }
            .collect(f)
      }

    module.globalNamespace.map(impl(namespace.ns))
  }

  def lookupGlobalClass[TContext <: Context with Singleton, TPayloadSpec[_, _]]: PartialFunction[GlobalBinding[TContext, TPayloadSpec], ArClass[TContext, TPayloadSpec]] =  {
    case GlobalBinding.GlobalClass(_, _, arClass) => arClass
  }



}
