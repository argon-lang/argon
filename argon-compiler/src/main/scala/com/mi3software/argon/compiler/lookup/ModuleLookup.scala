package com.mi3software.argon.compiler.lookup

import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.NamespacePath

import scala.collection.immutable.Vector
import scalaz._
import Scalaz._

object ModuleLookup {

  def lookupValue[T, TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleDescriptor)
  (namespace: NamespacePath, name: GlobalName)
  (f: PartialFunction[GlobalBinding[context.type, TPayloadSpec], T])
  : Option[T] =
    findModule(context)(referencedModules)(moduleDesc).flatMap { module =>
      lookupNamespaceValue(context)(module)(namespace, name)(f)
    }

  def findModule[TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleDescriptor)
  : Option[ArModule[context.type, TPayloadSpec]] =
    referencedModules.find { _.descriptor === moduleDesc }

  def lookupNamespaceValue[T, TPayloadSpec[_, _]]
  (context: Context)
  (module: ArModule[context.type, TPayloadSpec])
  (namespace: NamespacePath, name: GlobalName)
  (f: PartialFunction[GlobalBinding[context.type, TPayloadSpec], T])
  : Option[T] = {
    def impl(namespaceParts: Vector[String])(namespaceValues: Namespace[context.type, TPayloadSpec]): Option[T] =
      namespaceParts match {
        case head +: tail =>
          namespaceValues.bindings.find(_.name === GlobalName.Normal(head)) match {
            case Some(binding) =>
              binding match {
                case GlobalBinding.NestedNamespace(_, nestedNS) => impl(tail)(nestedNS)
                case _ => None
              }

            case None => None
          }

        case Vector() =>
          namespaceValues.bindings
            .find { _.name === name }
            .collect(f)
      }

    impl(namespace.ns)(module.globalNamespace)
  }

  def lookupGlobalClass[TContext <: Context, TPayloadSpec[_, _]]: PartialFunction[GlobalBinding[TContext, TPayloadSpec], ArClass[TContext, TPayloadSpec]] =  {
    case GlobalBinding.GlobalClass(_, _, arClass) => arClass
  }



}
