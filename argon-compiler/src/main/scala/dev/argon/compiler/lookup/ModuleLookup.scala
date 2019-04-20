package dev.argon.compiler.lookup

import dev.argon.compiler.core._
import dev.argon.util.NamespacePath

import scala.collection.immutable.Vector
import scalaz._
import Scalaz._
import dev.argon.compiler.Compilation

object ModuleLookup {

  def lookupValue[T, TPayloadSpec[_, _]]
  (context: Context)
  (referencedModules: Vector[ArModule[context.type, TPayloadSpec]])
  (moduleDesc: ModuleDescriptor)
  (namespace: NamespacePath, name: GlobalName)
  (f: PartialFunction[GlobalBinding[context.type, TPayloadSpec], T])
  : context.Comp[Option[T]] =
    findModule(context)(referencedModules)(moduleDesc).traverseM { module =>
      lookupNamespaceValue(context)(module)(namespace, name)(f)
    }(context.compCompilationInstance, implicitly)

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
  : context.Comp[Option[T]] = {
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

    context.compCompilationInstance.map(module.globalNamespace)(impl(namespace.ns))
  }

  def lookupGlobalClass[TContext <: Context with Singleton, TPayloadSpec[_, _]]: PartialFunction[GlobalBinding[TContext, TPayloadSpec], ArClass[TContext, TPayloadSpec]] =  {
    case GlobalBinding.GlobalClass(_, _, arClass) => arClass
  }



}
